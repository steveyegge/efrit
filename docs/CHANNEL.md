# The Efrit Channel

A playwright-style channel into a live Emacs for external agents. One
bash command per interaction; every action returns a perception.

```bash
bin/efrit ping                  # is the channel alive?
bin/efrit eval '(find-file "~/notes.org")'
bin/efrit snapshot              # editor state only
bin/efrit eval -  <<'EOF'       # heredoc for multi-line elisp
(progn
  (goto-char (point-max))
  (insert "appended\n")
  (save-buffer))
EOF
```

## Design

The 2024-era efrit stack failed because external agents drove Emacs
blind: requests crossed multiple async hops (MCP server → file queue →
file watcher → inner LLM loop) and nothing returned the resulting
editor state. Models rationally fell back to `emacs --batch`.

The channel collapses this to a single synchronous hop:

```
agent → bin/efrit → emacsclient --eval → efrit-channel.el → JSON back
```

- **No escaping problems.** Elisp payloads and JSON responses cross the
  emacsclient boundary base64-encoded in both directions.
- **Every eval returns a snapshot**: selected buffer (name, file, mode,
  line/col, modified), text around point with `<|point|>` marking the
  cursor, window layout, buffer list, echo area.
- **Errors are data**: `{"ok":false,"error":{"type":"wrong-type-argument",
  "message":"..."}}` — including reader errors on unbalanced input.
- **`*Messages*` output during the eval is captured** and returned.
- **Self-healing**: the CLI probes the default emacsclient socket, then
  the `efrit` daemon, auto-starting it if nothing is reachable. The
  elisp side is auto-loaded on first use; `efrit reload` after editing.
- **Timeouts fail loudly**: if Emacs is busy past `-t SECONDS` (default
  30), you get `{"error":{"type":"timeout"}}`, exit 1 — never a hang.

`lisp/efrit-channel.el` is standalone — it requires nothing from the
rest of efrit and loads into any Emacs ≥ 28.

## Envelope

```json
{
  "ok": true,
  "value": "42",                      // prin1 of last form's value
  "error": null,                      // or {"type": ..., "message": ...}
  "messages": "Wrote /tmp/foo\n",     // new *Messages* output
  "snapshot": {
    "buffer":  {"name": ..., "file": ..., "mode": ..., "line": ..., "column": ...,
                "point": ..., "size": ..., "modified": ..., "read_only": ..., "narrowed": ...},
    "content": "line one\n<|point|>line two\n",
    "windows": [{"buffer": ..., "selected": true, "width": 80, "height": 23}],
    "buffers": [{"name": ..., "mode": ..., "file": ..., "modified": ...}],
    "echo":    null
  }
}
```

`--no-snapshot` slims eval responses when state isn't needed.

## Sockets

- `-s NAME` or `$EFRIT_SOCKET` pins a socket (auto-starts a daemon of
  that name if unreachable).
- Otherwise: default socket (your running Emacs) → `efrit` daemon →
  auto-start `efrit` daemon.
- `efrit stop` only kills explicitly named daemons, never the default
  session.

## Limits

Output is bounded: printed values 20k chars, snapshot content 8k,
messages 4k, 15 buffers, 60 lines around point. Tunable via the
`efrit-channel-*` defvars.
