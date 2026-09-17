#!/usr/bin/env python3
"""Stand-in for curl that emits a canned Anthropic SSE stream.

efrit-api-stream runs `curl --config FILE --data-binary @BODY ... URL`.
We read the request body, and decide the response from the *user
message text* so a test can pick a scenario without touching elisp:

  "text"      -> two text deltas, end_turn
  "tool"      -> a text block then a tool_use block whose input arrives
                 as three input_json_delta chunks, stop_reason tool_use
  "error"     -> an SSE `error` event (invalid model)
  "http401"   -> exit like curl --fail-with-body on a 401: print the JSON
                 error body, exit 22
  "truncate"  -> first delta then exit 28 (timeout) with no message_stop
  "slow"      -> like text but sleeps between deltas (for cancel tests)
  "cancel"    -> stream one delta then sleep 30s (test sends SIGINT)

Everything is written in small chunks with flushes so the elisp
filter sees partial events, as it would over a network.
"""
import json, os, sys, time

def arg_after(flag):
    a = sys.argv
    return a[a.index(flag) + 1] if flag in a else None

body_path = arg_after("--data-binary")
body = json.load(open(body_path[1:])) if body_path else {}
msgs = body.get("messages", [])
user = ""
if msgs:
    c = msgs[-1].get("content")
    user = c if isinstance(c, str) else json.dumps(c)
scenario = next((s for s in ("http401","truncate","cancel","slow","error","tool","text") if s in user), "text")

out = sys.stdout
def ev(name, obj):
    # deliberately split across writes to exercise reassembly
    s = f"event: {name}\ndata: {json.dumps(obj)}\n\n"
    mid = len(s) // 2
    out.write(s[:mid]); out.flush(); time.sleep(0.005)
    out.write(s[mid:]); out.flush()

def start():
    ev("message_start", {"type":"message_start","message":{
        "id":"msg_mock","type":"message","role":"assistant","model":body.get("model","mock"),
        "content":[],"stop_reason":None,
        "usage":{"input_tokens":12,"cache_read_input_tokens":3,"cache_creation_input_tokens":0,"output_tokens":0}}})

def text_block(idx, parts, delay=0.0):
    ev("content_block_start", {"type":"content_block_start","index":idx,"content_block":{"type":"text","text":""}})
    for p in parts:
        ev("content_block_delta", {"type":"content_block_delta","index":idx,"delta":{"type":"text_delta","text":p}})
        if delay: time.sleep(delay)
    ev("content_block_stop", {"type":"content_block_stop","index":idx})

def finish(stop, out_tokens=7):
    ev("message_delta", {"type":"message_delta","delta":{"stop_reason":stop,"stop_sequence":None},"usage":{"output_tokens":out_tokens}})
    ev("message_stop", {"type":"message_stop"})

if scenario == "http401":
    out.write(json.dumps({"type":"error","error":{"type":"authentication_error","message":"invalid x-api-key"}}))
    out.flush(); sys.exit(22)

start()
if scenario == "text":
    text_block(0, ["Hello, ", "world."]); finish("end_turn")
elif scenario == "slow":
    text_block(0, ["one ", "two ", "three"], delay=0.3); finish("end_turn")
elif scenario == "tool":
    text_block(0, ["Let me check."])
    ev("content_block_start", {"type":"content_block_start","index":1,"content_block":{"type":"tool_use","id":"toolu_mock1","name":"eval_sexp","input":{}}})
    for chunk in ['{"expr": "(+ 1', ' 2', ')"}']:
        ev("content_block_delta", {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":chunk}})
    ev("content_block_stop", {"type":"content_block_stop","index":1})
    finish("tool_use", 15)
elif scenario == "error":
    ev("error", {"type":"error","error":{"type":"invalid_request_error","message":"no keys found that support model: mock"}})
    sys.exit(0)
elif scenario == "truncate":
    ev("content_block_start", {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}})
    ev("content_block_delta", {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"partial answer"}})
    sys.exit(28)
elif scenario == "cancel":
    ev("content_block_start", {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}})
    ev("content_block_delta", {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"started"}})
    time.sleep(30)
