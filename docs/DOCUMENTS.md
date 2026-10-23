# Documents from outside Emacs: Google Drive, Google Calendar, Confluence

efrit can read documents that live in other systems and bring them
into an analysis: the Google Doc a meeting recap links to, the notes
Calendar took for that meeting, the Confluence page with the design.
This document explains what the pieces are, how to set each one up,
and where the Google side needs to be switched on.

## What you get

With at least one source configured:

- `doc_fetch`, `doc_search` and `doc_sources` are tools the model can
  call in any conversation. `doc_search` finds documents by words and
  dates across every configured source; `doc_fetch` returns the text of
  one by URL or reference.
- In Gnus, `efrit-gnus` follows the document links in an article and
  puts the documents' text after the article in everything the model
  reads, then adds *related* documents: the ones a source finds by the
  subject's words near the article's date, and the ones the Calendar
  provider finds on the matching event.
- `gnus-treat-related-documents` (off by default) writes those related
  documents as a footnote into the article you read, with URLs, so you
  can follow them by hand; an analysis of that article reads the
  footnote back instead of searching again. `W R` in a summary applies
  it to any article.

Everything is read-only. Nothing here writes to Drive, Calendar or
Confluence.

## The pieces

| File | What it is |
|---|---|
| `efrit-auth.el` | Credentials from auth-source, OAuth2 through oauth2.el, authenticated HTTP. |
| `efrit-documents.el` | The source protocol, the session cache, related-document lookup, the `doc_*` tools. |
| `efrit-documents-gdrive.el` | Google Drive: Docs, Sheets, Slides, text files. Fetch and search. |
| `efrit-documents-gcalendar.el` | Google Calendar: not a source but a *provider* of related documents (an event's attachments). |
| `efrit-documents-confluence.el` | Confluence pages and blog posts, Atlassian Cloud and self-hosted. |
| `efrit-documents-jira.el` | Jira issues with their comments, through the jira.el package. Also a related-documents provider: issue keys an article mentions. |

`efrit-documents-sources-libraries` lists the libraries loaded when the
tools are registered (`efrit-documents-ensure-tools`, which the Gnus
commands run). Remove a library you do not use, or add your own.

## Credentials: one rule

Every source looks its credentials up in auth-source (`~/.authinfo.gpg`
or whatever `auth-sources` says) by a *host* name. The entry decides how
to authenticate:

- An entry with `client-id`, `client-secret`, `auth-url` and
  `token-url` is an OAuth2 client. efrit-auth runs the consent flow
  through oauth2.el (`oauth2-auth-and-store`), so the token is stored
  in the same plstore, refreshed the same way, and — when
  `oauth2-loopback` is loaded — the authorization code is caught on
  localhost as for Gnus and nngmail. The stored token is keyed by
  auth-url, token-url, scope, client-id and user, so an entry whose
  scope grows gets one new consent and is then cached.
- An entry with only `login` and `password` is a token: sent as
  `Authorization: Bearer PASSWORD`. Add `auth-type basic` to send
  HTTP Basic (`login:password`) instead, which Atlassian Cloud API
  tokens want.

Lookups run with auth-source's cache off and the xoauth2 plugin's
advice disabled for the call, so a negative cache entry or the plugin's
own token dance cannot get in the way.

Two commands help: `M-x efrit-auth-check HOST` says what the entry
looks like and whether a token can be obtained; `M-x
efrit-auth-reauthorize HOST` forgets the token in memory and runs the
consent flow again, which is what you do after adding a scope.

## Google: Drive and Calendar

### One entry, several scopes

The simplest setup is one Google OAuth client and one auth-source
entry whose `scope` lists everything you want, space-separated:

```
machine gmail login you@example.com
  client-id 1234-abcd.apps.googleusercontent.com
  client-secret GOCSPX-...
  auth-url https://accounts.google.com/o/oauth2/auth
  token-url https://oauth2.googleapis.com/token
  redirect-uri http://localhost:8999
  scope "https://mail.google.com/ https://www.googleapis.com/auth/drive.readonly https://www.googleapis.com/auth/calendar.events.readonly"
```

Drive is found through the host `gdrive` if such an entry exists, else
through the first of `efrit-documents-gdrive-auth-hosts` (`gmail`,
`gmail.com`, `imap.gmail.com`) whose scope contains a Drive scope.
Calendar does the same with host `gcalendar` and a Calendar scope
(`efrit-documents-gcalendar-auth-hosts` also tries `gdrive` first). So
if you already have a Gmail entry for nngmail or the xoauth2 plugin,
you add the two scopes to it and re-consent once; no second client, no
second entry.

If you prefer separate entries (a client that only ever sees Drive, or
a different account), name them `gdrive` and `gcalendar` and give each
its own scope.

The scopes:

| For | Scope | Why this one |
|---|---|---|
| Drive fetch and search | `https://www.googleapis.com/auth/drive.readonly` | Reads any file the account can open, no writes. `drive.file` would only see files the app itself created, which is none of them. |
| Calendar events | `https://www.googleapis.com/auth/calendar.events.readonly` | Reads events and their attachments. `calendar.readonly` also works and additionally lists calendars. |

### Where to enable the APIs in the Google Cloud console

A Google OAuth client belongs to a Cloud project, and the project must
have each API *enabled* before a token for it works; otherwise the
first request fails with `403 ... API has not been used in project ...
before or it is disabled` and a link. Do this once per project, in the
project that owns your client id:

1. Open https://console.cloud.google.com/ and pick the project (the
   project selector is at the top; the client id's prefix, the number
   before the dash, is the project number).
2. **APIs & Services → Library** (direct: https://console.cloud.google.com/apis/library).
   Search for and enable, one at a time:
   - **Google Drive API** (https://console.cloud.google.com/apis/library/drive.googleapis.com)
   - **Google Calendar API** (https://console.cloud.google.com/apis/library/calendar-json.googleapis.com)
   - Gmail API is already enabled if nngmail works.
3. **APIs & Services → OAuth consent screen** (in newer consoles:
   **Google Auth Platform → Data Access**,
   https://console.cloud.google.com/auth/scopes). Add the two scopes
   above to the app's scopes. For an app in *Testing* status with only
   you as a test user this is what makes the consent screen offer them;
   for an *Internal* app in a Workspace domain it is the same page.
   Non-sensitive scopes need no verification; `drive.readonly` is
   listed as *restricted* and `calendar.events.readonly` as
   *sensitive*, but neither needs Google's review while the app is
   Internal or in Testing.
4. Nothing changes on the **Credentials** page: the same OAuth client
   id and secret serve all three APIs. The redirect URI
   (`http://localhost:8999`) stays as it was.
5. **Workspace accounts only.** Your administrator may gate which
   third-party apps can use which scopes (Admin console → Security →
   Access and data control → API controls → App access control). If
   Drive or Calendar returns 403 with `access_denied` or
   `admin_policy_enforced` even though the project has the API enabled
   and you consented, that gate is closed for your client id; the admin
   has to mark the app *Trusted* or allow those scopes.

Then, in Emacs: edit the `scope` of the entry, and run `M-x
efrit-auth-reauthorize gmail` (or `M-x nngmail-reauthorize gmail`, same
effect). A browser tab asks you to consent to the new scopes. After
that:

- `M-x efrit-documents-gdrive-check` calls Drive's `about` endpoint and
  reports which auth-source host it used and as whom.
- `M-x efrit-documents-gcalendar-check` lists the calendars the token
  can read.

Both are on the efrit menu (`M-x efrit-menu`: `g` and `k`).

### What Drive does

Fetch: a metadata call (title, modified time; this drives the session
cache, so an unchanged document is never exported twice), then Drive's
export endpoint: Docs as Markdown for the model, as HTML for a renderer
(Gnus washing), Sheets as CSV, Slides as text; plain files are
downloaded as they are. Text is capped at `efrit-documents-max-chars`
(40k).

Search: `files.list` with a `q=` built from the words and dates:
`name contains` for title words (any of them; results are then ranked
by how many they share, so "Notes - Platform weekly" is found for a
recap titled "Recap: 2026-09-21 - Platform weekly"), `fullText
contains` for body words, a `modifiedTime` *or* `createdTime` window,
Google document types and text files only, across shared drives too.

Recognised URLs: `docs.google.com/document|spreadsheets|presentation/d/ID`,
`drive.google.com/file/d/ID`, `drive.google.com/open?id=ID`.

### What Calendar does, and why it exists

Calendar's "take meeting notes" creates a Doc and attaches it to the
event. The recap e-mail for the meeting does not link that Doc, and a
title search of Drive finds it only while the Doc and the meeting still
share a name. Meetings get renamed; the notes keep the old title, or
the other way round. The event keeps the attachment whatever either is
called, so `efrit-documents-gcalendar` goes by the event.

Two layers:

- `efrit-documents-gcalendar-attachments DAY PREDICATE` is the
  primitive: list every event of DAY (a time; the whole local calendar
  day) on `efrit-documents-gcalendar-calendars` (default `("primary")`;
  add a shared calendar's id from its *Integrate calendar* settings),
  keep those whose title satisfies PREDICATE, return their Drive
  attachments as documents. No scoring, no nearest-in-time.
- `efrit-documents-gcalendar-related` is the default provider on
  `efrit-documents-related-functions`: the day is a `YYYY-MM-DD` in the
  item's title, else its Date; an event matches when the item's title
  contains the event's title whole (case and punctuation aside).

### Related documents in Gnus: one function, yours to replace

Everything above is reached from Gnus through one variable,
`efrit-gnus-related-documents-function`. It is called with no
arguments in a buffer holding the article as delivered — headers, a
blank line, the decoded body — and returns a list of document plists
(`:source`, `:id`, `:title`, `:url`; no text). efrit fetches and
renders what it returns. The same function serves the analysis (each
article) and the footnote treatment (the displayed article).

The default, `efrit-gnus-related-documents-default`, builds an item
from Subject, Date, From/To/Cc and the body's links and asks
`efrit-documents-related`, which runs the providers (Calendar) and,
only with `efrit-documents-related-search` set, a title-word search of
every source. That search is off by default: it finds more and guesses
more.

Mail with a fixed subject format deserves an exact rule instead of the
default's guess. A recap bot that writes `Recap: DATE - TITLE`
gets, in your configuration:

```elisp
(defun my-related-documents ()
  (let ((subject (save-restriction (message-narrow-to-head)
                                   (or (message-fetch-field "Subject") ""))))
    (if (string-match "\\`Recap: \\([0-9-]+\\) - \\(.+\\)\\'" subject)
        (let* ((date (match-string 1 subject))
               (name (efrit-documents-gcalendar--normalize (match-string 2 subject))))
          (efrit-documents-gcalendar-attachments
           (date-to-time (concat date "T12:00:00"))
           (lambda (event-title)
             (equal (efrit-documents-gcalendar--normalize event-title) name))))
      (efrit-gnus-related-documents-default))))

(with-eval-after-load 'efrit-gnus
  (setq efrit-gnus-related-documents-function #'my-related-documents))
```

That day's events, the one with exactly that title, its notes. Nothing
else is looked at.

## Confluence

One source per site, from `efrit-documents-confluence-sites`:

```elisp
(setq efrit-documents-confluence-sites
      '(("https://example.atlassian.net/wiki" . "example.atlassian.net")   ; Atlassian Cloud: keep /wiki
        ("https://wiki.example.com" "wiki.example.com" "intranet")))       ; self-hosted, given a name
```

Each entry is the web root and the auth-source host; an optional third
element names the source (default `confluence`, `confluence-2`, ...).

Credentials:

- **Atlassian Cloud**: an API token from
  https://id.atlassian.com/manage-profile/security/api-tokens, sent as
  HTTP Basic with your e-mail. The entry needs `auth-type basic`:
  ```
  machine example.atlassian.net login you@example.com password ATATT3x... auth-type basic
  ```
  No admin step is needed for reading with your own account; the token
  has your permissions.
- **Server / Data Center 7.9 and later**: a personal access token
  (profile picture → Settings → Personal Access Tokens), sent as Bearer,
  which is the default for a plain entry:
  ```
  machine wiki.example.com login you password NjE2...
  ```
  A server without PATs takes your password with `auth-type basic`.
- An OAuth2 entry works too (Atlassian 3LO), but is more setup for the
  same access.

What it does: fetch `content/ID` with `body.storage` (Confluence's
XHTML) and the version; the XHTML is rendered to text with shr for the
model (code-block macros keep their content), or handed over as HTML
for a renderer. Search is CQL: `title ~` / `text ~` words within
`lastmodified` bounds, pages and blog posts, newest first. URLs
recognised: `/spaces/KEY/pages/ID/...`, `viewpage.action?pageId=ID`,
`/display/KEY/Title` (one title lookup), `/x/TINY` (one authenticated
HEAD to follow the redirect).

`M-x efrit-documents-confluence-check` (menu `w`) calls `/user/current`
on each site and says who you are, or why not.

## Jira

Jira comes through [jira.el](https://github.com/unmonoqueteclea/jira.el),
which already holds the connection: set `jira-base-url` (and
`jira-api-version 2` for a self-hosted server) and put the token in
auth-source under the bare host, as jira.el's own README describes.
Nothing else is configured; without jira.el the source registers as
unavailable with that reason (`doc_sources` shows it).

What it does: `doc_fetch` on a key (`INFRA-123`, `jira:INFRA-123`) or a
`/browse/KEY` URL returns the issue as text — key, type,
status, priority, assignee, sprint, components, versions, labels,
links, description — followed by every comment with author and date.
`doc_search` words become `text ~` clauses; the model's `jira_search`
tool takes raw JQL. As a related-documents provider it takes the issue
keys mentioned in an article's subject or body and adds those issues,
exact match only. `M-x efrit-documents-jira-check` (menu `j`) calls
`myself`.

## Gnus

`efrit-gnus` uses the layer in three places:

- **Links in an article.** Every URL in the body that a source claims is
  fetched and appended as `--- Linked document: URL ---` followed by the
  text, up to `efrit-gnus-expand-links-max` (5) per article. Backends
  may add expanders for other links through
  `efrit-gnus-expand-link-functions`; nngmail withdraws its own Google
  Docs expander when the Drive source is present.
- **Related documents.** With `efrit-gnus-related-documents` (default
  t) each article also carries `--- Related document (SOURCE): URL ---`
  blocks, from `efrit-gnus-related-documents-function` (above). Set it
  to nil for large analyses of unrelated mail.
- **The footnote.** `gnus-treat-related-documents` is a washing
  treatment like `gnus-treat-buttonize`. It takes the usual values
  (nil, t, a list of group regexps, ...); default nil. Where it is on,
  the article ends with

  ```
  Related documents:
  - Notes: Widget bringup prep (gdrive, 2026-09-15) https://docs.google.com/document/d/.../edit
  ```

  with the URLs buttonized. When you then ask efrit about the displayed
  article, the footnote is read back from the article buffer: its
  documents are fetched as links and no search runs. `W R` applies it by
  hand.

A reasonable configuration for a label of meeting recaps:

```elisp
(with-eval-after-load 'efrit-gnus
  (setq gnus-treat-related-documents '("recaps")))
```

## Debugging

Set `efrit-log-level` to `debug` (`M-x efrit-log-toggle-debug`, or `G`
on the efrit menu) and open the log (`M-x efrit-log-show`). A related-
documents lookup writes one line per step:

```
documents: related for title="Recap: 2026-09-21 - Platform weekly" -> words=("platform") date=... linked=(("gdrive" . "1Czw...")) providers=(efrit-documents-gcalendar-related) sources=("gdrive")
documents: gcalendar primary 2026-09-21T04:00:00Z..2026-09-22T04:00:00Z -> 6 events: "Standup", "Platform weekly"*, "Lunch", ...
documents: gcalendar event "Platform weekly" at 2026-09-21T14:00:00Z: matches, 1 attachment(s)
documents: provider gcalendar -> 1: "Notes - Platform weekly"
documents: related result 1: gdrive:N1 "Notes - Platform weekly"
efrit-gnus: related footnote for "Recap: ..." (treatment): 1 document(s)
```

A provider or source that fails is a `WARN` line with the reason, and
`W R` by hand says the same in the echo area: "no related documents for
\"platform\" (gcalendar: no auth-source entry for Calendar ...)".
With `efrit-documents-related-search` on, `gdrive q=...` lines show the
exact Drive query and its hits.

## A worked setup: self-hosted Confluence and Jira

Two Data Center sites, `wiki.example.com` (pages like
`https://wiki.example.com/spaces/SYS/pages/101510469/Some+Page`) and
`jira.example.com`, both taking personal access tokens.

1. Mint a PAT on each site: profile picture → *Settings* → *Personal
   Access Tokens* → *Create token*. Read scope is enough; nothing here
   writes.
2. Put them in auth-source under the bare host names. `.authinfo.gpg`:
   ```
   machine wiki.example.com login jdoe password <confluence PAT>
   machine jira.example.com login jdoe password <jira PAT>
   ```
   or the JSON form (`~/.authinfo.json.gpg`):
   ```json
   [{"machine": "wiki.example.com", "login": "jdoe", "password": "<confluence PAT>"},
    {"machine": "jira.example.com", "login": "jdoe", "password": "<jira PAT>"}]
   ```
   No `auth-type`: a PAT goes as `Bearer`, which is the default. (An
   Atlassian Cloud API token would add `auth-type basic`.)
3. Tell efrit where Confluence is (Data Center: no `/wiki`):
   ```elisp
   (setq efrit-documents-confluence-sites
         '(("https://wiki.example.com" . "wiki.example.com")))
   ```
4. Tell jira.el where Jira is; efrit's Jira source reads the same
   variables:
   ```elisp
   (setq jira-base-url "https://jira.example.com"
         jira-api-version 2
         jira-token-is-personal-access-token t)
   ```
5. Check both: `M-x efrit-documents-confluence-check` should say
   `confluence (https://wiki.example.com) works as Jane Doe`, and
   `M-x efrit-documents-jira-check` `Jira at jira.example.com works as
   Jane Doe`. A 401 on either means the PAT is wrong or expired; a 403
   on Confluence's `/rest/api/user/current` means the token is valid but
   the REST API is disabled for your user — ask the admin.
6. Try it: in the efrit REPL, "what does
   https://wiki.example.com/spaces/SYS/pages/101510469/Some+Page say
   about rollout?" — the model calls `doc_fetch` on the URL. In Gnus,
   an article that links that page or mentions `INFRA-123` gets both
   appended when you `L A a` it.

`M-x efrit-auth-describe wiki.example.com` lists what auth-source holds
for a host, secrets hidden, if a check fails for a reason you cannot
see.

## Failure messages

Errors are turned into one line naming what to fix:

- `the token lacks a scope (403): ...; add it to the auth-source entry and run M-x efrit-auth-reauthorize` — the scope is missing, or was added to the entry but not consented to yet.
- `access refused (403): ... API has not been used in project ...` — enable the API in the console (above).
- `access refused (403): ... admin_policy_enforced` — the Workspace admin gate (above).
- `no auth-source entry for Drive: tried hosts ...` — none of the candidate hosts has an entry with a Drive scope; add the scope or an entry named `gdrive`.
- `not found (404)` — a deleted document, or an id that is not a Google Doc.
- `Drive refuses to export ... in any text format (over its 10 MB export limit)` — Drive caps every export at 10 MB. A Doc with embedded images exceeds it as HTML, so HTML falls back to Markdown, then plain text (the images are dropped, the words stay); this message means even the plain text is over the cap.

`doc_sources` (or `M-x efrit-documents-list-sources`) shows each
source and, when a check command found it unusable, why.

## Writing another source

A source is a subclass of `efrit-document-source` with methods for
`efrit-documents-source-match` (URL → id or nil),
`efrit-documents-source-fetch` (id, format → document plist with
`:text`), optionally `efrit-documents-source-metadata` (cheap title and
`:modified`, for the cache) and `efrit-documents-source-search` (query
plist → document plists). Register with `efrit-documents-register`.
Talk to the API through `efrit-auth-request`; turn its errors into
`efrit-documents-error` with `efrit-auth-explain`. A *provider* of
related documents that is not itself a source (like Calendar) is a
function on `efrit-documents-related-functions` returning document
plists of some registered source. `efrit-documents-confluence.el` is
the shortest complete example of a source.
