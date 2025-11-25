#!/bin/bash
# efrit-tail.sh - Tail progress events from an Efrit session
#
# This script provides live streaming of progress events from a running
# Efrit session, with optional formatting for readability.
#
# Usage:
#   efrit-tail.sh [OPTIONS] [session-id]
#   efrit-tail.sh --list
#   efrit-tail.sh --find-active
#
# Options:
#   --raw       Output raw JSON lines (default: formatted)
#   --list      List available sessions with progress files
#   --find-active  Auto-detect and tail the most recent session
#   --follow    Keep following even after session ends (default: stop on session-end)
#
# If no session-id is given, tries to find the most recently active session.
#
# Examples:
#   efrit-tail.sh                        # Tail most recent session
#   efrit-tail.sh async-20251125120000   # Tail specific session
#   efrit-tail.sh --raw                  # Raw JSON output
#   efrit-tail.sh --list                 # Show available sessions

set -e

EFRIT_DIR="${HOME}/.emacs.d/.efrit"
SESSIONS_DIR="${EFRIT_DIR}/sessions"

RAW_OUTPUT=false
AUTO_FIND=false
FOLLOW_FOREVER=false
NO_FOLLOW=false

usage() {
    echo "Usage: efrit-tail.sh [OPTIONS] [session-id]"
    echo "       efrit-tail.sh --list"
    echo ""
    echo "Options:"
    echo "  --raw          Output raw JSON lines"
    echo "  --list         List available sessions"
    echo "  --find-active  Auto-detect most recent session (default if no session given)"
    echo "  --no-follow    Just print existing content, don't follow"
    echo "  --follow       Keep following even after session ends"
    exit 1
}

list_sessions() {
    echo "Sessions with progress files:"
    if [ ! -d "$SESSIONS_DIR" ]; then
        echo "  (no sessions directory)"
        return
    fi

    for progress_file in "$SESSIONS_DIR"/*/progress.jsonl; do
        [ -f "$progress_file" ] || continue
        session_dir=$(dirname "$progress_file")
        session_id=$(basename "$session_dir")

        # Get event count and last timestamp
        event_count=$(wc -l < "$progress_file" | tr -d ' ')
        last_event=$(tail -1 "$progress_file" 2>/dev/null | jq -r '.type // "?"' 2>/dev/null || echo "?")
        last_time=$(tail -1 "$progress_file" 2>/dev/null | jq -r '.timestamp // "?"' 2>/dev/null || echo "?")

        printf "  %-30s %4d events  last: %s (%s)\n" "$session_id" "$event_count" "$last_event" "$last_time"
    done
}

find_active_session() {
    local latest_session=""
    local latest_time=0

    [ -d "$SESSIONS_DIR" ] || return

    for progress_file in "$SESSIONS_DIR"/*/progress.jsonl; do
        [ -f "$progress_file" ] || continue
        session_dir=$(dirname "$progress_file")
        session_id=$(basename "$session_dir")

        # Get file modification time
        if [ "$(uname)" = "Darwin" ]; then
            mtime=$(stat -f %m "$progress_file" 2>/dev/null || echo 0)
        else
            mtime=$(stat -c %Y "$progress_file" 2>/dev/null || echo 0)
        fi
        if [ "$mtime" -gt "$latest_time" ]; then
            latest_time=$mtime
            latest_session=$session_id
        fi
    done

    echo "$latest_session"
}

format_event() {
    # Format a single JSON line for human readability
    local line="$1"
    local event_type=$(echo "$line" | jq -r '.type // "?"')
    local timestamp=$(echo "$line" | jq -r '.timestamp // ""' | sed 's/.*T//' | sed 's/[-+][0-9:]*//')

    case "$event_type" in
        session-start)
            local command=$(echo "$line" | jq -r '.command // "?"')
            printf "\033[1;36m[%s] === SESSION START ===\033[0m\n" "$timestamp"
            printf "  Command: %s\n" "$command"
            ;;
        session-end)
            local success=$(echo "$line" | jq -r '.success // false')
            if [ "$success" = "true" ]; then
                printf "\033[1;32m[%s] === SESSION COMPLETE ===\033[0m\n" "$timestamp"
            else
                printf "\033[1;31m[%s] === SESSION FAILED ===\033[0m\n" "$timestamp"
            fi
            ;;
        tool-start)
            local tool=$(echo "$line" | jq -r '.tool // "?"')
            local repeat=$(echo "$line" | jq -r '.repeat_count // 1')
            if [ "$repeat" -gt 2 ]; then
                printf "\033[33m[%s] ▶ %s (repeat #%d)\033[0m\n" "$timestamp" "$tool" "$repeat"
            else
                printf "\033[33m[%s] ▶ %s\033[0m\n" "$timestamp" "$tool"
            fi
            ;;
        tool-result)
            local tool=$(echo "$line" | jq -r '.tool // "?"')
            local success=$(echo "$line" | jq -r '.success // false')
            if [ "$success" = "true" ]; then
                printf "\033[32m[%s] ◀ %s ✓\033[0m\n" "$timestamp" "$tool"
            else
                printf "\033[31m[%s] ◀ %s ✗\033[0m\n" "$timestamp" "$tool"
            fi
            ;;
        text)
            local message=$(echo "$line" | jq -r '.message // ""' | head -c 200)
            local msg_type=$(echo "$line" | jq -r '.message_type // "info"')
            case "$msg_type" in
                claude)
                    printf "\033[34m[%s] 🤖 %s\033[0m\n" "$timestamp" "$message"
                    ;;
                error)
                    printf "\033[31m[%s] ❌ %s\033[0m\n" "$timestamp" "$message"
                    ;;
                *)
                    printf "[%s] %s\n" "$timestamp" "$message"
                    ;;
            esac
            ;;
        injection-received)
            local content_type=$(echo "$line" | jq -r '.content.type // "?"')
            local content_msg=$(echo "$line" | jq -r '.content.message // ""' | head -c 100)
            printf "\033[35m[%s] 📥 INJECTION [%s]: %s\033[0m\n" "$timestamp" "$content_type" "$content_msg"
            ;;
        *)
            printf "[%s] %s: %s\n" "$timestamp" "$event_type" "$(echo "$line" | jq -c 'del(.type, .timestamp, .seq, .session)')"
            ;;
    esac
}

tail_session() {
    local session_id="$1"
    local progress_file="${SESSIONS_DIR}/${session_id}/progress.jsonl"

    if [ ! -f "$progress_file" ]; then
        echo "Error: No progress file for session '$session_id'" >&2
        echo "Expected: $progress_file" >&2
        exit 1
    fi

    echo "Tailing session: $session_id"
    echo "File: $progress_file"
    echo "---"

    if [ "$RAW_OUTPUT" = "true" ]; then
        # Raw JSON mode
        if [ "$NO_FOLLOW" = "true" ]; then
            cat "$progress_file"
        elif [ "$FOLLOW_FOREVER" = "true" ]; then
            tail -f "$progress_file"
        else
            tail -f "$progress_file" | while read -r line; do
                echo "$line"
                # Check for session-end
                if echo "$line" | jq -e '.type == "session-end"' >/dev/null 2>&1; then
                    echo "---" >&2
                    echo "Session ended. Use --follow to keep tailing." >&2
                    break
                fi
            done
        fi
    else
        # Formatted mode - show existing content first
        while read -r line; do
            format_event "$line"
        done < "$progress_file"

        # Then follow new content (unless --no-follow)
        if [ "$NO_FOLLOW" = "true" ]; then
            return 0
        elif [ "$FOLLOW_FOREVER" = "true" ]; then
            tail -f -n 0 "$progress_file" | while read -r line; do
                format_event "$line"
            done
        else
            tail -f -n 0 "$progress_file" | while read -r line; do
                format_event "$line"
                if echo "$line" | jq -e '.type == "session-end"' >/dev/null 2>&1; then
                    echo "---"
                    echo "Session ended. Use --follow to keep tailing."
                    break
                fi
            done
        fi
    fi
}

# Parse arguments
SESSION_ID=""
while [ $# -gt 0 ]; do
    case "$1" in
        --help|-h)
            usage
            ;;
        --raw)
            RAW_OUTPUT=true
            shift
            ;;
        --list)
            list_sessions
            exit 0
            ;;
        --find-active)
            AUTO_FIND=true
            shift
            ;;
        --follow)
            FOLLOW_FOREVER=true
            shift
            ;;
        --no-follow)
            NO_FOLLOW=true
            shift
            ;;
        -*)
            echo "Unknown option: $1" >&2
            usage
            ;;
        *)
            SESSION_ID="$1"
            shift
            ;;
    esac
done

# Find session if not specified
if [ -z "$SESSION_ID" ]; then
    SESSION_ID=$(find_active_session)
    if [ -z "$SESSION_ID" ]; then
        echo "Error: No active session found" >&2
        echo "Run 'efrit-tail.sh --list' to see available sessions" >&2
        exit 1
    fi
    echo "Auto-detected session: $SESSION_ID"
fi

tail_session "$SESSION_ID"
