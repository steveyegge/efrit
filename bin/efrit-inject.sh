#!/bin/bash
# efrit-inject.sh - Inject messages into an active Efrit session
#
# This script allows external processes (like Claude Code) to send
# guidance, context, or abort signals to a running Efrit session.
#
# Usage:
#   efrit-inject.sh <session-id> <type> <message>
#   efrit-inject.sh --find-active <type> <message>
#   efrit-inject.sh --list
#
# Types:
#   guidance  - Hints for Claude's next action
#   context   - Additional context information
#   abort     - Request graceful termination
#   priority  - Change task priority (message should be 0-3)
#
# Examples:
#   efrit-inject.sh async-20251125120000 guidance "Try using grep instead"
#   efrit-inject.sh --find-active abort "User cancelled the task"
#   efrit-inject.sh --list

set -e

EFRIT_DIR="${HOME}/.emacs.d/.efrit"
SESSIONS_DIR="${EFRIT_DIR}/sessions"

usage() {
    echo "Usage: efrit-inject.sh <session-id> <type> <message>"
    echo "       efrit-inject.sh --find-active <type> <message>"
    echo "       efrit-inject.sh --list"
    echo ""
    echo "Types: guidance, context, abort, priority"
    exit 1
}

list_sessions() {
    echo "Active sessions (directories with inject/ subdirectory):"
    if [ ! -d "$SESSIONS_DIR" ]; then
        echo "  (no sessions directory)"
        return
    fi

    for dir in "$SESSIONS_DIR"/*/; do
        [ -d "$dir" ] || continue
        if [ -d "${dir}inject" ]; then
            session_id=$(basename "$dir")
            progress_file="${dir}progress.jsonl"
            if [ -f "$progress_file" ]; then
                # Get last event timestamp
                last_event=$(tail -1 "$progress_file" 2>/dev/null | jq -r '.timestamp // "unknown"' 2>/dev/null || echo "unknown")
                echo "  $session_id (last: $last_event)"
            else
                echo "  $session_id"
            fi
        fi
    done
}

find_active_session() {
    # Find most recently active session by checking progress.jsonl timestamps
    local latest_session=""
    local latest_time=0

    [ -d "$SESSIONS_DIR" ] || return

    for dir in "$SESSIONS_DIR"/*/; do
        [ -d "$dir" ] || continue
        if [ -d "${dir}inject" ]; then
            progress_file="${dir}progress.jsonl"
            if [ -f "$progress_file" ]; then
                # Get file modification time
                if [ "$(uname)" = "Darwin" ]; then
                    mtime=$(stat -f %m "$progress_file" 2>/dev/null || echo 0)
                else
                    mtime=$(stat -c %Y "$progress_file" 2>/dev/null || echo 0)
                fi
                if [ "$mtime" -gt "$latest_time" ]; then
                    latest_time=$mtime
                    latest_session=$(basename "$dir")
                fi
            fi
        fi
    done

    echo "$latest_session"
}

inject_message() {
    local session_id="$1"
    local msg_type="$2"
    local message="$3"

    # Validate type
    case "$msg_type" in
        guidance|context|abort|priority)
            ;;
        *)
            echo "Error: Invalid type '$msg_type'. Must be: guidance, context, abort, priority" >&2
            exit 1
            ;;
    esac

    # Check session exists
    local inject_dir="${SESSIONS_DIR}/${session_id}/inject"
    if [ ! -d "$inject_dir" ]; then
        echo "Error: Session '$session_id' not found or has no inject directory" >&2
        echo "Run 'efrit-inject.sh --list' to see available sessions" >&2
        exit 1
    fi

    # Generate filename with timestamp (use random suffix for uniqueness on macOS)
    local timestamp=$(date +%Y%m%d%H%M%S)
    local random_suffix=$(printf '%04d' $((RANDOM % 10000)))
    local filename="${timestamp}${random_suffix}_${msg_type}.json"
    local filepath="${inject_dir}/${filename}"
    local temp_file=$(mktemp)

    # Create JSON content
    local iso_timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)
    cat > "$temp_file" << EOF
{
  "type": "${msg_type}",
  "message": $(printf '%s' "$message" | jq -Rs .),
  "timestamp": "${iso_timestamp}"
}
EOF

    # Atomic write
    mv "$temp_file" "$filepath"

    echo "Injected ${msg_type} message to session ${session_id}"
    echo "File: ${filepath}"
}

# Main
case "${1:-}" in
    --help|-h)
        usage
        ;;
    --list)
        list_sessions
        ;;
    --find-active)
        if [ $# -lt 3 ]; then
            echo "Error: --find-active requires <type> <message>" >&2
            usage
        fi
        session=$(find_active_session)
        if [ -z "$session" ]; then
            echo "Error: No active session found" >&2
            exit 1
        fi
        echo "Found active session: $session"
        inject_message "$session" "$2" "$3"
        ;;
    "")
        usage
        ;;
    *)
        if [ $# -lt 3 ]; then
            echo "Error: Missing arguments" >&2
            usage
        fi
        inject_message "$1" "$2" "$3"
        ;;
esac
