#!/bin/bash

# launch-autonomous-efrit.sh - Launch autonomous AI development environment
# 
# This script starts an isolated Emacs daemon with Efrit pre-loaded,
# enabling AI agents to develop and enhance Efrit autonomously.
#
# Usage:
#   ./launch-autonomous-efrit.sh [start|stop|status|restart]

set -e

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DAEMON_NAME="efrit-ai"
STARTUP_FILE="$SCRIPT_DIR/../lisp/efrit-autonomous-startup.el"
QUEUE_DIR="$HOME/.emacs.d/efrit-queue-ai"
WORK_DIR="$HOME/.emacs.d/efrit-ai-workspace"
LOG_FILE="$WORK_DIR/daemon.log"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if Emacs is available
check_emacs() {
    if ! command -v emacs &> /dev/null; then
        log_error "Emacs not found in PATH"
        exit 1
    fi
    log_info "Found Emacs: $(emacs --version | head -1)"
}

# Create necessary directories
setup_directories() {
    log_info "Setting up autonomous environment directories..."
    
    mkdir -p "$QUEUE_DIR/requests"
    mkdir -p "$QUEUE_DIR/responses" 
    mkdir -p "$WORK_DIR/backups"
    mkdir -p "$WORK_DIR/auto-saves"
    
    log_success "Directories created"
}

# Check if daemon is running
is_daemon_running() {
    emacs --eval "(server-running-p)" 2>/dev/null | grep -q "$DAEMON_NAME" || 
    pgrep -f "emacs.*daemon.*$DAEMON_NAME" > /dev/null
}

# Start the autonomous Efrit daemon
start_daemon() {
    log_info "Starting Efrit Autonomous AI Development Environment..."
    
    check_emacs
    setup_directories
    
    if is_daemon_running; then
        log_warning "Daemon '$DAEMON_NAME' is already running"
        return 0
    fi
    
    if [[ ! -f "$STARTUP_FILE" ]]; then
        log_error "Startup file not found: $STARTUP_FILE"
        exit 1
    fi
    
    log_info "Loading Efrit startup configuration from: $STARTUP_FILE"
    log_info "Queue directory: $QUEUE_DIR"
    log_info "Work directory: $WORK_DIR"
    log_info "Log file: $LOG_FILE"
    
    # Start Emacs daemon with Efrit configuration (skip user config)
    emacs --no-init-file \
          --daemon="$DAEMON_NAME" \
          --load "$STARTUP_FILE" \
          --eval "(setq efrit-autonomous-log-file \"$LOG_FILE\")" \
          > "$LOG_FILE" 2>&1 &
    
    # Wait for daemon to start
    local retries=0
    while ! is_daemon_running && [ $retries -lt 10 ]; do
        sleep 1
        retries=$((retries + 1))
        echo -n "."
    done
    echo
    
    if is_daemon_running; then
        log_success "Efrit Autonomous daemon started successfully"
        log_info "AI can now communicate via: $QUEUE_DIR"
        
        # Verify queue system is active
        sleep 2
        if [[ -d "$QUEUE_DIR/requests" ]] && [[ -d "$QUEUE_DIR/responses" ]]; then
            log_success "Queue system is active and ready"
        else
            log_warning "Queue directories not found - daemon may still be initializing"
        fi
    else
        log_error "Failed to start daemon. Check log: $LOG_FILE"
        exit 1
    fi
}

# Stop the daemon
stop_daemon() {
    log_info "Stopping Efrit Autonomous daemon..."
    
    if ! is_daemon_running; then
        log_warning "Daemon '$DAEMON_NAME' is not running"
        return 0
    fi
    
    # Try graceful shutdown first
    emacsclient --socket-name="$DAEMON_NAME" --eval "(efrit-autonomous-shutdown)" 2>/dev/null || true
    emacsclient --socket-name="$DAEMON_NAME" --eval "(kill-emacs)" 2>/dev/null || true
    
    # Wait for graceful shutdown
    local retries=0
    while is_daemon_running && [ $retries -lt 5 ]; do
        sleep 1
        retries=$((retries + 1))
        echo -n "."
    done
    echo
    
    # Force kill if necessary
    if is_daemon_running; then
        log_warning "Graceful shutdown failed, force killing..."
        pkill -f "emacs.*daemon.*$DAEMON_NAME" || true
        sleep 1
    fi
    
    if ! is_daemon_running; then
        log_success "Efrit Autonomous daemon stopped"
    else
        log_error "Failed to stop daemon"
        exit 1
    fi
}

# Show daemon status
show_status() {
    echo "=== Efrit Autonomous AI Development Environment Status ==="
    echo
    
    if is_daemon_running; then
        echo -e "Daemon Status: ${GREEN}RUNNING${NC}"
        
        # Try to get detailed status from daemon
        if emacsclient --socket-name="$DAEMON_NAME" --eval "(efrit-autonomous-status)" 2>/dev/null; then
            echo "Status retrieved from daemon"
        else
            echo "Daemon is running but not responding to status requests"
        fi
    else
        echo -e "Daemon Status: ${RED}STOPPED${NC}"
    fi
    
    echo
    echo "Configuration:"
    echo "  Daemon Name: $DAEMON_NAME"
    echo "  Queue Directory: $QUEUE_DIR"
    echo "  Work Directory: $WORK_DIR"
    echo "  Startup File: $STARTUP_FILE"
    echo "  Log File: $LOG_FILE"
    
    if [[ -d "$QUEUE_DIR" ]]; then
        echo
        echo "Queue Status:"
        local requests=$(find "$QUEUE_DIR/requests" -name "req_*.json" 2>/dev/null | wc -l)
        local responses=$(find "$QUEUE_DIR/responses" -name "resp_*.json" 2>/dev/null | wc -l)
        echo "  Pending Requests: $requests"
        echo "  Available Responses: $responses"
    fi
    
    if [[ -f "$LOG_FILE" ]]; then
        echo
        echo "Recent Log Entries:"
        tail -5 "$LOG_FILE" 2>/dev/null || echo "  (no log entries)"
    fi
}

# Test communication with daemon
test_communication() {
    log_info "Testing communication with autonomous Efrit daemon..."
    
    if ! is_daemon_running; then
        log_error "Daemon is not running. Start it first with: $0 start"
        exit 1
    fi
    
    # Create test request
    local test_id=$(date +%s)
    local test_request="$QUEUE_DIR/requests/req_test_${test_id}.json"
    
    log_info "Creating test request: $test_request"
    cat > "$test_request" << EOF
{
  "id": "test_${test_id}",
  "type": "eval",
  "code": "(message \"Autonomous AI communication test successful!\")",
  "timestamp": "$(date -Iseconds)"
}
EOF
    
    # Wait for response
    local response_file="$QUEUE_DIR/responses/resp_test_${test_id}.json"
    local retries=0
    
    log_info "Waiting for response..."
    while [[ ! -f "$response_file" ]] && [ $retries -lt 10 ]; do
        sleep 1
        retries=$((retries + 1))
        echo -n "."
    done
    echo
    
    if [[ -f "$response_file" ]]; then
        log_success "Communication test successful!"
        log_info "Response received:"
        cat "$response_file"
        
        # Clean up test files
        rm -f "$test_request" "$response_file"
    else
        log_error "Communication test failed - no response received"
        log_info "Check daemon status and log file: $LOG_FILE"
        exit 1
    fi
}

# Main command dispatcher
case "${1:-start}" in
    start)
        start_daemon
        ;;
    stop)
        stop_daemon
        ;;
    restart)
        stop_daemon
        sleep 2
        start_daemon
        ;;
    status)
        show_status
        ;;
    test)
        test_communication
        ;;
    *)
        echo "Usage: $0 [start|stop|restart|status|test]"
        echo
        echo "Commands:"
        echo "  start    - Start the autonomous Efrit daemon"
        echo "  stop     - Stop the autonomous Efrit daemon"
        echo "  restart  - Restart the autonomous Efrit daemon"
        echo "  status   - Show daemon and queue status"
        echo "  test     - Test communication with the daemon"
        echo
        echo "This launches an isolated Emacs instance where AI agents can"
        echo "develop and enhance Efrit autonomously via the queue system."
        exit 1
        ;;
esac
