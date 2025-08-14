# Efrit Remote Queue System

This system enables AI-to-Efrit communication through file-based JSON messaging, allowing autonomous development and debugging.

## Setup

1. Start Efrit queue system in Emacs:
```elisp
;; In Emacs
(require 'efrit)
(efrit-remote-queue-start)  ; or C-c C-e q
```

2. Verify queue is running:
```elisp
(efrit-remote-queue-status)  ; or C-c C-e Q
```

The queue watches `~/.emacs.d/efrit-queue/requests/` for JSON files.

## Example 1: Simple Elisp Evaluation

### AI writes request:
```json
{
  "id": "req_001",
  "timestamp": "2025-01-13T10:30:00Z", 
  "type": "eval",
  "content": "(+ 40 2)"
}
```
Save to: `~/.emacs.d/efrit-queue/requests/req_001.json`

### Efrit responds:
```json
{
  "id": "req_001",
  "timestamp": "2025-01-13T10:30:01Z",
  "status": "success", 
  "result": "42",
  "execution_time": 0.001
}
```
Written to: `~/.emacs.d/efrit-queue/responses/resp_001.json`

## Example 2: Natural Language Commands

### AI writes request:
```json
{
  "id": "req_002",
  "type": "command",
  "content": "open dired in my downloads folder and split window horizontally"
}
```

### Efrit responds:
```json
{
  "id": "req_002", 
  "status": "success",
  "result": "Command executed successfully",
  "execution_time": 0.15
}
```

## Example 3: Complex Request with Context

### AI writes request:
```json
{
  "id": "req_003", 
  "type": "eval",
  "content": "(with-current-buffer \"*scratch*\" (insert \"Hello from AI!\") (buffer-string))",
  "options": {
    "return_context": true,
    "timeout": 10
  }
}
```

### Efrit responds:
```json
{
  "id": "req_003",
  "status": "success", 
  "result": ";; This buffer is for text that is not saved, and for Lisp evaluation.\\n;; To create a file, visit it with C-x C-f and enter text in its buffer.\\n\\nHello from AI!",
  "execution_time": 0.05,
  "context": "Current buffer: *scratch* (fundamental-mode)\\nBuffers: *scratch*, *Messages*\\nDirectory: /Users/stevey/src/efrit"
}
```

## Python Integration Example

Here's how an AI system in Python could use the queue:

```python
import json
import os
import time
from pathlib import Path

class EfritQueue:
    def __init__(self, queue_dir="~/.emacs.d/efrit-queue"):
        self.queue_dir = Path(queue_dir).expanduser()
        self.requests_dir = self.queue_dir / "requests"
        self.responses_dir = self.queue_dir / "responses"
    
    def send_request(self, request_type, content, options=None, timeout=30):
        """Send a request to Efrit and wait for response."""
        import uuid
        request_id = f"ai_{int(time.time())}_{uuid.uuid4().hex[:8]}"
        
        # Create request
        request = {
            "id": request_id,
            "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ"),
            "type": request_type,
            "content": content
        }
        if options:
            request["options"] = options
        
        # Write request file
        request_file = self.requests_dir / f"req_{request_id}.json"
        with open(request_file, 'w') as f:
            json.dump(request, f)
        
        # Wait for response
        response_file = self.responses_dir / f"resp_{request_id}.json"
        deadline = time.time() + timeout
        
        while time.time() < deadline:
            if response_file.exists():
                with open(response_file) as f:
                    response = json.load(f)
                
                # Clean up files (optional)
                request_file.unlink(missing_ok=True)
                response_file.unlink(missing_ok=True)
                
                return response
            
            time.sleep(0.1)
        
        raise TimeoutError(f"No response received within {timeout}s")

# Usage examples
efrit = EfritQueue()

# Example 1: Evaluate elisp
result = efrit.send_request("eval", "(* 6 7)")
print(f"6 * 7 = {result['result']}")

# Example 2: Execute command
result = efrit.send_request("command", "show me the current time")
print(f"Command result: {result['status']}")

# Example 3: Chat interaction
result = efrit.send_request("chat", "What files are in the current directory?")
print(f"Chat response: {result['result']}")
```

## Sourcegraph Amp Integration

This is exactly how Sourcegraph Amp could interact with Efrit to replace our current conversation:

```python
# In Sourcegraph Amp's code execution environment
def execute_in_efrit(elisp_code):
    """Execute elisp code in user's Emacs via Efrit queue."""
    efrit = EfritQueue()
    
    try:
        response = efrit.send_request("eval", elisp_code, timeout=30)
        
        if response['status'] == 'success':
            return response['result']
        else:
            raise Exception(f"Efrit error: {response.get('error', 'Unknown error')}")
            
    except TimeoutError:
        return "Error: Efrit request timed out"

def send_command_to_efrit(natural_language_command):
    """Send natural language command to Efrit."""
    efrit = EfritQueue()
    response = efrit.send_request("command", natural_language_command)
    return response

# Example usage in Amp conversation:
# User: "Can you split my emacs window and open dired?"
# Amp: 
result = send_command_to_efrit("split window horizontally and open dired in current directory")
print(f"✓ Executed: {result['status']}")

# User: "What's in my current buffer?"
buffer_content = execute_in_efrit("(buffer-string)")
print(f"Current buffer content: {buffer_content}")
```

## Benefits

1. **Asynchronous**: AI doesn't block Emacs
2. **Robust**: File-based communication is reliable
3. **Debuggable**: All requests/responses are logged as files
4. **Language Agnostic**: Any language can read/write JSON files
5. **Secure**: Only allows pre-approved efrit operations
6. **Scalable**: Handle multiple concurrent requests

## Next Steps

1. **Start the queue**: `C-c C-e q` in Emacs
2. **Test with simple requests**: Create JSON files manually first
3. **Integrate with AI system**: Use the Python example above
4. **Monitor with status**: `C-c C-e Q` to see queue activity
5. **Iterate**: Add more sophisticated request types as needed

This system enables **seamless AI-Efrit integration** while maintaining efrit's core "zero client-side intelligence" principle - all the smarts remain in Claude, accessed through this simple file-based channel.
