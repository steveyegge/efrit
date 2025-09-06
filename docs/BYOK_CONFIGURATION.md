# Efrit BYOK (Bring Your Own Key) Configuration

Efrit now supports flexible API key configuration to make it easy for users to provide their own Anthropic API keys.

## Configuration Methods

Efrit tries to find your API key in the following order:

### 1. Auth-source (Recommended)

The most secure method is to use Emacs's built-in auth-source system. Add to your `~/.authinfo` or `~/.authinfo.gpg`:

```
machine api.anthropic.com login personal password sk-ant-api03-YOUR-KEY-HERE
```

This is the default configuration and requires no additional setup.

### 2. Environment Variable

You can set the `ANTHROPIC_API_KEY` environment variable:

```bash
export ANTHROPIC_API_KEY="sk-ant-api03-YOUR-KEY-HERE"
```

Or configure Efrit to use a custom environment variable:

```elisp
(setq efrit-api-key 'MY_ANTHROPIC_KEY)
```

### 3. Direct Configuration

For testing or single-user systems, you can set the key directly:

```elisp
(setq efrit-api-key "sk-ant-api03-YOUR-KEY-HERE")
```

⚠️ **Warning**: This method stores your key in plain text in your Emacs configuration.

### 4. Dynamic Function

For advanced use cases, provide a function that returns the key:

```elisp
(setq efrit-api-key 
      (lambda () 
        (password-read "Enter Anthropic API Key: ")))
```

## Customization Options

### Auth-source Configuration

If you want to use a different host/user combination in your `.authinfo`:

```elisp
;; Use machine my-anthropic login myuser
(setq efrit-api-auth-source-host "my-anthropic")
(setq efrit-api-auth-source-user "myuser")
```

Then in `~/.authinfo`:
```
machine my-anthropic login myuser password sk-ant-api03-YOUR-KEY-HERE
```

## Getting an API Key

1. Visit [console.anthropic.com](https://console.anthropic.com)
2. Sign in or create an account
3. Navigate to API Keys
4. Create a new key
5. Copy the key (it starts with `sk-ant-api03-`)

## Troubleshooting

If you're having issues with API authentication:

1. Check your key is correctly formatted:
   ```elisp
   M-: (efrit-common-get-api-key) RET
   ```

2. Verify auth-source can find your entry:
   ```elisp
   M-: (auth-source-search :host "api.anthropic.com" :user "personal") RET
   ```

3. Enable debug logging:
   ```elisp
   (setq efrit-log-level 'debug)
   ```

## Example Configuration

Here's a complete example for your Emacs init file:

```elisp
;; Load Efrit
(add-to-list 'load-path "~/src/efrit/lisp")
(require 'efrit)

;; Option 1: Use default auth-source (recommended)
;; No additional configuration needed if using:
;; machine api.anthropic.com login personal password YOUR-KEY

;; Option 2: Use environment variable
;; (setq efrit-api-key 'ANTHROPIC_API_KEY)

;; Option 3: Use custom auth-source entry
;; (setq efrit-api-auth-source-host "my-anthropic")
;; (setq efrit-api-auth-source-user "steve")

;; Set up convenient keybinding
(global-set-key (kbd "C-c C-e") efrit-keymap)
```

## Security Best Practices

1. **Use auth-source**: Store keys in `~/.authinfo.gpg` for encryption
2. **Avoid plain text**: Don't commit API keys to version control
3. **Use environment variables**: Good for CI/CD or shared systems
4. **Rotate keys regularly**: Generate new keys periodically
5. **Limit key scope**: Use different keys for different projects if needed