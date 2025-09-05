# Efrit Docker Setup

## Build Commands

### Build the Docker image
```bash
docker build -t efrit:latest .
```

### Build with specific tag
```bash
docker build -t efrit:0.3.0 .
```

## Test Commands

### Run basic functionality test
```bash
docker run --rm efrit:latest emacs --batch --eval "(progn (add-to-list 'load-path \"~/lisp\") (require 'efrit-config) (message \"Efrit loaded successfully\"))"
```

### Run syntax check
```bash
docker run --rm efrit:latest emacs --batch --eval "(check-parens)" ~/lisp/efrit-config.el
```

### Test Emacs version
```bash
docker run --rm efrit:latest emacs --version
```

## Runtime Commands

### Run interactive container
```bash
docker run -it --rm efrit:latest bash
```

### Run with persistent data
```bash
docker run -it --rm -v efrit_data:/home/efrit/.emacs.d/.efrit efrit:latest bash
```

### Run daemon mode
```bash
docker run -d --name efrit-daemon -p 8080:8080 -v efrit_data:/home/efrit/.emacs.d/.efrit efrit:latest
```

### Access running container
```bash
docker exec -it efrit-daemon bash
```

## Development Commands

### Build and test in one command
```bash
docker build -t efrit:latest . && docker run --rm efrit:latest emacs --batch --eval "(progn (add-to-list 'load-path \"~/lisp\") (require 'efrit-config) (message \"Build and test successful\"))"
```

### Clean up containers and images
```bash
docker container prune -f
docker image prune -f
```

### Remove efrit images
```bash
docker rmi efrit:latest efrit:0.3.0
```

## Container Details

- **Base Image**: `cgr.dev/chainguard/wolfi-base:latest`
- **User**: `efrit` (non-root)
- **Working Directory**: `/home/efrit`
- **Data Directory**: `/home/efrit/.emacs.d/.efrit`
- **Emacs Version**: 30.2
- **Exposed Ports**: 8080

## Volume Management

### Create named volume
```bash
docker volume create efrit_data
```

### Backup volume
```bash
docker run --rm -v efrit_data:/data -v $(pwd):/backup alpine tar czf /backup/efrit_backup.tar.gz -C /data .
```

### Restore volume
```bash
docker run --rm -v efrit_data:/data -v $(pwd):/backup alpine tar xzf /backup/efrit_backup.tar.gz -C /data
```
