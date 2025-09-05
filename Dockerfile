# Dockerfile for Efrit - AI-Powered Emacs Assistant
# Multi-stage build using Wolfi Linux for security and minimal size

# Build stage
FROM cgr.dev/chainguard/wolfi-base:latest AS builder
USER root

# Install build dependencies (tar and gzip are part of busybox in wolfi)
RUN apk update && \
    apk add --no-cache \
    emacs \
    make \
    bash \
    coreutils \
    findutils \
    grep \
    sed \
    busybox

# Set working directory
WORKDIR /src

# Copy source files
COPY . .

# Build the project (skip tests due to dependency issues in container)
RUN make clean && \
    make compile || echo "Compilation warnings ignored for container build"

# Create distribution
RUN make dist

# Runtime stage
FROM cgr.dev/chainguard/wolfi-base:latest

# Install runtime dependencies
RUN apk update && \
    apk add --no-cache \
    emacs \
    bash \
    curl \
    ca-certificates \
    coreutils \
    findutils

# Create efrit user
RUN adduser -D -s /bin/bash efrit

# Set up efrit home directory
WORKDIR /home/efrit

# Copy built artifacts from builder stage
COPY --from=builder /src/efrit-*.tar.gz /tmp/
RUN cd /tmp && \
    tar -xzf efrit-*.tar.gz && \
    cp -r efrit-*/lisp /home/efrit/ && \
    cp -r efrit-*/bin /home/efrit/ && \
    rm -rf /tmp/efrit-* && \
    chmod +x /home/efrit/bin/*.sh

# Set up Emacs configuration directory
RUN mkdir -p /home/efrit/.emacs.d && \
    chown -R efrit:efrit /home/efrit

# Create efrit data directories
USER efrit
RUN mkdir -p /home/efrit/.emacs.d/.efrit/{cache,sessions,queues/{requests,processing,responses,archive},logs,context,workspace/{auto-saves,backups}}

# Set up basic Emacs configuration
RUN echo '(add-to-list '"'"'load-path "~/lisp")' > /home/efrit/.emacs.d/init.el && \
    echo '(require '"'"'efrit)' >> /home/efrit/.emacs.d/init.el && \
    echo '(setq efrit-data-directory "~/.emacs.d/.efrit")' >> /home/efrit/.emacs.d/init.el

# Expose ports for potential daemon mode
EXPOSE 8080

# Set environment
ENV HOME=/home/efrit
ENV EFRIT_DATA_DIRECTORY=/home/efrit/.emacs.d/.efrit

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD emacs --batch --eval "(progn (add-to-list 'load-path \"~/lisp\") (require 'efrit) (message \"Efrit OK\"))" || exit 1

# Default command - launch efrit in daemon mode
CMD ["bash", "-c", "emacs --daemon=efrit --eval '(progn (add-to-list '\"'\"'load-path \"~/lisp\") (require '\"'\"'efrit) (message \"Efrit daemon started\"))' && tail -f ~/.emacs.d/.efrit/logs/*.log 2>/dev/null || sleep infinity"]

# Volume for persistent data
VOLUME ["/home/efrit/.emacs.d/.efrit"]

# Labels for metadata
LABEL maintainer="Steve Yegge <steve.yegge@gmail.com>"
LABEL description="Efrit - AI-Powered Autonomous Emacs Assistant"
LABEL version="0.3.0"
LABEL org.opencontainers.image.source="https://github.com/steveyegge/efrit"
