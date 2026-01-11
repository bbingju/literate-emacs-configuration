# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a **literate Emacs configuration** using org-babel. The main configuration lives in `README.org` which is loaded by `init.el` via `org-babel-load-file`. All configuration changes should be made to `README.org`, not to generated files.

## Architecture

### Configuration Loading

1. **init.el** - Minimal bootstrap that loads `README.org` via org-babel
2. **early-init.el** - Pre-initialization (MSYS2 PATH setup on Windows)
3. **README.org** - Main literate configuration (elisp code blocks with documentation)
4. **.custom.el** - Emacs customize system output (auto-generated)

### Custom Elisp Modules (`lisp/`)

- **fontutil.el** - Font scaling and preset management with CJK support
- **my-c-ts-mode.el** - Custom C/C++ tree-sitter mode with clang-format integration

### Key Systems

- **Package Management**: ELPA + MELPA with `use-package` (always-ensure enabled)
- **Completion**: vertico, orderless, marginalia, consult stack
- **LSP**: Eglot with language servers for bash, python, cmake, C/C++, Java
- **Git**: Magit with diff-hl gutter markers
- **Tree-sitter**: Enabled for supported languages, grammars in `tree-sitter/`

## Platform Detection

Platform-specific code uses these macros:
```elisp
(when-linux ...)
(when-windows ...)
(when-mac ...)
```

Primary development platform is **Windows with MSYS2 (UCRT64)**.

## Claude Integration

Two AI packages are configured:
- **claude-code-ide** - WebSocket MCP server for Claude CLI integration (C-c C-')
- **ai-code** - Backend abstraction with global menu (C-c i)

Manual Claude functions: `claude-vterm`, `claude-send-region`, `claude-ask`, `claude-project`

## Conventions

- Comments in English (per user preference)
- Korean language support is important (input methods, fonts, encoding)
- Configuration uses org-mode code blocks with `:tangle` directives where applicable
- Org files stored in `~/org` directory (agenda, capture, refile targets)

## Testing Changes

After modifying `README.org`, restart Emacs or:
1. Evaluate changed code blocks with `C-c C-c` in org-mode
2. Or run `M-x org-babel-load-file` on README.org

For elisp modules in `lisp/`:
```elisp
M-x load-file RET lisp/<module>.el
```

## Key Bindings Reference

| Binding | Function |
|---------|----------|
| C-x g | Magit status |
| C-c C-' | Claude Code IDE menu |
| C-c i | AI-Code global menu |
| M-x fontutil/set-font | Switch font preset |
