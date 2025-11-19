# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a **literate Emacs configuration** using org-mode. The entire Emacs configuration is written in `README.org` as a self-documenting file that combines both documentation (in Korean and English) and executable Emacs Lisp code blocks.

## Architecture

### Loading Mechanism

1. Emacs starts and loads `init.el` (7 lines)
2. `init.el` requires org-mode and calls `org-babel-load-file` on `README.org`
3. Org-mode extracts all `#+BEGIN_SRC emacs-lisp` code blocks and executes them sequentially
4. Packages are automatically installed via `use-package` with `:ensure t` (auto-ensure is enabled globally)

**Important**: There is no traditional build system. The configuration is "executed" at Emacs startup by tangling and loading the org file.

### Directory Structure

```
.
├── README.org              # Master literate configuration (~1300 lines)
├── init.el                 # Bootstrap loader (loads README.org)
├── lisp/                   # Custom Emacs Lisp libraries
│   ├── fontutil.el        # Font management utilities (mixed fonts, ligatures, scaling)
│   └── my-c-ts-mode.el    # C/C++ Tree-Sitter config (TAB handling, clang-format integration)
├── ditaa.jar              # DiTaa diagram generation for org-mode
└── tree-sitter/           # Tree-sitter language grammar modules (populated after install)
```

### Configuration Sections (README.org)

1. **First of All** - Startup optimization, package management, custom macros (`when-linux`, `when-mac`, `when-windows`)
2. **Appearance** - Themes (doom-themes), modeline (doom-modeline), fonts, line numbers
3. **Org Mode** - Task management, agenda, capture templates, babel languages
4. **Programming** - Tree-Sitter, Projectile, Eglot (LSP), Magit, language-specific configs
5. **Docker** - Docker and docker-compose support
6. **Tools** - markdown-mode, deft, Google Translate, Claude CLI integration

## Development Commands

### Modifying Configuration

- **Edit**: Modify `README.org` directly
- **Reload**: Restart Emacs or evaluate specific code blocks with `C-c C-c` in org-mode
- **Evaluate buffer**: `<f6>` in Emacs Lisp buffers

### Tree-Sitter Language Grammars

Tree-sitter grammars must be installed manually. The configuration defines sources in `treesit-language-source-alist` for:
- bash, cmake, css, elisp, go, html, javascript, json, make, python, rust, toml, tsx, typescript, yaml

**Install all grammars**:
```emacs-lisp
;; In ielm (M-x ielm):
(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
```

**Install single grammar**:
```
M-x treesit-install-language-grammar
```

### Package Management

- **Repository**: MELPA
- **Refresh packages**: `M-x package-refresh-contents`
- **Auto-install**: Packages with `:ensure t` are automatically installed on first load
- **Global auto-ensure**: Enabled via `(setq use-package-always-ensure t)`

## Language Server Protocol (Eglot)

Configured LSP servers (in `eglot-server-programs`):

- **Bash**: `bash-language-server start` (for bash-ts-mode, sh-mode)
- **Python**: `jedi-language-server` (for python-ts-mode, python-mode)
- **CMake**: `cmake-language-server` (for cmake-ts-mode, cmake-mode)
- **C/C++**: `clangd` (for c-ts-mode, c++-ts-mode)
- **Rust**: `~/.cargo/bin/rust-analyzer` with clippy and cargo buildScripts enabled

**Auto-start Eglot**: Enabled for bash-ts-mode and cmake-ts-mode via `:hook`

**Format code**: `C-M-\` (eglot-format)

## C/C++ Development (my-c-ts-mode.el)

The `lisp/my-c-ts-mode.el` library provides:
- Real TAB characters with 8-space visual width
- Smart TAB integration: if `.clang-format` exists in project and `clang-format` is available, TAB runs `clang-format-buffer`
- Optional format-on-save: controlled by `my/c-ts-mode-format-on-save` (default: nil)
- Toggle format-on-save: `M-x my/toggle-c-ts-format-on-save`

## Claude CLI Integration

The configuration includes custom Claude CLI integration (recently added). Functions available:

- `claude-vterm`: Start Claude CLI in vterm (`C-c t` opens vterm)
- `claude-send-region`: Send selected region to Claude CLI with `--print`
- `claude-send-buffer`: Send entire buffer to Claude CLI
- `claude-ask`: Ask Claude a question interactively
- `claude-project`: Start Claude CLI in project root (uses projectile if available)

**Requirements**:
- Claude CLI must be installed: https://docs.anthropic.com/en/docs/claude-code/setup
- `libvterm` must be installed system-wide (Ubuntu: `sudo apt install libvterm-dev`)

## Platform Considerations

The configuration supports Linux, macOS (Darwin), and Windows with platform-specific macros:
- `when-linux`: Execute body on Linux
- `when-mac`: Execute body on macOS
- `when-windows`: Execute body on Windows

**macOS specifics**:
- File name encoding: `utf-8-hfs` (for proper Korean hangul display)
- PATH sync: `exec-path-from-shell` syncs environment variables in GUI mode

**Windows specifics**:
- File name encoding: `euc-kr`
- Hangul input method keybindings: `S-SPC`, `<Hangul>`, `<Hangul_Hanja>`

## Language Support

### Korean Language
- Language environment: "Korean"
- File name encoding: `utf-8` (Linux), `utf-8-hfs` (macOS), `euc-kr` (Windows)
- Default input method: `korean-hangul`

### Programming Languages

Supported languages with Tree-Sitter modes:
- **C/C++**: c-ts-mode, c++-ts-mode with clang-format integration
- **Python**: python-ts-mode with elpy and jedi-language-server
- **Rust**: rust-ts-mode with rust-analyzer, cargo integration
- **Bash**: bash-ts-mode with bash-language-server
- **CMake**: cmake-ts-mode with cmake-language-server
- **YAML, JSON, TOML**: yaml-ts-mode, json-ts-mode, toml-ts-mode

### Rust Development

- **LSP**: rust-analyzer at `~/.cargo/bin/rust-analyzer`
- **Check command**: clippy (enabled in rust-analyzer config)
- **Cargo buildScripts**: enabled
- **Key bindings**:
  - `C-c C-t`: Test current file
  - `C-c C-c C-b`: cargo-process-build
  - `C-c C-c C-t`: cargo-process-test

## Key Tools and Packages

- **Completion**: vertico + orderless + consult + marginalia (modern completion framework)
- **Project management**: projectile (`C-c p` prefix)
- **Git**: magit (`C-x g`)
- **Terminal**: vterm (`C-c t`)
- **Snippets**: yasnippet
- **Syntax checking**: flycheck
- **Code formatting**: editorconfig support
- **Version control indicators**: diff-hl

## Requirements

- **Emacs**: Version 29+ (requires built-in tree-sitter support, compile with `--with-tree-sitter`)
- **System packages** (as needed):
  - libvterm-dev (for vterm and Claude CLI integration)
  - clang-format (for C/C++ formatting)
  - LSP servers: bash-language-server, jedi-language-server, cmake-language-server, clangd, rust-analyzer

## Custom File

If `~/.emacs.d/.custom.el` exists, it is loaded after the main configuration. Use this for machine-specific settings that shouldn't be committed.

## Notes

- The configuration uses `org-babel-load-file` which means org-mode must be available at startup
- Startup is optimized with increased GC threshold during init (128MB), then reduced to 20MB after startup
- Process output max is increased to 1MB for better LSP performance: `(setq read-process-output-max (* 1024 1024))`
- Auto-revert is enabled globally with 3-second interval
- Backup files are disabled: `(setq make-backup-files nil)`
- Default browser: google-chrome
