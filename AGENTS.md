# Repository Guidelines

## Project Structure & Module Organization

This repository is a literate Emacs configuration. `init.el` is the minimal
bootstrap and loads `README.org` with `org-babel-load-file`; make general
configuration changes in `README.org`, not in generated `README.el`.

Reusable modules live in `lisp/`; ERT tests are under `test/`, with shared
setup in `test/test-helper.el`. `tree-sitter/` contains grammar artifacts, and
`ditaa.jar` supports Org diagrams. Do not commit `.custom.el`, histories,
caches, package installations, or generated playgrounds.

## Architecture & Platform Notes

Packages come from ELPA/MELPA through `use-package`. Core systems include
Vertico/Orderless/Consult, Eglot, Magit/`diff-hl`, and tree-sitter. Claude
integrations include `claude-code-ide`, `ai-code`, and `README.org` helpers.

Windows with MSYS2/UCRT64 is primary, but preserve Linux and macOS behavior.
Use `when-linux`, `when-windows`, and `when-mac` to isolate platform logic.
Korean input, fonts, and UTF-8 handling are requirements.

## Build, Test, and Development Commands

- `./test/run-tests.sh` runs the complete ERT suite in batch mode.
- `emacs --batch -L . -L test -l test/test-helper.el -l test/test-fontutil.el -f ert-run-tests-batch-and-exit`
  runs one test file while preserving the common test setup.
- In Emacs, use `C-c C-c` on a changed Org source block for quick evaluation,
  or run `M-x org-babel-load-file` on `README.org` to reload the full
  configuration.
- Use `M-x load-file RET lisp/<module>.el` when developing a standalone module.

There is no separate build step; startup tangles and loads the configuration.

## Coding Style & Naming Conventions

Follow standard Emacs Lisp formatting: two-space body indentation, balanced
forms, concise docstrings, and `lexical-binding: t` file headers. Keep comments
in English. Prefix repository-specific functions and variables with `my/`;
module APIs may use their module prefix, such as `fontutil/`. Use lowercase
Org Babel markers (`#+begin_src` and `#+end_src`) and `:tangle` only where
needed. Avoid duplicate `use-package` declarations and hardcoded `.emacs.d` or
personal `~/org` paths.

## Testing Guidelines

Add tests for behavior changes and regressions. Name files
`test/test-<feature>.el` and ERT cases `test-<feature>/<behavior>`. Tests should
run headlessly, avoid network access and user configuration, and clean up
temporary resources. No coverage threshold is enforced; run the full suite.

## Commit & Pull Request Guidelines

History uses Conventional Commit-style subjects: `feat:`, `fix:`, `refactor:`,
and `test:`. Keep commits focused. Pull requests should explain motivation,
impact, tests, and platform-specific behavior. Link issues and include
screenshots only for visible UI changes.
