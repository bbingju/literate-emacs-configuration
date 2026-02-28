#!/bin/bash
# Run all ERT tests for the Emacs configuration.
# Usage: ./test/run-tests.sh

set -e

cd "$(dirname "$0")/.."

emacs --batch -L . -L test \
  -l test/test-helper.el \
  -l test/test-readme-structure.el \
  -l test/test-ediff-org.el \
  -l test/test-custom-functions.el \
  -l test/test-fontutil.el \
  -l test/test-c-ts-mode.el \
  -l test/test-mu4e-load-path.el \
  -f ert-run-tests-batch-and-exit
