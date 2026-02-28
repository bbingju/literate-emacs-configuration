#!/bin/bash
# Run all ERT tests for the Emacs configuration.
# Usage: ./test/run-tests.sh

set -e

cd "$(dirname "$0")/.."

emacs --batch -L . -L test \
  -l test/test-helper.el \
  -l test/test-readme-structure.el \
  -l test/test-ediff-org.el \
  -f ert-run-tests-batch-and-exit
