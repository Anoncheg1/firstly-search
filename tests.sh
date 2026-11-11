#!/bin/bash


emacs -Q --batch --no-site-file -l ert.el -l firstly-search.el -l firstly-search-tests.el -f ert-run-tests-batch-and-exit || exit 1

emacs -Q --batch --no-site-file -l ert.el -l firstly-search.el -l firstly-search-recentf.el -l firstly-search-recentf-tests.el -f ert-run-tests-batch-and-exit || exit 1

emacs -Q --batch --no-site-file -l ert.el -l firstly-search.el -l firstly-search-dired.el -l firstly-search-dired-tests.el -f ert-run-tests-batch-and-exit || exit 1
