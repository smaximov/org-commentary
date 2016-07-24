help:
	@echo "Usage: make COMMAND"
	@echo
	@echo "Commands:"
	@echo "  help             display this message"
	@echo "  dep-install      install dependencies"
	@echo "  test             run tests"
	@echo "  update-headers   update library headers"

dep-install:
	cask install

test:
	cask exec buttercup -L . -L tests

update-headers:
	cask emacs --batch -L . -l org-doc-cli -f org-doc -- \
		README.org org-doc.el --charset utf-8

	cask emacs --batch -L . -l org-doc-cli -f org-doc -- \
		CHANGELOG.org org-doc.el --charset utf-8 --section changelog

.PHONY: help dep-install test update-headers
