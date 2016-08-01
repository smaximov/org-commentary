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
	cask emacs --batch -L . -l org-commentary-cli -f org-commentary -- \
		README.org org-commentary.el --charset utf-8

	cask emacs --batch -L . -l org-commentary-cli -f org-commentary -- \
		CHANGELOG.org org-commentary.el --charset utf-8 --section changelog

.PHONY: help dep-install test update-headers
