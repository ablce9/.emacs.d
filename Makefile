VERSION ?= $(shell git describe 2>&1 >/dev/null|| true)
EMACS_HOME ?= ~/.emacs.d/
SRC_FILES := elpa vendor ./init.el

all: build

.PHONY: build
build:
	@ echo "building ... $(VERSION)"
	$(shell find $(SRC_FILES) $(EMACS_HOME) -name \
		'*.el' -exec emacs --batch --eval \
		'(byte-compile-file "{}")' \; 2>&1 >/dev/null)
