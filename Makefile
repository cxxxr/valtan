LISP ?= sbcl
PROGNAME=valtan

all: build

deps:
	git submodule update --init --recursive

build: deps
	$(LISP) --eval '(ql:quickload :valtan-cli)' \
		--eval '(asdf:make :valtan-cli/executable)' \
		--eval '(quit)'

# end
