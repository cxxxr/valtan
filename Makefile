LISP ?= sbcl
PROGNAME=valtan

deps:
	git submodule update --init --recursive

build: deps
	$(LISP) --eval '(asdf:make :valtan-cli/executable)' \
		--eval '(quit)'

# end
