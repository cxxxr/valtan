##
# Valtan
#

LISP ?= sbcl
INSTALLDIR=/usr/bin
PROGNAME=valtan

deps:
	git submodule update --init --recursive

build: deps
	$(LISP) --eval '(asdf:load-system (asdf:find-system :valtan-cli/executable))' \
		--eval '(asdf:make :valtan-cli/executable)' \
		--eval '(quit)'

install:
	cp $(PROGNAME) $(INSTALLDIR)/$(PROGNAME)

uninstall:
	rm $(INSTALLDIR)/$(PROGNAME)

# end
