LISP ?= sbcl
CC ?= cc
PROGNAME=valtan
LIBDIR=$(CURDIR)/lib
ASYNC_PROCESS_LIB=$(LIBDIR)/libasyncprocess.so

all: build

deps:
	git submodule update --init --recursive

# Quicklisp's async-process ships only C sources, so build the shared library
# ourselves. It gets embedded into the executable (see cli/foreign-libraries.lisp).
$(ASYNC_PROCESS_LIB):
	@mkdir -p $(LIBDIR)
	src=$$($(LISP) --noinform --non-interactive \
		--eval '(princ (namestring (ql:where-is-system "async-process")))' 2>/dev/null | tail -n 1) && \
	test -n "$$src" && \
	$(CC) -shared -fPIC -O2 -I"$$src" -o $@ "$$src/async-process.c"

lib: $(ASYNC_PROCESS_LIB)

build: deps lib
	$(LISP) --eval '(asdf:initialize-source-registry (quote (:source-registry (:directory "$(CURDIR)/") (:tree "$(CURDIR)/library/valtan-core/") (:tree "$(CURDIR)/submodules/") :inherit-configuration)))' \
		--eval '(ql:quickload :cffi)' \
		--eval '(push #p"$(LIBDIR)/" cffi:*foreign-library-directories*)' \
		--eval '(ql:quickload :valtan-cli)' \
		--eval '(asdf:make :valtan-cli/executable)' \
		--eval '(quit)'

.PHONY: all deps lib build

# end
