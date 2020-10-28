# Makefile
# Creates 'myemacs' executable.
# Dependencies:
# * sbcl or ccl
# * quicklisp
#
# Allows the user to run 'myemacs' in three different ways:
# :standalone, :script and :repl.
#
# The 'myemacs' program keeps track of different configurations of 'emacs'.
# The user can change between them.
#
# Copyright (c) 2020 - José A. Navarro Ramón <janr.devel@gmail.com>
# License: BSD 3-Clause

sysname = myemacs
target1 = $(sysname)_sbcl
target2 = $(sysname)_ccl

FILES = src/packages.lisp \
		src/globals.lisp \
		src/funcs.lisp \
		src/lang-en.lisp \
		src/lang-es.lisp \
		src/lang.lisp \
		src/os.lisp \
		src/cl.lisp \
		src/lex-args.lisp \
		src/syn-args.lisp \
		src/main.lisp

all: $(target1) $(target2)

$(target1): $(FILES)
	$(RM) $(sysname)
	sbcl --eval "(asdf:make :$(sysname)/executable)"
	mv $(sysname) $(target1)

$(target2): $(FILES)
	$(RM) $(sysname)
	ccl --eval "(asdf:make :$(sysname)/executable)"
	mv $(sysname) $(target2)

.PHONY: clean

clean:
	$(RM) $(target1) $(target2) $(sysname) *~ src/*.fasl src/*~ *slime-repl*



