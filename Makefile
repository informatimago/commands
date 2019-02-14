FUTURE_PROGRAMS= \
	box \

ALL_PROGRAMS=   \
	add-cookie \
	add-paths \
	ansi-test \
	batch-emerge \
	bin-to-c-array \
	buzzword \
	capitalize \
	cddb-to-tag \
	check-surface \
	clar \
	clash \
	clean-bd-archive \
	clean-name \
	clean-paths \
	columnify \
	cookie-diff \
	cookie-loop \
	cookie-merge \
	cookie \
	cpcd \
	dedup \
	departement \
	diss \
	downcase \
	edit-comments-of-ogg \
	entropy \
	euronews \
	extend-identifiers \
	fetch-pop \
	fpm \
	generate-hw \
	generate \
	get-cams \
	get-directory \
	grave \
	group-files \
	hacking-too-long-p \
	hexbin \
	html-make-image-index \
	insulte \
	kwic \
	lc \
	llen \
	lrev \
	macosx-port-uninstall-recursively \
	memo \
	menu \
	merge \
	mfod \
	new-password \
	news-to-mbox \
	nls \
	one-of \
	pic-resize \
	pjb-diff \
	programmer \
	pseudo-pop \
	radio \
	random \
	record-rc \
	religion \
	remove-duplicate-files \
	revlines \
	rotate \
	rss2email \
	rstuml \
	schedule-radio-courtoisie \
	shell \
	sleep-schedule \
	split-dir \
	split-merge \
	substitute \
	surveille-host \
	surveille-web-pages \
	svn-locate-revision \
	text \
	when

# all:$(ALL_PROGRAMS)
all:commands

CLISP=clisp
CCL=ccl
ECL=ecl
SBCL=sbcl
CC=cc
LINE="//----------------------------------------------------------------------"
HERE=$(shell pwd)

.PHONY: all clean test commands

commands:bin/commands

bin/commands bin/symlink-commands:generate-commands.lisp generate.lisp Makefile sources/*.lisp sources/commands/*.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ccl-*$(HERE)
	@$(CCL) -n -l generate-commands.lisp # > commands-lisp-ccl.log 2>&1
	mv commands symlink-commands bin/
	chmod 755 bin/symlink-commands

hw-c:hw.c
	@printf "// Generating Executable from %s source: %s\n" "C" $@
	@$(CC) -o hw-c hw.c > hw-c.log 2>&1

hw-pascal:hw.pas
	@printf "// Generating Executable from %s source: %s\n" "Pascal" $@
	@if [ -x $(FPC) ] ; then $(FPC) -ohw-pascal hw.pas > hw-pascal.log 2>&1 ; else printf 'fpc not installed\n' 1>&2 ; fi

hw-lisp-ccl:generate-hw.lisp generate.lisp hw.asd hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ccl-*$(HERE)
	@$(CCL) -n < generate-hw.lisp > hw-lisp-ccl.log 2>&1
	-@mv hw hw-lisp-ccl

hw-lisp-clisp:generate-hw.lisp generate.lisp hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/clisp-*$(HERE)
	@$(CLISP) -norc < generate-hw.lisp > hw-lisp-clisp.log 2>&1
	-@mv hw hw-lisp-clisp

hw-lisp-clisp-fas:Makefile hw.fas
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	  cat hw.fas  ;\
	  echo '(defun argv () (cons (elt (ext:argv) 0) ext:*args*))' ;\
	  echo '(defun start () ' ;\
	  echo '   (handler-case' ;\
	  echo '       (progn' ;\
	  echo '         (load #P"~/.hw.lisp" :if-does-not-exist nil)' ;\
	  echo '         (apply (function hello-world:hw) (argv)))' ;\
	  echo '     (error (err)' ;\
	  echo '       (finish-output *standard-output*)' ;\
	  echo '       (finish-output *trace-output*)' ;\
	  echo '       (format *error-output* "~%~A~%" err)' ;\
	  echo '       (finish-output *error-output*)' ;\
	  echo '       (ext:quit 1)))' ;\
	  echo '   (finish-output *standard-output*)' ;\
	  echo '   (finish-output *trace-output*)' ;\
	  echo '   (finish-output *error-output*)' ;\
	  echo '   (ext:quit 0))' ;\
	  echo '(start)' ) > $@
	@chmod 755 $@

hw.fas:hw.lisp
	@printf "// Compiling: %s\n" $@
	@clisp -ansi -q -E utf-8 -norc -c $^ -o $@ > hw-lisp-clisp-fas.log 2>&1

hw-lisp-ecl:generate-hw.lisp generate.lisp hw.asd hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ecl-*$(HERE)
	@$(ECL) -norc < generate-hw.lisp > hw-lisp-ecl.log 2>&1
	-@mv hw hw-lisp-ecl

hw-lisp-sbcl:generate-hw.lisp generate.lisp hw.asd hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/sbcl-*$(HERE)
	@$(SBCL) --no-userinit < generate-hw.lisp > hw-lisp-sbcl.log 2>&1
	-@mv hw hw-lisp-sbcl

test:$(ALL_PROGRAMS)
	@for p in $(ALL_PROGRAMS) ; do printf "%-20s: %s\n" "$$p"  "$$(./$$p)" ; done
	@ls -l $(ALL_PROGRAMS)

clean:
	-rm -f bin/commands
	-find . \( -name \*.o -o -name \*.fas -o -name \*.lib -o -name \*.log -o -name \*.[dl]x64fsl \) -exec rm {} +
#	-rm -f $(ALL_PROGRAMS)

install:bin/commands bin/symlink-commands
	install -m 755 bin/commands         ~/bin/
	install -m 755 bin/symlink-commands ~/bin/
