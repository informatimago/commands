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

EXECUTABLE = bin/commands-$(shell uname -m)

CLISP = clisp
CLISP_OPTIONS =
CCL   = ccl
CCL_OPTIONS = --no-init
ECL  = ecl
ECL_OPTIONS =
SBCL = sbcl
SBCL_OPTIONS = --noinform --no-userinit --non-interactive
LISP=$(SBCL)
LISP_OPTIONS=$(SBCL_OPTIONS)
CC=cc
LINE="//----------------------------------------------------------------------"
HERE=$(shell pwd)


.PHONY: all clean test commands

commands:$(EXECUTABLE)

$(EXECUTABLE) bin/symlink-commands:generate-commands.lisp generate.lisp Makefile sources/*.lisp sources/commands/*.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	@printf "// Using %s\n" "$(LISP)"
	-rm -rf ~/.cache/common-lisp/$(LISP)-*$(HERE)
	$(LISP) $(LISP_OPTIONS) --load generate-commands.lisp # > commands-lisp-ccl.log 2>&1
	@mv -v commands         $(EXECUTABLE)
	@mv -v symlink-commands bin/
	chmod 755 bin/symlink-commands

clean:
	-rm -f bin/commands
	-find . \( -name \*.o -o -name \*.fas -o -name \*.lib -o -name \*.log -o -name \*.[dl]x64fsl \) -exec rm {} +
#	-rm -f $(ALL_PROGRAMS)

install:$(EXECUTABLE) bin/symlink-commands
	install -m 755 $(EXECUTABLE)        ~/bin/commands
	install -m 755 bin/symlink-commands ~/bin/
