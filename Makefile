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
	dedup \
	departement \
	diss \
	downcase \
	edit-comments-of-ogg \
	entropy \
	extend-identifiers \
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

OS_NAME = $(shell uname -s)
ARCH_NAME = $(shell uname -m)
HOST_NAME = $(shell hostname -s 2>/dev/null || hostname)
EXECUTABLE = bin/commands-$(OS_NAME)-$(ARCH_NAME)
HOST_EXECUTABLE = bin/commands-$(OS_NAME)-$(ARCH_NAME)-$(HOST_NAME)
COMPAT_EXECUTABLE = bin/commands-$(ARCH_NAME)
WRAPPER = bin/commands

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

commands:$(EXECUTABLE) $(HOST_EXECUTABLE) $(WRAPPER)

$(EXECUTABLE) $(HOST_EXECUTABLE) $(COMPAT_EXECUTABLE) bin/symlink-commands:generate-commands.lisp generate.lisp Makefile sources/*.lisp sources/commands/*.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	@printf "// Using %s\n" "$(LISP)"
	-rm -rf ~/.cache/common-lisp/$(LISP)-*$(HERE)
	$(LISP) $(LISP_OPTIONS) --load generate-commands.lisp # > commands-lisp-ccl.log 2>&1
	@mv -v commands         $(EXECUTABLE)
	@ln -sf $(notdir $(EXECUTABLE)) $(HOST_EXECUTABLE)
	@ln -sf $(notdir $(EXECUTABLE)) $(COMPAT_EXECUTABLE)
	@mv -v symlink-commands bin/
	chmod 755 bin/symlink-commands

$(WRAPPER):commands-wrapper.sh
	install -m 755 commands-wrapper.sh $(WRAPPER)

clean:
	-rm -f bin/commands
	-rm -f bin/commands-*
	-find . \( -name \*.o -o -name \*.fas -o -name \*.lib -o -name \*.log -o -name \*.[dl]x64fsl \) -exec rm {} +
#	-rm -f $(ALL_PROGRAMS)

install:$(EXECUTABLE) $(HOST_EXECUTABLE) $(WRAPPER) bin/symlink-commands
	install -m 755 $(EXECUTABLE)        ~/bin/$(notdir $(EXECUTABLE))
	ln -sf $(notdir $(EXECUTABLE))      ~/bin/$(notdir $(HOST_EXECUTABLE))
	ln -sf $(notdir $(EXECUTABLE))      ~/bin/$(notdir $(COMPAT_EXECUTABLE))
	install -m 755 $(WRAPPER)           ~/bin/commands
	install -m 755 bin/symlink-commands ~/bin/
