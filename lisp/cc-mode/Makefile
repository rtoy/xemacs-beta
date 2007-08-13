# Modified by slb for integration with XEmacs.

EMACS = xemacs

OPTS = -batch -q -no-site-file

CMD = -f batch-byte-compile

PRELOADS = -l ./cc-vars.el -l ./cc-mode.el -l ./cc-menus.el -l ./cc-langs.el

.el.elc:
	$(EMACS) $(OPTS) $(PRELOADS) $(CMD) $<

ALL_ELCS = \
	cc-align.elc cc-auto.elc cc-cmds.elc cc-compat.elc \
	cc-engine.elc cc-langs.elc cc-menus.elc cc-mode.elc \
	cc-styles.elc cc-vars.elc

all: $(ALL_ELCS)

clean:
	-rm $(ALL_ELCS)

autoloads: auto-autoloads.el

auto-autoloads.el: $(ALL_ELC:.elc=.el)
	$(EMACS) -batch -q -no-site-file \
		-eval '(setq autoload-target-directory "'`pwd`'/")' \
		-l autoload \
		-f batch-update-autoloads $?
