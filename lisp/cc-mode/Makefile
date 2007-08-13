# Modified by slb for integration with XEmacs.

EMACS = xemacs

OPTS = -batch -q -no-site-file

CMD = -f batch-byte-compile

PRELOADS = -l ./cc-vars.el -l ./cc-mode.el -l ./cc-menus.el -l ./cc-langs.el

SPECIAL_ELCS = custom-load.elc

.el.elc:
	$(EMACS) $(OPTS) $(PRELOADS) $(CMD) $<

ALL_ELCS = \
	cc-align.elc cc-auto.elc cc-cmds.elc cc-compat.elc \
	cc-engine.elc cc-langs.elc cc-menus.elc cc-mode.elc \
	cc-styles.elc cc-vars.elc

all: $(ALL_ELCS) $(SPECIAL_ELCS)

custom-load.elc: custom-load.el
	$(EMACS) $(OPTS) $(CMD) $<

clean:
	-rm $(ALL_ELCS)
