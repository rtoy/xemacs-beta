;;; @(#)locale-start.el.euc	1.7 97/03/06 13:43:21
;;; locale/ja/locale-start.el --- startup.el customized for ja locale.
;;;			This file, being an .el file, 
;;;			must be in ISO 2022 encoding after installation.
;; Copyright (C) 1985-1986, 1990, 1992-1995 Free Software Foundation, Inc.
;; Copyright (c) 1993-1997 Sun Microsystems, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois

;; Maintainer: XEmacs
;; Keywords: internal

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(defun startup-splash-frame-body ()
  `("\n" ,(emacs-version) "\n"
    (face bold-italic "\
Copyright (C) 1985-1996 Free Software Foundation, Inc.
Copyright (C) 1990-1994 Lucid, Inc.
Copyright (C) 1993-1997 Sun Microsystems, Inc. All Rights Reserved.
Copyright (C) 1994-1996 Board of Trustees, University of Illinois
Copyright (C) 1995-1996 Ben Wing\n\n")
    
    ,@(if (featurep 'sparcworks)
          `( "\
$(B%5%s$O!"(BWorkShop/Emacs $(BE}9g2=%Q%C%1!<%8$N$_$r%5%]!<%H$7$^$9!#(B
$(BB>$N(B XEmacs $(B%Q%C%1!<%8$O$9$Y$F!"!V8=>u$N$^$^!W$G6!5k$5$l$^$9!#(B
$(B>\:Y(B ($(B1QJ8(B) $(B$O(B" (key describe-no-warranty) "$(B$H%?%$%W$7$F!"(B
1991 $(BG/(B 6 $(B7nHG(B GPL $(B%P!<%8%g%s(B 2 $(B$r$4Mw$/$@$5$$!#(B
$(BK\(B XEmacs $(B$O4pK\%a%K%e!<$K8B$jF|K\8l2=$5$l$F$$$^$9!#AH$_9~$^$l$?(B
$(B%a!<%k(B (VM$(B!"(BMH) $(B$d%K%e!<%9$rFI$`$?$a$N%Q%C%1!<%8(B (Gnus) $(BEy$O!"(B
$(BF|K\8l$r40`z$K$O=hM}$G$-$^$;$s$N$G!"$4;HMQ$K$J$k>l9g$O==J,$4Cm0U(B
$(B$/$@$5$$!#(B\n")

        '("XEmacs $(B$K$O(B *$(B$^$C$?$/2?$N(B* $(BJ]>c$b$"$j$^$;$s!#>\:Y$O!"(B"
          (key describe-no-warranty) "$(B$r%?%$%W$7$F$/$@$5$$!#(B\n"))

    "XEmacs $(B$NJ#@=$O5v2D$5$l$F$$$^$9!#5v2D$N>r7o$r8+$k$K$O!"(B\n"
    (key describe-copying) " $(B$H%?%$%W$7$F$/$@$5$$!#(B\n"
    "$(B:G?7HG$N<hF@J}K!$K$D$$$F$O!"(B" (key describe-distribution)
    " $(B$H%?%$%W$7$F$/$@$5$$!#(B\n\n"
    
    "$(B%X%k%W>pJs$r8+$k$K$O!"(B" (key help-command) " $(B$H%?%$%W!"$^$?$O(B" 
    (face bold "$(B%X%k%W(B") "$(B%a%K%e!<$r;HMQ(B\n"
    "$(B$7$F$/$@$5$$(B\n"
    (key advertised-undo) " $(B$K$h$jJQ99$r<h$j>C$;$^$;$9!#(B(C- $(B$O!"(BControl $(B%-!<$G$9!#(B)\n"
    "XEmacs $(B$r=*N;$9$k$K$O!"(B" (key save-buffers-kill-emacs) " $(B$H%?%$%W$7$F$/$@$5$$!#(B\n"
    (key help-with-tutorial) " $(B$G(B XEmacs $(B$N;HMQJ}K!$N<B=,$r3+;O$G$-$^$9!#(B\n"
    (key info) " $(B$K$h$j!"(BInfo $(B%b!<%I$KF~$j!"%*%s%i%$%s$N%I%-%e%a%s%H$r(B\n"
    "$(BFI$`$3$H$,$G$-$^$9!#(B\n"
    (face (bold red) ( "\
$(BIQHK$K?R$M$i$l$k<ALd$H$=$NEz$($O!"(BXEmacs $(BLdEz=8$K$"$j$^$9!#(B\n"
    "$(B%X%k%W%a%K%e!<$r;HMQ$9$k$+!"(B"(key xemacs-local-faq) " $(B$H%?%$%W$7$F$/$@$5$$!#(B"))))


(defun command-line-do-help (arg)
  "Print the XEmacs usage message and exit."
  (let ((standard-output 'external-debugging-output))
    (princ (concat "\n" (emacs-version) "\n\n"))
    (princ
     (if (featurep 'x)
	 "XEmacs $(B$O!"(BX $(B%D!<%k%-%C%H$NI8=`%3%^%s%I%*%W%7%g%s$r$9$Y$FG'<1$7$^$9!#(B\n$(B$=$l$K2C$($F!"(B"
       "XEmacs $(B$O!"(B "))
    (princ " $(B0J2<$N%*%W%7%g%s$rG'<1$7!"=P8==g$K=hM}$7$^$9(B:

  -t $(B%G%P%$%9L>(B         $(BC<Kv$NBe$o$j$K;XDj$N(B TTY $(B%G%P%$%9$r;HMQ$7$FF~=PNO$r(B
			$(B9T$J$&!#(B-nw $(B$,<+F0E*$K2>Dj$5$l$k!#(B
  -nw                   $(B%&%#%s%I%&%7%9%F%`$r;HMQ$;$:!"8=(B TTY $(B$r;HMQ!#(B($(BCm(B: TTY 
                        $(B%b!<%I$G$NF|K\8lI=<($O!"K\%P!<%8%g%s$N(B XEmacs $(B$G$O$G$-(B
                        $(B$^$;$s!#(B)
  -batch                $(BHsBPOCE*;HMQ!#%a%C%;!<%8$O!"I8=`%(%i!<(B (stderr) $(B%X!#(B
  -debug-init           $(B=i4|2=%U%!%$%k$G$N%(%i!<H/@8;~$K$O%G%P%C%,$r5/F0!#(B
  -unmapped             $(B=i4|%U%l!<%`$N%^%C%W$r$7$J$$!#(B
  -no-site-file         $(B%5%$%HFC2==i4|2=%U%!%$%k(B (site-start.el) $(B$rFI$_9~$^$J$$!#(B
  -no-init-file         $(B%f!<%6!<FC2==i4|2=%U%!%$%k(B (~/.emacs) $(B$rFI$_9~$^$J$$!#(B
  -q                    -no-init-file $(B$HF15A!#(B
  -user $(B%f!<%6!<L>(B      $(B<+J,$N=i4|2=%U%!%$%k$NBe$o$j$K;XDj%f!<%6!<$N%U%!%$%k$r;HMQ!#(B
  -u $(B%f!<%6!<L>(B         -user $(B$HF15A!#(B\n")
;; $(B1Q8lHG$G$O!"0J2<$N$h$&$J%3!<%I$K$h$j!"%I%-%e%a%s%H$N0lIt$,I=<($5$l$F$$$?$,!"(B
;; $(B$3$l$G$O!"1Q8l$N%I%-%e%a%s%H$7$+I=<($G$-$J$$$N$G!"$3$N%3!<%I$O%3%a%s%H%"%&%H$7!"(B
;; $(BC1=c$K@bL@$r0u;z$9$k%3!<%I$HCV$-49$($k!#(B
;;   (let ((l command-switch-alis\nt)
;;	  (insert (lambda (&rest x)
;;		    (princ "  ")
;;		    (let ((len 2))
;;		      (while x
;;			(princ (car x))
;;			(incf len (length (car x)))
;;			(setq x (cdr x)))
;;		      (when (>= len 24)
;;			(terpri) (setq len 0))
;;		      (while (< len 24)
;;			(princ " ")
;;			(incf len))))))
;;      (while l
;;        (let ((name (car (car l)))
;;              (fn (cdr (car l)))
;;	      doc arg cons)
;;	  (cond
;;	   ((and (symbolp fn) (get fn 'undocumented)) nil)
;;	   (t
;;	    (setq doc (documentation fn))
;;	    (if (member doc '(nil "")) (setq doc "(undocumented)"))
;;	    (cond ((string-match "\n\\(<.*>\\)\n?\\'" doc)
;;		   ;; Doc of the form "The frobber switch\n<arg1> <arg2>"
;;		   (setq arg (substring doc (match-beginning 1) (match-end 1))
;;			 doc (substring doc 0 (match-beginning 0))))
;;		  ((string-match "\n+\\'" doc)
;;		   (setq doc (substring doc 0 (match-beginning 0)))))
;;	    (if (and (setq cons (rassq fn command-switch-alist))
;;		     (not (eq cons (car l))))
;;		(setq doc (format "Same as %s." (car cons))))
;;
;;	    (if arg
;;		(funcall insert name " " arg)
;;	      (funcall insert name))
;;	    (princ doc)
;;	    (terpri))))
;;        (setq l (cdr l))))
;; $(BCV$-49$(ItJ,3+;O(B
  (princ "\
  -help                 XEmacs $(B;HMQK!$rI=<($7$F=*N;!#(B
  -flags                -help $(B$HF15A!#(B
  -h                    -help $(B$HF15A!#(B
  -?                    -help $(B$HF15A!#(B
  -version              $(B%P!<%8%g%s>pJs$rI=<($7$F=*N;!#(B
  -V                    -version $(B$HF15A!#(B
  -funcall $(B4X?tL>(B       $(B;XDj$N(B lisp $(B4X?t$r0z?t$J$7$G5/F0!#(B
  -f $(B4X?tL>(B             -funcall $(B$HF15A!#(B
  -eval $(B%U%)!<%`(B        lisp $(B$N%U%)!<%`$rI>2A!#0zMQ(B (quote) $(B$OCm0U?<$/9T$J$C$F$/$@$5$$!#(B
  -load $(B%U%!%$%kL>(B      $(B;XDj$N(B lisp $(B%3!<%I$r(B XEmacs $(B$KFI$_9~$`!#(B
  -l $(B%U%!%$%kL>(B         -load $(B$HF15A!#(B
  -insert $(B%U%!%$%kL>(B    $(B8=%P%C%U%!$K%U%!%$%k$rA^F~!#(B
  -i $(B%U%!%$%kL>(B         -insert $(B$HF15A!#(B
  -kill                 XEmacs $(B$r=*N;!#(B
  -tooltalk             ToolTalk $(B%5!<%P!<$K@\B3!#(B\n")
;; $(BCV$-49$(ItJ,=*N;(B
  (princ "\
  +N $(B%U%!%$%kL>(B         $(B;XDj%U%!%$%k$r(B N $(B9TL\$+$iI=<(!#(B

$(BB>$N$9$Y$F7A<0$N0z?t$O%U%!%$%kL>$H2r<a$5$l!"JT=8$N$?$a$K%P%C%U%!$KFI$_9~$^(B
$(B$l$^$9!#(B

XEmacs $(B$K$O!"%*%s%i%$%s$N<+=,=q$H%^%K%e%"%k(B  ($(BN>J}$H$b1Q8lHG$N$_(B) $(B$,IUB0$7(B
$(B$F$$$^$9!#<+=,=q$r3+;O$9$k$K$O!"(BXEmacs $(B3+;O8e$K!"(B^Ht (Control-h t) $(B$r%?%$%W(B
$(B$7$F$/$@$5$$!#%^%K%e%"%k$rFI$`$K$O!"(B^Hi$(B!"$5$i$K>\$7$$%X%k%W>pJs$O!"(B^H^H^H 
(Control-h $(B$r(B 3 $(B2s(B) $(B$r%?%$%W$7$F$/$@$5$$!#(B\n")
    (kill-emacs 0)))
