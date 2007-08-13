;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'psgml-autoloads))
    (progn

;;;### (autoloads (style-format) "psgml-fs" "psgml/psgml-fs.el")

(autoload 'style-format "psgml-fs" nil t nil)

;;;***

;;;### (autoloads nil "psgml-html" "psgml/psgml-html.el")

(autoload 'html-mode "psgml-html" "\
HTML mode." t)

(autoload 'html3-mode "psgml-html" "\
HTML3 mode." t)

;;;***

;;;### (autoloads (sgml-mode) "psgml" "psgml/psgml.el")

(autoload 'sgml-mode "psgml" "\
Major mode for editing SGML.\\<sgml-mode-map>
Makes > display the matching <.  Makes / display matching /.
Use \\[sgml-validate] to validate your document with an SGML parser.

You can find information with:
\\[sgml-show-context]  Show the nesting of elements at cursor position.
\\[sgml-list-valid-tags]  Show the tags valid at cursor position.

Insert tags with completion of contextually valid tags with \\[sgml-insert-tag].
End the current element with \\[sgml-insert-end-tag].  Insert an element (i.e.
both start and end tag) with \\[sgml-insert-element].  Or tag a region with 
\\[sgml-tag-region]. 

To tag a region with the mouse, use transient mark mode or secondary selection.

Structure editing:
\\[sgml-backward-element]  Moves backwards over the previous element.
\\[sgml-forward-element]  Moves forward over the next element.
\\[sgml-down-element]  Move forward and down one level in the element structure.
\\[sgml-backward-up-element]  Move backward out of this element level.
\\[sgml-beginning-of-element]  Move to after the start tag of the current element.
\\[sgml-end-of-element]  Move to before the end tag of the current element.
\\[sgml-kill-element]  Kill the element following the cursor.

Finding interesting positions
\\[sgml-next-data-field]  Move forward to next point where data is allowed.
\\[sgml-next-trouble-spot]  Move forward to next point where something is 
	amiss with the structure.

Folding and unfolding
\\[sgml-fold-element]  Fold the lines comprising the current element, leaving 
	the first line visible.
\\[sgml-fold-subelement]  Fold the elements in the content of the current element.
	Leaving the first line of every element visible.
\\[sgml-unfold-line]  Show hidden lines in current line.

User options:

sgml-omittag  Set this to reflect OMITTAG in the SGML declaration.
sgml-shortag  Set this to reflect SHORTTAG in the SGML declaration.
sgml-auto-insert-required-elements  If non-nil, automatically insert required 
	elements in the content of an inserted element.
sgml-balanced-tag-edit  If non-nil, always insert start-end tag pairs.
sgml-omittag-transparent  If non-nil, will show legal tags inside elements
	with omitable start tags and legal tags beyond omitable end tags.
sgml-leave-point-after-insert  If non-nil, the point will remain after 
	inserted tag(s).
sgml-warn-about-undefined-elements  If non-nil, print a warning when a tag 
	for a undefined element is found.
sgml-max-menu-size  Max number of entries in Tags and Entities menus before
 	they are split into several panes.
sgml-always-quote-attributes  If non-nil, quote all attribute values 
	inserted after finishing edit attributes.
sgml-minimize-attributes  Determines minimization of attributes inserted by 
	edit-attributes.
sgml-normalize-trims  If non-nil, sgml-normalize will trim off white space 
	from end of element when adding end tag.
sgml-indent-step  How much to increament indent for every element level.
sgml-indent-data  If non-nil, indent in data/mixed context also.
sgml-set-face     If non-nil, psgml will set the face of parsed markup.
sgml-markup-faces The faces used when the above variable is non-nil.
sgml-system-path  List of directories used to look for system identifiers.
sgml-public-map  Mapping from public identifiers to file names.
sgml-offer-save  If non-nil, ask about saving modified buffers before
		\\[sgml-validate] is run.

All bindings:
\\{sgml-mode-map}
" t nil)

;;;***

(provide 'psgml-autoloads)
))
