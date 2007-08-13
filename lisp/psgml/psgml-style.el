;;; style.el --- example style file for psgml-fs.el and catalog.sgml

(
 ("example"
  default-top 1
  default-bottom 1)
 ("front")
 ("body")
 ("pubfront" text "")
 ("abstract"
  block t
  before (block t text "ABSTRACT")
  left 4)
 ("p" block t)
 ("title" block t)
 ("titlegrp" block t bottom 2 default-bottom 0 default-top 0)
 ("subtitle" before (text " // "))
 ("list" block t)
 ("head" block t)
 ("item"
  left (+ (fs-char 'left) 3)
  hang-from " * ")
 ("keyword"
  before (text "|")
  after (text "|"))
 ("chapter"  block t  top 2  before (text "CHAPTER"))
 ("lit" block t literal t)
 (t
  before (text (format "<%s>" (sgml-element-gi e)))
  after (text " ")
  )
) 
