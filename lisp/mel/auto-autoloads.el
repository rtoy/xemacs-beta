;;; DO NOT MODIFY THIS FILE
(if (featurep 'mel-autoloads) (error "Already loaded"))

;;;### (autoloads (mime-insert-encoded-file mime-decode-region mime-encode-region) "mel" "mel/mel.el")

(autoload 'mime-encode-region "mel" "\
Encode region START to END of current buffer using ENCODING." t nil)

(autoload 'mime-decode-region "mel" "\
Decode region START to END of current buffer using ENCODING." t nil)

(autoload 'mime-insert-encoded-file "mel" "\
Insert file FILENAME encoded by ENCODING format." t nil)

;;;***

(provide 'mel-autoloads)
