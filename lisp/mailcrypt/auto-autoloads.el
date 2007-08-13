;;; DO NOT MODIFY THIS FILE
(if (featurep 'mailcrypt-autoloads) (error "Already loaded"))

;;;### (autoloads (mc-deactivate-passwd mc-install-write-mode mc-install-read-mode) "mailcrypt" "mailcrypt/mailcrypt.el")

(autoload 'mc-install-read-mode "mailcrypt" nil t nil)

(autoload 'mc-install-write-mode "mailcrypt" nil t nil)

(autoload 'mc-deactivate-passwd "mailcrypt" "\
*Deactivate the passphrase cache." t nil)

;;;***

;;;### (autoloads (mc-pgp-fetch-key mc-scheme-pgp) "mc-pgp" "mailcrypt/mc-pgp.el")

(autoload 'mc-scheme-pgp "mc-pgp" nil nil nil)

(autoload 'mc-pgp-fetch-key "mc-pgp" "\
Attempt to fetch a key for addition to PGP keyring.  Interactively,
prompt for string matching key to fetch.

Non-interactively, ID must be a pair.  The CAR must be a bare Email
address and the CDR a keyID (with \"0x\" prefix).  Either, but not
both, may be nil.

Return t if we think we were successful; nil otherwise.  Note that nil
is not necessarily an error, since we may have merely fired off an Email
request for the key." t nil)

;;;***

;;;### (autoloads (mc-remailer-insert-response-block mc-remailer-encrypt-for-chain mc-remailer-insert-pseudonym) "mc-remail" "mailcrypt/mc-remail.el")

(autoload 'mc-remailer-insert-pseudonym "mc-remail" "\
Insert pseudonym as a From field in the hash-mark header.

See the documentation for the variable `mc-remailer-pseudonyms' for
more information." t nil)

(autoload 'mc-remailer-encrypt-for-chain "mc-remail" "\
Encrypt message for a remailer chain, prompting for chain to use.

With \\[universal-argument], pause before each encryption." t nil)

(autoload 'mc-remailer-insert-response-block "mc-remail" "\
Insert response block at point, prompting for chain to use.

With \\[universal-argument], enter a recursive edit of the innermost
layer of the block before encrypting it." t nil)

;;;***

;;;### (autoloads (mc-mh-snarf-keys mc-mh-verify-signature mc-mh-decrypt-message mc-gnus-decrypt-message mc-gnus-snarf-keys mc-gnus-verify-signature mc-vm-snarf-keys mc-vm-decrypt-message mc-vm-verify-signature mc-rmail-decrypt-message mc-rmail-verify-signature mc-rmail-summary-snarf-keys mc-rmail-summary-decrypt-message mc-rmail-summary-verify-signature mc-snarf-keys mc-snarf mc-insert-public-key mc-verify-signature mc-verify mc-sign-message mc-sign mc-decrypt-message mc-decrypt mc-encrypt-message mc-encrypt mc-cleanup-recipient-headers) "mc-toplev" "mailcrypt/mc-toplev.el")

(autoload 'mc-cleanup-recipient-headers "mc-toplev" nil nil nil)

(autoload 'mc-encrypt "mc-toplev" "\
*Encrypt the current buffer.

Exact behavior depends on current major mode.

With \\[universal-argument], prompt for User ID to sign as.

With \\[universal-argument] \\[universal-argument], prompt for encryption scheme to use." t nil)

(autoload 'mc-encrypt-message "mc-toplev" "\
*Encrypt a message for RECIPIENTS using the given encryption SCHEME.
RECIPIENTS is a comma separated string. If SCHEME is nil, use the value
of `mc-default-scheme'.  Returns t on success, nil otherwise." nil nil)

(autoload 'mc-decrypt "mc-toplev" "\
*Decrypt a message in the current buffer.

Exact behavior depends on current major mode." t nil)

(autoload 'mc-decrypt-message "mc-toplev" "\
Decrypt whatever message is in the current buffer.
Returns a pair (SUCCEEDED . VERIFIED) where SUCCEEDED is t if the encryption
succeeded and VERIFIED is t if it had a valid signature." nil nil)

(autoload 'mc-sign "mc-toplev" "\
*Sign a message in the current buffer.

Exact behavior depends on current major mode.

With one prefix arg, prompts for private key to use, with two prefix args,
also prompts for encryption scheme to use.  With negative prefix arg,
inhibits clearsigning (pgp)." t nil)

(autoload 'mc-sign-message "mc-toplev" "\
Clear sign the message." nil nil)

(autoload 'mc-verify "mc-toplev" "\
*Verify a message in the current buffer.

Exact behavior depends on current major mode." t nil)

(autoload 'mc-verify-signature "mc-toplev" "\
*Verify the signature of the signed message in the current buffer.
Show the result as a message in the minibuffer. Returns t if the signature
is verified." nil nil)

(autoload 'mc-insert-public-key "mc-toplev" "\
*Insert your public key at point.
With one prefix arg, prompts for user id to use. With two prefix
args, prompts for encryption scheme." t nil)

(autoload 'mc-snarf "mc-toplev" "\
*Add all public keys in the buffer to your keyring.

Exact behavior depends on current major mode." t nil)

(autoload 'mc-snarf-keys "mc-toplev" "\
*Add all public keys in the buffer to your keyring." t nil)

(autoload 'mc-rmail-summary-verify-signature "mc-toplev" "\
*Verify the signature in the current message." t nil)

(autoload 'mc-rmail-summary-decrypt-message "mc-toplev" "\
*Decrypt the contents of this message" t nil)

(autoload 'mc-rmail-summary-snarf-keys "mc-toplev" "\
*Adds keys from current message to public key ring" t nil)

(autoload 'mc-rmail-verify-signature "mc-toplev" "\
*Verify the signature in the current message." t nil)

(autoload 'mc-rmail-decrypt-message "mc-toplev" "\
*Decrypt the contents of this message" t nil)

(autoload 'mc-vm-verify-signature "mc-toplev" "\
*Verify the signature in the current VM message" t nil)

(autoload 'mc-vm-decrypt-message "mc-toplev" "\
*Decrypt the contents of the current VM message" t nil)

(autoload 'mc-vm-snarf-keys "mc-toplev" "\
*Snarf public key from the contents of the current VM message" t nil)

(autoload 'mc-gnus-verify-signature "mc-toplev" nil t nil)

(autoload 'mc-gnus-snarf-keys "mc-toplev" nil t nil)

(autoload 'mc-gnus-decrypt-message "mc-toplev" nil t nil)

(autoload 'mc-mh-decrypt-message "mc-toplev" "\
Decrypt the contents of the current MH message in the show buffer." t nil)

(autoload 'mc-mh-verify-signature "mc-toplev" "\
*Verify the signature in the current MH message." t nil)

(autoload 'mc-mh-snarf-keys "mc-toplev" nil t nil)

;;;***

(provide 'mailcrypt-autoloads)
