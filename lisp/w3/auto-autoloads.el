;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'w3-autoloads))
    (progn

;;;### (autoloads (x-font-build-cache font-default-size-for-device font-default-encoding-for-device font-default-registry-for-device font-default-family-for-device font-default-object-for-device font-default-font-for-device font-create-object) "font" "w3/font.el")

(autoload 'font-create-object "font" nil nil nil)

(autoload 'font-default-font-for-device "font" nil nil nil)

(autoload 'font-default-object-for-device "font" nil nil nil)

(autoload 'font-default-family-for-device "font" nil nil nil)

(autoload 'font-default-registry-for-device "font" nil nil nil)

(autoload 'font-default-encoding-for-device "font" nil nil nil)

(autoload 'font-default-size-for-device "font" nil nil nil)

(autoload 'x-font-build-cache "font" nil nil nil)

;;;***

;;;### (autoloads (url-cache-expired url-cache-extract url-is-cached url-store-in-cache) "url-cache" "w3/url-cache.el")

(autoload 'url-store-in-cache "url-cache" "\
Store buffer BUFF in the cache" nil nil)

(autoload 'url-is-cached "url-cache" "\
Return non-nil if the URL is cached." nil nil)

(autoload 'url-cache-extract "url-cache" "\
Extract FNAM from the local disk cache" nil nil)

(autoload 'url-cache-expired "url-cache" "\
Return t iff a cached file has expired." nil nil)

;;;***

;;;### (autoloads (url-gateway-nslookup-host) "url-gw" "w3/url-gw.el")

(autoload 'url-gateway-nslookup-host "url-gw" "\
Attempt to resolve the given HOSTNAME using nslookup if possible." t nil)

;;;***

;;;### (autoloads (url-retrieve url-popup-info url-get-url-at-point url-buffer-visiting url-normalize-url url-file-attributes) "url" "w3/url.el")

(autoload 'url-file-attributes "url" "\
Return a list of attributes of URL.
Value is nil if specified file cannot be opened.
Otherwise, list elements are:
 0. t for directory, string (name linked to) for symbolic link, or nil.
 1. Number of links to file.
 2. File uid.
 3. File gid.
 4. Last access time, as a list of two integers.
  First integer has high-order 16 bits of time, second has low 16 bits.
 5. Last modification time, likewise.
 6. Last status change time, likewise.
 7. Size in bytes. (-1, if number is out of range).
 8. File modes, as a string of ten letters or dashes as in ls -l.
    If URL is on an http server, this will return the content-type if possible.
 9. t iff file's gid would change if file were deleted and recreated.
10. inode number.
11. Device number.

If file does not exist, returns nil." nil nil)

(autoload 'url-normalize-url "url" "\
Return a 'normalized' version of URL.  This strips out default port
numbers, etc." nil nil)

(autoload 'url-buffer-visiting "url" "\
Return the name of a buffer (if any) that is visiting URL." nil nil)

(autoload 'url-get-url-at-point "url" "\
Get the URL closest to point, but don't change your
position. Has a preference for looking backward when not
directly on a symbol." nil nil)

(autoload 'url-popup-info "url" "\
Retrieve the HTTP/1.0 headers and display them in a temp buffer." nil nil)

(autoload 'url-retrieve "url" "\
Retrieve a document over the World Wide Web.
The document should be specified by its fully specified
Uniform Resource Locator.  No parsing is done, just return the
document as the server sent it.  The document is left in the
buffer specified by url-working-buffer.  url-working-buffer is killed
immediately before starting the transfer, so that no buffer-local
variables interfere with the retrieval.  HTTP/1.0 redirection will
be honored before this function exits." nil nil)

;;;***

;;;### (autoloads (w3-hotlist-add-document w3-use-hotlist w3-hotlist-append w3-hotlist-rename-entry w3-hotlist-delete) "w3-hot" "w3/w3-hot.el")

(autoload 'w3-hotlist-delete "w3-hot" "\
Deletes a document from your hotlist file" t nil)

(autoload 'w3-hotlist-rename-entry "w3-hot" "\
Rename a hotlist item" t nil)

(autoload 'w3-hotlist-append "w3-hot" "\
Append a hotlist to the one in memory" t nil)

(autoload 'w3-use-hotlist "w3-hot" "\
Possibly go to a link in your W3/Mosaic hotlist.
This is part of the emacs World Wide Web browser.  It will prompt for
one of the items in your 'hotlist'.  A hotlist is a list of often
visited or interesting items you have found on the World Wide Web." t nil)

(autoload 'w3-hotlist-add-document "w3-hot" "\
Add this documents url to the hotlist" t nil)

;;;***

;;;### (autoloads (w3-follow-link w3-follow-link-other-frame w3-do-setup w3 w3-preview-this-buffer w3-follow-url-at-point w3-follow-url-at-point-other-frame w3-maybe-follow-link w3-maybe-follow-link-mouse w3-fetch w3-fetch-other-frame w3-find-file w3-open-local) "w3" "w3/w3.el")

(autoload 'w3-open-local "w3" "\
Find a local file, and interpret it as a hypertext document.
It will prompt for an existing file or directory, and retrieve it as a
hypertext document." t nil)

(autoload 'w3-find-file "w3" "\
Find a local file, and interpret it as a hypertext document.
It will prompt for an existing file or directory, and retrieve it as a
hypertext document." t nil)

(autoload 'w3-fetch-other-frame "w3" "\
Attempt to follow the hypertext reference under point in a new frame.
With prefix-arg P, ignore viewers and dump the link straight
to disk." t nil)

(autoload 'w3-fetch "w3" "\
Retrieve a document over the World Wide Web.
Defaults to URL of the current document, if any.
With prefix argument, use the URL of the hyperlink under point instead." t nil)

(autoload 'w3-maybe-follow-link-mouse "w3" "\
Maybe follow a hypertext link under point.
If there is no link under point, this will try using
url-get-url-at-point" t nil)

(autoload 'w3-maybe-follow-link "w3" "\
Maybe follow a hypertext link under point.
If there is no link under point, this will try using
url-get-url-at-point" t nil)

(autoload 'w3-follow-url-at-point-other-frame "w3" "\
Follow the URL under PT, defaults to link under (point)" t nil)

(autoload 'w3-follow-url-at-point "w3" "\
Follow the URL under PT, defaults to link under (point)" t nil)

(autoload 'w3-preview-this-buffer "w3" "\
See what this buffer will look like when its formatted as HTML.
HTML is the HyperText Markup Language used by the World Wide Web to
specify formatting for text.  More information on HTML can be found at
ftp.w3.org:/pub/www/doc." t nil)

(autoload 'w3 "w3" "\
Retrieve the default World Wide Web home page.
The World Wide Web is a global hypertext system started by CERN in
Switzerland in 1991.

The home page is specified by the variable w3-default-homepage.  The
document should be specified by its fully specified Uniform Resource
Locator.  The document will be parsed as HTML (if appropriate) and
displayed in a new buffer." t nil)

(autoload 'w3-do-setup "w3" "\
Do setup - this is to avoid conflict with user settings when W3 is
dumped with emacs." nil nil)

(autoload 'w3-follow-link-other-frame "w3" "\
Attempt to follow the hypertext reference under point in a new frame.
With prefix-arg P, ignore viewers and dump the link straight
to disk." nil nil)

(autoload 'w3-follow-link "w3" "\
Attempt to follow the hypertext reference under point.
With prefix-arg P, ignore viewers and dump the link straight
to disk." t nil)

;;;***

(provide 'w3-autoloads)
))
