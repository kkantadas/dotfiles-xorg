;;; feebleline-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "feebleline" "feebleline.el" (0 0 0 0))
;;; Generated autoloads from feebleline.el

(defvar feebleline-mode nil "\
Non-nil if Feebleline mode is enabled.
See the `feebleline-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `feebleline-mode'.")

(custom-autoload 'feebleline-mode "feebleline" nil)

(autoload 'feebleline-mode "feebleline" "\
Replace modeline with a slimmer proxy.

If called interactively, enable Feebleline mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "feebleline" '("feebleline-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; feebleline-autoloads.el ends here
