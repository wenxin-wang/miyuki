;;; miyuki.el --- miyuki for emacs
;; Package-Version: 20190318.1427

;; I never really understand all these licenses.. Hope this would not cause much trouble.

;; Copyright (C) Wenxin Wang 2017 - 2019.
;; Distributed under the MIT License ( license terms are at https://opensource.org/licenses/MIT ).

;; This seems not a package but more like a bunch of helper functions.
;; I do not think it suits general usage, but feel free to try and change it.

(require 'cl-lib)

(defgroup miyuki nil
  "Settings for miyuki, the simple wiki manager"
  :group 'tools)

(defcustom miyuki-bin (expand-file-name "~/.local/bin/miyuki")
  "Path of the miyuki binary. Default value 'miyuki'."
  :type 'executable
  :group 'miyuki)

(defun miyuki//call-process (command &rest args)
  (let* ((errfile (make-temp-file "miyuki-el-"))
	 (outstr
	  (with-temp-buffer
	    (apply #'call-process miyuki-bin nil `(t ,errfile) nil
		   command args)
	    (buffer-string))))
    (with-temp-buffer
      (insert-file-contents errfile)
      (message (string-trim (buffer-string))))
    ;; (delete-file errfile)
    outstr))

(defun miyuki//split-lines-remove-blank (str)
  (cl-remove-if
     #'(lambda (line) (string-match-p "^#*\\s-*$" line))
     (split-string str "\n+")))

;;;###autoload
(defun miyuki/list-wikis ()
  "List wikis for miyuki. Blocking call"
  (interactive)
  (miyuki//split-lines-remove-blank (miyuki//call-process "ls")))

(defun miyuki//parse-wiki-line (line)
  (split-string line))

(defun miyuki//list-wiki-files (name)
  (miyuki//split-lines-remove-blank
   (miyuki//call-process "find" name "-type" "f")))

(defun miyuki//list-all-wiki-files ()
  (miyuki//list-wiki-files "all"))

(defun miyuki//wiki-to-realpath (wiki name)
  (string-trim (miyuki//call-process "w2rp" (concat wiki "/" name))))

(defun miyuki//wiki-to-realpath-src (path name)
  (string-trim (miyuki//call-process "w2rp" path name)))

(defun miyuki//realpath-to-wiki (rel path)
  (string-trim (miyuki//call-process "r2wp" rel path)))

(provide 'miyuki)
