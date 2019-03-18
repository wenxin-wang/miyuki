;;; miyuki.el --- simple wiki extension for org-mode
;; Package-Version: 20190318.1427
;; Inspired by https://caiorss.github.io/org-wiki/
;; Thank you caiorss!

;; I never really understand all these licenses.. Hope this would not cause much trouble.

;; Copyright (C) Wenxin Wang 2017 - 2019.
;; Distributed under the MIT License ( license terms are at https://opensource.org/licenses/MIT ).

;; This seems not a package but more like a bunch of helper functions.
;; I do not think it suits general usage, but feel free to try and change it.

(require 'miyuki)
(require 'counsel)
(require 'org)

(defgroup org-miyuki nil
  "Settings for the simple org-mode-based wiki"
  :group 'tools)

(defcustom org-miyuki/keyword "wiki"
  "The org keyword for wiki keyword, as in #+WIKI: keyword1,keyword2"
  :type 'string
  :group 'org-miyuki)

(defcustom org-miyuki/link-type "wiki"
  "The org keyword for wiki keyword, as in #+WIKI: keyword1,keyword2"
  :type 'string
  :group 'org-miyuki)

(defcustom org-miyuki/keyword-sep ","
  "The seperator for wiki keyword, as in #+WIKI: keyword1,keyword2"
  :type 'string
  :group 'org-miyuki)

;;;###autoload
(defun org-miyuki//list-keywords ()
  "List all keywords"
  (cl-remove-if
   #'(lambda (line) (string-match-p "^#*\\s-*$" line))
   (cl-remove-duplicates
    (split-string
     (miyuki//call-process
      "ag" "-o" "--nocolor" "--nofilename"
      (format "(?<=#\\+%s:).*" (upcase org-miyuki/keyword)))
     (format "\\s-+\\|\\s-*%s+" org-miyuki/keyword-sep))
    :test 'string=)))

(defun org-miyuki//insert-keywords (&rest words)
  "org miyuki insert keyword"
  (interactive)
  (apply #'insert words)
  (insert ","))

(defun org-miyuki/ivy-list-keywords ()
  "List all keywords for insertion"
  (interactive)
  (ivy-read "miyuki keywords: "
	    (org-miyuki//list-keywords)
	    :action #'org-miyuki//insert-keywords
	    :sort t
	    :caller 'org-miyuki-list-keywords))

;;;###autoload
(defun org-miyuki/insert-header ()
  "Insert wiki header at the top of the file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert (format "#+TITLE: %s\n"
                    (file-name-base (buffer-file-name))))))

;;===== wiki protocol for org-miyuki =====
(defun org-miyuki//follow-link (link)
  "Open page in wiki"
  (find-file
   (miyuki//wiki-to-realpath-src
    (or (buffer-file-name) default-directory) link)))

(defun org-miyuki//complete-link (&optional arg)
  "complete wiki page link"
  (let* ((tgtpath
	  (let ((counsel-find-file-speedup-remote nil))
	    (ivy-read
	     "miyuki link to: " (miyuki//list-all-wiki-files)
	     :matcher #'counsel--find-file-matcher
	     :preselect (counsel--preselect-file)
	     :require-match 'confirm-after-completion
	     :history 'file-name-history
	     :keymap counsel-find-file-map
	     :sort t
	     :caller 'org-miyuki-complete-link))))
    (concat
     org-miyuki/link-type ":"
     (miyuki//realpath-to-wiki
      (or (buffer-file-name) default-directory) tgtpath))))

;;;###autoload
(defun org-miyuki/org-link-init ()
  "Run when org-mode is loaded"
  (org-link-set-parameters
   org-miyuki/link-type
   :follow #'org-miyuki//follow-link
   :complete #'org-miyuki//complete-link))

(provide 'org-miyuki)

;;; org-simple-wiki.el ends here
