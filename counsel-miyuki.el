;;; counsel-miyuki.el --- miyuki for counsel
;; Package-Version: 20190318.1427
;; Inspired by https://caiorss.github.io/org-wiki/
;; Thank you caiorss!

;; I never really understand all these licenses.. Hope this would not cause much trouble.

;; Copyright (C) Wenxin Wang 2017 - 2019.
;; Distributed under the MIT License ( license terms are at https://opensource.org/licenses/MIT ).

;; This seems not a package but more like a bunch of helper functions.
;; I do not think it suits general usage, but feel free to try and change it.

(require 'cl-lib)
(require 'counsel)
(require 'miyuki)

(defgroup counsel-miyuki nil
  "Settings for using miyuki, the simple wiki manager, with counsel"
  :group 'tools)

(defun counsel-miyuki//find-wiki-file (name repo &optional initial-input)
  (let ((default-directory repo)
	(counsel-find-file-speedup-remote nil))
    (ivy-read
     (format "miyuki file %s: " name)
     (miyuki//list-wiki-files name)
     :matcher #'counsel--find-file-matcher
     :initial-input initial-input
     :action #'(lambda (path)
		 (counsel-find-file-action
		  (if (string-prefix-p "/" path)
		      path
		    (miyuki//wiki-to-realpath name path))))
     :preselect (counsel--preselect-file)
     :require-match 'confirm-after-completion
     :history 'file-name-history
     :keymap counsel-find-file-map
     :sort t
     :caller 'counsel-miyuki-find-file)))

;;;###autoload
(defun counsel-miyuki/find-file (&optional initial-input)
  "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (ivy-read
   "miyuki: " (miyuki/list-wikis)
   :action
   #'(lambda (str)
       (let* ((wikip (miyuki//parse-wiki-line str)))
	 (counsel-miyuki//find-wiki-file
	  (car wikip) (cadr wikip) initial-input)))
   :sort t
   :caller 'counsel-miyuki-find-file))

;;;###autoload
(defun counsel-miyuki/find-all-file (&optional initial-input)
  "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (let ((counsel-find-file-speedup-remote nil))
    (ivy-read "miyuki find all: " (miyuki//list-all-wiki-files)
	      :matcher #'counsel--find-file-matcher
	      :initial-input initial-input
	      :action
	      #'(lambda (path)
		  (if (string-prefix-p "/" path)
		      (counsel-find-file-action path)
		    (counsel-miyuki/find-file path)))
	      :preselect (counsel--preselect-file)
	      :require-match 'confirm-after-completion
	      :history 'file-name-history
	      :keymap counsel-find-file-map
	      :sort t
	      :caller 'counsel-miyuki-find-all-file)))

(defvar counsel-miyuki/ag-base-command
  (concat miyuki-bin " " counsel-ag-base-command " -i"))

(defvar counsel-miyuki/ag-command nil)

(counsel-set-async-exit-code 'counsel-miyuki-ag 1 "No matches found")
(ivy-set-occur 'counsel-miyuki-ag 'counsel-miyuki/ag-occur)
(ivy-set-display-transformer 'counsel-miyuki-ag 'counsel-git-grep-transformer)

(defun counsel-miyuki//ag-format-command (extra-args needle)
  "Construct a complete `counsel-myuiki/ag-command' as a string.
EXTRA-ARGS is a string of the additional arguments.
NEEDLE is the search string."
  (format counsel-miyuki/ag-command
	  (if (string-match " \\(--\\) " extra-args)
	      (replace-match needle t t extra-args 1)
	    (concat extra-args " " needle))))

(defun counsel-miyuki/ag-function (string)
  "Grep in the current directory for STRING."
  (let ((command-args (counsel--split-command-args string)))
    (let ((switches (car command-args))
	  (search-term (cdr command-args)))
      (or
       (let ((ivy-text search-term))
	 (ivy-more-chars))
       (let ((default-directory (ivy-state-directory ivy-last))
	     (regex (counsel--grep-regex search-term)))
	 (counsel--async-command (counsel-miyuki//ag-format-command
				  switches
				  (shell-quote-argument regex)))
	 nil)))))

;;;###autoload
(defun counsel-miyuki/ag (&optional initial-input initial-directory extra-miyuki-args miyuki-prompt)
  "Grep for a string in the current directory using ag.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS string, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument."
  (interactive)
  (setq counsel-miyuki/ag-command counsel-miyuki/ag-base-command)
  (counsel-require-program counsel-miyuki/ag-command)
  (when current-prefix-arg
    (setq extra-miyuki-args
	  (or extra-miyuki-args
	      (read-from-minibuffer "miyuki-ag args: "))))
  (setq counsel-miyuki/ag-command
	(counsel-miyuki//ag-format-command (or extra-miyuki-args "") "%s"))
  (ivy-read (or miyuki-prompt
		(concat "miyuki-ag: "))
	    #'counsel-miyuki/ag-function
	    :initial-input initial-input
	    :dynamic-collection t
	    :keymap counsel-ag-map
	    :history #'counsel-git-grep-history
	    :action #'counsel-git-grep-action
	    :unwind (lambda ()
		      (counsel-delete-process)
		      (swiper--cleanup))
	    :sort t
	    :caller 'counsel-miyuki-ag))

(cl-pushnew 'counsel-miyuki-ag ivy-highlight-grep-commands)

(defun counsel-miyuki-occur ()
  "Generate a custom occur buffer for `counsel-ag'."
  (counsel-grep-like-occur
   counsel-miyuki/ag-command))

(provide 'counsel-miyuki)

;;; org-simple-wiki.el ends here
