;;; eshell-prompt-extras.el --- Display extra information for your eshell prompt.

;; Copyright (C) 2014-2019 Wei Zhao

;; Author: zwild <judezhao@outlook.com>
;; Contributors: Lee Hinman
;; Maintainer: Chunyang Xu <mail@xuchunyang.me>
;; URL: https://github.com/zwild/eshell-prompt-extras
;; Version: 1.0
;; Created: 2014-08-16
;; Keywords: eshell, prompt
;; Package-Requires: ((emacs "25"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library display remote user, remote host, python virtual
;; environment info, git branch, git dirty info and git unpushed
;; number for eshell prompt.

;; If you want to display the python virtual environment info, you
;; need to install `virtualenvwrapper.el'.
;; M-x: package-install: virtualenvwrapper

;; Installation
;; It is recommended installed by the ELPA package system.
;; You could install it by M-x: with
;; package-install: eshell-prompt-extras.

;; Usage
;; before emacs24.4
;; (eval-after-load 'esh-opt
;;   (progn
;;     (autoload 'epe-theme-lambda "eshell-prompt-extras")
;;     (setq eshell-highlight-prompt nil
;;           eshell-prompt-function 'epe-theme-lambda)))
;;
;; If you want to display python virtual environment information:
;; (eval-after-load 'esh-opt
;;   (progn
;;     (require 'virtualenvwrapper)
;;     (venv-initialize-eshell)
;;     (autoload 'epe-theme-lambda "eshell-prompt-extras")
;;     (setq eshell-highlight-prompt nil
;;           eshell-prompt-function 'epe-theme-lambda)))

;; after emacs24.4
;; (with-eval-after-load "esh-opt"
;;   (autoload 'epe-theme-lambda "eshell-prompt-extras")
;;   (setq eshell-highlight-prompt nil
;;         eshell-prompt-function 'epe-theme-lambda))
;;
;; If you want to display python virtual environment information:
;; (with-eval-after-load "esh-opt"
;;   (require 'virtualenvwrapper)
;;   (venv-initialize-eshell)
;;   (autoload 'epe-theme-lambda "eshell-prompt-extras")
;;   (setq eshell-highlight-prompt nil
;;         eshell-prompt-function 'epe-theme-lambda))

;;; Code:
(require 'em-ls)
(require 'em-dirs)
(require 'esh-ext)
(require 'tramp)
(require 'subr-x)
(require 'seq)
(autoload 'cl-reduce "cl-lib")
(autoload 'vc-git-branches "vc-git")
(autoload 'vc-find-root "vc-hooks")

(defgroup epe nil
  "Eshell extras"
  :group 'eshell-prompt)

(defcustom epe-show-python-info t
  "non nil will show python info."
  :group 'epe
  :type 'boolean)

(defcustom epe-git-dirty-char "*"
  "Character to show for a changed git repository"
  :group 'epe
  :type 'string)

(defcustom epe-git-untracked-char "?"
  "Character to show for an untracked file in the git repository"
  :group 'epe
  :type 'string)

(defcustom epe-git-detached-HEAD-char "D:"
  "Character to show for an detached HEAD in the git repository"
  :group 'epe
  :type 'string)

(defcustom epe-show-local-working-directory nil
  "A flag which indicates whether epe-pipeline should show the local
directory even when Tramp is active."
  :group 'epe
  :type 'boolean)

(defcustom epe-path-style 'fish
  "Prompt path name style."
  :group 'epe
  :type '(choice (const :tag "fish-style-dir-name" fish)
          (const :tag "single-dir-name" single)
          (const :tag "full-path-name" full)))

(defcustom epe-fish-path-max-len 30
  "Default maximum length for path in `epe-fish-path'."
  :group 'epe
  :type 'number)

(defcustom epe-pipeline-show-time t
  "A flag which indicates whether epe-pipeline should show the time."
  :group 'epe
  :type 'boolean)

(defface epe-remote-face
  '((t (:inherit font-lock-comment-face)))
  "Face of remote info in prompt."
  :group 'epe)

(defface epe-venv-face
  '((t (:inherit font-lock-comment-face)))
  "Face of python virtual environment info in prompt."
  :group 'epe)

(defface epe-dir-face
  `((t (:inherit ,(if (facep 'eshell-ls-directory)
                      'eshell-ls-directory
                    'eshell-ls-directory-face) )))
  "Face of directory in prompt."
  :group 'epe)

(defface epe-git-dir-face
  `((t (:foreground "gold")))
  "Face of git path component in prompt."
  :group 'epe)

(defface epe-git-face
  '((t (:inherit font-lock-constant-face)))
  "Face of git info in prompt."
  :group 'epe)

(defface epe-symbol-face
  `((t (:inherit ,(if (facep 'eshell-ls-unreadable)
                      'eshell-ls-unreadable
                    'eshell-ls-unreadable-face))))
  "Face of your symbol in prompt."
  :group 'epe)

(defface epe-sudo-symbol-face
  `((t (:inherit ,(if (facep 'eshell-ls-unreadable)
                      'eshell-ls-unreadable
                    'eshell-ls-unreadable-face))))
  "Face of your sudo symbol in prompt."
  :group 'epe)

(defface epe-success-face
  '((t (:inherit success)))
  "Face of success info in prompt."
  :group 'epe)

(defface epe-error-face
  '((t (:inherit error)))
  "Face of failure info in prompt."
  :group 'epe)

(defface epe-pipeline-delimiter-face
  '((t :foreground "green"))
  "Face for pipeline theme delimiter."
  :group 'epe)

(defface epe-pipeline-user-face
  '((t :foreground "red"))
  "Face for user in pipeline theme."
  :group 'epe)

(defface epe-pipeline-host-face
  '((t :foreground "blue"))
  "Face for host in pipeline theme."
  :group 'epe)

(defface epe-pipeline-time-face
  '((t :foreground "yellow"))
  "Face for time in pipeline theme."
  :group 'epe)

(defface epe-status-face
  '((t (:inherit font-lock-keyword-face)))
  "Face of command status line (duration, termination timestamp)."
  :group 'epe)

;; help definations
;; (epe-colorize-with-face "abc" 'font-lock-comment-face)
(defmacro epe-colorize-with-face (str face)
  `(propertize ,str 'face ,face))

(defun epe-pwd ()
  "Return the current working directory-name."
  (if epe-show-local-working-directory
      (tramp-file-local-name (eshell/pwd))
    (eshell/pwd)))

(defun epe-abbrev-dir-name (dir)
  "Return the base directory name."
  (if (string= dir (getenv "HOME"))
      "~"
    (let ((dirname (file-name-nondirectory dir)))
      (if (string= dirname "") "/" dirname))))

(defun epe-trim-newline (string)
  (replace-regexp-in-string "\n$" "" string))

;; https://www.emacswiki.org/emacs/EshellPrompt
(defun epe-fish-path (path &optional max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (max-len (or max-len epe-fish-path-max-len))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

(defun epe-extract-git-component (path)
  "Extract and return the tuple (prefix git-component) from PATH."
  (let ((prefix path)
        git-component)
    (when (epe-git-p)
      ;; We need "--show-prefix and not "--top-level" when we don't follow symlinks.
      (let* ((git-file-path (abbreviate-file-name
                             (string-trim-right
                              (with-output-to-string
                                (with-current-buffer standard-output
                                  (call-process "git" nil t nil
                                                "rev-parse"
                                                "--show-prefix"))))))
             (common-folder (car (split-string git-file-path "/"))))
        (setq prefix (string-join (seq-take-while
                                   (lambda (s)
                                     (not (string= s common-folder)))
                                   (split-string path "/"))
                                  "/"))
        (setq git-component
              (substring-no-properties path
                                       (min (length path) (1+ (length prefix)))))))
    (list prefix git-component)))

(defun epe-user-name ()
  "User information."
  (if (epe-remote-p)
      (epe-remote-user)
    (getenv "USER")))

(defun epe-date-time (&optional format)
  "Date time information."
  (format-time-string (or format "%Y-%m-%d %H:%M") (current-time)))

(defun epe-status-formatter (timestamp duration)
  "Return the status display for `epe-status'.
TIMESTAMP is the value returned by `current-time' and DURATION is the floating
time the command took to complete in seconds."
  (format "#[STATUS] End time %s, duration %.3fs\n"
          (format-time-string "%F %T" timestamp)
          duration))

(defcustom epe-status-min-duration 1
  "If a command takes more time than this, display its status with `epe-status'."
  :group 'epe
  :type 'number)

(defvar epe-status--last-command-time nil)
(make-variable-buffer-local 'epe-status--last-command-time)

(defun epe-status--record ()
  (setq epe-status--last-command-time (current-time)))

(defun epe-status (&optional formatter min-duration)
  "Termination timestamp and duration of command.
Status is only returned if command duration was longer than
MIN-DURATION \(defaults to `epe-status-min-duration').  FORMATTER
is a function of two arguments, TIMESTAMP and DURATION, that
returns a string."
  (if epe-status--last-command-time
      (let ((duration (time-to-seconds
                       (time-subtract (current-time) epe-status--last-command-time))))
        (setq epe-status--last-command-time nil)
        (if (> duration (or min-duration
                            epe-status-min-duration))
            (funcall (or formatter
                         #'epe-status-formatter)
                     (current-time)
                     duration)
          ""))
    (progn
      (add-hook 'eshell-pre-command-hook #'epe-status--record)
      "")))

;; tramp info
(defun epe-remote-p ()
  (tramp-tramp-file-p default-directory))

(defun epe-remote-user ()
  "Return remote user name."
  (tramp-file-name-user (tramp-dissect-file-name default-directory)))

(defun epe-remote-host ()
  "Return remote host."
  ;; `tramp-file-name-real-host' is removed and replaced by
  ;; `tramp-file-name-host' in Emacs 26, see issue #18
  (if (fboundp 'tramp-file-name-real-host)
      (tramp-file-name-real-host (tramp-dissect-file-name default-directory))
    (tramp-file-name-host (tramp-dissect-file-name default-directory))))


;; git info
;; (defun epe-git-p ()
;;   "If you installed git and in a git project."
;;   (let ((command "git rev-parse --is-inside-work-tree 2> /dev/null"))
;;     (and (eshell-search-path "git")
;;          (string= "true" (epe-trim-newline (shell-command-to-string command))))))

(defun epe-git-p ()
  "If you installed git and in a git project."
  (unless (epe-remote-p)                ; Work-around for issue #20
    (and (eshell-search-path "git")
         (vc-find-root (eshell/pwd) ".git"))))

(defun epe-git-short-sha1 ()
  (epe-trim-newline (shell-command-to-string "git rev-parse --short HEAD")))

;; (defun epe-git-branch ()
;;   "Return your git branch name."
;;   (let ((name (shell-command-to-string "git symbolic-ref HEAD --short || echo -n 'detached'")))
;;     (if (string-match "detached" name)
;;         (concat epe-git-detached-HEAD-char (epe-git-short-sha1))
;;       (epe-trim-newline name))))

(defun epe-git-branch ()
  "Return your git branch name."
  (let ((branch (car (vc-git-branches))))
    (cond
     ((null branch) "no-branch")
     ((string-match "^(HEAD detached at \\(.+\\))$" branch)
      (concat epe-git-detached-HEAD-char (match-string 1 branch)))
     (t branch))))

(defun epe-git-tag (&optional rev with-distance)
  ;; Inspired by `magit-get-current-tag'.
  "Return the closest tag reachable from REV.

If optional REV is nil, then default to `HEAD'.
If optional WITH-DISTANCE is non-nil then return (TAG COMMITS),
if it is `dirty' return (TAG COMMIT DIRTY). COMMITS is the number
of commits in `HEAD' but not in TAG and DIRTY is t if there are
uncommitted changes, nil otherwise."
  (let ((it (with-output-to-string
              (with-current-buffer standard-output
                (apply #'call-process "git" nil t nil "describe" "--long" "--tags"
                       (delq nil (list (and (eq with-distance 'dirty) "--dirty") rev)))))))
    (unless (string-empty-p it)
      (save-match-data
        (string-match
         "\\(.+\\)-\\(?:0[0-9]*\\|\\([0-9]+\\)\\)-g[0-9a-z]+\\(-dirty\\)?$" it)
        (if with-distance
            `(,(match-string 1 it)
              ,(string-to-number (or (match-string 2 it) "0"))
              ,@(and (match-string 3 it) (list t)))
          (match-string 1 it))))))

(defun epe-git-dirty ()
  "Return if your git is 'dirty'."
  (if (string-match "dirty"
                    (shell-command-to-string "git diff-index --quiet HEAD -- || echo -n 'dirty'"))
      epe-git-dirty-char ""))

(defun epe-git-unpushed-number ()
  "Return unpushed number."
  (string-to-number
   (shell-command-to-string "git log @{u}.. --oneline 2> /dev/null | wc -l")))

(defun epe-git-untracked ()
  (and (epe-git-untracked-p) epe-git-untracked-char))

(defvar epe-git-status
  "git status --porcelain -b 2> /dev/null")

(defun epe-git-p-helper (command)
  (not (string= (shell-command-to-string command) "")))

(defun epe-git-untracked-p ()
  (epe-git-p-helper (concat epe-git-status " | grep '^\?\? '")))

(defun epe-git-added-p ()
  (or (epe-git-p-helper (concat epe-git-status " | grep '^A '"))
      (epe-git-p-helper (concat epe-git-status " | grep '^M '"))))

(defun epe-git-modified-p ()
  (or (epe-git-p-helper (concat epe-git-status " | grep '^ M '"))
      (epe-git-p-helper (concat epe-git-status " | grep '^AM '"))
      (epe-git-p-helper (concat epe-git-status " | grep '^ T '"))))

(defun epe-git-renamed-p ()
  (epe-git-p-helper (concat epe-git-status " | grep '^R '")))

(defun epe-git-deleted-p ()
  (or (epe-git-p-helper (concat epe-git-status " | grep '^ D '"))
      (epe-git-p-helper (concat epe-git-status " | grep '^D '"))
      (epe-git-p-helper (concat epe-git-status " | grep '^AD '"))))

(defun epe-git-unmerged-p ()
  (epe-git-p-helper (concat epe-git-status " | grep '^UU '")))

(defun epe-git-ahead-p ()
  (epe-git-p-helper (concat epe-git-status " | grep '^## .*ahead'")))

(defun epe-git-behind-p ()
  (epe-git-p-helper (concat epe-git-status " | grep '^## .*behind'")))

(defun epe-git-diverged-p ()
  (epe-git-p-helper (concat epe-git-status " | grep '^## .*deverged'")))

(defvar eshell-prompt-regexp)
(defvar venv-current-name)
(defvar conda-env-current-name)

;;; Themes
;; Please post your theme here if you want.
;; Each theme should correctly set `eshell-prompt-regexp'
(defun epe-theme-lambda ()
  "A eshell-prompt lambda theme."
  (setq eshell-prompt-regexp "^[^#\nλ]*[#λ] ")
  (concat
   (when (epe-remote-p)
     (epe-colorize-with-face
      (concat (epe-remote-user) "@" (epe-remote-host) " ")
      'epe-remote-face))
   (let ((env-current-name (or (bound-and-true-p venv-current-name)
                               (bound-and-true-p conda-env-current-name))))
     (when (and epe-show-python-info (bound-and-true-p env-current-name))
       (epe-colorize-with-face (concat "(" env-current-name ") ") 'epe-venv-face)))
   (let ((f (cond ((eq epe-path-style 'fish) 'epe-fish-path)
                  ((eq epe-path-style 'single) 'epe-abbrev-dir-name)
                  ((eq epe-path-style 'full) 'abbreviate-file-name))))
     (epe-colorize-with-face (funcall f (epe-pwd)) 'epe-dir-face))
   (when (epe-git-p)
     (concat
      (epe-colorize-with-face ":" 'epe-dir-face)
      (epe-colorize-with-face
       (concat (epe-git-branch)
               (epe-git-dirty)
               (epe-git-untracked)
               (let ((unpushed (epe-git-unpushed-number)))
                 (unless (= unpushed 0)
                   (concat ":" (number-to-string unpushed)))))
       'epe-git-face)))
   (epe-colorize-with-face " λ" (if (zerop eshell-last-command-status)
                                    'epe-success-face
                                  'epe-error-face))
   (epe-colorize-with-face (if (= (user-uid) 0) "#" "") 'epe-sudo-symbol-face)
   " "))

(defun epe-theme-dakrone ()
  "A eshell-prompt lambda theme with directory shrinking."
  (setq eshell-prompt-regexp "^[^#\nλ]* λ[#]* ")
  (let* ((pwd-repl-home (lambda (pwd)
                          (let* ((home (expand-file-name (getenv "HOME")))
                                 (home-len (length home)))
                            (if (and
                                 (>= (length pwd) home-len)
                                 (equal home (substring pwd 0 home-len)))
                                (concat "~" (substring pwd home-len))
                              pwd))))
         (shrink-paths (lambda (p-lst)
                         (if (> (length p-lst) 3) ;; shrink paths deeper than 3 dirs
                             (concat
                              (mapconcat (lambda (elm)
                                           (if (zerop (length elm)) ""
                                             (substring elm 0 1)))
                                         (butlast p-lst 3)
                                         "/")
                              "/"
                              (mapconcat (lambda (elm) elm)
                                         (last p-lst 3)
                                         "/"))
                           (mapconcat (lambda (elm) elm)
                                      p-lst
                                      "/")))))
    (concat
     (when (epe-remote-p)
       (epe-colorize-with-face
        (concat (epe-remote-user) "@" (epe-remote-host) " ")
        'epe-remote-face))
     (when (and epe-show-python-info (bound-and-true-p venv-current-name))
       (epe-colorize-with-face (concat "(" venv-current-name ") ") 'epe-venv-face))
     (epe-colorize-with-face (funcall
                              shrink-paths
                              (split-string (funcall pwd-repl-home (epe-pwd))
                                            "/"))
                             'epe-dir-face)
     (when (epe-git-p)
       (concat
        (epe-colorize-with-face ":" 'epe-dir-face)
        (epe-colorize-with-face
         (concat (epe-git-branch)
                 (epe-git-dirty)
                 (epe-git-untracked)
                 (unless (= (epe-git-unpushed-number) 0)
                   (concat ":" (number-to-string (epe-git-unpushed-number)))))
         'epe-git-face)))
     (epe-colorize-with-face " λ" (if (zerop eshell-last-command-status)
                                      'epe-success-face
                                    'epe-error-face))
     (epe-colorize-with-face (if (= (user-uid) 0) "#" "") 'epe-sudo-symbol-face)
     " ")))

(defun epe-theme-pipeline ()
  "A eshell-prompt theme with full path, smiliar to oh-my-zsh theme."
  (setq eshell-prompt-regexp "^[^#\nλ]* λ[#]* ")
  (concat
   (if (epe-remote-p)
       (progn
	 (concat
	  (epe-colorize-with-face "┌─[" 'epe-pipeline-delimiter-face)
	  (epe-colorize-with-face (epe-remote-user) 'epe-pipeline-user-face)
	  (epe-colorize-with-face "@" 'epe-pipeline-delimiter-face)
	  (epe-colorize-with-face (epe-remote-host) 'epe-pipeline-host-face)))
     (progn
       (concat
	(epe-colorize-with-face "┌─[" 'epe-pipeline-delimiter-face)
	(epe-colorize-with-face (user-login-name) 'epe-pipeline-user-face)
	(epe-colorize-with-face "@" 'epe-pipeline-delimiter-face)
	(epe-colorize-with-face (system-name) 'epe-pipeline-host-face))))
   (concat
    (epe-colorize-with-face "]──[" 'epe-pipeline-delimiter-face)
    (when epe-pipeline-show-time
      (concat
       (epe-colorize-with-face
        (format-time-string "%H:%M" (current-time)) 'epe-pipeline-time-face)
       (epe-colorize-with-face "]──[" 'epe-pipeline-delimiter-face)))
    (epe-colorize-with-face (epe-pwd) 'epe-dir-face)
    (epe-colorize-with-face  "]\n" 'epe-pipeline-delimiter-face)
    (epe-colorize-with-face "└─>" 'epe-pipeline-delimiter-face))
   (when (and epe-show-python-info (bound-and-true-p venv-current-name))
     (epe-colorize-with-face (concat "(" venv-current-name ") ") 'epe-venv-face))
   (when (epe-git-p)
     (concat
      (epe-colorize-with-face ":" 'epe-dir-face)
      (epe-colorize-with-face
       (concat (epe-git-branch)
	       (epe-git-dirty)
	       (epe-git-untracked)
	       (let ((unpushed (epe-git-unpushed-number)))
		 (unless (= unpushed 0)
		   (concat ":" (number-to-string unpushed)))))
       'epe-git-face)))
   (epe-colorize-with-face " λ" 'epe-symbol-face)
   (epe-colorize-with-face (if (= (user-uid) 0) "#" "") 'epe-sudo-symbol-face)
   " "))

(defun epe-theme-multiline-with-status ()
  "A simple eshell-prompt theme with information on its own line
and status display on command termination."
  ;; If the prompt spans over multiple lines, the regexp should match
  ;; last line only.
  (setq eshell-prompt-regexp "^> ")
  (concat
   (epe-colorize-with-face (epe-status) 'epe-status-face)
   (when (epe-remote-p)
     (epe-colorize-with-face
      (concat "(" (epe-remote-user) "@" (epe-remote-host) ")")
      'epe-remote-face))
   (when (and epe-show-python-info (bound-and-true-p venv-current-name))
     (epe-colorize-with-face (concat "(" venv-current-name ") ") 'epe-venv-face))
   (let ((f (cond ((eq epe-path-style 'fish) 'epe-fish-path)
                  ((eq epe-path-style 'single) 'epe-abbrev-dir-name)
                  ((eq epe-path-style 'full) 'abbreviate-file-name))))
     (pcase (epe-extract-git-component (funcall f (epe-pwd)))
       (`(,prefix nil)
        (format
         (propertize "[%s]" 'face '(:weight bold))
         (propertize prefix 'face 'epe-dir-face)))
       (`(,prefix ,git-component)
        (format
         (epe-colorize-with-face "[%s%s@%s]" '(:weight bold))
         (epe-colorize-with-face prefix 'epe-dir-face)
         (if (string-empty-p git-component)
             ""
           (concat "/"
                   (epe-colorize-with-face git-component 'epe-git-dir-face)))
         (epe-colorize-with-face
          (concat (or (epe-git-branch)
                      (epe-git-tag))
                  (epe-git-dirty)
                  (epe-git-untracked)
                  (let ((unpushed (epe-git-unpushed-number)))
                    (unless (= unpushed 0)
                      (concat ":" (number-to-string unpushed)))))
          'epe-git-face)))))
   (epe-colorize-with-face "\n>" '(:weight bold))
   " "))

(provide 'eshell-prompt-extras)

;;; eshell-prompt-extras.el ends here
