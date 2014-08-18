;;; eshell-prompt-extras.el --- Display extra information for your eshell prompt.

;; Copyright (C) 2014 Wei Zhao
;; Author: Wei Zhao <kaihaosw@gmail.com>
;; Git: https://github.com/kaihaosw/eshell-prompt-extras.git
;; Version: 0.71
;; Created: 2014-08-16
;; Keywords: eshell, prompt

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
;; need to install `virtualenvwrapper' and `virtualenvwrapper.el'.
;; pip install virtualenvwrapper
;; M-x: package-install: virtualenvwrapper

;; Installation
;; It is recommended installed by the ELPA package system.
;; You could install it by M-x: with
;; package-install: eshell-prompt-extras.

;; Usage
;; (eval-after-load 'esh-opt
;;   (progn
;;     (require 'eshell-prompt-extras)
;;     (setq eshell-prompt-regexp "^[^#\n|]*[#|] "
;;           eshell-highlight-prompt nil
;;           eshell-prompt-function 'epe-theme-lambda)))
;; If you want to display python virtual environment information:
;; (eval-after-load 'esh-opt
;;   (progn
;;     (require 'virtualenvwrapper)
;;     (venv-initialize-eshell)
;;     (require 'eshell-prompt-extras)
;;     (setq eshell-prompt-regexp "^[^#\n|]*[#|] "
;;           eshell-highlight-prompt nil
;;           eshell-prompt-function 'epe-theme-lambda)))

;;; Code:
(require 'em-ls)
(require 'em-dirs)
(require 'esh-ext)
(require 'tramp)
(when (require 'virtualenvwrapper nil t)
  (defun epe-venv-p ()
    "If you are `workon'ing some virtual environment."
    (and (eshell-search-path "virtualenvwrapper.sh")
         venv-current-name)))

;; (epe-colorize "abc" "red")
(defmacro epe-colorize (str color)
  `(propertize ,str 'face '(:foreground ,color)))

;; (epe-colorize-with-face "abc" 'font-lock-comment-face)
(defmacro epe-colorize-with-face (str face)
  `(propertize ,str 'face ,face))

;; (epe-colorize-with-properties "abc" :foreground "red" :backgroud "black")
(defmacro epe-colorize-with-properties (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun epe-abbrev-dir-name (dir)
  "Return the base directory name."
  (if (string= dir (getenv "HOME"))
      "~"
    (let ((dirname (eshell/basename dir)))
      (if (string= dirname "") "/" dirname))))

(defun epe-user-name ()
  "User information."
  (if (epe-remote-p)
      (epe-remote-user)
    (getenv "USER")))

(defun epe-date-time (&optional format)
  "Date time information."
  (format-time-string (if format format "%Y-%m-%d %H:%M") (current-time)))


;; tramp info
(defun epe-remote-p ()
  (tramp-tramp-file-p default-directory))

(defun epe-remote-user ()
  "Return remote user name."
  (tramp-file-name-user (tramp-dissect-file-name default-directory)))

(defun epe-remote-host ()
  "Return remote host."
  (tramp-file-name-real-host (tramp-dissect-file-name default-directory)))


;; git info
(defun epe-git-p ()
  "If you installed git and in a git project."
  (and (eshell-search-path "git")
       (locate-dominating-file (eshell/pwd) ".git")))

(defun epe-git-branch ()
  "Return your git branch name."
  (let ((name (shell-command-to-string "git branch | grep \\* | awk '{print $2}'")))
    (if (> (length name) 0)
        (substring name 0 -1)
      "no-branch")))

(defun epe-git-dirty ()
  "Return if your git is 'dirty'."
  (if (string-match "nothing to commit.*clean"
                    (shell-command-to-string "git status"))
      "" "*"))

(defun epe-git-unpushed-number ()
  "Return unpushed number."
  (string-to-number
   (shell-command-to-string "git log @{u}.. --oneline 2> /dev/null | wc -l")))

(defvar epe-git-status
  "git status --porcelain -b 2> /dev/null")

(defun epe-git-p-helper (command)
  (not
   (string=
    (shell-command-to-string command) "")))

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


;; An example theme. Please post your theme here if you want.

;; (setq eshell-prompt-regexp "^[^#\n|]*[#|] "
;;       eshell-highlight-prompt nil
;;       eshell-prompt-function 'epe-theme-lambda)
(defun epe-theme-lambda ()
  "A eshell-prompt lambda theme. Using this theme,
please set eshell-prompt-regexp with '^[^#\n|]*[#|] ' "
  (concat
   (when (epe-remote-p)
     (epe-colorize-with-face
      (concat (epe-remote-user) "@" (epe-remote-host) " ")
      'font-lock-comment-face))
   (when (fboundp 'epe-venv-p)
     (when (epe-venv-p)
       (epe-colorize-with-face (concat "(" venv-current-name ") ") 'font-lock-comment-face)))
   (epe-colorize-with-face (epe-abbrev-dir-name (eshell/pwd)) 'eshell-ls-directory-face)
   (when (epe-git-p)
     (concat
      (epe-colorize-with-face ":" 'eshell-ls-directory-face)
      (epe-colorize-with-face
       (concat (epe-git-branch)
               (epe-git-dirty)
               (unless (= (epe-git-unpushed-number) 0)
                 (concat ":" (number-to-string (epe-git-unpushed-number)))))
       'font-lock-constant-face)))
   (epe-colorize-with-face " λ" 'eshell-ls-unreadable-face)
   (epe-colorize-with-face (if (= (user-uid) 0) "#" "|") 'eshell-ls-unreadable-face)
   " "))

(provide 'eshell-prompt-extras)

;;; eshell-prompt-extras.el ends here

