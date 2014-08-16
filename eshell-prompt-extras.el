;;; eshell-prompt-extras.el --- Display extra information for your eshell prompt.

;; Copyright (C) 2014 Wei Zhao
;; Author: Wei Zhao <kaihaosw@gmail.com>
;; Git: https://github.com/kaihaosw/eshell-prompt-extras.git
;; Version: 0.3
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
;; environment info, git branch and git dirty info for eshell prompt.

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
;;   (require 'eshell-prompt-extras))

;; Config
;; (setq epe-symbol nil)      remove sysbol
;; (setq epe-symbol "道")     or what sysbol you love

;;; Code:
(require 'em-prompt)
(require 'em-ls)
(require 'em-unix)
(require 'esh-ext)
(require 'tramp)
(when (require 'virtualenvwrapper nil t)
  (defun epe-venv-p ()
    "If you are `workon' some virtual environment."
    (and (eshell-search-path "virtualenvwrapper.sh")
         venv-current-name)))

(defvar epe-symbol "λ"
  "The symbol you love.")

(defmacro epe-colorize (str face)
  `(propertize ,str 'face ,face))

(defun epe-abbrev-dir-name (dir)
  "Return the base directory name."
  (if (string= dir (getenv "HOME"))
      "~"
    (let ((dirname (eshell/basename dir)))
      (if (string= dirname "") "/" dirname))))

(defun epe-remote-p ()
  (tramp-tramp-file-p default-directory))

(defun epe-remote-user ()
  "Return remote user name."
  (tramp-file-name-user (tramp-dissect-file-name default-directory)))

(defun epe-remote-host ()
  "Return remote host."
  (tramp-file-name-real-host (tramp-dissect-file-name default-directory)))

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

(setq eshell-prompt-regexp "^[^#\n|]*[#|] "
      eshell-highlight-prompt nil
      eshell-prompt-function
      (lambda ()
        (concat

         (when (epe-remote-p)
           (epe-colorize
            (concat (epe-remote-user) "@" (epe-remote-host) " ")
            'font-lock-comment-face))

         (when (fboundp 'epe-venv-p)
           (when (epe-venv-p)
             (epe-colorize (concat "(" venv-current-name ") ") 'font-lock-comment-face)))

         (epe-colorize (epe-abbrev-dir-name (eshell/pwd)) 'eshell-ls-directory-face)

         (when (epe-git-p)
           (concat
            (epe-colorize ":" 'eshell-ls-directory-face)
            (epe-colorize
             (concat (epe-git-branch) (epe-git-dirty))
             'font-lock-constant-face)))

         " "                            ; space between them

         (when epe-symbol
           (epe-colorize epe-symbol 'eshell-ls-unreadable-face))

         (epe-colorize (if (= (user-uid) 0) "#" "|") 'eshell-ls-unreadable-face)

         " ")))

(provide 'eshell-prompt-extras)

;;; eshell-prompt-extras.el ends here

