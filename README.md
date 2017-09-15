eshell-prompt-extras
====================
[![MELPA](https://melpa.org/packages/eshell-prompt-extras-badge.svg)](https://melpa.org/#/eshell-prompt-extras)
[![MELPA Stable](https://stable.melpa.org/packages/eshell-prompt-extras-badge.svg)](https://stable.melpa.org/#/eshell-prompt-extras)

Display extra information and color for your eshell prompt.

Introduction
------------
This library display remote user, remote host, python virtual  
environment info, git branch, git dirty info and git unpushed number  
for eshell prompt.  

If you want to display the python virtual environment info, you  
need to install `virtualenvwrapper`.  

`pip install virtualenvwrapper`  

And [virtualenvwrapper.el](https://github.com/porterjamesj/virtualenvwrapper.el)

Installation
------------
It is recommended installed by the ELPA package system.  
You could install it by `M-x`: with  
`package-install`: `eshell-prompt-extras`.

Usage
-----
before emacs24.4

    (eval-after-load 'esh-opt
      (progn
        (autoload 'epe-theme-lambda "eshell-prompt-extras")
        (setq eshell-highlight-prompt nil
              eshell-prompt-function 'epe-theme-lambda)))

If you want to display python virtual environment information.

    (eval-after-load 'esh-opt
      (progn
        (require 'virtualenvwrapper)
        (venv-initialize-eshell)
        (autoload 'epe-theme-lambda "eshell-prompt-extras")
        (setq eshell-highlight-prompt nil
              eshell-prompt-function 'epe-theme-lambda))))

after emacs24.4

    (with-eval-after-load "esh-opt"
      (autoload 'epe-theme-lambda "eshell-prompt-extras")
      (setq eshell-highlight-prompt nil
            eshell-prompt-function 'epe-theme-lambda))

If you want to display python virtual environment information:

    (with-eval-after-load "esh-opt"
      (require 'virtualenvwrapper)
      (venv-initialize-eshell)
      (autoload 'epe-theme-lambda "eshell-prompt-extras")
      (setq eshell-highlight-prompt nil
            eshell-prompt-function 'epe-theme-lambda))

Themes
------

    epe-theme-lambda  
    epe-theme-dakrone

Custom Variables
----------------

    epe-show-python-info (default t)  
    epe-git-dirty-char (default *)  
    epe-git-untracked-char (default ?)  
    epe-git-detached-HEAD-char (default D:)  
    epe-path-style: (options: fish, single or full. default fish)

Faces
-----

    epe-remote-face  
    epe-venv-face  
    epe-dir-face  
    epe-git-face  
    epe-symbol-face  
    epe-sudo-symbol-face

Screenshot
----------

![picture of eshell-prompt-extras](https://raw.githubusercontent.com/kaihaosw/eshell-prompt-extras/master/screenshot.png)
