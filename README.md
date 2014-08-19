eshell-prompt-extras
====================

Display extra information and color for your eshell prompt.

Introtuction
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
    (eval-after-load 'esh-opt
      (progn
        (require 'eshell-prompt-extras)
        (setq eshell-highlight-prompt nil
              eshell-prompt-function 'epe-theme-lambda)))

If you want to display python virtual environment information.

    (eval-after-load 'esh-opt
      (progn
        (require 'virtualenvwrapper)
        (venv-initialize-eshell)
        (require 'eshell-prompt-extras)
        (setq eshell-highlight-prompt nil
              eshell-prompt-function 'epe-theme-lambda))))

Screenshot
----------

![picture of eshell-prompt-extras](https://raw.githubusercontent.com/kaihaosw/eshell-prompt-extras/master/screenshot.png)
