eshell-prompt-extras
====================

Display extra information and color for your eshell prompt.

Introtuction
------------
This library display remote user, remote host, python virtual  
environment info, git branch and git dirty info for eshell prompt.  

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
      (require 'eshell-prompt-extras))

If you want to display python virtual environment information.

    (eval-after-load 'esh-opt
      (require 'virtualenvwrapper)
      (venv-initialize-eshell)
      (require 'eshell-prompt-extras))

Config
------

Remove sysbol: `(setq epe-symbol nil)`  
Set what sysbol you love: `(setq epe-symbol "道")`  

![](./screenshot.png?raw=true)
