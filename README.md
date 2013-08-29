
My Emacs Config
===============
>>"Emacs outshines all other editing software in approximately the same way that the noonday sun does the stars. It is not just bigger and brighter; it simply makes everything else vanish."
_-Neal Stephenson, "In the Beginning was the Command Line"_

>>"Show me your ~/.emacs and I will tell you who you are."
_-Bogdan Maryniuk_

>>"Emacs is like a laser guided missile. It only has to be slightly mis-configured to ruin your whole day."
_-Sean McGrathi_

>>"While any text editor can save your files, only Emacs can save your soul."
_-Per Abrahamseni_

>>"Lisp isn't a language, it's a building material."
_- Alan Kay_

>>"The reasonable man adapts himself to Emacs; the unreasonable one persists in trying to adapt Emacs to himself. Therefore all progress depends on the unreasonable man."
_- G.B. Shaw_

>> "Compared to Emacs Wizards, graphical-IDE users are the equivalent of amateur musicians, pawing at their instrument with a sort of desperation. An IDE has blinking lights and pretty dialogs that you can't interact with properly, and gives newbies a nice comfortable sense of control. But that control is extremely crude, and all serious programmers prefer something that gives them more power."
_- Steve Yegge_


Install
-------

Using el-get, so everything should just magically download itself on first launch. Lots of plugins (about 102 megs at this point), so expect it to take some time, and probably crash once or twice. I am using trunk right now, and know of at least one thing that won't even work with 24.1 (create-lockfiles to nil), so YMMV, but the newer the emacs the better.

Dependancies
------------

ubuntu:

`sudo apt-get install texi2html texinfo python ctags-exuberant w3m gpg sbcl`

osx:

`brew install ctags node w3m gpg sbcl`

lein:

`lein plugin install swank-clojure 1.3.3`

`lein plugin install clj-stacktrace 0.2.3`

python:
`pip install virtualenv rope ropemacs flake8`

it depends on a `~/.secrets.el` file being there. This is a good place to put passwords or other sensitive information required for plugins, but that you don't want sitting on public github.

If ctags isn't pointing at the right version
--------------------------------------------

on ubuntu run

`sudo update-alternatives --config ctags`

and choose `ctags-exuberant`

on osx lion do

`sudo mv /usr/bin/ctags /usr/bin/ctags-old`
