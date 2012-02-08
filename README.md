
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


Install
-------

Using el-get, so everything should just magically download itself on first launch. Lots of plugins, so expect it to take some time, and probably crash once or twice

Dependancies
------------

`sudo apt-get install texi2html texinfo python global`

ruby + `gem install fuzzy_file_finder`

`lein plugin install swank-clojure 1.3.3`

`lein plugin install clj-stacktrace 0.2.3`

to install rtags, clone gaizkas fork: `git clone git@github.com:gaizka/rtags.git && sudo rtags/install.sh`
