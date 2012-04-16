
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

ubuntu:

`sudo apt-get install texi2html texinfo python ctags-exuberant w3m`

osx:

`brew install ctags node w3m`

ruby + `gem install fuzzy_file_finder`

`lein plugin install swank-clojure 1.3.3`

`lein plugin install clj-stacktrace 0.2.3`

it depends on a `~/.secrets.el` file being there. This is a good place to put passwords or other sensitive information required for plugins, but that you don't want sitting on public github.

I use it to set `github-user` and `github-token`

Install node on Linux
---------------------

```bash
git clone git@github.com:joyent/node.git
cd node
./configure && make && make install
```

install npm

`curl http://npmjs.org/install.sh | sh`

make sure node can find npm modules

`echo 'export NODE_PATH="'$(npm root -g)'"' >> ~/.zshrc`

then install jshint

`sudo npm install -g jshint`

and configure jshint options in `~/.jshint.json`

If ctags isn't pointing at the right version
--------------------------------------------

on ubuntu run

`sudo update-alternatives --config ctags`

and choose `ctags-exuberant`

on osx lion do

`sudo mv /usr/bin/ctags /usr/bin/ctags-old`
