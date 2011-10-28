;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (anything-other-buffer anything-at-point anything)
;;;;;;  "anything-config/anything" "anything-config/anything.el"
;;;;;;  (20138 8733))
;;; Generated autoloads from anything-config/anything.el

(autoload 'anything "anything-config/anything" "\
Main function to execute anything sources.
When call interactively with no arguments deprecated `anything-sources'
will be used if non--nil.

PLIST is a list like (:key1 val1 :key2 val2 ...) or
 (&optional sources input prompt resume preselect buffer keymap).

Basic keywords are the following:

- :sources

  Temporary value of `anything-sources'.  It also accepts a
  symbol, interpreted as a variable of an anything source.  It
  also accepts an alist representing an anything source, which is
  detected by (assq 'name ANY-SOURCES)

- :input

  Temporary value of `anything-pattern', ie. initial input of minibuffer.

- :prompt

  Prompt other than \"pattern: \".

- :resume

  If t, Resurrect previously instance of `anything'.  Skip the initialization.
  If 'noresume, this instance of `anything' cannot be resumed.

- :preselect

  Initially selected candidate.  Specified by exact candidate or a regexp.
  Note that it is not working with delayed sources.

- :buffer

  `anything-buffer' instead of *anything*.

- :keymap

  `anything-map' for current `anything' session.

- :default

 A default argument that will be inserted in minibuffer with  \\<minibuffer-local-map>\\[next-history-element].
 When nil of not present `thing-at-point' will be used instead.

Of course, conventional arguments are supported, the two are same.

 (anything :sources sources :input input :prompt prompt :resume resume
           :preselect preselect :buffer buffer :keymap keymap)
 (anything sources input prompt resume preselect buffer keymap)


Other keywords are interpreted as local variables of this anything session.
The `anything-' prefix can be omitted.  For example,

 (anything :sources 'anything-c-source-buffers
           :buffer \"*buffers*\" :candidate-number-limit 10)

means starting anything session with `anything-c-source-buffers'
source in *buffers* buffer and set variable `anything-candidate-number-limit'
to 10 as session local variable.

\(fn &rest PLIST)" t nil)

(autoload 'anything-at-point "anything-config/anything" "\
Call anything with symbol at point as initial input.
ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT and ANY-BUFFER
are same args as in `anything'.

\(fn &optional ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT ANY-BUFFER)" t nil)

(autoload 'anything-other-buffer "anything-config/anything" "\
Simplified interface of `anything' with other `anything-buffer'.
Call `anything' with only ANY-SOURCES and ANY-BUFFER as args.

\(fn ANY-SOURCES ANY-BUFFER)" nil nil)

;;;***

;;;### (autoloads (anything-ucs anything-ratpoison-commands anything-c-run-external-command
;;;;;;  anything-eshell-history anything-esh-pcomplete anything-apt
;;;;;;  anything-world-time anything-select-xfont anything-top anything-create
;;;;;;  anything-execute-anything-command anything-call-source anything-surfraw
;;;;;;  anything-calcul-expression anything-eval-expression-with-eldoc
;;;;;;  anything-eval-expression anything-yaoddmuse-emacswiki-post-library
;;;;;;  anything-yaoddmuse-emacswiki-edit-or-view anything-all-mark-rings
;;;;;;  anything-global-mark-ring anything-mark-ring anything-simple-call-tree
;;;;;;  anything-bookmark-ext anything-manage-advice anything-M-x
;;;;;;  anything-filelist+ anything-filelist anything-do-zgrep anything-do-grep
;;;;;;  anything-dired-hardlink-file anything-dired-symlink-file
;;;;;;  anything-dired-copy-file anything-dired-rename-file anything-insert-file
;;;;;;  anything-write-file anything-find-files anything-regexp anything-info-gnus
;;;;;;  anything-org-headlines anything-browse-code anything-occur
;;;;;;  anything-list-emacs-process anything-timers anything-bm-list
;;;;;;  anything-eev-anchors anything-emms anything-org-keywords
;;;;;;  anything-man-woman anything-register anything-c-insert-latex-math
;;;;;;  anything-c-pp-bookmarks anything-bookmarks anything-colors
;;;;;;  anything-firefox-bookmarks anything-w3m-bookmarks anything-locate
;;;;;;  anything-bbdb anything-buffers-list anything-for-buffers
;;;;;;  anything-yahoo-suggest anything-google-suggest anything-imenu
;;;;;;  anything-gentoo anything-minibuffer-history anything-show-kill-ring
;;;;;;  anything-info-emacs anything-info-at-point anything-recentf
;;;;;;  anything-for-files anything-mini anything-c-toggle-match-plugin
;;;;;;  anything-c-reset-adaptative-history anything-c-set-variable
;;;;;;  anything-c-call-interactively w32-shell-execute-open-file
;;;;;;  anything-lisp-completion-or-file-name-at-point anything-lisp-completion-at-point-or-indent
;;;;;;  anything-c-complete-file-name-at-point anything-lisp-completion-at-point
;;;;;;  anything-completion-mode anything-yaoddmuse-cache-pages anything-yank-text-at-point
;;;;;;  anything-c-grep-run-save-buffer anything-c-grep-run-other-window-action
;;;;;;  anything-c-grep-run-default-action anything-c-grep-run-persistent-action
;;;;;;  anything-c-goto-next-file anything-c-goto-precedent-file
;;;;;;  anything-dired-mode anything-ff-run-kill-buffer-persistent
;;;;;;  anything-ff-properties-persistent anything-ff-run-print-file
;;;;;;  anything-ff-run-etags anything-ff-run-gnus-attach-files anything-ff-run-locate
;;;;;;  anything-ff-run-open-file-externally anything-ff-run-switch-other-frame
;;;;;;  anything-ff-run-switch-other-window anything-ff-run-switch-to-eshell
;;;;;;  anything-ff-run-complete-fn-at-point anything-ff-run-delete-file
;;;;;;  anything-ff-run-symlink-file anything-ff-run-ediff-merge-file
;;;;;;  anything-ff-run-ediff-file anything-ff-run-eshell-command-on-file
;;;;;;  anything-ff-run-load-file anything-ff-run-byte-compile-file
;;;;;;  anything-ff-run-rename-file anything-ff-run-copy-file anything-ff-run-zgrep
;;;;;;  anything-ff-run-pdfgrep anything-ff-run-grep anything-ff-run-switch-to-history
;;;;;;  anything-ff-run-toggle-auto-update anything-buffer-run-ediff
;;;;;;  anything-buffer-switch-to-elscreen anything-buffer-switch-other-frame
;;;;;;  anything-buffer-switch-other-window anything-buffer-run-query-replace
;;;;;;  anything-buffer-run-query-replace-regexp anything-buffer-run-zgrep
;;;;;;  anything-buffer-run-grep anything-buffer-run-kill-buffers
;;;;;;  anything-buffer-save-persistent anything-buffer-revert-persistent
;;;;;;  anything-buffer-diff-persistent anything-toggle-all-marks
;;;;;;  anything-unmark-all anything-mark-all anything-test-sources
;;;;;;  anything-etags-help anything-pdfgrep-help anything-grep-help
;;;;;;  anything-generic-file-help anything-read-file-name-help anything-ff-help
;;;;;;  anything-c-buffer-help anything-configuration) "anything-config/anything-config"
;;;;;;  "anything-config/anything-config.el" (20138 8733))
;;; Generated autoloads from anything-config/anything-config.el

(autoload 'anything-configuration "anything-config/anything-config" "\
Customize `anything'.

\(fn)" t nil)

(defvar anything-command-map)

(autoload 'anything-c-buffer-help "anything-config/anything-config" "\
Help command for anything buffers.

\(fn)" t nil)

(autoload 'anything-ff-help "anything-config/anything-config" "\
Help command for `anything-find-files'.

\(fn)" t nil)

(autoload 'anything-read-file-name-help "anything-config/anything-config" "\


\(fn)" t nil)

(autoload 'anything-generic-file-help "anything-config/anything-config" "\


\(fn)" t nil)

(autoload 'anything-grep-help "anything-config/anything-config" "\


\(fn)" t nil)

(autoload 'anything-pdfgrep-help "anything-config/anything-config" "\


\(fn)" t nil)

(autoload 'anything-etags-help "anything-config/anything-config" "\
The help function for etags.

\(fn)" t nil)

(autoload 'anything-test-sources "anything-config/anything-config" "\
List all anything sources for test.
The output is sexps which are evaluated by \\[eval-last-sexp].

\(fn)" t nil)

(autoload 'anything-mark-all "anything-config/anything-config" "\
Mark all visible unmarked candidates in current source.

\(fn)" t nil)

(autoload 'anything-unmark-all "anything-config/anything-config" "\
Unmark all candidates in all sources of current anything session.

\(fn)" t nil)

(autoload 'anything-toggle-all-marks "anything-config/anything-config" "\
Toggle all marks.
Mark all visible candidates of current source or unmark all candidates
visible or invisible in all sources of current anything session

\(fn)" t nil)

(autoload 'anything-buffer-diff-persistent "anything-config/anything-config" "\
Toggle diff buffer without quitting anything.

\(fn)" t nil)

(autoload 'anything-buffer-revert-persistent "anything-config/anything-config" "\
Revert buffer without quitting anything.

\(fn)" t nil)

(autoload 'anything-buffer-save-persistent "anything-config/anything-config" "\
Save buffer without quitting anything.

\(fn)" t nil)

(autoload 'anything-buffer-run-kill-buffers "anything-config/anything-config" "\
Run kill buffer action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-grep "anything-config/anything-config" "\
Run Grep action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-zgrep "anything-config/anything-config" "\
Run Grep action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-query-replace-regexp "anything-config/anything-config" "\
Run Query replace regexp action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-query-replace "anything-config/anything-config" "\
Run Query replace action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-switch-other-window "anything-config/anything-config" "\
Run switch to other window action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-switch-other-frame "anything-config/anything-config" "\
Run switch to other frame action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-switch-to-elscreen "anything-config/anything-config" "\
Run switch to elscreen  action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-ediff "anything-config/anything-config" "\
Run ediff action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-ff-run-toggle-auto-update "anything-config/anything-config" "\


\(fn)" t nil)

(autoload 'anything-ff-run-switch-to-history "anything-config/anything-config" "\
Run Switch to history action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-grep "anything-config/anything-config" "\
Run Grep action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-pdfgrep "anything-config/anything-config" "\
Run Pdfgrep action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-zgrep "anything-config/anything-config" "\
Run Grep action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-copy-file "anything-config/anything-config" "\
Run Copy file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-rename-file "anything-config/anything-config" "\
Run Rename file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-byte-compile-file "anything-config/anything-config" "\
Run Byte compile file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-load-file "anything-config/anything-config" "\
Run Load file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-eshell-command-on-file "anything-config/anything-config" "\
Run eshell command on file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-ediff-file "anything-config/anything-config" "\
Run Ediff file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-ediff-merge-file "anything-config/anything-config" "\
Run Ediff merge file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-symlink-file "anything-config/anything-config" "\
Run Symlink file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-delete-file "anything-config/anything-config" "\
Run Delete file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-complete-fn-at-point "anything-config/anything-config" "\
Run complete file name action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-switch-to-eshell "anything-config/anything-config" "\
Run switch to eshell action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-switch-other-window "anything-config/anything-config" "\
Run switch to other window action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-switch-other-frame "anything-config/anything-config" "\
Run switch to other frame action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-open-file-externally "anything-config/anything-config" "\
Run open file externally command action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-locate "anything-config/anything-config" "\
Run locate action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-gnus-attach-files "anything-config/anything-config" "\
Run gnus attach files command action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-etags "anything-config/anything-config" "\
Run Etags command action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-print-file "anything-config/anything-config" "\
Run Print file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-properties-persistent "anything-config/anything-config" "\
Show properties without quitting anything.

\(fn)" t nil)

(autoload 'anything-ff-run-kill-buffer-persistent "anything-config/anything-config" "\
Execute `anything-ff-kill-buffer-fname' whitout quitting.

\(fn)" t nil)

(defvar anything-dired-mode "Enable anything completion in Dired functions.\nBindings affected are C, R, S, H.\nThis is deprecated for Emacs24+ users, use `ac-mode' instead." "\
Non-nil if Anything-Dired mode is enabled.
See the command `anything-dired-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `anything-dired-mode'.")

(custom-autoload 'anything-dired-mode "anything-config/anything-config" nil)

(autoload 'anything-dired-mode "anything-config/anything-config" "\
Toggle Anything-Dired mode on or off.
Interactively, with no prefix argument, toggle the mode.
With universal prefix ARG turn mode on.
With zero or negative ARG turn mode off.
\\{anything-dired-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'anything-c-goto-precedent-file "anything-config/anything-config" "\
Go to precedent file in anything grep/etags buffers.

\(fn)" t nil)

(autoload 'anything-c-goto-next-file "anything-config/anything-config" "\
Go to precedent file in anything grep/etags buffers.

\(fn)" t nil)

(autoload 'anything-c-grep-run-persistent-action "anything-config/anything-config" "\
Run grep persistent action from `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-c-grep-run-default-action "anything-config/anything-config" "\
Run grep default action from `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-c-grep-run-other-window-action "anything-config/anything-config" "\
Run grep goto other window action from `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-c-grep-run-save-buffer "anything-config/anything-config" "\
Run grep save results action from `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-yank-text-at-point "anything-config/anything-config" "\
Yank text at point in minibuffer.

\(fn)" t nil)

(autoload 'anything-yaoddmuse-cache-pages "anything-config/anything-config" "\
Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'.

\(fn &optional LOAD)" t nil)

(defvar anything-completion-mode nil "\
Non-nil if Anything-Completion mode is enabled.
See the command `anything-completion-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `anything-completion-mode'.")

(custom-autoload 'anything-completion-mode "anything-config/anything-config" nil)

(autoload 'anything-completion-mode "anything-config/anything-config" "\
Toggle generic anything completion.

All functions in Emacs that use `completing-read'
or `read-file-name' and friends will use anything interface
when this mode is turned on.
However you can modify this behavior for functions of your choice
with `anything-completing-read-handlers-alist'.

Called with a positive arg, turn on inconditionnaly, with a
negative arg turn off.
You can turn it on with `ac-mode'.

Some crap emacs functions may not be supported,
e.g `ffap-alternate-file' and maybe others
You can add such functions to `anything-completing-read-handlers-alist'
with a nil value.

Note: This mode will work only partially on Emacs23.

\(fn &optional ARG)" t nil)

(autoload 'anything-lisp-completion-at-point "anything-config/anything-config" "\
Anything lisp symbol completion at point.

\(fn)" t nil)

(autoload 'anything-c-complete-file-name-at-point "anything-config/anything-config" "\
Complete file name at point.

\(fn)" t nil)

(autoload 'anything-lisp-completion-at-point-or-indent "anything-config/anything-config" "\
First call indent and second call complete lisp symbol.
The second call should happen before `anything-lisp-completion-or-indent-delay',
after this delay, next call will indent again.
After completion, next call is always indent.
See that like click and double mouse click.
One hit indent, two quick hits maybe indent and complete.

\(fn ARG)" t nil)

(autoload 'anything-lisp-completion-or-file-name-at-point "anything-config/anything-config" "\
Complete lisp symbol or filename at point.
Filename completion happen if filename is started in
or between double quotes.

\(fn)" t nil)

(autoload 'w32-shell-execute-open-file "anything-config/anything-config" "\


\(fn FILE)" t nil)

(autoload 'anything-c-call-interactively "anything-config/anything-config" "\
Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`anything-current-prefix-arg' is used as the command's prefix argument.

\(fn CMD-OR-NAME)" nil nil)

(autoload 'anything-c-set-variable "anything-config/anything-config" "\
Set value to VAR interactively.

\(fn VAR)" t nil)

(autoload 'anything-c-reset-adaptative-history "anything-config/anything-config" "\
Delete all `anything-c-adaptive-history' and his file.
Useful when you have a old or corrupted `anything-c-adaptive-history-file'.

\(fn)" t nil)

(autoload 'anything-c-toggle-match-plugin "anything-config/anything-config" "\
Toggle anything-match-plugin.

\(fn)" t nil)

(autoload 'anything-mini "anything-config/anything-config" "\
Preconfigured `anything' lightweight version (buffer -> recentf).

\(fn)" t nil)

(autoload 'anything-for-files "anything-config/anything-config" "\
Preconfigured `anything' for opening files.
ffap -> recentf -> buffer -> bookmark -> file-cache -> files-in-current-dir -> locate.

\(fn)" t nil)

(autoload 'anything-recentf "anything-config/anything-config" "\
Preconfigured `anything' for `recentf'.

\(fn)" t nil)

(autoload 'anything-info-at-point "anything-config/anything-config" "\
Preconfigured `anything' for searching info at point.
With a prefix-arg insert symbol at point.

\(fn ARG)" t nil)

(autoload 'anything-info-emacs "anything-config/anything-config" "\
Preconfigured anything for Emacs manual index.

\(fn)" t nil)

(autoload 'anything-show-kill-ring "anything-config/anything-config" "\
Preconfigured `anything' for `kill-ring'.
It is drop-in replacement of `yank-pop'.
You may bind this command to M-y.
First call open the kill-ring browser, next calls move to next line.

\(fn)" t nil)

(autoload 'anything-minibuffer-history "anything-config/anything-config" "\
Preconfigured `anything' for `minibuffer-history'.

\(fn)" t nil)

(autoload 'anything-gentoo "anything-config/anything-config" "\
Preconfigured `anything' for gentoo linux.

\(fn)" t nil)

(autoload 'anything-imenu "anything-config/anything-config" "\
Preconfigured `anything' for `imenu'.

\(fn)" t nil)

(autoload 'anything-google-suggest "anything-config/anything-config" "\
Preconfigured `anything' for google search with google suggest.

\(fn)" t nil)

(autoload 'anything-yahoo-suggest "anything-config/anything-config" "\
Preconfigured `anything' for Yahoo searching with Yahoo suggest.

\(fn)" t nil)

(autoload 'anything-for-buffers "anything-config/anything-config" "\
Preconfigured `anything' for buffer.

\(fn)" t nil)

(autoload 'anything-buffers-list "anything-config/anything-config" "\
Enhanced preconfigured `anything' for buffer.

\(fn)" t nil)

(autoload 'anything-bbdb "anything-config/anything-config" "\
Preconfigured `anything' for BBDB.

Needs BBDB.

http://bbdb.sourceforge.net/

\(fn)" t nil)

(autoload 'anything-locate "anything-config/anything-config" "\
Preconfigured `anything' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options.

You can specify a specific database with prefix argument ARG (C-u).
Many databases can be used: navigate and mark them.
See also `anything-locate-with-db'.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`anything-locate-db-file-regexp'.

\(fn ARG)" t nil)

(autoload 'anything-w3m-bookmarks "anything-config/anything-config" "\
Preconfigured `anything' for w3m bookmark.

Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/

\(fn)" t nil)

(autoload 'anything-firefox-bookmarks "anything-config/anything-config" "\
Preconfigured `anything' for firefox bookmark.
You will have to enable html bookmarks in firefox:
open about:config in firefox and double click on this line to enable value to true:

user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse you bookmarks.

\(fn)" t nil)

(autoload 'anything-colors "anything-config/anything-config" "\
Preconfigured `anything' for color.

\(fn)" t nil)

(autoload 'anything-bookmarks "anything-config/anything-config" "\
Preconfigured `anything' for bookmarks.

\(fn)" t nil)

(autoload 'anything-c-pp-bookmarks "anything-config/anything-config" "\
Preconfigured `anything' for bookmarks (pretty-printed).

\(fn)" t nil)

(autoload 'anything-c-insert-latex-math "anything-config/anything-config" "\
Preconfigured anything for latex math symbols completion.

\(fn)" t nil)

(autoload 'anything-register "anything-config/anything-config" "\
Preconfigured `anything' for Emacs registers.

\(fn)" t nil)

(autoload 'anything-man-woman "anything-config/anything-config" "\
Preconfigured `anything' for Man and Woman pages.

\(fn)" t nil)

(autoload 'anything-org-keywords "anything-config/anything-config" "\
Preconfigured `anything' for org keywords.

\(fn)" t nil)

(autoload 'anything-emms "anything-config/anything-config" "\
Preconfigured `anything' for emms sources.

\(fn)" t nil)

(autoload 'anything-eev-anchors "anything-config/anything-config" "\
Preconfigured `anything' for eev anchors.

\(fn)" t nil)

(autoload 'anything-bm-list "anything-config/anything-config" "\
Preconfigured `anything' for visible bookmarks.

Needs bm.el

http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el

\(fn)" t nil)

(autoload 'anything-timers "anything-config/anything-config" "\
Preconfigured `anything' for timers.

\(fn)" t nil)

(autoload 'anything-list-emacs-process "anything-config/anything-config" "\
Preconfigured `anything' for emacs process.

\(fn)" t nil)

(autoload 'anything-occur "anything-config/anything-config" "\
Preconfigured Anything for Occur source.
If region is active, search only in region,
otherwise search in whole buffer.

\(fn)" t nil)

(autoload 'anything-browse-code "anything-config/anything-config" "\
Preconfigured anything to browse code.

\(fn)" t nil)

(autoload 'anything-org-headlines "anything-config/anything-config" "\
Preconfigured anything to show org headlines.

\(fn)" t nil)

(autoload 'anything-info-gnus "anything-config/anything-config" "\
Preconfigured anything to browse Gnus Manual.

\(fn)" t nil)

(autoload 'anything-regexp "anything-config/anything-config" "\
Preconfigured anything to build regexps and run query-replace-regexp against.

\(fn)" t nil)

(autoload 'anything-find-files "anything-config/anything-config" "\
Preconfigured `anything' for anything implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `anything-find-files-1' instead.
This is the starting point for nearly all actions you can do on files.

\(fn ARG)" t nil)

(autoload 'anything-write-file "anything-config/anything-config" "\
Preconfigured `anything' providing completion for `write-file'.

\(fn)" t nil)

(autoload 'anything-insert-file "anything-config/anything-config" "\
Preconfigured `anything' providing completion for `insert-file'.

\(fn)" t nil)

(autoload 'anything-dired-rename-file "anything-config/anything-config" "\
Preconfigured `anything' to rename files from dired.

\(fn)" t nil)

(autoload 'anything-dired-copy-file "anything-config/anything-config" "\
Preconfigured `anything' to copy files from dired.

\(fn)" t nil)

(autoload 'anything-dired-symlink-file "anything-config/anything-config" "\
Preconfigured `anything' to symlink files from dired.

\(fn)" t nil)

(autoload 'anything-dired-hardlink-file "anything-config/anything-config" "\
Preconfigured `anything' to hardlink files from dired.

\(fn)" t nil)

(autoload 'anything-do-grep "anything-config/anything-config" "\
Preconfigured anything for grep.
Contrarily to Emacs `grep' no default directory is given, but
the full path of candidates in ONLY.
That allow to grep different files not only in `default-directory' but anywhere
by marking them (C-<SPACE>). If one or more directory is selected
grep will search in all files of these directories.
You can use also wildcard in the base name of candidate.
If a prefix arg is given use the -r option of grep.
The prefix arg can be passed before or after start.
See also `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-do-zgrep "anything-config/anything-config" "\
Preconfigured anything for zgrep.

\(fn CANDIDATE)" nil nil)

(autoload 'anything-filelist "anything-config/anything-config" "\
Preconfigured `anything' to open files instantly.

See `anything-c-filelist-file-name' docstring for usage.

\(fn)" t nil)

(autoload 'anything-filelist+ "anything-config/anything-config" "\
Preconfigured `anything' to open files/buffers/bookmarks instantly.

This is a replacement for `anything-for-files'.
See `anything-c-filelist-file-name' docstring for usage.

\(fn)" t nil)

(autoload 'anything-M-x "anything-config/anything-config" "\
Preconfigured `anything' for Emacs commands.
It is `anything' replacement of regular `M-x' `execute-extended-command'.

\(fn)" t nil)

(autoload 'anything-manage-advice "anything-config/anything-config" "\
Preconfigured `anything' to disable/enable function advices.

\(fn)" t nil)

(autoload 'anything-bookmark-ext "anything-config/anything-config" "\
Preconfigured `anything' for bookmark-extensions sources.
Needs bookmark-ext.el:
<http://mercurial.intuxication.org/hg/emacs-bookmark-extension>.
Contain also `anything-c-source-google-suggest'.

\(fn)" t nil)

(autoload 'anything-simple-call-tree "anything-config/anything-config" "\
Preconfigured `anything' for simple-call-tree. List function relationships.

Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el

\(fn)" t nil)

(autoload 'anything-mark-ring "anything-config/anything-config" "\
Preconfigured `anything' for `anything-c-source-mark-ring'.

\(fn)" t nil)

(autoload 'anything-global-mark-ring "anything-config/anything-config" "\
Preconfigured `anything' for `anything-c-source-global-mark-ring'.

\(fn)" t nil)

(autoload 'anything-all-mark-rings "anything-config/anything-config" "\
Preconfigured `anything' for `anything-c-source-global-mark-ring' and `anything-c-source-mark-ring'.

\(fn)" t nil)

(autoload 'anything-yaoddmuse-emacswiki-edit-or-view "anything-config/anything-config" "\
Preconfigured `anything' to edit or view EmacsWiki page.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

(autoload 'anything-yaoddmuse-emacswiki-post-library "anything-config/anything-config" "\
Preconfigured `anything' to post library to EmacsWiki.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

(autoload 'anything-eval-expression "anything-config/anything-config" "\
Preconfigured anything for `anything-c-source-evaluation-result'.

\(fn ARG)" t nil)

(autoload 'anything-eval-expression-with-eldoc "anything-config/anything-config" "\
Preconfigured anything for `anything-c-source-evaluation-result' with `eldoc' support. 

\(fn)" t nil)

(autoload 'anything-calcul-expression "anything-config/anything-config" "\
Preconfigured anything for `anything-c-source-calculation-result'.

\(fn)" t nil)

(autoload 'anything-surfraw "anything-config/anything-config" "\
Preconfigured `anything' to search PATTERN with search ENGINE.

\(fn PATTERN ENGINE)" t nil)

(autoload 'anything-call-source "anything-config/anything-config" "\
Preconfigured `anything' to call anything source.

\(fn)" t nil)

(autoload 'anything-execute-anything-command "anything-config/anything-config" "\
Preconfigured `anything' to execute preconfigured `anything'.

\(fn)" t nil)

(autoload 'anything-create "anything-config/anything-config" "\
Preconfigured `anything' to do many create actions from STRING.
See also `anything-create--actions'.

\(fn &optional STRING INITIAL-INPUT)" t nil)

(autoload 'anything-top "anything-config/anything-config" "\
Preconfigured `anything' for top command.

\(fn)" t nil)

(autoload 'anything-select-xfont "anything-config/anything-config" "\
Preconfigured `anything' to select Xfont.

\(fn)" t nil)

(autoload 'anything-world-time "anything-config/anything-config" "\
Preconfigured `anything' to show world time.

\(fn)" t nil)

(autoload 'anything-apt "anything-config/anything-config" "\
Preconfigured `anything' : frontend of APT package manager.
With a prefix arg reload cache.

\(fn ARG QUERY)" t nil)

(autoload 'anything-esh-pcomplete "anything-config/anything-config" "\
Preconfigured anything to provide anything completion in eshell.

\(fn)" t nil)

(autoload 'anything-eshell-history "anything-config/anything-config" "\
Preconfigured anything for eshell history.

\(fn)" t nil)

(autoload 'anything-c-run-external-command "anything-config/anything-config" "\
Preconfigured `anything' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`anything-c-external-commands-list'.

\(fn PROGRAM)" t nil)

(autoload 'anything-ratpoison-commands "anything-config/anything-config" "\
Preconfigured `anything' to execute ratpoison commands.

\(fn)" t nil)

(autoload 'anything-ucs "anything-config/anything-config" "\
Preconfigured anything for `ucs-names' math symbols.

\(fn)" t nil)

;;;***

;;;### (autoloads (color-theme-solarized-light color-theme-solarized-dark
;;;;;;  color-theme-solarized) "color-theme-solarized/color-theme-solarized"
;;;;;;  "color-theme-solarized/color-theme-solarized.el" (20138 4809))
;;; Generated autoloads from color-theme-solarized/color-theme-solarized.el

(autoload 'color-theme-solarized "color-theme-solarized/color-theme-solarized" "\
Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized.

\(fn MODE)" t nil)

(autoload 'color-theme-solarized-dark "color-theme-solarized/color-theme-solarized" "\


\(fn)" t nil)

(autoload 'color-theme-solarized-light "color-theme-solarized/color-theme-solarized" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (color-theme-initialize color-theme-submit color-theme-install
;;;;;;  color-theme-compare color-theme-make-snapshot color-theme-analyze-defun
;;;;;;  color-theme-print color-theme-install-at-point-for-current-frame
;;;;;;  color-theme-install-at-mouse color-theme-describe color-theme-select)
;;;;;;  "color-theme/color-theme" "color-theme/color-theme.el" (17529
;;;;;;  41105))
;;; Generated autoloads from color-theme/color-theme.el

(autoload 'color-theme-select "color-theme/color-theme" "\
Displays a special buffer for selecting and installing a color theme.
With optional prefix ARG, this buffer will include color theme libraries
as well.  A color theme library is in itself not complete, it must be
used as part of another color theme to be useful.  Thus, color theme
libraries are mainly useful for color theme authors.

\(fn &optional ARG)" t nil)

(autoload 'color-theme-describe "color-theme/color-theme" "\
Describe color theme listed at point.
This shows the documentation of the value of text-property color-theme
at point.  The text-property color-theme should be a color theme
function.  See `color-themes'.

\(fn)" t nil)

(autoload 'color-theme-install-at-mouse "color-theme/color-theme" "\
Install color theme clicked upon using the mouse.
First argument EVENT is used to set point.  Then
`color-theme-install-at-point' is called.

\(fn EVENT)" t nil)

(autoload 'color-theme-install-at-point-for-current-frame "color-theme/color-theme" "\
Install color theme at point for current frame only.
Binds `color-theme-is-global' to nil and calls
`color-theme-install-at-point'.

\(fn)" t nil)

(autoload 'color-theme-print "color-theme/color-theme" "\
Print the current color theme function.

You can contribute this function to <URL:news:gnu.emacs.sources> or
paste it into your .emacs file and call it.  That should recreate all
the settings necessary for your color theme.

Example:

    (require 'color-theme)
    (defun my-color-theme ()
      \"Color theme by Alex Schroeder, created 2000-05-17.\"
      (interactive)
      (color-theme-install
       '(...
	 ...
	 ...)))
    (my-color-theme)

If you want to use a specific color theme function, you can call the
color theme function in your .emacs directly.

Example:

    (require 'color-theme)
    (color-theme-gnome2)

\(fn &optional BUF)" t nil)

(autoload 'color-theme-analyze-defun "color-theme/color-theme" "\
Once you have a color-theme printed, check for missing faces.
This is used by maintainers who receive a color-theme submission
and want to make sure it follows the guidelines by the color-theme
author.

\(fn)" t nil)

(autoload 'color-theme-make-snapshot "color-theme/color-theme" "\
Return the definition of the current color-theme.
The function returned will recreate the color-theme in use at the moment.

\(fn)" nil nil)

(autoload 'color-theme-compare "color-theme/color-theme" "\
Compare two color themes.
This will print the differences between installing THEME-A and
installing THEME-B.  Note that the order is important: If a face is
defined in THEME-A and not in THEME-B, then this will not show up as a
difference, because there is no reset before installing THEME-B.  If a
face is defined in THEME-B and not in THEME-A, then this will show up as
a difference.

\(fn THEME-A THEME-B)" t nil)

(autoload 'color-theme-install "color-theme/color-theme" "\
Install a color theme defined by frame parameters, variables and faces.

The theme is installed for all present and future frames; any missing
faces are created.  See `color-theme-install-faces'.

THEME is a color theme definition.  See below for more information.

If you want to install a color theme from your .emacs, use the output
generated by `color-theme-print'.  This produces color theme function
which you can copy to your .emacs.

A color theme definition is a list:
\([FUNCTION] FRAME-PARAMETERS VARIABLE-SETTINGS FACE-DEFINITIONS)

FUNCTION is the color theme function which called `color-theme-install'.
This is no longer used.  There was a time when this package supported
automatic factoring of color themes.  This has been abandoned.

FRAME-PARAMETERS is an alist of frame parameters.  These are installed
with `color-theme-install-frame-params'.  These are installed last such
that any changes to the default face can be changed by the frame
parameters.

VARIABLE-DEFINITIONS is an alist of variable settings.  These are
installed with `color-theme-install-variables'.

FACE-DEFINITIONS is an alist of face definitions.  These are installed
with `color-theme-install-faces'.

If `color-theme-is-cumulative' is nil, a color theme will undo face and
frame-parameter settings of previous color themes.

\(fn THEME)" nil nil)

(autoload 'color-theme-submit "color-theme/color-theme" "\
Submit your color-theme to the maintainer.

\(fn)" t nil)

(autoload 'color-theme-initialize "color-theme/color-theme" "\
Initialize the color theme package by loading color-theme-libraries.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "evil/evil-core" "evil/evil-core.el" (20138
;;;;;;  4635))
;;; Generated autoloads from evil/evil-core.el
 (autoload 'evil-mode "evil")

;;;***

;;;### (autoloads (global-linum-mode linum-mode linum-format) "linum-ex/linum-ex"
;;;;;;  "linum-ex/linum-ex.el" (20138 7613))
;;; Generated autoloads from linum-ex/linum-ex.el

(defvar linum-format 'dynamic "\
Format used to display line numbers. Either a format string
like \"%7d\", 'dynamic to adapt the width as needed, or a
function that is called with a line number as its argument and
should evaluate to a string to be shown on that line. See also
`linum-before-numbering-hook'.")

(custom-autoload 'linum-format "linum-ex/linum-ex" t)

(autoload 'linum-mode "linum-ex/linum-ex" "\
Toggle display of line numbers in the left marginal area.

\(fn &optional ARG)" t nil)

(defvar global-linum-mode nil "\
Non-nil if Global-Linum mode is enabled.
See the command `global-linum-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-linum-mode'.")

(custom-autoload 'global-linum-mode "linum-ex/linum-ex" nil)

(autoload 'global-linum-mode "linum-ex/linum-ex" "\
Toggle Linum mode in every possible buffer.
With prefix ARG, turn Global-Linum mode on if and only if
ARG is positive.
Linum mode is enabled in all buffers where
`linum-on' would do it.
See `linum-mode' for more information on Linum mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (magit-status) "magit/magit" "magit/magit.el" (20138
;;;;;;  9960))
;;; Generated autoloads from magit/magit.el

(autoload 'magit-status "magit/magit" "\
Open a Magit status buffer for the Git repository containing
DIR.  If DIR is not within a Git repository, offer to create a
Git repository in DIR.

Interactively, a prefix argument means to ask the user which Git
repository to use even if `default-directory' is under Git control.
Two prefix arguments means to ignore `magit-repo-dirs' when asking for
user input.

\(fn DIR)" t nil)

;;;***

;;;### (autoloads (rebase-mode) "magit/rebase-mode" "magit/rebase-mode.el"
;;;;;;  (20138 9960))
;;; Generated autoloads from magit/rebase-mode.el

(autoload 'rebase-mode "magit/rebase-mode" "\
Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("git-rebase-todo" . rebase-mode))

;;;***

;;;### (autoloads (rvm-open-gem rvm-use rvm-activate-corresponding-ruby
;;;;;;  rvm-use-default) "rvm/rvm" "rvm/rvm.el" (20138 5800))
;;; Generated autoloads from rvm/rvm.el

(autoload 'rvm-use-default "rvm/rvm" "\
use the rvm-default ruby as the current ruby version

\(fn)" t nil)

(autoload 'rvm-activate-corresponding-ruby "rvm/rvm" "\
activate the corresponding ruby version for the file in the current buffer.
This function searches for an .rvmrc file and actiavtes the configured ruby.
If no .rvmrc file is found, the default ruby is used insted.

\(fn)" t nil)

(autoload 'rvm-use "rvm/rvm" "\
switch the current ruby version to any ruby, which is installed with rvm

\(fn NEW-RUBY NEW-GEMSET)" t nil)

(autoload 'rvm-open-gem "rvm/rvm" "\


\(fn GEMHOME)" t nil)

;;;***

;;;### (autoloads (textmate-mode) "textmate/textmate" "textmate/textmate.el"
;;;;;;  (20138 5818))
;;; Generated autoloads from textmate/textmate.el

(defvar textmate-mode nil "\
Non-nil if Textmate mode is enabled.
See the command `textmate-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `textmate-mode'.")

(custom-autoload 'textmate-mode "textmate/textmate" nil)

(autoload 'textmate-mode "textmate/textmate" "\
TextMate Emulation Minor Mode

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (global-undo-tree-mode undo-tree-mode) "undo-tree/undo-tree"
;;;;;;  "undo-tree/undo-tree.el" (20138 4510))
;;; Generated autoloads from undo-tree/undo-tree.el

(autoload 'undo-tree-mode "undo-tree/undo-tree" "\
Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-map}

\(fn &optional ARG)" t nil)

(defvar global-undo-tree-mode nil "\
Non-nil if Global-Undo-Tree mode is enabled.
See the command `global-undo-tree-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-tree-mode'.")

(custom-autoload 'global-undo-tree-mode "undo-tree/undo-tree" nil)

(autoload 'global-undo-tree-mode "undo-tree/undo-tree" "\
Toggle Undo-Tree mode in every possible buffer.
With prefix ARG, turn Global-Undo-Tree mode on if and only if
ARG is positive.
Undo-Tree mode is enabled in all buffers where
`turn-on-undo-tree-mode' would do it.
See `undo-tree-mode' for more information on Undo-Tree mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("anything-config/anything-match-plugin.el"
;;;;;;  "auto-complete/auto-complete-config.el" "auto-complete/auto-complete.el"
;;;;;;  "auto-complete/fuzzy.el" "auto-complete/popup.el" "color-theme-solarized/solarized-dark-theme.el"
;;;;;;  "color-theme-solarized/solarized-definitions.el" "color-theme-solarized/solarized-light-theme.el"
;;;;;;  "color-theme/color-theme-autoloads.el" "el-get/el-get-install.el"
;;;;;;  "el-get/el-get.el" "evil/evil-common.el" "evil/evil-compatibility.el"
;;;;;;  "evil/evil-digraphs.el" "evil/evil-ex.el" "evil/evil-insert.el"
;;;;;;  "evil/evil-integration.el" "evil/evil-interactive.el" "evil/evil-maps.el"
;;;;;;  "evil/evil-motions.el" "evil/evil-operators.el" "evil/evil-pkg.el"
;;;;;;  "evil/evil-repeat.el" "evil/evil-replace.el" "evil/evil-search.el"
;;;;;;  "evil/evil-tests.el" "evil/evil-types.el" "evil/evil-undo.el"
;;;;;;  "evil/evil-vars.el" "evil/evil-visual.el" "evil/evil-window.el"
;;;;;;  "evil/evil.el" "flymake-ruby/flymake-ruby.el" "magit/50magit.el"
;;;;;;  "magit/magit-bisect.el" "magit/magit-key-mode.el" "magit/magit-pkg.el"
;;;;;;  "magit/magit-stgit.el" "magit/magit-svn.el" "magit/magit-topgit.el"
;;;;;;  "package/package.el" "rhtml-mode/rhtml-erb.el" "rhtml-mode/rhtml-fonts.el"
;;;;;;  "rhtml-mode/rhtml-mode.el" "rhtml-mode/rhtml-navigation.el"
;;;;;;  "rhtml-mode/rhtml-ruby-hook.el" "rhtml-mode/rhtml-sgml-hacks.el"
;;;;;;  "yaml-mode/yaml-mode.el") (20138 9963 39095))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
