# helm-git-grep.el

[![License GPL 3][gplv3-badge]][LICENCE]
[![travis badge][travis-badge]][travis-link]
[![coveralls badge][coveralls-badge]][coveralls-link]
[![melpa badge][melpa-badge]][melpa-link]
[![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

[gplv3-badge]:https://img.shields.io/github/license/yasuyk/helm-git-grep.svg
[LICENCE]: https://github.com/yasuyk/helm-git-grep/blob/master/LICENSE
[travis-badge]: https://travis-ci.org/yasuyk/helm-git-grep.svg
[travis-link]: https://travis-ci.org/yasuyk/helm-git-grep
[coveralls-badge]: https://coveralls.io/repos/github/yasuyk/helm-git-grep/badge.svg?branch=master
[coveralls-link]:https://coveralls.io/github/yasuyk/helm-git-grep?branch=master
[melpa-link]: http://melpa.org/#/helm-git-grep
[melpa-stable-link]: http://stable.melpa.org/#/helm-git-grep
[melpa-badge]: http://melpa.org/packages/helm-git-grep-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/helm-git-grep-badge.svg

[helm] for [git-grep(1)][git-grep], an incremental [git-grep(1)][git-grep].

![helm-git-grep](https://cloud.githubusercontent.com/assets/833383/10489416/0d7e15d6-726b-11e5-9d3e-3f7fc7ee1221.gif)

# Table of Contents

  * [helm\-git\-grep\.el](#helm-git-grepel)
  * [Table of Contents](#table-of-contents)
    * [Features](#features)
    * [Minimum requirements](#minimum-requirements)
    * [Optional requirements](#optional-requirements)
    * [Installation](#installation)
    * [Configuration](#configuration)
    * [Basic usage](#basic-usage)
        * [<kbd>M\-x</kbd> helm\-git\-grep](#m-x-helm-git-grep)
        * [<kbd>M\-x</kbd> helm\-git\-grep\-at\-point](#m-x-helm-git-grep-at-point)
        * [Actions](#actions)
    * [Advanced usage](#advanced-usage)
        * [Keymap helm\-git\-grep\-map](#keymap-helm-git-grep-map)
        * [<kbd>C\-c i</kbd> helm\-git\-grep\-toggle\-ignore\-case](#c-c-i-helm-git-grep-toggle-ignore-case)
    * [Customization](#customization)
      * [Variables](#variables)
        * [helm\-git\-grep\-sources](#helm-git-grep-sources)
        * [helm\-git\-grep\-candidate\-number\-limit](#helm-git-grep-candidate-number-limit)
        * [helm\-git\-grep\-max\-length\-history](#helm-git-grep-max-length-history)
        * [helm\-git\-grep\-use\-ioccur\-style\-keys](#helm-git-grep-use-ioccur-style-keys)
        * [helm\-git\-grep\-ignore\-case](#helm-git-grep-ignore-case)
        * [helm\-git\-grep\-at\-point\-deactivate\-mark](#helm-git-grep-at-point-deactivate-mark)
        * [helm\-git\-grep\-base\-directory](#helm-git-grep-base-directory)
        * [helm\-git\-grep\-pathspecs](#helm-git-grep-pathspecs)
      * [Faces](#faces)
        * [helm\-git\-grep\-match](#helm-git-grep-match)
        * [helm\-git\-grep\-file](#helm-git-grep-file)
        * [helm\-git\-grep\-line](#helm-git-grep-line)

Created by [gh-md-toc](https://github.com/ekalinin/github-markdown-toc.go)
## Features

- Grep submodules too, if submodules exists.
- Open in other window, other frame, or [`elscreen`].
- Toggle ignore case option when incremental greping.
- Save grep result in grep buffer which is writable with [`wgrep`].

## Minimum requirements

- [helm]
- [git]

## Optional requirements

- [wgrep]

    wgrep enable a grep buffer editable and apply those changes to the file buffer.

- [elscreen]

    You can open a file in elscleen.

- [git] >= 1.9.0

    You must have the git version 1.9.0 or above to use [pathspec] feature.

## Installation

If you're an Emacs 24 user or you have a recent version of package.el
you can install `helm-git-grep.el` from the [MELPA](http://melpa.milkbox.net/) repository.

## Configuration

Add the following to your emacs init file.

    (require 'helm-git-grep) ;; Not necessary if installed by package.el
    (global-set-key (kbd "C-c g") 'helm-git-grep)
    ;; Invoke `helm-git-grep' from isearch.
    (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
    ;; Invoke `helm-git-grep' from other helm.
    (eval-after-load 'helm
      '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))

## Basic usage

#### <kbd>M-x</kbd> `helm-git-grep`

Helm git grep. if submodules exists, grep submodules too.

#### <kbd>M-x</kbd> `helm-git-grep-at-point`

Helm git grep with symbol at point. if submodules exists, grep submodules too.

#### Actions

These Actions are available.

|Action|Description|
|------|-----------|
|Find file | Jump to result.|
|Find file other frame | Jump to result in other frame.|
|Find file in Elscreen | Jump to result in elscreen.|
|Save results in grep buffer | Save helm git grep result in a `grep-mode` buffer.|
|Find file other window | Jump to result in other window.|

## Advanced usage

#### Keymap `helm-git-grep-map`

These commands are available when executing incremental [git grep] by `helm-git-grep`.

For more information about keymap, execute <kbd>C-c ?</kbd> `helm-git-grep-help`.

|Key |Command|Description|
|----|-------|-----------|
|<kbd>C-z</kbd>|helm-git-grep-persistent-action |Persistent action. With a prefix arg record candidate in `mark-ring`.|
|<kbd>C-c C-o</kbd>|helm-git-grep-run-other-frame-action|Jump to result in other frame.|
|<kbd>C-c e</kbd>|helm-git-grep-run-elscreen-action |Jump to result in elscreen.|
|<kbd>C-x C-s</kbd>|helm-git-grep-run-save-buffer|Save helm git grep result in a `grep-mode` buffer.
|<kbd>C-c o</kbd>|helm-git-grep-run-other-window-action |Jump to result in other window.|
|<kbd>C-c i</kbd>|helm-git-grep-toggle-ignore-case| Toggle ignore case option.|
|<kbd>C-w</kbd>|helm-yank-text-at-point|Yank text at point in invocation buffer into minibuffer.|
|<kbd>C-c ?</kbd>|helm-git-grep-help |Help command for `helm-git-grep`.|
|<kbd>M-&lt;down&gt;</kbd> |helm-goto-next-file | Go to precedent file in helm git grep buffers. |
|<kbd>M-&lt;up&gt;</kbd>|helm-goto-precedent-file| Go to next file in helm git grep buffers. |

#### <kbd>C-c i</kbd> helm-git-grep-toggle-ignore-case

Toggle ignore case option when incremental greping.

The ignore case option is correspond to `-i` option of `git grep`.

A buffer name is `*helm git grep [i]*` with ignore case option.

![](https://raw.github.com/yasuyk/misc/master/helm-git-grep/image/with-ignore-case-option.png)

A buffer name is `*helm git grep*` without ignore case option.

![](https://raw.github.com/yasuyk/misc/master/helm-git-grep/image/without-ignore-case-option.png)

## Customization

### Variables

#### `helm-git-grep-sources`

**(Default: `'(helm-git-grep-source helm-git-grep-submodule-source)`)**

Default helm sources for `helm-git-grep`.

If you don't want to search in submodules, Set only `helm-git-grep-source` like this:

    (setq helm-git-grep-sources '(helm-git-grep-sources))

#### `helm-git-grep-candidate-number-limit`

**(Default: `300`)**

Limit candidate number `helm-git-grep`.

#### `helm-git-grep-max-length-history`

**(Default: `100`)**

Max number of elements to save in `helm-git-grep-history`.

#### `helm-git-grep-use-ioccur-style-keys`

**(Default: `t`)**

Use Arrow keys to jump to occurrences.

#### `helm-git-grep-ignore-case`

**(Default: `t`)**

Ignore case when matching.

#### `helm-git-grep-at-point-deactivate-mark`

**(Default: `nil`)**

Deactivate the mark when `helm-git-grep-at-point` is invoked.

#### `helm-git-grep-base-directory`

**(Default: `'root`)**

Base directory for search by git-grep(1).

Possible value are:

- root: git root directory
- current: current directory (default directory of current buffer)

#### `helm-git-grep-pathspecs`

**(Default: `nil`)**

Pattern used to limit paths in git-grep(1) commands.

Each pathspec have not to be quoted by singe quotation like executing git command in inferior shell.  Because `helm-git-grep` run git command by `start-process`, and `start-process` is not executed in inferior shell. So, if pathspec is quoted by singe quotation, pathspec can't work in git-grep(1) by `helm-git-grep`.

For more information about pathspec, See [pathspec] in Git gitglossary Documentation.

If there is something wrong about pathspec configuration, you can check limit paths by pathspec using `helm-git-grep-ls-files-limited-by-pathspec`.

### Faces

#### `helm-git-grep-match`

Face used to highlight git-grep(1) matches.

#### `helm-git-grep-file`

Face used to highlight git-grep(1) results filenames.

#### `helm-git-grep-line`

Face used to highlight git-grep(1) number lines.

[helm]:https://github.com/emacs-helm/helm
[git]:http://git-scm.com/
[git-grep]:http://git-scm.com/docs/git-grep
[elscreen]:https://github.com/shosti/elscreen
[wgrep]:https://github.com/mhayashi1120/Emacs-wgrep
[pathspec]:https://git-scm.com/docs/gitglossary#def_pathspec
