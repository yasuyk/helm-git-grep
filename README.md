# helm-git-grep.el

[helm] for [git-grep(1)][git-grep], an incremental [git-grep(1)][git-grep].

![](https://raw.github.com/yasuyk/misc/master/helm-git-grep/image/helm-git-grep.gif)

## Features

- Grep submodules too, if submodules exists.
- Open in other window, other frame, or [`elscreen`](https://github.com/shosti/elscreen).
- Toggle ignore case option when incremental greping.
- Save grep result in grep buffer which is writable with [`wgrep`](https://github.com/mhayashi1120/Emacs-wgrep).

## Requirements

- [helm]
- [git]

## Installation

If you're an Emacs 24 user or you have a recent version of package.el
you can install `helm-git-grep.el` from the [MELPA](http://melpa.milkbox.net/) repository.

## Configuration

Add the following to your emacs init file.

    (require 'helm-git-grep) ;; Not necessary if installed by package.el
    (global-set-key (kbd "C-c g") 'helm-git-grep)
    ;; Invoke `helm-git-grep' from isearch.
    (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)


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

#### `helm-git-grep-candidate-number-limit`(Default: `300`)

Limit candidate number `helm-git-grep`.

#### `helm-git-grep-max-length-history`(Default: `100`)

Max number of elements to save in `helm-git-grep-history`.

#### `helm-git-grep-use-ioccur-style-keys`(Default: `t`)

Use Arrow keys to jump to occurrences.

## Note

I'm poor at English. Please point out or correct errors in this document, if any.

[helm]:https://github.com/emacs-helm/helm
[git]:http://git-scm.com/
[git-grep]:http://git-scm.com/docs/git-grep
