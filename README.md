# Proof-General Bullet

This is a [proof general](https://github.com/ProofGeneral) extension
which detects when the current subgoal, annotated with a bullet, is
proved (after `C-c C-n` command) and automatically inserts a proper
bullet for a next sibling subgoal or for a sibling of the goal (if
last subgoal is proved).

## Installation

### sources

``` shell
mkdir -p ~/.emacs.d
cd ~/.emacs.d
git clone https://github.com/yaitskov/proof-general-bullet.git
```

### init.el

Append following lines to `~/.emacs.d/init.el` file:

``` emacs-lisp
(add-to-list 'load-path "~/.emacs.d/proof-general-bullet")
(require 'proof-general-bullet)
```
