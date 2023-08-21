[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/lsp-uniteai.svg)](https://jcs-emacs.github.io/jcs-elpa/#/lsp-uniteai)

# lsp-uniteai
> LSP Clients for UniteAI

`lsp-mode` client leveraging [uniteai](https://github.com/freckletonj/uniteai).

## 💾 Quickstart

```elisp
(use-package lsp-uniteai
  :ensure t
  :hook (python-mode . (lambda ()
                         (require lsp-uniteai)
                         (lsp))))  ; or lsp-deferred
```
