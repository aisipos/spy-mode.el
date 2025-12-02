# spy-mode

An Emacs major mode for the [SPy](https://github.com/spylang/spy) programming language.

## Features

- **Syntax highlighting** - Derives from `python-ts-mode` (if available) or `python-mode`
- **Compile-time/Runtime colorization** - Visual distinction between blue (compile-time) and red (runtime) code
- **Compiler pipeline inspection** - View Python AST, SPy AST, imports, symbol tables, and more
- **Execution & compilation** - Run SPy code directly or compile to native executables
- **Hydra menu** - Quick access to all SPy commands via `C-c s` (hydra-spy/body)

## Installation

### use-package

```elisp
(use-package spy-mode
  :ensure t
  :vc (:fetcher github :repo aisipos/spy-mode.el)
  :files ("*.el")
  :custom
  (spy-command "spy")  ; or "uv run spy"
  (spy-mode-reuse-output-buffer nil))  ; set to t to reuse a single output buffer
```

### Manual Installation

1. Clone or download this repository
2. Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/spy-mode")
(require 'spy-mode)
```

### Configuration

Set the SPy compiler command (defaults to `"spy"`):

```elisp
(setq spy-command "spy")
;; Or if using uv:
(setq spy-command "uv run spy")
```

Optionally reuse a single output buffer for all commands:

```elisp
(setq spy-mode-reuse-output-buffer t)
```

## Usage

### Hydra Menu

Press `C-c s` in a SPy buffer to open the hydra menu with quick access to all commands:

### Manual Commands

All commands are also available via `M-x`:

- `spy-show-pyparse`
- `spy-show-parse`
- `spy-show-imports`
- `spy-show-symtable`
- `spy-show-redshift`
- `spy-compile-executable`
- `spy-execute-buffer`
- `spy-redshift-execute-buffer`
- `spy-show-cwrite`
- `spy-show-cdump`
- `spy-colorize-buffer` - With prefix arg (`C-u`), retains output buffer
- `spy-colorize-clear-buffer`
- `spy-toggle-colorize-buffer`

## Colorization

Use `spy-colorize-buffer` (or `c` in the hydra) to apply colorization based on `spy --colorize --format=json` output.

## Requirements

- [SPy compiler](https://github.com/spylang/spy)
- Optional: [hydra](https://github.com/abo-abo/hydra) for the command menu

