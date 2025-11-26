;;; spy-mode.el --- Major mode for SPy language -*- lexical-binding: t -*-

(defface spy-blue
  '((((class color) (min-colors 256) (background light))
     :background "blue" :foreground "white")
    (((class color) (min-colors 256) (background dark))
     :background "blue" :foreground "white")
    (((class color) (min-colors 16))
     :background "blue" :foreground "white")
    (t :inverse-video t))
  "Face for compile-time (blue) code in SPy.")

(defface spy-red
  '((((class color) (min-colors 256) (background light))
     :background "red" :foreground "white")
    (((class color) (min-colors 256) (background dark))
     :background "red" :foreground "white")
    (((class color) (min-colors 16))
     :background "red" :foreground "white")
    (t :inverse-video t))
  "Face for runtime (red) code in SPy.")

(defvar-local spy-buffer-colorized-p nil
  "Non-nil if the current buffer has spy colorization applied.")

(defun spy-call-spy (args &optional output-buffer-name)
  "Call spy compiler with ARGS and display output in OUTPUT-BUFFER-NAME.
ARGS should be a list of strings (e.g., '(\"--parse\" \"--dump\")).
OUTPUT-BUFFER-NAME defaults to \"*SPy output*\".
Shows the output buffer in another window as output is generated."
  (interactive "sspy args: ")
  (unless buffer-file-name
    (error "Buffer is not visiting a file"))
  (let* ((filename buffer-file-name)
         (buf-name (or output-buffer-name "*SPy output*"))
         (args-list (if (stringp args)
                        (split-string args)
                      args))
         (full-command (append (list "spy") args-list (list filename))))
    ;; Create or clear the output buffer
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (compilation-mode)
      (display-buffer (current-buffer)))
    ;; Start the async process
    (make-process
     :name "spy"
     :buffer buf-name
     :command full-command
     :sentinel (lambda (proc event)
                 (with-current-buffer (process-buffer proc)
                   (let ((inhibit-read-only t))
                     (goto-char (point-max))
                     (insert (format "\n\nProcess %s %s"
                                   (process-name proc)
                                   event))))))))

(defmacro spy-defcommand (name flag buffer-name docstring)
  "Define an interactive command to call spy with FLAG and show output in BUFFER-NAME."
  `(defun ,name ()
     ,docstring
     (interactive)
     (spy-call-spy (list ,flag) ,buffer-name)))

(spy-defcommand spy-show-pyparse "--pyparse" "*SPy Python AST*"
                "Run 'spy --pyparse' and show the Python AST in another window.")

(spy-defcommand spy-show-parse "--parse" "*SPy AST*"
                "Run 'spy --parse' and show the SPy AST in another window.")

(spy-defcommand spy-show-imports "--imports" "*SPy imports*"
                "Run 'spy --imports' and show the recursive list of imports in another window.")

(spy-defcommand spy-show-symtable "--symtable" "*SPy symtable*"
                "Run 'spy --symtable' and show the symbol tables in another window.")

(spy-defcommand spy-show-redshift "--redshift" "*SPy redshift*"
                "Run 'spy --redshift' and show the redshifted AST in another window.")

(spy-defcommand spy-show-cwrite "--cwrite" "*SPy C code*"
                "Run 'spy --cwrite' and show the generated C code in another window.")

(spy-defcommand spy-show-cdump "--cdump" "*SPy C dump*"
                "Run 'spy --cdump' and show the generated C code in another window.")

(defun spy-apply-highlights-from-json (json-string)
  "Parse JSON-STRING and apply spy-blue or spy-red overlays to current buffer.

JSON-STRING should be the output from 'spy --colorize --format=json',
with format: [{\"line\": N, \"col\": C, \"length\": L, \"type\": \"blue|red\"}, ...]"
  (let ((highlights (json-parse-string json-string
                                       :object-type 'plist
                                       :array-type 'list)))
    ;; Remove any existing spy highlighting overlays
    (remove-overlays (point-min) (point-max) 'spy-highlight t)

    ;; Apply new highlights
    (save-excursion
      (dolist (hl highlights)
        (let* ((line (plist-get hl :line))
               (col (plist-get hl :col))
               (len (plist-get hl :length))
               (type (plist-get hl :type))
               (face (intern (format "spy-%s-face" type))))
          ;; Go to the specified line and column
          (goto-char (point-min))
          (forward-line (1- line))
          (move-to-column col)
          (let* ((start (point))
                 (end (+ start len))
                 (ov (make-overlay start end)))
            ;; Tag this overlay as belonging to spy-mode (not a face!)
            (overlay-put ov 'spy-highlight t)
            ;; Apply the actual face
            (overlay-put ov 'face face)))))))

(defun spy-colorize-buffer ()
  "Run 'spy --colorize --format=json' asynchronously on current buffer and apply highlighting."
  (interactive)
  (unless buffer-file-name
    (error "Buffer is not visiting a file"))
  (if spy-buffer-colorized-p
      (spy-colorize-clear-buffer))
  ;; Kill any existing output buffer to start fresh
  (when (get-buffer "*spy-colorize-output*")
    (kill-buffer "*spy-colorize-output*"))
  (let* ((filename buffer-file-name)
         (buffer (current-buffer)))
    (make-process
     :name "spy-colorize"
     :buffer "*spy-colorize-output*"
     :command (list "spy" "--colorize" "--format=json" filename)
     :sentinel (lambda (proc event)
                 (when (string= event "finished\n")
                   (with-current-buffer buffer
                     (let ((output (with-current-buffer (process-buffer proc)
                                     (buffer-string))))
                       ;; Extract just the JSON array from the output
                       ;; Use \\(?:.\\|\n\\) to match any character including newlines
                       (if (string-match "\\(\\[\\(?:.\\|\n\\)*?\\]\\)" output)
                           (let ((json-output (match-string 1 output)))
                             (condition-case err
                                 (progn
                                   (spy-apply-highlights-from-json json-output)
                                   (setq spy-buffer-colorized-p t)
                                   (kill-buffer (process-buffer proc))
                                   (message "Applied spy --colorize to current buffer"))
                               (error
                                (message "spy-colorize error: %s. Check *spy-colorize-output* buffer" err))))
                         (message "spy-colorize: No JSON found in output. Check *spy-colorize-output* buffer")))))))))

(defun spy-colorize-clear-buffer ()
  "Remove all spy colorization overlays from the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'spy-highlight t)
  (message "Cleared spy --colorize from current buffer")
  (setq spy-buffer-colorized-p nil))

(defun spy-toggle-colorize-buffer ()
  "Toggle spy colorization for the current buffer."
  (interactive)
  (if spy-buffer-colorized-p
      (spy-colorize-clear-buffer)
    (spy-colorize-buffer)))

(defconst spy-mode-name
  (if (char-displayable-p ?🥸)
      "🥸 SPy"
    "SPy")
  "Name of spy-mode for display in the mode line.")

(defvar-keymap spy-mode-map
  :doc "Keymap for spy-mode, inherits from python-ts-mode-map."
  :parent python-ts-mode-map
  "C-c C-c" #'spy-colorize-buffer
  "C-c C-k" #'spy-colorize-clear-buffer
  "C-c C-t" #'spy-toggle-colorize-buffer)

(define-derived-mode spy-mode python-ts-mode spy-mode-name
  "Major mode for editing SPy files, derived from python-ts-mode.")

;; Hydra menu for SPy commands (if hydra is available)
(when (require 'hydra nil t)
  (defhydra hydra-spy (:color blue :hint nil)
    "
^Compiler Pipeline^     ^Colorization^       ^Output^
^^^^^^^^─────────────────────────────────────────────
_P_: Python AST         _c_: Colorize        _w_: C Write
_p_: SPy AST            _k_: Clear           _d_: C Dump
_i_: Imports            _t_: Toggle
_s_: Symtable
_r_: Redshift
_q_: quit
"
    ("P" spy-show-pyparse "Python AST")
    ("p" spy-show-parse "SPy AST")
    ("i" spy-show-imports "Imports")
    ("s" spy-show-symtable "Symtable")
    ("r" spy-show-redshift "Redshift")
    ("w" spy-show-cwrite "C Write")
    ("d" spy-show-cdump "C Dump")
    ("c" spy-colorize-buffer "Colorize")
    ("k" spy-colorize-clear-buffer "Clear")
    ("t" spy-toggle-colorize-buffer "Toggle")
    ("q" nil "quit"))

  ;; Add keybinding to invoke the hydra
  (define-key spy-mode-map (kbd "C-c s") 'hydra-spy/body))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.spy\\'" . spy-mode))

(provide 'spy-mode)
