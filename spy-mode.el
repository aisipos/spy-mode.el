;;; spy-mode.el --- Major mode for SPy language -*- lexical-binding: t -*-
;; spy-mode.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; spy-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with spy-mode.el. If not, see https://www.gnu.org/licenses/

(defface spy-blue
  '((((class color) (min-colors 256) (background light))
     :background "#e3f2fd" :foreground "#0d47a1")
    (((class color) (min-colors 256) (background dark))
     :background "#1a237e" :foreground "#bbdefb")
    (((class color) (min-colors 16))
     :background "blue" :foreground "white")
    (t :inverse-video t))
  "Face for compile-time (blue) code in SPy.")

(defface spy-red
  '((((class color) (min-colors 256) (background light))
     :background "#ffebee" :foreground "#c62828")
    (((class color) (min-colors 256) (background dark))
     :background "#4a1a1a" :foreground "#ff6b6b")
    (((class color) (min-colors 16))
     :background "red" :foreground "white")
    (t :inverse-video t))
  "Face for runtime (red) code in SPy.")

(defvar-local spy-buffer-colorized-p nil
  "Non-nil if the current buffer has spy colorization applied.")

(defvar spy-command "spy"
  "Command to run the SPy compiler.")

(defcustom spy-mode-reuse-output-buffer nil
  "If non-nil, reuse a single buffer named *SPy Output* for all spy command output.
When nil, each spy command uses its own dedicated output buffer."
  :type 'boolean
  :group 'spy)

(defun spy--build-command (args filename)
  "Build a command list for running spy with ARGS and FILENAME.
Handles splitting spy-command if it contains a runner like \"uv run spy\"."
  (let ((spy-cmd-parts (split-string-and-unquote spy-command)))
    (append spy-cmd-parts args (list filename))))

(defun spy--warn-on-process-failure (proc full-command)
  "Check if PROC exited with non-zero code and display warning with FULL-COMMAND.
Shows the exit code, command, and first 3 lines of output.
Returns t if a warning was displayed, nil otherwise."
  (let ((exit-status (process-exit-status proc)))
    (when (and (eq (process-status proc) 'exit)
               (not (zerop exit-status)))
      (let* ((output (with-current-buffer (process-buffer proc)
                       (buffer-substring-no-properties (point-min) (point-max))))
             (output-lines (split-string output "\n" t))
             (first-3-lines (string-join (seq-take output-lines 3) "\n"))
             (cmd-str (mapconcat #'shell-quote-argument full-command " ")))
        (display-warning 'spy
                         (format "Command failed with exit code %d:\n  %s\n\nFirst 3 lines of output:\n%s"
                                 exit-status
                                 cmd-str
                                 first-3-lines)
                         :warning)
        t))))

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
         (full-command (spy--build-command args-list filename))
         (output-window nil)
         (start-time (current-time)))
    ;; Create or clear the output buffer and capture the window
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (compilation-mode)
      (setq output-window (display-buffer (current-buffer))))
    ;; Start the async process
    (make-process
     :name "spy"
     :buffer buf-name
     :command full-command
     :filter (lambda (proc string)
               ;; Process ANSI colors in the string, then insert
               (when (buffer-live-p (process-buffer proc))
                 (with-current-buffer (process-buffer proc)
                   (let ((inhibit-read-only t)
                         (moving (= (point) (process-mark proc)))
                         ;; Process ANSI codes BEFORE inserting
                         (colored-string (ansi-color-apply string)))
                     (save-excursion
                       (goto-char (process-mark proc))
                       (insert colored-string)
                       (set-marker (process-mark proc) (point)))
                     ;; If point was at end, keep it at end
                     (when moving
                       (goto-char (process-mark proc)))
                     ;; Scroll the output window if it still exists
                     (when (and output-window (window-live-p output-window))
                       (with-selected-window output-window
                         (goto-char (point-max))))))))
     :sentinel (lambda (proc event)
                 (with-current-buffer (process-buffer proc)
                   (let* ((inhibit-read-only t)
                          (elapsed (float-time (time-subtract (current-time) start-time)))
                          (event-str (string-trim-right event)))
                     (goto-char (point-max))
                     (insert (format "\nProcess %s %s (elapsed: %.2f sec)"
                                     (process-name proc)
                                     event-str
                                     elapsed))))
                 (spy--warn-on-process-failure proc full-command)))))

(defmacro spy-defcommand (name flag buffer-name docstring)
  "Define an interactive command to call spy with FLAG and show output in BUFFER-NAME.
If `spy-mode-reuse-output-buffer' is non-nil, output goes to *SPy Output* instead."
  `(defun ,name ()
     ,docstring
     (interactive)
     (spy-call-spy (list ,flag)
                   (if spy-mode-reuse-output-buffer
                       "*SPy Output*"
                     ,buffer-name))))

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

(defun spy-compile-executable ()
  "Compile the current buffer to a native executable."
  (interactive)
  (spy-call-spy '("-c" "-t" "native")
                (if spy-mode-reuse-output-buffer "*SPy Output*" "*SPy compile*")))

(defun spy-execute-buffer ()
  "Execute the current buffer in interpreted mode and show output."
  (interactive)
  (spy-call-spy '()
                (if spy-mode-reuse-output-buffer "*SPy Output*" "*SPy run*")))

(defun spy-redshift-execute-buffer ()
  "Redshift and execute the current buffer, showing output."
  (interactive)
  (spy-call-spy '("-r" "-x")
                (if spy-mode-reuse-output-buffer "*SPy Output*" "*SPy run*")))

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
               (face (intern (format "spy-%s" type))))
          (unless (facep face)
            (error "Unknown face: %s" face))
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

(defun spy-colorize-buffer (&optional keep-output)
  "Run 'spy --colorize --format=json' asynchronously on current buffer and apply highlighting.
With prefix argument KEEP-OUTPUT, retain the *spy-colorize-output* buffer for inspection."
  (interactive "P")
  (unless buffer-file-name
    (error "Buffer is not visiting a file"))
  (if spy-buffer-colorized-p
      (spy-colorize-clear-buffer))
  ;; Kill any existing output buffer to start fresh (unless keeping output)
  (when (and (not keep-output) (get-buffer "*spy-colorize-output*"))
    (kill-buffer "*spy-colorize-output*"))
  (let* ((filename buffer-file-name)
         (buffer (current-buffer))
         (full-command (spy--build-command '("--colorize" "--format=json") filename)))
    (make-process
     :name "spy-colorize"
     :buffer "*spy-colorize-output*"
     :command full-command
     :sentinel (lambda (proc event)
                 (unless (spy--warn-on-process-failure proc full-command)
                   ;; Success case - process JSON output
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
                                     (unless keep-output
                                       (kill-buffer (process-buffer proc)))
                                     (message "Applied spy --colorize to current buffer%s"
                                              (if keep-output " (output buffer retained)" "")))
                                 (error
                                  (message "spy-colorize error: %s. Check *spy-colorize-output* buffer" err))))
                           (message "spy-colorize: No JSON found in output. Check *spy-colorize-output* buffer"))))))))))

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
  :doc "Keymap for spy-mode."
  :parent (if (fboundp 'python-ts-mode) python-ts-mode-map python-mode-map)
  "C-c C-c" #'spy-colorize-buffer
  "C-c C-k" #'spy-colorize-clear-buffer
  "C-c C-t" #'spy-toggle-colorize-buffer)

(if (fboundp 'python-ts-mode)
    (define-derived-mode spy-mode python-ts-mode spy-mode-name
      "Major mode for editing SPy files, derived from python-ts-mode.")
  (define-derived-mode spy-mode python-mode spy-mode-name
    "Major mode for editing SPy files, derived from python-mode."))

;; Hydra menu for SPy commands (if hydra is available)
(when (require 'hydra nil t)
  (defhydra hydra-spy (:color pink :hint nil)
    "
^Compiler Pipeline^     ^Execute^            ^Colorization^       ^Output^
^^^^^^^^─────────────────────────────────────────────────────────────────
_P_: Python AST         _x_: Run             _c_: Colorize        _w_: C Write
_p_: SPy AST            _X_: Redshift Run    _k_: Clear           _d_: C Dump
_i_: Imports                               _t_: Toggle
_s_: Symtable
_r_: Redshift
_C_: Compile
_q_: quit
"
    ("P" spy-show-pyparse "Python AST")
    ("p" spy-show-parse "SPy AST")
    ("i" spy-show-imports "Imports")
    ("s" spy-show-symtable "Symtable")
    ("r" spy-show-redshift "Redshift")
    ("C" spy-compile-executable "Compile")
    ("x" spy-execute-buffer "Run")
    ("X" spy-redshift-execute-buffer "Redshift Run")
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
