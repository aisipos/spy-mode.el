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

(provide 'spy-mode)
