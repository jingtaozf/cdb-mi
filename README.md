# cdb-mi
cdb debugger interface with emacs,like gdb-mi

## Feature

* Stack backtrace in a buffer
  select one frame will move position to corresponding source file and line.

* Threads list in a buffer
  select one thread will update to corresponding stack backtrace 

* simple one-level local variables in a buffer

* automatically set last breakpoints when debug same process second time.

## Usage

```elisp
(global-set-key [C-f10] 'cdb-mi-run-to-cursor)
(global-set-key [f5] 'my-gud-cont)
(defun my-gud-cont ()
  (interactive)
  (if (get-buffer-process gud-comint-buffer)
    (call-interactively 'gud-cont)
    (windev-debug)))
(global-set-key [f6] 'goto-gud-comint-buffer)
(global-set-key [f10] 'gud-next)
(global-set-key [C-f10] 'cdb-mi-run-to-cursor)
(global-set-key [f11] 'gud-step)
```

Interesting elisp function can set a shortcut to them
```elisp
   ;; c-mode c++-mode csharp-mode fundamental-mode
   ("break" gud-break)
   ("Break" cdb-mi-custom-break)
   ("run to cursor" cdb-mi-run-to-cursor)
   ;; ("run to cursor" gud-tbreak)
   ("go" gud-cont)
   ("p breakpoint manage" cdb-mi-breakpoint-command)
   ("refresh" update-cdb-mi-buffers)
   ("step" gud-step)
   ("message cdb command result" cdb-mi-print-var-at-point)
   ("next" gud-next)
   ("to gud buffer" goto-gud-comint-buffer)
   ("jump to recent breakpoints" cdb-mi-anything-recent-breakpoints)
   ("execute until the current function is complete" cdbmi-go-up)
   ("show cdbmi windows" my-gud-setup-windows)
```
