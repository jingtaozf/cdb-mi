;; Description: User Interface for running CDB
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2015.02.04 21:18:52(+0800)
;; Last-Updated: 2015.08.26 17:19:29(+0800)
;;     Update #: 1078
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  reference to cdb-gui.el & gdb-mi.el
;;
;; to start a remote windbg server,please use following command line arguments in server side
;; `windbg -server tcp:port=5005 -c "l+*;l-s"'
;; to start a reverse remote windbg client,please use following command line arguments in client first
;; `windbg -remote tcp:port=1234,clicon=0.0.0.0 -c "l+*;l-s"'
;; then use following command in server to connect to client
;; `windbg -server tcp:port=1234,clicon=192.168.0.209'
;; where `192.168.0.209' is your local IP address

(require 'gud)
(require 'json)
(require 'bindat)
(require 'cl-lib)
(require 'tree-mode)
(require 'my-utils)
(provide 'cdb-mi)

;;;; special variables
(defcustom cdb-mi-cdb-path "cdb.exe" "cdb executable path")
(defvar cdb-mi-break-id-string "__enter_into_debugger__")
(defcustom cdb-mi-initial-commands-list
  `("l+*;l-s"
    ".server tcp:port=5005"
    ;; "sxn av" ; not handle access violation
    ,(concat ".idle_cmd .echo " cdb-mi-break-id-string ";|.")
    "vercommand;|"
    )
    "initial commands list of cdb, will be saved in a script file and run immediatelly after start db.")
(defcustom cdb-mi-initial-commands ;".load pykd.pyd;" will make Ctrl-C failed to work.
 nil 
  "initial commands of cdb.")
(defcustom cdb-mi-dedicate-buffer-p t "whether dedicate window to a buffer.")
(defcustom cdb-mi-symbol-path nil "symbol path")
(defcustom cdb-mi-alt-symbol-path nil "alt symbol path")
(defcustom gud-cdb-directories nil "gud cdb directories to find file")
(defvar cdbmi-debug-mode nil)
;; (defvar cdb-log-file "c:\\cdb.log")
;; (defvar cdb-ignores-initial-breakpoint t)
(defvar cdb-thread-number nil
  "Main current thread.

Invalidation triggers use this variable to query CDB for
information on the specified thread by wrapping CDB/MI commands
in `cdb-current-context-command'.

This variable may be updated implicitly by CDB via `cdb-stopped'
or explicitly by `cdb-select-thread'.

Only `cdb-setq-thread-number' should be used to change this
value.")

(defvar cdb-frame-number nil
  "Selected frame level for main current thread.

Updated according to the following rules:

When a thread is selected or current thread stops, set to \"0\".

When current thread goes running (and possibly exits eventually),
set to nil.

May be manually changed by user with `cdb-select-frame'.")

(defvar cdb-frame-address nil "Identity of frame for watch expression.")

(defvar cdb-buf-publisher '()
  "Used to invalidate CDB buffers by emitting a signal in `cdb-update'.
Must be a list of pairs with cars being buffers and cdr's being
valid signal handlers.")

(defvar-local cdb-buffer-type nil
  "One of the symbols bound in `cdb-buffer-rules'.")

(defcustom cdb-directories nil
  "*A list of directories that cdb should search for source code.
If nil, only source files in the program directory
will be known to cdb.

The file names should be absolute, or relative to the directory
containing the executable being debugged."
  :type '(choice (const :tag "Current Directory" nil)
          (repeat :value ("")
           directory))
  :group 'gud)

(defvar cdb-threads-list nil
  "Associative list of threads provided by \"-thread-info\" MI command.

Keys are thread numbers (in strings) and values are structures as
returned from -thread-info by `cdb-json-partial-output'. Updated in
`cdb-thread-list-handler-custom'.")

;;;; utility
(defun cdbmi-debug (format-string &rest args)
  (when cdbmi-debug-mode
    (with-current-buffer (get-buffer-create "*cdbmi-debug*")
      (goto-char (point-max))
      (insert (apply 'format format-string args) "\n"))))

(defun cdb-bind-function-to-buffer (expr buffer)
  "Return a function which will evaluate EXPR in BUFFER."
  `(lambda (&rest args)
     (with-current-buffer ,buffer
       (apply ',expr args))))

(defun cdb-preempt-existing-or-display-buffer (buf &optional split-horizontal)
  "Find window displaying a buffer with the same
`cdb-buffer-type' as BUF and show BUF there.  If no such window
exists, just call `cdb-display-buffer' for BUF.  If the window
found is already dedicated, split window according to
SPLIT-HORIZONTAL and show BUF in the new window."
  (if buf
    (when (not (get-buffer-window buf))
      (let* ((buf-type (cdb-buffer-type buf))
             (existing-window
               (get-window-with-predicate
                #'(lambda (w)
                    (and (eq buf-type
                             (cdb-buffer-type (window-buffer w)))
                         (not (window-dedicated-p w)))))))
        (if existing-window
          (set-window-buffer existing-window buf)
          (let ((dedicated-window
                 (get-window-with-predicate
                  #'(lambda (w)
                      (eq buf-type
                          (cdb-buffer-type (window-buffer w)))))))
            (if dedicated-window
              (set-window-buffer
               (split-window dedicated-window nil split-horizontal) buf)
              (cdb-display-buffer buf))))))
    (error "Null buffer")))

;; Used to display windows with thread-bound buffers
(defmacro def-cdb-preempt-display-buffer (name buffer &optional doc
					       split-horizontal)
  `(defun ,name (&optional thread)
     ,(when doc doc)
     (message "%s" thread)
     (cdb-preempt-existing-or-display-buffer
      (cdb-get-buffer-create ,buffer thread)
      ,split-horizontal)))

(defun cdb-current-context-buffer-name (name)
  "Add thread information and asterisks to string NAME.

If `cdb-thread-number' is nil, just wrap NAME in asterisks."
  (concat "*" name
          (if (local-variable-p 'cdb-thread-number)
            (format " (bound to thread %s)" cdb-thread-number)
            "")
          "*"))

(defun cdb-get-target-string ()
  (with-current-buffer gud-comint-buffer
    gud-target-name))

;;from make-mode-line-mouse-map
(defun cdb-make-header-line-mouse-map (mouse function) "\
Return a keymap with single entry for mouse key MOUSE on the header line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
       (let ((map (make-sparse-keymap)))
         (define-key map (vector 'header-line mouse) function)
         (define-key map (vector 'header-line 'down-mouse-1) 'ignore)
         map))

(defun cdb-set-window-buffer (name &optional ignore-dedicated window)
  "Set buffer of selected window to NAME and dedicate window.

When IGNORE-DEDICATED is non-nil, buffer is set even if selected
window is dedicated."
  (unless window (setq window (selected-window)))
  (when ignore-dedicated
    (set-window-dedicated-p window nil))
  (set-window-buffer window (get-buffer name))
  (when cdb-mi-dedicate-buffer-p
    (set-window-dedicated-p window t)))

(defmacro cdb-propertize-header (name buffer help-echo mouse-face face)
  `(propertize ,name
	       'help-echo ,help-echo
	       'mouse-face ',mouse-face
	       'face ',face
	       'local-map
	       (cdb-make-header-line-mouse-map
		'mouse-1
		(lambda (event) (interactive "e")
                   (save-selected-window
                     (select-window (posn-window (event-start event)))
                     (cdb-set-window-buffer
                      (cdb-get-buffer-create ',buffer) t)
                     (update-cdb-mi-buffers))))))

(defun cdb-current-context-command (command)
  "Add --thread to cdb COMMAND when needed."
  (if (and cdb-thread-number
	   cdb-supports-non-stop)
    (concat command " --thread " cdb-thread-number)
    command))

(defun cdb-current-context-mode-name (mode)
  "Add thread information to MODE which is to be used as `mode-name'."
  (concat mode
          (if cdb-thread-number
            (format " [thread %s]" cdb-thread-number)
            "")))

(defun cdb-display-buffer (buf)
  "Show buffer BUF, and make that window dedicated."
  (let ((window (display-buffer buf)))
    (set-window-dedicated-p window t)
    window))

(defun cdb-input (command handler-function &optional trigger-name)
  "Send COMMAND to CDB via the MI interface.
Run the function HANDLER-FUNCTION, with no arguments, once the command is
complete.  Do not send COMMAND to CDB if TRIGGER-NAME is non-nil and
Emacs is still waiting for a reply from another command previously
sent with the same TRIGGER-NAME."
  (when (or (not trigger-name))
    (setq cdb-token-number (1+ cdb-token-number))
    (setq command (concat (number-to-string cdb-token-number) command))

    ;; (if cdb-enable-debug (push (list 'send-item command handler-function)
    ;;                            cdb-debug-log))

    ;; (cdb-add-handler cdb-token-number handler-function trigger-name)

    ;; (if cdbmi-debug-mode (message "cdb-input: %s" command))
    ;; (process-send-string (get-buffer-process gud-comint-buffer)
    ;;                      (concat command "\n"))
    ))
(defvar cdb-mi-input-texts nil)
(defvar cdb-mi-busy-p nil)
(defun my-cdb-mi-input-filter-functions (text)
  (when (or (string= text "g\n")
            (string-prefix-p "g " text))
    (setf cdb-mi-busy-p t))
  (when cdbmi-debug-mode
    (push text cdb-mi-input-texts)))
(add-hook 'comint-input-filter-functions 'my-cdb-mi-input-filter-functions)

(defvar cdb-mi-locals-win nil)
(defvar cdb-mi-stack-win nil)
(defadvice process-send-string (before cdbmi/before/process-send-string (process string))
  (when (eq process (get-buffer-process gud-comint-buffer))
    (loop for win in (list cdb-mi-locals-win cdb-mi-stack-win)
          do (when (and win
                        (window-live-p win)
                        (window-dedicated-p win))
               (set-window-dedicated-p win nil)))
    (setf cdb-mi-busy-p t)))
(defadvice process-send-string (after cdbmi/after/process-send-string (process string))
  (when (eq process (get-buffer-process gud-comint-buffer))
    (loop for win in (list cdb-mi-locals-win cdb-mi-stack-win)
          do (when (and win
                        (window-live-p win)
                        cdb-mi-dedicate-buffer-p)
               (set-window-dedicated-p win t)))))
(ad-activate 'process-send-string)

;;;; recent breakpoints anything
(defvar cdb-mi-current-module nil)
(defun cdb-mi-recent-breakpoints-cache-file ()
  (if cdb-mi-current-module
    (format "~/.cdbmi-breakpints-%s" cdb-mi-current-module)
    "~/.cdbmi-breakpints"))
(defun cdb-mi-load-recent-breakpoints ()
  (setf cdb-mi-recent-breakpoints
          (when (file-exists-p (cdb-mi-recent-breakpoints-cache-file))
            (my-file-to-lisp (cdb-mi-recent-breakpoints-cache-file)))))
(defun cdb-mi-save-recent-breakpoints ()
  (my-lisp-to-file (cdb-mi-recent-breakpoints-cache-file) cdb-mi-recent-breakpoints))

(defun update-cdb-mi-current-module (name modules)
  (setf cdb-mi-current-module name)
  (setf cdb-mi-modules modules)
  (cdb-mi-load-recent-breakpoints))

(defvar cdb-mi-breakpoints (make-hash-table))

(defun insert-cdb-mi-breakpoints ()
  (interactive)
  (clear-cdb-breakpoint-icons)
  (let ((cmds ""))
    (loop for ((file line) . bp-fmt) in (gethash (or cdb-mi-current-module 'nil) cdb-mi-breakpoints)
          do (save-excursion
               (find-file file)
               (goto-line line)
               (cdb-mi-put-breakpoint-icon t 0))
             (setf cmds (concat cmds bp-fmt "\n")))
    (insert cmds)))
(defvar cdb-mi-recent-breakpoints (cdb-mi-load-recent-breakpoints))

(cl-defun cdb-mi-update-to-recent-breakpoints ()
  (loop for (nil file line) in cdb-mi-recent-breakpoints
        do (when (and (string-equal file buffer-file-name)
                      (= line (line-number-at-pos)))
             (return-from cdb-mi-update-to-recent-breakpoints)))
  (push (list
         (format "%s:%d (in %s)" buffer-file-name (line-number-at-pos) (which-function))
         buffer-file-name
         (line-number-at-pos))
        cdb-mi-recent-breakpoints)
  (cdb-mi-save-recent-breakpoints))

(defun cdb-mi-anything-breakpoints-candidates () cdb-mi-recent-breakpoints)

(defun cdb-mi-anything-breakpoints-action (breakpoint)
  (find-file (first breakpoint))
  (goto-line (second breakpoint)))

(defvar anything-c-source-cdb-mi-recent-breakpionts
    '((name . "cdb Recent Breakpoints")
      (candidates . cdb-mi-anything-breakpoints-candidates)
      (action . cdb-mi-anything-breakpoints-action)
      (volatile)))

(defun cdb-mi-anything-recent-breakpoints ()
  (interactive)
  (let ((case-fold-search t))
    (anything-other-buffer '(anything-c-source-cdb-mi-recent-breakpionts)
                           "*anything cdb-mi breakpoints*")))

;;;; code from cdb-gud.el

(defvar gud-cdb-marker-acc "")
(make-variable-buffer-local 'gud-cdb-marker-acc)

(defvar gud-cdb-output-acc ""
  "accumulates all input between input markers 0:000>")
(make-variable-buffer-local 'gud-cdb-output-acc)

;; paths are hard-coded, it may be useful to remap
;; a path to another, e.g. the visual studio std library
;; is apparently compiled in f:/RTM/..., but I have it
;; installed under program files.
(defvar cdb-remap-fname-hooks nil
  "Hook to be run by cdb-remap-fname when a source file is looked up")

(defun gud-cdb-remap-fname (fname)
  (cond
    ((not fname) nil)
    ;;   ((file-exists-p fname) fname)
    (t ;; try to look the file up
     (let (;;(file-name-directory fname)
           ;;(file-name-nondirectory fname)
           (full-filename (expand-file-name fname))
           )
       (or
         (loop for i in cdb-remap-fname-hooks
               if (and (setq full-filename (funcall i fname)) (file-exists-p full-filename)) return full-filename)
         fname)
       )
     )
    )
  )

(defun gud-cdb-file-name (f)
  "Transform a relative file name to an absolute file name, for cdb."
  (let ((result nil))
    (if (file-exists-p f)
      (setq result (expand-file-name f))
      (let ((directories gud-cdb-directories))
        (while directories
          (let ((path (concat (car directories) "/" f)))
            (if (file-exists-p path)
              (setq result (expand-file-name path)
                    directories nil)))
          (setq directories (cdr directories)))))
    result))

(defun gud-cdb-find-file (f)
  (save-excursion
    (let ((realf (gud-cdb-file-name f)))
      (if (file-exists-p (or realf f))
        (if realf
          (find-file-noselect realf t)
          (find-file-noselect f 'nowarn)
          )
        ))))

;; sending .frame command after each cmd to get line source info

(defvar gud-cdb-options-hook nil
  "the default options to use when starting a coh cdb instance")

(defvar cdb-mi-remote-p nil)
(defvar cdb-mi-command-line nil)
(defun cdb-mi-massage-args (file args)
  (if cdb-mi-remote-p
    (setq tmp (append args
                    ;;  (cons "-c" (cons cdb-mi-initial-commands 
		    (cons "-lines" nil)))
    (with-current-buffer (find-file-noselect "~/cdb-init.script")
      (delete-region (point-min) (point-max))
      (loop for cmd in cdb-mi-initial-commands-list
            do (insert cmd "\n"))
      (loop for cmd in cdb-mi-initial-commands 
            do (insert cmd "\n"))
      (loop for ((file line) . bp-fmt) in (gethash (or cdb-mi-current-module 'nil) cdb-mi-breakpoints)
          do (save-excursion
               (find-file file)
               (goto-line line)
               (cdb-mi-put-breakpoint-icon t 0))
             (insert bp-fmt "\n"))
      (save-buffer)
      (prog1
        (setq tmp (append 
                          (loop for i in gud-cdb-options-hook append (funcall i))
                          (list "-cf" (concat "" (expand-file-name "~/cdb-init.script") ""))
                          (list "-lines")
                          args))
        (kill-current-buffer))))
  (setf cdb-mi-command-line tmp)
  (message "cdb-mi-command-line:%s" cdb-mi-command-line)
  tmp)

;; a list with form `(key . value)'.
(defvar cdb-mi-modules nil)
(defvar cdb-mi-last-module nil)
(defun cdb-mi-modules-anything-action (module)
  (setf cdb-mi-last-module module))
(defvar anything-c-source-cdb-mi-modules
    '((name . "cdb mi modules")
      (candidates . cdb-mi-modules)
      (action . cdb-mi-modules-anything-action)
      (volatile)))

(defun cdb-mi-moudles-anything ()
  (anything :sources '(anything-c-source-cdb-mi-modules)
            :buffer "*anything-cdb-mi-modules*")
  cdb-mi-last-module)
(defun cdb-mi-select-module ()
  (let* ((file (downcase buffer-file-name))
         (candidate (loop with result = nil
                          for (key . value) in cdb-mi-modules
                          do (when (search key file)
                               (push value result))
                          finally (return (or (and result
                                                   (my-select-window result :delay-seconds 0))
                                              (cdb-mi-moudles-anything))))))
    (trim-string (read-from-minibuffer "Limit Module Name: " candidate))))
(defun cdb-mi-gud-break ()
  (let* ((module (cdb-mi-select-module))
         (bp-fmt (if (> (length module) 0)
                   (concat "bu `" module "!%d%f:%l` ")
                   "bu `%d%f:%l` ")))
    (push
      (cons
       (list buffer-file-name (line-number-at-pos))
       (gud-format-command bp-fmt nil))
      (gethash (or cdb-mi-current-module 'nil) cdb-mi-breakpoints))
    (gud-call bp-fmt)
    (cdb-mi-update-to-recent-breakpoints)
    (cdb-mi-put-breakpoint-icon t 0)))

(defun cdb-mi-query-before-kill-function ()
  (if (get-buffer-process gud-comint-buffer)
    (y-or-n-p "Are you sure to kill gud buffer?")
    t))
(defun cdb-mi-query-before-kill ()
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'cdb-mi-query-before-kill-function))
(add-hook 'gud-mode-hook 'cdb-mi-query-before-kill)

(defvar cdb-called-count 0)
(defvar last-cdb-command-line nil)
(defun cdb-mi-internal (command-line)
  (delete-other-windows)

  (setf in-emacs-cdb-result-p nil)
  (setf last-cdb-command-line command-line)
  (split-window-vertically)
  (let ((old-symbol-path (getenv "_NT_SYMBOL_PATH"))
        (old-alt-symbol-path (getenv "_NT_ALT_SYMBOL_PATH")))
    (when cdb-mi-symbol-path
      (setenv "_NT_SYMBOL_PATH" cdb-mi-symbol-path))
    (when cdb-mi-alt-symbol-path
      (setenv "_NT_ALT_SYMBOL_PATH" cdb-mi-alt-symbol-path))
    (gud-common-init command-line 'cdb-mi-massage-args
                     'gud-cdb-marker-filter 'gud-cdb-find-file)
    (when cdb-mi-symbol-path
      (setenv "_NT_SYMBOL_PATH" old-symbol-path))
    (when cdb-mi-alt-symbol-path
      (setenv "_NT_ALT_SYMBOL_PATH" old-alt-symbol-path)))
  (incf cdb-called-count)

  (set (make-local-variable 'gud-minor-mode) 'cdbmi)

  (gud-def gud-break (progn (cdb-mi-gud-break)) "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak "g `%d%f:%l` "  "\C-t" "Set temporary breakpoint at current line.")
  (gud-def gud-step   "t "            "\C-s" "Step one source line with display.")
  (gud-def gud-next   "p "            "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "g "            "\C-r" "Continue with display.")
  (gud-def gud-finish "g @$ra "       "\C-f" "Finish executing current function.")
  (gud-def gud-print  "?? %e "        "\C-p" "Evaluate C expression at point.")

  (setq comint-prompt-regexp "^[0-9a-f]:[0-9a-f][0-9a-f][0-9a-f]> ")
  (my-add-modeline-eval-function 'cdb-mi-mode-line)
  (setq paragraph-start comint-prompt-regexp))
(defun cdb-mi (command-line)
  "Run cdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive (list (gud-query-cmdline 'cdb (concat cdb-mi-cdb-path "-G -logau c:\\cdb.log -o -pb "))))
  (cdb-mi-internal command-line))

(defvar cdb-mi-reverse-remote-platform nil)
(defun cdb-mi-reverse-remote ()
  (interactive)
  (when current-prefix-arg
    (setf cdb-mi-reverse-remote-platform nil))
  (let* ((platform (or cdb-mi-reverse-remote-platform
                       (setf cdb-mi-reverse-remote-platform
                               (my-select-window '("x86" "x64") :delay-seconds 0))))
         (cdb-mi-remote-p t)
         (cdb (concat "c:/debugger/" platform "/cdb.exe")))
    (cdb-mi-internal (concat cdb " -remote tcp:port=1234,clicon=0.0.0.0 -c \"l+*;l-s\""))
    (message "windbg -server tcp:port=1234,clicon=10.35.10.209")
    ))

(setq cdb-mi-kd-wrapper-path (concat (file-name-directory (or load-file-name buffer-file-name)) "kd-wrapper.cmd"))
(defun cdb-mi-kd-query-cmdline ()
  (let* ((hist-sym 'kd-history))
    (unless (boundp hist-sym) (set hist-sym nil))
    (read-from-minibuffer
     (format "Enter remote session <port> <hostname>: ")
     (or (car-safe (symbol-value hist-sym))
         "port hostname")
     gud-minibuffer-local-map nil
     hist-sym)))

(defun cdb-mi-remote (command-line)
  (interactive (list (concat cdb-mi-kd-wrapper-path " " (cdb-mi-kd-query-cmdline))))
  (let ((cdb-mi-remote-p t))
    (cdb-mi command-line)))

(defun cdbmi-go-up ()
  (interactive)
  (comint-send-string (get-buffer-process gud-comint-buffer) "gu\n"))

(defvar *cdb-mi-mode-index* 0)
(defvar *cdb-mi-mode-line-indicators* "/-\\|")
(defvar *cdb-mi-mode-max-index* (length *cdb-mi-mode-line-indicators*))
(defvar *cdb-mi-mode-line* " - ")
(defun cdb-mi-mode-line-face ()
  (if (or in-emacs-cdb-result-p
          cdb-mi-busy-p)
    'error
    'cursor))

(defvar cdb-mi-alive-p nil)
(defvar cdb-mi-quit-hook nil)
(defun cdb-mi-mode-line ()
  (cond ((get-buffer-process gud-comint-buffer)
         (setf cdb-mi-alive-p t)
         (propertize
          *cdb-mi-mode-line*
          'mouse-face 'mode-line-highlight
          ;; 加上颜色
          'face (cdb-mi-mode-line-face)
          'help-echo "CDB"))
        (t (when cdb-mi-alive-p
             (run-hooks 'cdb-mi-quit-hook)
             (setf cdb-mi-alive-p nil)))))
(add-hook 'cdb-mi-quit-hook 'clear-cdb-breakpoint-icons)

(defun notify-cdb-mi-activity ()
  (if (>= *cdb-mi-mode-index* *cdb-mi-mode-max-index*)
    (setf *cdb-mi-mode-index* 0))
  (setf *cdb-mi-mode-line* (format " D%c " (aref *cdb-mi-mode-line-indicators* *cdb-mi-mode-index*)))
  (incf *cdb-mi-mode-index*))

(defvar last-debug-string-list (make-queue))
;; cdb process filter will not put the command result in one string,so put the intermmidate
;; result here.
(defvar in-emacs-cdb-result-p nil)
(defvar emacs-cdb-command-result nil)
(defvar emacs-cdb-buffer-type nil)
(defun cdb-mi-add-filter-command-line (line)
  (with-current-buffer (get-buffer-create "*cdb-mi-result*")
    (goto-char (point-max))
    (insert "\n" line )))
(cl-defun cdb-marker-filter-mi-command-result (string)
  (let (lines left-string)
    (if (> (queue-length last-debug-string-list) 5)
      (dequeue last-debug-string-list))
    (enqueue string last-debug-string-list)
    (setq lines (split-string string "\n"))
    (when (and in-emacs-cdb-result-p emacs-cdb-command-result)
      (setf string (concat (pop emacs-cdb-command-result) string)))
    (loop for line in lines
          do (if (and (> (length line) 0) (string-match "\s*emacs-cdbmi\s+?\\(.*?\\)$" line))
               (cond (in-emacs-cdb-result-p
                      (cdbmi-debug "meet a cdb result:%s" emacs-cdb-buffer-type)
                      (cdb-mi-add-filter-command-line line)
                      (setf in-emacs-cdb-result-p nil)
                      (when emacs-cdb-buffer-type
                        (if emacs-cdb-command-result
                          (unwind-protect
                              (update-cdb-mi-buffers-from-result emacs-cdb-buffer-type (copy-list (reverse emacs-cdb-command-result)))
                            (setf emacs-cdb-command-result nil
                                  emacs-cdb-buffer-type nil))
                          (message "%s has no result" emacs-cdb-buffer-type))))
                     (t (setf emacs-cdb-buffer-type
                                (intern (substring line (match-beginning 1) (match-end 1))))
                        (cdbmi-debug "find a cdb result:%s" emacs-cdb-buffer-type)
                        (cdb-mi-add-filter-command-line line)
                        (setf in-emacs-cdb-result-p t)))
               (if in-emacs-cdb-result-p
                 (progn
                   (cdb-mi-add-filter-command-line line)
                   (unless (or (string-match comint-prompt-regexp line)
                               (= 0 (length line)))
                     (push line emacs-cdb-command-result)))
                 (setf left-string (concat left-string line "\n")))))
    ;;(cdbmi-debug "cdb-marker-filter-mi-command-result,return:%s" left-string)
    ;; ignore last '\n' added in this function.
    (if (and (not in-emacs-cdb-result-p) left-string)
      (substring left-string 0 -1)
      "")))

(defun cdb-mi-pre-mark-filter (string)
  (when (or (string-match "Breakpoint\s+?[0-9]+?\s+?hit$" string)
            (string-match " Break instruction exception " string)
            (string-match "^KERNELBASE!RaiseException" string)
            (string-match cdb-mi-break-id-string string)
            (string-match "First chance exceptions are reported before any exception handling." string))
    (unless emacs-has-focus-p
      ;; The following codes will make emacs freeze!!!
      ;; (sit-for 0)
      ;; (select-frame-set-input-focus (window-frame (selected-window))) ;active it
      ;; (w32-send-sys-command 61488 nil); maximize it
      (start-process "topmost" nil "nircmdc.exe" "win" "activate" "class" "Emacs")
      (start-process "topmost" nil "nircmdc.exe" "win" "max" "class" "Emacs"))
    ;; (unless (and (string-match cdb-mi-break-id-string string)
    ;;              (not (string-match "Breakpoint\s+?[0-9]+?\s+?hit$" string)))
    ;;   (comint-send-string (get-buffer-process gud-comint-buffer) "|.\n"))
    (unless (or (string-match "Breakpoint\s+?[0-9]+?\s+?hit$" string)
                (string-match cdb-mi-break-id-string string))
      (goto-gud-comint-buffer))))

(defvar cdb-mi-ignore-filter-p nil)
(cl-defun gud-cdb-marker-filter (string)
  (setf cdb-mi-busy-p t)
  (when cdb-mi-ignore-filter-p
    (return-from gud-cdb-marker-filter string))
  (cdbmi-debug "gud-cdb-marker-filter:%s" string)

  (buffer-disable-undo gud-comint-buffer);TODO: disable all extra feature to this buffer when filter.
  (cdb-mi-pre-mark-filter string)
  (notify-cdb-mi-activity)
  (setf string (cdb-marker-filter-mi-command-result string))

  (setq gud-cdb-marker-acc (concat gud-cdb-marker-acc string))

  ;; " \\[\\(.*\\) @ \\([0-9]+\\)\\]" ;; file @ line
  ;; "^\\(.*?\\)(\\([0-9]+?\\))\\+?0?x?[0-9]*?$"
  (let* ((output "")
         (input-cursor-found)
         (lines)
         (find-stack-marker
           (lambda (stack-lines)
             (if (string-match
                  (concat
                   "^[0-9a-zA-Z]*" ;; optional frame number
                   " *[0-9a-zA-Z`]+" ;; instruction pointer
                   " [0-9a-zA-Z`]+ " ;; return address
                   ".+!.+\\"	;; module!function
                   ;;"+0x[0-9a-zA-Z]+" ;; offset (optional, ab: removing)
                   " \\[\\(.*\\) @ \\([0-9]+\\)\\]" ;; file @ line
                   ) stack-lines)
               (let
                   ((fname)
                    (linenum))
                 (setq fname (substring stack-lines (match-beginning 1) (match-end 1)))
                 (setq linenum (string-to-number (substring stack-lines (match-beginning 2) (match-end 2))))
                 (setq fname (gud-cdb-remap-fname fname))
                 (if (and fname (file-exists-p fname))
                   (cons fname linenum)
                   (funcall find-stack-marker (substring stack-lines (match-end 0))))))))
         (set-from-stack-marker (lambda (stack-lines)
                                  (setq gud-last-frame (or
                                                         (funcall find-stack-marker stack-lines)
                                                         gud-last-frame))))
         (set-gud-last-frame (lambda (fname line)
                               "change new mark for buffer if file found"
                               (let ((fn (gud-cdb-remap-fname fname)))
                                 (if fn
                                   (setq gud-last-frame (cons fn line)))))))

    ;; accum latest
    (setq lines (split-string string "\n"))
    (loop for line in lines
          until (setq input-cursor-found (string-match "^[0-9]:[0-9][0-9][0-9]\\(:.*\\)?>" line))
          do
       (setq gud-cdb-output-acc (concat gud-cdb-output-acc line "\n")))
    ;; ab: match frame markers.
    ;; example:
    ;;"0b 0012f8ac 00516f22 Auctionserver!XactLogLine_FromStr+0xd9 [c:\\src\\xactlog.c @ 372]
                                        ;	(debug)
    (when input-cursor-found
      (setf cdb-mi-busy-p nil)
      (cond
         ;;; info about a particular line, e.g. from a step
        ((string-match "^\\([a-zA-Z]:.*\\)(\\([0-9]+\\))" gud-cdb-output-acc)
         ;; find out the last line no, for example in a cdb command `pa'
         (loop for line in (reverse (split-string gud-cdb-output-acc "\n"))
               do (when (string-match "^\\([a-zA-Z]:.*\\)(\\([0-9]+\\))" line)
                    (funcall set-gud-last-frame (match-string 1 line)
                             (string-to-number (match-string 2 line)))
                    (return))))
         ;;; default, scrape for a stack trace
        (t (funcall set-from-stack-marker gud-cdb-output-acc))))

    ;; clear the accumulator
    (if input-cursor-found
      (setq gud-cdb-output-acc ""))

    ;; too munch, but that is the debugger's fault ...
    (while (string-match
            "^\\([-A-Za-z0-9_\.:\\]*\\)(\\([0-9]*\\))\n"
            gud-cdb-marker-acc)
      (setq


            ;; Append any text before the marker to the output we're going
            ;; to return - we don't include the marker in this text.
            output (concat output
                           (substring gud-cdb-marker-acc 0 (match-beginning 0)))

            ;; Set the accumulator to the remaining text.
            gud-cdb-marker-acc (substring gud-cdb-marker-acc (match-end 0))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-cdb-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\032.*\\'" gud-cdb-marker-acc)
      (progn
        ;; Everything before the potential marker start can be output.
        (setq output (concat output (substring gud-cdb-marker-acc
                                               0 (match-beginning 0))))

        ;; Everything after, we save, to combine with later input.
        (setq gud-cdb-marker-acc
                (substring gud-cdb-marker-acc (match-beginning 0))))
      (setq output (concat output gud-cdb-marker-acc)
            gud-cdb-marker-acc ""))
    (buffer-enable-undo gud-comint-buffer)
    output))

(defadvice gud-display-frame (around cdbmi/gud-display-frame)
  (let ((update-mi-windows (not (null gud-last-frame))))
    ad-do-it
    (when update-mi-windows
      (update-cdb-mi-buffers))))
(ad-activate 'gud-display-frame)

(defun update-cdb-mi-buffers ()
  (interactive)
  (loop for rules in cdb-buffer-rules
        for trigger = (cdb-rules-update-trigger rules)
        do (funcall trigger 'update)))

(defvar cdb-mi-last-command-result nil)
(defun update-cdb-mi-buffers-from-result (buffer-type command-result)
  (cdbmi-debug "update-cdb-mi-buffers-from-result,type:%s,result:%s"
               buffer-type command-result)
  (setf cdb-mi-last-command-result command-result)
  (let* ((rules (assoc buffer-type cdb-buffer-rules))
         (handler (cdb-rules-handle-trigger rules)))
    (if handler
      (funcall handler buffer-type command-result)
      (or
        (cdb-mi-command-result-handler buffer-type command-result)
        (message "failed to find hander for buffer type %s" buffer-type)))))

(defun cdb-mi-command-result-handler (command-type command-result)
  (case command-type
    (cdb-message-command (cdb-mi-message-command-handler command-result)
                         t)
    (cdb-breakpoint-command (cdb-mi-breakpoint-command-handler command-result)
                            t)
    ))

(defun cdb-mi-run-to-cursor ()
  (interactive)
  (if (not (file-exists-p buffer-file-name))
    (message "buffer-file-name is empty!")
    (comint-send-string
     (get-buffer-process gud-comint-buffer)
     (let ((module (cdb-mi-select-module)))
       (if (> (length module) 0)
         (format "pa `%s!%s:%s`\n"
                 module
                 (replace-regexp-in-string "/" "\\\\" buffer-file-name)
                 (line-number-at-pos))
         (format "pa `%s:%s`\n"
                 (replace-regexp-in-string "/" "\\\\" buffer-file-name)
                 (line-number-at-pos)))))
    ;; (comint-send-string (get-buffer-process gud-comint-buffer) ".frame\n")
    ))

;;;; cdb table
(defun cdb-pad-string (string padding)
  (format (concat "%" (number-to-string padding) "s") string))

;; cdb-table struct is a way to programmatically construct simple
;; tables. It help to reliably align columns of data in CDB buffers
;; and provides
(cl-defstruct cdb-table
  (column-sizes nil)
  (rows nil)
  (row-properties nil)
  (right-align nil))

(defun cdb-table-add-row (table row &optional properties)
  "Add ROW of string to TABLE and recalculate column sizes.

When non-nil, PROPERTIES will be added to the whole row when
calling `cdb-table-string'."
  (let ((rows (cdb-table-rows table))
        (row-properties (cdb-table-row-properties table))
        (column-sizes (cdb-table-column-sizes table))
        (right-align (cdb-table-right-align table)))
    (when (not column-sizes)
      (setf (cdb-table-column-sizes table)
              (make-list (length row) 0)))
    (setf (cdb-table-rows table)
            (append rows (list row)))
    (setf (cdb-table-row-properties table)
            (append row-properties (list properties)))
    (setf (cdb-table-column-sizes table)
            (cl-mapcar (lambda (x s)
                         (let ((new-x
                                (max (abs x) (string-width (or s "")))))
                           (if right-align new-x (- new-x))))
                       (cdb-table-column-sizes table)
                       row))
    ;; Avoid trailing whitespace at eol
    (if (not (cdb-table-right-align table))
      (setcar (last (cdb-table-column-sizes table)) 0))))

(defun cdb-table-string (table &optional sep)
  "Return TABLE as a string with columns separated with SEP."
  (let ((column-sizes (cdb-table-column-sizes table)))
    (mapconcat
     'identity
     (cl-mapcar
      (lambda (row properties)
        (apply 'propertize
               (mapconcat 'identity
                          (cl-mapcar (lambda (s x) (cdb-pad-string s x))
                                     row column-sizes)
                          sep)
               properties))
      (cdb-table-rows table)
      (cdb-table-row-properties table))
     "\n")))

;; bindat-get-field goes deep, cdb-get-many-fields goes wide
(defun cdb-get-many-fields (struct &rest fields)
  "Return a list of FIELDS values from STRUCT."
  (let ((values))
    (dolist (field fields)
      (push (bindat-get-field struct field) values))
    (nreverse values)))

(defmacro def-cdb-auto-update-trigger (buffer-type trigger-name cdb-command
                                       &optional signal-list)
  "Define a trigger TRIGGER-NAME which sends CDB-COMMAND.

If SIGNAL-LIST is non-nil, CDB-COMMAND is sent only when the
defined trigger is called with an argument from SIGNAL-LIST.  It's
not recommended to define triggers with empty SIGNAL-LIST.
Normally triggers should respond at least to 'update signal.

Triggers defined by this command are meant to be used as a
trigger argument when describing buffer types with
`cdb-set-buffer-rules'."
  `(defun ,trigger-name (&optional signal)
     (when
         (or (not ,signal-list)
             (memq signal ,signal-list))
       (when (cdb-get-buffer ',buffer-type)
             (comint-send-string
              (get-buffer-process gud-comint-buffer)
              (format ".echo emacs-cdbmi %s;%s;.echo emacs-cdbmi %s\n"
                      ',buffer-type ,cdb-command ',buffer-type))))))

;; Used by disassembly buffer only, the rest use
;; def-cdb-trigger-and-handler
(defmacro def-cdb-auto-update-handler (handler-name custom-defun
                                       &optional nopreserve)
  "Define a handler HANDLER-NAME calling CUSTOM-DEFUN.

Handlers are normally called from the buffers they put output in.

Erase current buffer and evaluate CUSTOM-DEFUN.
Then call `cdb-update-buffer-name'.

If NOPRESERVE is non-nil, window point is not restored after CUSTOM-DEFUN."
  `(defun ,handler-name (buffer-type command-result)
     (with-current-buffer (cdb-get-buffer buffer-type)
       (let* ((inhibit-read-only t)
              ,@(unless nopreserve
                  '((window (get-buffer-window (current-buffer) 0))
                    (start (window-start window))
                    (p (window-point window)))))
         (erase-buffer)
         (,custom-defun buffer-type command-result)
         (cdb-update-buffer-name)
         ,@(when (not nopreserve)
             '((set-window-start window start)
               (set-window-point window p)))))))

(defmacro def-cdb-trigger-and-handler (buffer-type
                                       trigger-name cdb-command
                                       handler-name custom-defun
                                       &optional signal-list)
  "Define trigger and handler.

TRIGGER-NAME trigger is defined to send CDB-COMMAND.
See `def-cdb-auto-update-trigger'.

HANDLER-NAME handler uses customization of CUSTOM-DEFUN.
See `def-cdb-auto-update-handler'."
  `(progn
     (def-cdb-auto-update-trigger ,buffer-type ,trigger-name
         ,cdb-command ,signal-list)
     (def-cdb-auto-update-handler ,handler-name
         ,custom-defun)))


;;;; Publish-subscribe

(defmacro cdb-add-subscriber (publisher subscriber)
  "Register new PUBLISHER's SUBSCRIBER.

SUBSCRIBER must be a pair, where cdr is a function of one
argument (see `cdb-emit-signal')."
  `(add-to-list ',publisher ,subscriber t))

(defmacro cdb-delete-subscriber (publisher subscriber)
  "Unregister SUBSCRIBER from PUBLISHER."
  `(setq ,publisher (delete ,subscriber
                            ,publisher)))

(defun cdb-get-subscribers (publisher)
  publisher)

;;;; cdb parent mode.
(defun cdb-parent-mode ()
  "Generic mode to derive all other CDB buffer modes from."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  ;; Delete buffer from cdb-buf-publisher when it's killed
  ;; (if it has an associated update trigger)
  (add-hook
   'kill-buffer-hook
   (function
     (lambda ()
       (let ((trigger (cdb-rules-update-trigger
                       (cdb-current-buffer-rules))))
         (when trigger
           (cdb-delete-subscriber
            cdb-buf-publisher
            ;; This should match cdb-add-subscriber done in
            ;; cdb-get-buffer-create
            (cons (current-buffer)
                  (cdb-bind-function-to-buffer trigger (current-buffer))))))))
   nil t))

;;;; cdb breakpoint icon

(defconst breakpoint-xpm-data
  "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"10 10 2 1\",
\"  c red\",
\"+ c None\",
/* pixels */
\"+++    +++\",
\"++      ++\",
\"+        +\",
\"          \",
\"          \",
\"          \",
\"          \",
\"+        +\",
\"++      ++\",
\"+++    +++\",
};"
  "XPM data used for breakpoint icon.")

(defconst breakpoint-enabled-pbm-data
  "P1
10 10\",
0 0 0 0 1 1 1 1 0 0 0 0
0 0 0 1 1 1 1 1 1 0 0 0
0 0 1 1 1 1 1 1 1 1 0 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 0 1 1 1 1 1 1 1 1 0 0
0 0 0 1 1 1 1 1 1 0 0 0
0 0 0 0 1 1 1 1 0 0 0 0"
  "PBM data used for enabled breakpoint icon.")

(defconst breakpoint-disabled-pbm-data
  "P1
10 10\",
0 0 1 0 1 0 1 0 0 0
0 1 0 1 0 1 0 1 0 0
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
0 0 1 0 1 0 1 0 1 0
0 0 0 1 0 1 0 1 0 0"
  "PBM data used for disabled breakpoint icon.")

(defvar breakpoint-enabled-icon nil
  "Icon for enabled breakpoint in display margin.")

(defvar breakpoint-disabled-icon nil
  "Icon for disabled breakpoint in display margin.")

(declare-function define-fringe-bitmap "fringe.c"
		  (bitmap bits &optional height width align))

(and (display-images-p)
     ;; Bitmap for breakpoint in fringe
     (define-fringe-bitmap 'breakpoint
       "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")
     ;; Bitmap for gud-overlay-arrow in fringe
     (define-fringe-bitmap 'hollow-right-triangle
       "\xe0\x90\x88\x84\x84\x88\x90\xe0"))

(defface breakpoint-enabled
  '((t
     :foreground "red1"
     :weight bold))
  "Face for enabled breakpoint icon in fringe."
  :group 'cdb-mi)

(defface breakpoint-disabled
  '((((class color) (min-colors 88)) :foreground "grey70")
    ;; Ensure that on low-color displays that we end up something visible.
    (((class color) (min-colors 8) (background light))
     :foreground "black")
    (((class color) (min-colors 8) (background dark))
     :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face for disabled breakpoint icon in fringe."
  :group 'cdb-mi)

;;from put-image
(defun cdb-mi-line-posns (line)
  "Return a pair of LINE beginning and end positions."
  (let ((offset (1+ (- line (line-number-at-pos)))))
    (cons
     (line-beginning-position offset)
     (line-end-position offset))))

(defvar cdb-mi-breakpoint-overlays nil)
(defun cdb-mi-put-string (putstring pos &optional dprop &rest sprops)
  "Put string PUTSTRING in front of POS in the current buffer.
PUTSTRING is displayed by putting an overlay into the current buffer with a
`before-string' string that has a `display' property whose value is
PUTSTRING."
  (let ((string (make-string 1 ?x))
        (buffer (current-buffer)))
    (setq putstring (copy-sequence putstring))
    (let ((overlay (make-overlay pos pos buffer))
          (prop (or dprop
                    (list (list 'margin 'left-margin) putstring))))
      (push overlay cdb-mi-breakpoint-overlays)
      (put-text-property 0 1 'display prop string)
      (if sprops
          (add-text-properties 0 1 sprops string))
      (overlay-put overlay 'put-break t)
      (overlay-put overlay 'before-string string))))

;;from remove-images
(defun cdb-mi-remove-strings (start end &optional buffer)
  "Remove strings between START and END in BUFFER.
Remove only strings that were put in BUFFER with calls to `cdb-mi-put-string'.
BUFFER nil or omitted means use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (dolist (overlay (overlays-in start end))
    (when (overlay-get overlay 'put-break)
      (setf cdb-mi-breakpoint-overlays (delete overlay cdb-mi-breakpoint-overlays))
      (delete-overlay overlay))))

(defun clear-cdb-breakpoint-icons ()
  (loop for overlay in cdb-mi-breakpoint-overlays
        do (delete-overlay overlay))
  (setf cdb-mi-breakpoint-overlays nil))

(defun cdb-mi-remove-breakpoint-icons (start end &optional remove-margin)
  (cdb-mi-remove-strings start end)
  (if (display-images-p)
      (remove-images start end))
  (when remove-margin
    (setq left-margin-width 0)
    (let ((window (get-buffer-window (current-buffer) 0)))
      (if window
          (set-window-margins
           window left-margin-width right-margin-width)))))

(defvar cdb-mi-buffer-fringe-width nil)
(defun cdb-mi-put-breakpoint-icon (enabled bptno &optional line)

  (let* ((posns (cdb-mi-line-posns (or line (line-number-at-pos))))
         (start (- (car posns) 1))
         (end (+ (cdr posns) 1))
         (putstring (if enabled "B" "b"))
         (source-window (get-buffer-window (current-buffer) 0)))
    (add-text-properties
     0 1 '(help-echo "mouse-1: clear bkpt, mouse-3: enable/disable bkpt")
     putstring)
    (if enabled
        (add-text-properties
         0 1 `(cdb-mi-bptno ,bptno cdb-mi-enabled t) putstring)
      (add-text-properties
       0 1 `(cdb-mi-bptno ,bptno cdb-mi-enabled nil) putstring))
    (cdb-mi-remove-breakpoint-icons start end)
    (if (display-images-p)
        (if (>= (or left-fringe-width
                    (if source-window (car (window-fringes source-window)))
                    cdb-mi-buffer-fringe-width) 8)
            (cdb-mi-put-string
             nil (1+ start)
             `(left-fringe breakpoint
                           ,(if enabled
                                'breakpoint-enabled
                              'breakpoint-disabled))
             'cdb-mi-bptno bptno
             'cdb-mi-enabled enabled)
          (when (< left-margin-width 2)
            (save-current-buffer
              (setq left-margin-width 2)
              (if source-window
                  (set-window-margins
                   source-window
                   left-margin-width right-margin-width))))
          (put-image
           (if enabled
               (or breakpoint-enabled-icon
                   (setq breakpoint-enabled-icon
                         (find-image `((:type xpm :data
                                        ,breakpoint-xpm-data
                                        :ascent 100 :pointer hand)
                                       (:type pbm :data
                                        ,breakpoint-enabled-pbm-data
                                        :ascent 100 :pointer hand)))))
             (or breakpoint-disabled-icon
                 (setq breakpoint-disabled-icon
                       (find-image `((:type xpm :data
                                      ,breakpoint-xpm-data
                                      :conversion disabled
                                      :ascent 100 :pointer hand)
                                     (:type pbm :data
                                      ,breakpoint-disabled-pbm-data
                                      :ascent 100 :pointer hand))))))
           (+ start 1)
           putstring
           'left-margin))
      (when (< left-margin-width 2)
        (save-current-buffer
          (setq left-margin-width 2)
          (let ((window (get-buffer-window (current-buffer) 0)))
            (if window
                (set-window-margins
                 window left-margin-width right-margin-width)))))
      (cdb-mi-put-string
       (propertize putstring
                   'face (if enabled
                             'breakpoint-enabled 'breakpoint-disabled))
       (1+ start)))))

;;
;;;; cdb buffers.
;;
;; Each buffer has a TYPE -- a symbol that identifies the function
;; of that particular buffer.
;;
;; The usual cdb interaction buffer is given the type `cdbmi' and
;; is constructed specially.
;;
;; Others are constructed by cdb-get-buffer-create and
;; named according to the rules set forth in the cdb-buffer-rules

(defvar cdb-buffer-rules '())

(defun cdb-rules-name-maker (rules-entry)
  (cadr rules-entry))
(defun cdb-rules-buffer-mode (rules-entry)
  (nth 2 rules-entry))
(defun cdb-rules-update-trigger (rules-entry)
  (nth 3 rules-entry))

(defun cdb-rules-handle-trigger (rules-entry)
  (nth 4 rules-entry))

(defun cdb-update-buffer-name ()
  "Rename current buffer according to name-maker associated with
it in `cdb-buffer-rules'."
  (let ((f (cdb-rules-name-maker (assoc cdb-buffer-type
                                        cdb-buffer-rules))))
    (when f (rename-buffer (funcall f)))))

(defun cdb-current-buffer-rules ()
  "Get `cdb-buffer-rules' entry for current buffer type."
  (assoc cdb-buffer-type cdb-buffer-rules))

;; This assoc maps buffer type symbols to rules.  Each rule is a list of
;; at least one and possible more functions.  The functions have these
;; roles in defining a buffer type:
;;
;;     NAME - Return a name for this  buffer type.
;;
;; The remaining function(s) are optional:
;;
;;     MODE - called in a new buffer with no arguments, should establish
;;	      the proper mode for the buffer.
;;

(defun cdb-set-buffer-rules (buffer-type &rest rules)
  (let ((binding (assoc buffer-type cdb-buffer-rules)))
    (if binding
      (setcdr binding rules)
      (push (cons buffer-type rules)
	    cdb-buffer-rules))))

(defun cdb-current-buffer-thread ()
  "Get thread object of current buffer from `cdb-threads-list'.

When current buffer is not bound to any thread, return main
thread."
  (cdr (assoc cdb-thread-number cdb-threads-list)))

(defun cdb-current-buffer-frame ()
  "Get current stack frame object for thread of current buffer."
  (bindat-get-field (cdb-current-buffer-thread) 'frame))

(defun cdb-buffer-type (buffer)
  "Get value of `cdb-buffer-type' for BUFFER."
  (with-current-buffer buffer
    cdb-buffer-type))

(defun cdb-get-buffer (buffer-type &optional thread)
  "Get a specific CDB buffer.

In that buffer, `cdb-buffer-type' must be equal to BUFFER-TYPE
and `cdb-thread-number' (if provided) must be equal to THREAD."
  (catch 'found
    (dolist (buffer (buffer-list) nil)
      (with-current-buffer buffer
        (when (and (eq cdb-buffer-type buffer-type)
                   (or (not thread)
                       (equal cdb-thread-number thread)))
          (throw 'found buffer))))))

(defun cdb-get-buffer-create (buffer-type &optional thread)
  "Create a new CDB buffer of the type specified by BUFFER-TYPE.
The buffer-type should be one of the cars in `cdb-buffer-rules'.

If THREAD is non-nil, it is assigned to `cdb-thread-number'
buffer-local variable of the new buffer.

Buffer mode and name are selected according to buffer type.

If buffer has trigger associated with it in `cdb-buffer-rules',
this trigger is subscribed to `cdb-buf-publisher' and called with
'update argument."
  (message "cdb-get-buffer")
  (or (cdb-get-buffer buffer-type thread)
      (let ((rules (assoc buffer-type cdb-buffer-rules))
            (new (generate-new-buffer "limbo")))
        (message "create a cdb buffer")
	(with-current-buffer new
	  (let ((mode (cdb-rules-buffer-mode rules))
                (trigger (cdb-rules-update-trigger rules)))
	    (when mode (funcall mode))
	    (setf cdb-buffer-type buffer-type)
            (when thread
              (set (make-local-variable 'cdb-thread-number) thread))
	    (set (make-local-variable 'gud-minor-mode)
		 (buffer-local-value 'gud-minor-mode gud-comint-buffer))
	    (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
            (when (get-buffer (funcall (cdb-rules-name-maker rules)))
              (kill-buffer (funcall (cdb-rules-name-maker rules))))
            (rename-buffer (funcall (cdb-rules-name-maker rules)))
	    (when trigger
              (cdb-add-subscriber cdb-buf-publisher
                                  (cons (current-buffer)
                                        (cdb-bind-function-to-buffer
                                         trigger (current-buffer))))
              (funcall trigger 'start))
            (current-buffer))))))

;;;; print var at point
(defun cdb-mi-print-var-at-point ()
  (interactive)
  (let* ((what (read-from-minibuffer "cdb command: "
                                     (concat "dt " (thing-at-point 'symbol)))))
         (comint-send-string
              (get-buffer-process gud-comint-buffer)
              (format ".echo emacs-cdbmi %s;%s;.echo emacs-cdbmi %s\n"
                      'cdb-message-command what 'cdb-message-command))))
(defun cdb-mi-message-command-handler (command-result)
  (message (loop with result = ""
             for line in command-result
                 do (setf result (concat result line "\n"))
                    finally (return result))))
;;;; breakpont manager

(defun cdb-mi-custom-break ()
  (interactive)
  (let ((action (my-select-window '(:hitcount :custom) :delay-seconds 0)))
    (case action
      (:hitcount
       (let ((hitcount (read-from-minibuffer "hitcount number: " "1000"))
             (file buffer-file-name)
             (line-no (line-number-at-pos)))
         (cdb-mi-put-breakpoint-icon t 0)
         (comint-send-string
          (get-buffer-process gud-comint-buffer)
          (let* ((module (cdb-mi-select-module)))
            (if (> (length module) 0)
              (format "bu `%s!%s:%d` %s\n" module file line-no hitcount)
              (format "bu `%s:%d` %s\n" file line-no hitcount))))))
      (:custom
       (let* ((file buffer-file-name)
              (line-no (line-number-at-pos))
              (cmd (read-from-minibuffer
                   "Breakpoint Command: "
                   (let* ((module (cdb-mi-select-module)))
                     (if (> (length module) 0)
                       (format "bu `%s!%s:%d` \".if () {} .else {gc}\"\n" module file line-no)
                       (format "bu `%s:%d` \".if () {} .else {gc}\"\n" file line-no))))))
         (cdb-mi-put-breakpoint-icon t 0)
         (comint-send-string
          (get-buffer-process gud-comint-buffer)
          (format "%s\n" cmd)))))))

(defvar cdbmi-bp-cached-buffer-file-name "")
(defvar cdbmi-bp-cached-buffer-file-lineno 0)
(defun cdb-mi-breakpoint-command ()
  (interactive)
  (setf cdbmi-bp-cached-buffer-file-name (file-truename buffer-file-name))
  (setf cdbmi-bp-cached-buffer-file-lineno (line-number-at-pos))
  (comint-send-string
              (get-buffer-process gud-comint-buffer)
              (format ".echo emacs-cdbmi %s;bl;.echo emacs-cdbmi %s\n"
                      'cdb-breakpoint-command 'cdb-breakpoint-command)))

(defvar last-cdb-mi-breakpoint-command-result nil)
(defun cdb-mi-breakpoint-command-handler (command-result)
  (setf last-cdb-mi-breakpoint-command-result command-result)
  (let* ((breakpoints (loop with result = nil
                            for line in command-result
                            do (when (or (string-match "^\s*\\([0-9]+\\)\s+\\([a-zA-Z]+\\)\s+[0-9a-z`]+?\s+\\[\\(.*?\\)\s+@\s+\\([0-9]+\\)\\]\s+.*?" line)
                                         ;;  0 eu                      0001 (0001) (`d:/xxx/yy.cpp:213`)
                                         (string-match "^\s*\\([0-9]+\\)\s+\\([a-z]+\\)\s+[0-9]+\s+([0-9]+)\s+(`\\(.*?\\):\\([0-9]+\\)`)\s*$" line))
                                 (push (list (cl-parse-integer (match-string-no-properties 1 line))
                                             (match-string-no-properties 2 line)
                                             (let ((module-file
                                                    (replace-regexp-in-string
                                                     "\\\\" "/" (match-string-no-properties 3 line))))
                                               (aif (position ?! module-file)
                                                 (subseq module-file (1+ it))
                                                 module-file))
                                             (cl-parse-integer (match-string-no-properties 4 line)))
                                       result))
                            finally (return (reverse result))))
         (target-bp
           (or (loop for breakpoint in breakpoints
                     for file = (third breakpoint)
                     for lineno = (fourth breakpoint)
                     do (if (and cdbmi-bp-cached-buffer-file-name
                                 (string= (downcase (file-truename file))
                                          (downcase cdbmi-bp-cached-buffer-file-name))
                                 (= cdbmi-bp-cached-buffer-file-lineno lineno))
                          (return breakpoint)))
               (my-select-window breakpoints :always-select t :delay-seconds 0)))
         (action (my-select-window (list :delete-current-breakpoint
                                         :jump-to-position
                                         (if (find ?e (second target-bp))
                                           :disable-breakpoint
                                           :enable-breakpoint)) :delay-seconds 0)))
    (when action
      (ring-insert find-tag-marker-ring (point-marker))
      (find-file (third target-bp))
      (goto-line (fourth target-bp))
      (unless (eq action :jump-to-position)
        ;; set breakpoint limited in current buffer.
        (comint-send-string
         (get-buffer-process gud-comint-buffer)
         (format "%s %d\n"
                 (case action
                   (:delete-current-breakpoint
                    (setf (gethash (or cdb-mi-current-module 'nil) cdb-mi-breakpoints)
                            (loop for (file-line . cmd) in (gethash (or cdb-mi-current-module 'nil) cdb-mi-breakpoints)
                                  unless (and (string= buffer-file-name (car file-line))
                                              (= (line-number-at-pos) (second file-line)))
                                    collect (cons file-line cmd)))
                    (cdb-mi-remove-breakpoint-icons (line-beginning-position)
                                                    (line-end-position))
                    "bc")
                   (:disable-breakpoint
                    (cdb-mi-put-breakpoint-icon nil 0 (fourth target-bp))
                    "bd")
                   (:enable-breakpoint
                    (cdb-mi-put-breakpoint-icon t 0 (fourth target-bp))
                    "be"))
                 (first target-bp)))))))

;;;; local window

(defun cdb-locals-buffer-name ()
  (cdb-current-context-buffer-name
   (concat "locals of " (cdb-get-target-string))))

(defvar cdb-locals-header
    (list
     (cdb-propertize-header "Locals" cdb-locals-buffer
                            nil nil mode-line)
     " "
     (cdb-propertize-header "Thread" cdb-threads-buffer
                            "mouse-1: select" mode-line-highlight
                            mode-line-inactive)))

(defvar cdb-locals-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map [follow-link] 'mouse-face)
    map))
(evil-define-key 'normal cdb-locals-mode-map "o" 'other-window)
(evil-define-key 'normal cdb-locals-mode-map "s" 'my-global-choise)
(evil-define-key 'normal cdb-locals-mode-map "q" 'kill-this-buffer)

(define-derived-mode cdb-locals-mode cdb-parent-mode "Locals"
  "Major mode for cdb locals."
  (setq header-line-format cdb-locals-header)
  (setq mode-line-format nil)
  'cdb-invalidate-locals)

(defun cdb-locals-buffer-name ()
  (cdb-current-context-buffer-name
   (concat "locals of " (cdb-get-target-string))))

(defvar cdb-mi-current-locals nil)
(defun cdb-local-tree-browse (tree &rest ignore)
  (message "%s" (widget-get tree :var)))
(defun cdb-local-tree-expand-p (tree)
  (or (equalp (widget-get tree :var) cdb-mi-current-locals)
      (listp (car (widget-get tree :var)))))

(defun cdb-local-tree-expand (tree)
  (or (widget-get tree :args)
      (if (equalp (widget-get tree :var) cdb-mi-current-locals)
        (mapcar 'cdb-local-tree-widget
                cdb-mi-current-locals)
      (mapcar 'cdb-local-tree-widget
               (widget-get tree :var)))))

(cl-defun cdb-mi-var-tag (var)
  (if (equalp cdb-mi-current-locals var)
    (return-from cdb-mi-var-tag "Local Var"))
  (labels ((%print (var)
             (format "%s : %s" (first var)
                     (if (listp (cdr var))
                       (third var)
                       (cdr var)))))
    (if (stringp (car var))
      (%print var)
      (%print (car var)))))
(defun cdb-local-tree-widget (var)
  `(tree-widget
    :var ,var
    :open ,(or (equalp var cdb-mi-current-locals))
    :node (push-button
           :var ,var
           :tag ,(cdb-mi-var-tag var)
           :format "%[%t%]\n"
           :notify cdb-local-tree-browse)
    :expander-p cdb-local-tree-expand-p
    :expander cdb-local-tree-expand))
(defun test-locals-tree-buffer ()
  (with-current-buffer (get-buffer-create "*tree*")
    (make-local-variable 'cdb-mi-current-local-tree)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))
    (widget-create (cdb-local-tree-widget cdb-mi-current-locals))
    ;; (tree-mode)
    )
  (switch-to-buffer "*tree*"))

(defvar cdb-mi-locals-result nil)
(defun cdb-locals-handler-custom (buffer-type command-result)
  (let ((custom-cdb-buffer-type cdb-buffer-type)
        (custom-header-line-format header-line-format))
    (setf cdb-mi-locals-result command-result)
    (setf cdb-mi-current-locals (cdb-locals-handler-result-to-tree command-result))
    (delete-region (point-min) (point-max))
    (widget-create (cdb-local-tree-widget cdb-mi-current-locals))
    (tree-mode)
    ;; widget will destroy all local variables,restore it.
    (setf cdb-buffer-type custom-cdb-buffer-type
          header-line-format custom-header-line-format)))
(defvar cdb-struct-indent-spaces 3)
(defun cdb-mi-struct-level (line)
  (/ (loop for c across line
           for i from 0
           do (if (/= c ? )
                (return i)))
     cdb-struct-indent-spaces))

(defvar last-cdb-mi-local-struct nil)
(defun cdb-mi-convert-struct-to-tree (struct)
  (setf last-cdb-mi-local-struct struct)
  (loop with struct-list = (list nil)
        with current-level = (or (caar struct) 1)
        for (level var value) in struct
        do (cond ((= level current-level)
                  (push (cons var value) (car struct-list)))
                 ((> level current-level)
                  ;;FIXME::
                  ;;(assert (= (1+ current-level) level))
                  (when (= (1+ current-level) level)
                    (let* ((parent-var-value (pop (car struct-list)))
                           (parent-struct (list parent-var-value)))
                      (setf current-level level)
                      (push parent-struct struct-list)))
                  (push (cons var value) (car struct-list)))
                 ((< level current-level)
                  (loop repeat (- current-level level)
                        do (push (reverse (pop struct-list)) (car struct-list)))
                  (push (cons var value) (car struct-list))
                  (setf current-level level)))
        finally (loop repeat (1- current-level)
                      do (push (reverse (pop struct-list)) (car struct-list)))
                (return (reverse (car struct-list)))))
(defun cdb-locals-handler-result-to-tree (lines)
  (loop with result = nil
        for line in lines
        do (if (string-match "^\s*\\(.*?\\)\s*=\s*\\(.*?\\)\s*$" line)
             (push
               (list (match-string-no-properties 1 line)
                     " "
                     (match-string-no-properties 2 line))
               result))
        finally (return (reverse result))))

(defun cdb-locals-handler-result-to-tree-dt (lines)
  (loop with result = nil
        with in-a-var-p = nil
        with locals = nil
        with var-name = nil
        with var-type = nil
        with var-value = nil
        with var-subvalues = nil
        for line in lines
        do
     (cond ((string-match "^beginvar \\(.*?\\)$" line)
            (setf in-a-var-p t
                  var-name (trim-string (match-string-no-properties 1 line))))
           ((string-match "^endvar .*?$" line)
            (if var-subvalues
              (setf cdb-debug-last-struct-value var-subvalues))
            (if var-subvalues
              (push (cons (list var-name var-type var-value)
                          (cdb-mi-convert-struct-to-tree (reverse (copy-tree var-subvalues))))
                    result)
              (push (list var-name var-type var-value) result))
            (setf in-a-var-p nil
                  var-name nil
                  var-type nil
                  var-value nil
                  var-subvalues nil
                  ))
           (t (cond ((null var-type)
                     (setf var-type
                             (if (string-match ".*?\s+?Type\s+?\\(.*?\\)$" line)
                               (match-string-no-properties 1 line)
                               line)))
                    (t
                     (let ((sub-level (cdb-mi-struct-level line)))
                       (if (null var-value)
                         (setf var-value
                                 (if (= sub-level 0)
                                   line
                                   " "))
                         (when (> sub-level 0); ignore `(= i 0)' :  the real data type by a pointer
                           (if (string-match "^\s+\\(.+?\\)\s+:\s+\\(.+?\\)\s*$" line)
                             (let ((var (trim-string (match-string 1 line)))
                                   (value (trim-string (match-string 2 line))))
                               (push (list sub-level var value) var-subvalues))
                             (warn "failed to parse structure value:%s" line)))))))))
        finally (return (reverse result))))

(def-cdb-trigger-and-handler
    cdb-locals-buffer
    cdb-invalidate-locals
    "dv";"!for_each_local \".echo beginvar @#Local; dt @#Local -o;.echo endvar @#Local\""
    cdb-locals-handler cdb-locals-handler-custom
  '(update))

(cdb-set-buffer-rules
 'cdb-locals-buffer
 'cdb-locals-buffer-name
 'cdb-locals-mode
 'cdb-invalidate-locals
 'cdb-locals-handler)

(def-cdb-preempt-display-buffer
    cdb-preemptively-display-locals-buffer
    'cdb-locals-buffer nil t)

(defvar cdb-locals-watch-map
    (let ((map (make-sparse-keymap)))
      (suppress-keymap map)
      (define-key map "\r" 'gud-watch)
      (define-key map [mouse-2] 'gud-watch)
      map)
  "Keymap to create watch expression of a complex data type local variable.")

(defvar cdb-edit-locals-map-1
    (let ((map (make-sparse-keymap)))
      (suppress-keymap map)
      (define-key map "\r" 'cdb-edit-locals-value)
      (define-key map [mouse-2] 'cdb-edit-locals-value)
      map)
  "Keymap to edit value of a simple data type local variable.")

(defun cdb-edit-locals-value (&optional event)
  "Assign a value to a variable displayed in the locals buffer."
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (beginning-of-line)
    (let* ((var (bindat-get-field
                 (get-text-property (point) 'cdb-local-variable) 'name))
	   (value (read-string (format "New value (%s): " var))))
      (gud-basic-call
       (concat  "-cdb-set variable " var " = " value)))))


(defun cdb-display-locals-buffer (&optional thread)
  "Display the local variables of current CDB stack."
  (interactive)
  (cdb-display-buffer (cdb-get-buffer-create 'cdb-locals-buffer thread)))

;;;; stack window

(defvar cdb-frames-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "\r" 'cdb-select-frame)
    (define-key map [mouse-2] 'cdb-select-frame)
    (define-key map [follow-link] 'mouse-face)
    map))
(evil-define-key 'normal cdb-frames-mode-map "o" 'other-window)
(evil-define-key 'normal cdb-frames-mode-map "s" 'my-global-choise)
(evil-define-key 'normal cdb-frames-mode-map "q" 'kill-this-buffer)
(evil-define-key 'normal cdb-frames-mode-map (kbd "RET") 'cdb-select-frame)

(defvar cdb-frames-font-lock-keywords
  '((" \\(.+?\\)!"  (1 font-lock-function-name-face))
    ("!\\(.+?\\)[0-9]x"  (1 font-lock-variable-name-face)))
  "Font lock keywords used in `cdb-frames-mode'.")

(define-derived-mode cdb-frames-mode cdb-parent-mode "Frames"
  "Major mode for cdb call stack."
  (setq cdb-stack-position (make-marker))
  (setq mode-line-format nil)
  (setf header-line-format nil)
  (add-to-list 'overlay-arrow-variable-list 'cdb-stack-position)
  (setq truncate-lines t)  ;; Make it easier to see overlay arrow.

  (set (make-local-variable 'font-lock-defaults)
       '(cdb-frames-font-lock-keywords))
  'cdb-invalidate-frames)

(defun cdb-select-frame (&optional event)
  "Select the frame and display the relevant source."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (save-excursion
    (goto-char (line-beginning-position))
    (when (re-search-forward "^\\([0-9]+\\) " nil t)
      (cl-parse-integer (match-string 1))
      (let ((command (format ".frame %s\n" (match-string 1))))
        (process-send-string (get-buffer-process gud-comint-buffer) command)))))


(def-cdb-preempt-display-buffer
    cdb-preemptively-display-stack-buffer
    'cdb-stack-buffer nil t)

;; Frames buffer.  This displays a perpetually correct backtrack trace.
;;
(def-cdb-trigger-and-handler
  cdb-stack-buffer cdb-invalidate-frames "kn"
  cdb-stack-list-frames-handler cdb-stack-list-frames-custom
  '( update))

(cdb-set-buffer-rules
 'cdb-stack-buffer
 'cdb-stack-buffer-name
 'cdb-frames-mode
 'cdb-invalidate-frames
 'cdb-stack-list-frames-handler)

(defvar cdb-mi-last-frames nil)
(defun cdb-stack-list-frames-custom (buffer-type command-result)
  (let ((inhibit-read-only t))
    (setf cdb-mi-last-frames (copy-tree command-result))
    (pop command-result)
    (delete-region (point-min) (point-max))
    (insert (loop with result = ""
                  for line in command-result
                  do (when (string-match "^\\([0-9]+\\)\s+[0-9a-zA-Z`]+\s+[0-9a-zA-Z`]+\s+\\(.*\\)$" line)
                       (setf result (concat result (match-string 1 line) " " (match-string 2 line) "\n")))
                  finally (return result)))))

(defun cdb-stack-buffer-name ()
  (cdb-current-context-buffer-name
   (concat "stack frames of " (cdb-get-target-string))))

(defun cdb-display-stack-buffer (&optional thread)
  "Display CDB backtrace for current stack."
  (interactive)
  (cdb-display-buffer (cdb-get-buffer-create 'cdb-stack-buffer thread)))


(defun cdb-display-stack-buffer ()
  (interactive)
  (cdb-display-buffer (cdb-get-buffer-create 'cdb-stack-buffer)))

;;;; threads window
(defvar cdb-threads-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "\r" 'cdb-select-thread)
    (define-key map [mouse-2] 'cdb-select-thread)
    (define-key map [follow-link] 'mouse-face)
    map))
(evil-define-key 'normal cdb-threads-mode-map "o" 'other-window)
(evil-define-key 'normal cdb-threads-mode-map "s" 'my-global-choise)
(evil-define-key 'normal cdb-threads-mode-map "q" 'kill-this-buffer)
(evil-define-key 'normal cdb-threads-mode-map (kbd "RET") 'cdb-select-thread)

(defvar cdb-threads-font-lock-keywords
  '(("^\\([^ ]+\\)"  (1 font-lock-function-name-face)))
  "Font lock keywords used in `cdb-threads-mode'.")

(defvar cdb-threads-header
    (list
     (cdb-propertize-header "Locals" cdb-locals-buffer
                            nil nil mode-line-inactive)
     " "
     (cdb-propertize-header "Thread" cdb-threads-buffer
                            "mouse-1: select" mode-line-highlight
                            mode-line)))

(define-derived-mode cdb-threads-mode cdb-parent-mode "Threads"
  "Major mode for cdb call stack."
  (setq header-line-format cdb-threads-header)
  (setq mode-line-format nil)
  (setq cdb-thread-position (make-marker))
  (setq truncate-lines t)  ;; Make it easier to see overlay arrow.
  (set (make-local-variable 'font-lock-defaults)
       '(cdb-threads-font-lock-keywords))
  'cdb-invalidate-threads)

(defun cdb-select-thread (&optional event)
  "Select the thread and display the relevant source."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (save-excursion
    (goto-char (line-beginning-position))
    (when (re-search-forward "\s+\\([0-9]+?\\)\s+" nil t)
      (cl-parse-integer (match-string 1))
      (let ((command (format "~%ss\n" (match-string 1))))
        (process-send-string (get-buffer-process gud-comint-buffer) command)
        (sit-for 0.5)
        (update-cdb-mi-buffers)))))

(def-cdb-preempt-display-buffer
    cdb-preemptively-display-thread-buffer
    'cdb-thread-buffer nil t)

(def-cdb-trigger-and-handler
  cdb-threads-buffer cdb-invalidate-threads "~"
  cdb-thread-list-handler cdb-thread-list-custom
  '( update))

(cdb-set-buffer-rules
 'cdb-threads-buffer
 'cdb-threads-buffer-name
 'cdb-threads-mode
 'cdb-invalidate-threads
 'cdb-thread-list-handler)

(defvar cdb-mi-last-threads nil)
(defun cdb-thread-list-custom (buffer-type command-result)
  (let ((inhibit-read-only t))
    (setf cdb-mi-last-threads (copy-tree command-result))
    (delete-region (point-min) (point-max))
    (insert (loop with result = ""
                  for line in command-result
                  ;;"^\\([0-9]+\\)\s+[0-9a-zA-Z`]+\s+[0-9a-zA-Z`]+\s+\\(.*\\)$"
                  do (setf result (concat result line "\n"))
                  finally (return result)))))

(defun cdb-threads-buffer-name ()
  (cdb-current-context-buffer-name
   (concat "threads of " (cdb-get-target-string))))

;;;; window layout
(defun cdb-mi-setup-windows ()
  (interactive)
  (cdb-get-buffer-create 'cdb-locals-buffer)
  (cdb-get-buffer-create 'cdb-stack-buffer)
  (delete-other-windows)
  (cdb-get-buffer-create 'cdb-locals-buffer)
  (let* ((win0 (selected-window))
         (win1 (split-window nil (/ (* (window-height) 3) 5)))
         (win2 (progn (select-window win1) (split-window-right))))
    (setf cdb-mi-locals-win win1
          cdb-mi-stack-win win2)
    (cdb-set-window-buffer (cdb-get-buffer-create 'cdb-locals-buffer) nil win1)
    (cdb-set-window-buffer (cdb-get-buffer-create 'cdb-stack-buffer) nil win2)
    (update-cdb-mi-buffers)))
