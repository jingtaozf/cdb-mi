;;; -*- encoding:utf-8 -*-  ---
(provide 'my-utils)
(require 'cl)
(load "cl-seq")
(load "cl-loaddefs")
(unless (fboundp 'cl-parse-integer)
  (defalias 'cl-parse-integer 'parse-integer)) 
(unless (fboundp 'parse-integer)
  (defalias 'parse-integer 'cl-parse-integer)) 

;;;###autoload
(defun my-file-as-string (file)
  (interactive)
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))
;;;###autoload
(defun my-lisp-to-file (file lisp-obj)
  (interactive)
  (with-current-buffer (find-file-noselect file)
    (delete-region (point-min) (point-max))
    (insert (format "%S" lisp-obj))
    (save-buffer)
    (kill-buffer)))

;;;###autoload
(defun my-file-to-lisp (file)
  (interactive)
  (awhen (my-file-as-string file)
    (car (ignore-errors (read-from-string it)))))

(defvar *shortcut-keys* "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' []/;,.-=\\_")
;;;###autoload
(defun shortcut-menu (list)
  (let ((keys (loop for c across *shortcut-keys* collect c)))
    (loop for item in list
       collect (let ((shortcut (or (loop for c across item
                                         thereis (find c keys))
                                   (first keys))))
                 (when shortcut
                   (setf keys (delete shortcut keys))
                   (cons shortcut item))))))

;;;###autoload
(defun my-select-window (list &rest cl-keys)
  "Select a cmd with its prefix key."
  (interactive)
  (cl-parsing-keywords ((:prompt "String to select") (:delay-seconds 1)
                        (:always-select nil)
                        (:return 'identity) (:view 'identity)) ()

                        (when (null list)
                          (error "%s: list is empty" (if cl-prompt cl-prompt "Error in my-select-window")))
                        (if (and (null cl-always-select) (= 1 (length list)))
                          (funcall cl-return (car list)); return it if only one choice.
                          (let* ((view-list (loop for x in list collect (format "%s" (funcall cl-view x))))
                                 (shortcut-list (shortcut-menu view-list)))
                            (if (and (setq rpl (read-char-exclusive "" nil (or cl-delay-seconds 1)))
                                     (assoc rpl shortcut-list))
                              (loop with select = (cdr (assoc rpl shortcut-list))
                                    for view in view-list
                                    for item in list
                                    when (string= view select)
                                      return (funcall cl-return item))
                              (let (pop-up-frames)
                                (save-window-excursion
                                  (with-current-buffer;org-switch-to-buffer-other-window
                                      (get-buffer-create "*String to Select*")
                                    (erase-buffer)
                                    (insert (org-add-props (concat cl-prompt "\n")
                                                nil 'face 'bold))
                                    (loop for (k . s) in shortcut-list
                                          do (insert (format "[%c] %s\n" k s)))
                                    ;;(org-fit-window-to-buffer)
                                    ;;FIXME: how to make window height suitable for display.
                                    ;;(enlarge-window (window-height))
                                    (goto-char (point-min)))
                                  (pop-to-buffer (get-buffer-create "*String to Select*"))
                                  (prog1
                                    (loop for rpl = (read-char-exclusive cl-prompt nil)
                                          do (cond ((= rpl 27)
                                                    (return))
                                                   ((or (= rpl ?\^?); Backspace
                                                        (= rpl 134217846)); Alt-v
                                                    (ignore-errors (scroll-down 5)))
                                                   ((or (= rpl ?\r); Enter
                                                        (= rpl 22)); Ctrl-v
                                                    (ignore-errors (scroll-up 5)))
                                                   ((assoc rpl shortcut-list)
                                                    (return
                                                      (loop with select = (cdr (assoc rpl shortcut-list))
                                                            for view in view-list
                                                            for item in list
                                                            when (string= view select)
                                                              return (funcall cl-return item))))
                                                   (t (error "Invalid task choice %c" rpl))))
                                    (bury-buffer "*String to Select*")))))))))
;;;###autoload
(defun test-my-select-window ()
  (interactive)
  (let* ((cmds
          (loop for i from 1 to 30 collect
                (loop for i from 1 to 4 concat (char-to-string (+ ?a (random 26))))))
         (result (my-select-window cmds :prompt "Test")))
    (message result)))

