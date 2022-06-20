(defparameter *async-shell* (uiop:launch-program "bash" :input :stream :output :stream))

(defun async-run (command)
  (write-line command (uiop:process-info-input *async-shell*))
  (force-output (uiop:process-info-input *async-shell*))
  (let* ((output-string (read-line (uiop:process-info-output *async-shell*)))
         (stream (uiop:process-info-output *async-shell*)))
    (if (listen stream)
        (loop while (listen stream)
              do (setf output-string (concatenate 'string
                                                  output-string
                                                  '(#\Newline)
                                                  (read-line stream)))))
    output-string))


(defun dy/load (filename)
  "Load FILENAME, without extension."
  (let ((file (merge-pathnames (concat filename ".lisp")
                               *config-home*)))
    (if (probe-file file)
        (load file)
        (format *error-output* "File '~a' doesn't exist." file))))

;; s-sysinfo modeline
(setf *screen-mode-line-format*
      (list "^B%n^b:  %W ^> "
            " "
            '(:eval (string-trim (string #\newline) (async-run "s-sysinfo")))
            " "
            "%d"
            "     "))
