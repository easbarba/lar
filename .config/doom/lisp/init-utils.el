;;; -*- lexical-binding: t;

;; ============================
;; * UTILITIES
;; ============================

;; ========================================
;; ** e/MACROS

(defmacro assoc-val (var vars)
  "Get value(cdr) of alist."
  `(cdr (assoc ',var ,vars)))

(defmacro assoc-key (var vars)
  "Get key(car) of alist."
  `(car (assoc ',var ,vars)))

;; ========================================
;; ** e/PREDICATES

(defun pack-installed-p (package)
  "Predicate: Confirm if PACKAGE is installed."
  (when (executable-find package)
    t))

(defun e/return-exec (apps)
  "Return first executable in APPS found.
TODO: return if running"
  (require 'cl-lib)
  (cl-dolist (app apps)
    (when (executable-find app)
      (cl-return app))))

(provide 'init-utils)
