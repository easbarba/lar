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

(provide 'init-utils)
