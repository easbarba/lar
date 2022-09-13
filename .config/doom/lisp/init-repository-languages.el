;;; -*- lexical-binding: t;

;; ================================
;; ADDITIONAL LANGUAGES TOOLINGS
;; ================================

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
            java-mode))

(set-ligatures! '(java-mode)
  ;; Functional
  ;; :def "void "
  ;; Types
  :null "null"
  :true "true" :false "false"
  :int "int" :float "float"
  ;; :str "std::string"
  :bool "bool"
  ;; Flow
  :not "!"
  :and "&&" :or "||"
  :for "for"
  :return "return"
  :yield "import")


(provide 'init-repository-languages)
