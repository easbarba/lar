;;; -*- lexical-binding: t;

;; ================================
;; ADDITIONAL LANGUAGES TOOLINGS
;; ================================

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
