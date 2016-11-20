;; notes

;; (.Foo Bar)  ;; typeof(Bar).Foo
;;   (. (identity Bar) Foo)
;; (. Bar Foo) ;; Bar.Foo

;; (.instanceMember instance args*)
;; (.instanceMember Classname args*)
;; (.-instanceField instance)
;; (Classname/staticMethod args*)
;; Classname/staticField
;; 
;; (.. instance-expr member+)
;; (.. Classname-symbol member+)
;; (. instance-expr member-symbol)
;; (. Classname-symbol member-symbol)
;; (. instance-expr -field-symbol)
;; (. instance-expr (method-symbol args))*
;; (. instance-expr method-symbol args)*
;; (. Classname-symbol (method-symbol args))*
;; (. Classname-symbol method-symbol args)*

(let [[dot first-op second-op & args]]
  (if (and (symbol? first-op)
           (class-name? first-op))
    ;; static member
    (cond (and (symbol? second-op)
               (empty? args))
          ;; static field/property access
          ;; or static zero-arity-method call
          {:op :host-interop}
          (or (list? second-op)
              (not (empty? args)))
          ;; static method call
          {:op :host-call}
          )
    ;; instance member
    (cond (and (symbol? second-op)
               (empty? args))
          ;; instance field/property access
          ;; or instance zero-arity-method call
          (or (list? second-op)
              (not (empty? args)))
          ;; instance method call
          )))

;; If the first operand is a symbol that resolves to a class name, the access is considered to be to a static member of the named class. Note that nested classes are named EnclosingClass$NestedClass, per the JVM spec. Otherwise it is presumed to be an instance member and the first argument is evaluated to produce the target object.

;; If the second operand is a symbol and no args are supplied it is taken to be a field access - the name of the field is the name of the symbol, and the value of the expression is the value of the field, unless there is a no argument public method of the same name, in which case it resolves to a call to the method. If the second operand is a symbol starting with -, the member-symbol will resolve only as field access (never as a 0-arity method) and should be preferred when that is the intent.

;; If the second operand is a list, or args are supplied, it is taken to be a method call. The first element of the list must be a simple symbol, and the name of the method is the name of the symbol. The args, if any, are evaluated from left to right, and passed to the matching method, which is called, and its value returned. If the method has a void return type, the value of the expression will be nil. Note that placing the method name in a list with any args is optional in the canonic form, but can be useful to gather args in macros built upon the form.