#lang racket
(require (prefix-in stx:"syntax.rkt"))
(require (prefix-in par:"parser.rkt"))
(provide (all-defined-out))

;;オブジェクト
(struct decl (name lev kind type) #:transparent)

;;デルタ関数
(define initial-delta (lambda (x) #f))
(define (extend-delta delta x data)
  (lambda (y) (if (equal? x y) data (delta y))))
(define delta1 initial-delta) ;;初期化
(define delta
  (let* ((name 'print)
        (new-obj (decl name 0 'proto (list 'proto 'void 'int))))
  (extend-delta delta1 name new-obj)))


(define (print-error mes pos) (displayln mes (current-error-port)))
(define (print-warning mes pos) (displayln mes (current-error-port)))



;;意味解析
(define (sem-ast env cur-lev f-name ast notuse)
  (cond ((stx:declaration? ast);;変数宣言
         (map (lambda(s) (cdr s)) (foldr (lambda (x foldr-env)
                                          (let* ((type (cadr x))
                                                 (name (car x))
                                                 (pos (stx:declaration-pos ast))
                                                 (new-env (if (equal? foldr-env '()) env (caar foldr-env)))
                                                 (obj (new-env name)))
                                            (if (equal? type 'void)
                                                (print-error "void type is used" pos) #f)
                                            (if (equal? type (list 'pointer 'void))
                                                (print-error "void type is used" pos) #f)
                                            (if (list? type)
                                                (if (and (equal? (car type) 'array) (equal? (cadr type) 'void))
                                                    (print-error "void type is used" pos) #f) #f)
                                            (if (list? type)
                                                (if (and (equal? (car type) 'array) (equal? (cadr type) (list 'pointer 'void)))
                                                    (print-error "void type is used" pos) #f) #f) ;;void
                                            (if obj (let ((lev (decl-lev obj)) ;;objが見つかった場合
                                                          (kind (decl-kind obj)))
                                                      (cond ((or (equal? kind 'fun) (equal? kind 'proto))
                                                             (if (equal? cur-lev 0)
                                                                 (print-error "double declaration" pos)
                                         (let ((new-obj (decl name cur-lev 'var type)))
                                           (cons (cons (extend-delta new-env name new-obj) (stx:declaration new-obj pos (extend-delta new-env name new-obj))) foldr-env))))
                                                            ((equal? kind 'var)
                                                             (if (equal? cur-lev lev)
                                                                 (print-error "double declaration" pos)
                                                                 (let ((new-obj (decl name cur-lev 'var type)))
                                                                   (cons (cons (extend-delta new-env name new-obj) (stx:declaration new-obj pos (extend-delta new-env name new-obj))) foldr-env))))
                                                            ((equal? kind 'parm)
                                                             ((let ((new-obj (decl name cur-lev 'var type)))
                                                                (cons (cons (extend-delta new-env name new-obj) (stx:declaration new-obj pos (extend-delta new-env name new-obj))) foldr-env))
                                                              (print-warning "var hide parm" pos)))))
                                                (let ((new-obj (decl name cur-lev 'var type))) ;;objが見つからなかった場合
                                                  (cons (cons (extend-delta new-env name new-obj) (stx:declaration new-obj pos (extend-delta new-env name new-obj))) foldr-env))))) '() (stx:declaration-declaration-list ast))))          
        ((stx:function-prototype? ast) ;;関数プロトタイプ
         (let* ((parm-list (stx:function-prototype-function-declarator ast))
                (name (stx:function-prototype-function-name ast))
                (type (cons 'proto (cons (stx:function-prototype-type-specifire ast) (map (lambda (x) (cadr x)) parm-list))))
                (pos (stx:function-prototype-pos ast))
                (obj (env name))
                (sem-parm-list  (map (lambda(s) (cdr s)) (foldr (lambda (x foldr-env)
                                                                 (let* ((ptype (cadr x))
                                                                        (pname (car x))
                                                                        (new-env (if (equal? foldr-env '()) env (caar foldr-env)))
                                                                        (pobj (new-env pname)))
                                                                   (if (equal? ptype 'void)
                                                                       (print-error "void type is used" pos) #f)
                                                                   (if (equal? ptype (list 'pointer 'void))
                                                                       (print-error "void type is used" pos) #f)
                                                                   (if (list? ptype)
                                                                       (if (and (equal? (car ptype) 'array) (equal? (cadr ptype) 'void))
                                                                           (print-error "void type is used" pos) #f) #f)
                                                                   (if (list? ptype)
                                                                       (if (and (equal? (car ptype) 'array) (equal? (cadr ptype) (list 'pointer 'void)))
                                                                           (print-error "void type is used" pos) #f) #f)
                                                                   (if pobj (let ((plev (decl-lev pobj)) ;;objが見つかった場合
                                                                                  (pkind (decl-kind pobj)))
                                                                              (if (equal? pkind 'parm)
                                                                                  (print-error "double declaration" pos)
                                                                                  (let ((new-obj (decl pname 1 'parm ptype))) 
                                                                                    (cons (cons (extend-delta new-env pname new-obj) (stx:declaration new-obj pos (extend-delta new-env pname new-obj))) foldr-env))))
                                                                       (let ((new-obj (decl pname 1 'parm ptype))) ;;objが見つからなかった場合
                                                                         (cons (cons (extend-delta new-env pname new-obj) (stx:declaration new-obj pos (extend-delta new-env pname new-obj))) foldr-env))))) '() parm-list))))
           (if (equal? (stx:function-prototype-type-specifire ast) (list 'pointer 'void))
               (print-error "void type is used" pos) #f)
           (if (list? (stx:function-prototype-type-specifire ast))
               (if (and (equal? (car (stx:function-prototype-type-specifire ast)) 'array) (equal? (cadr (stx:function-prototype-type-specifire ast)) 'void))
                   (print-error "void type is used" pos) #f) #f)
           (if (list? (stx:function-prototype-type-specifire ast))
               (if (and (equal? (car (stx:function-prototype-type-specifire ast)) 'array) (equal? (cadr (stx:function-prototype-type-specifire ast)) (list 'pointer 'void)))
                   (print-error "void type is used" pos) #f) #f)
           (if obj (let ((lev (decl-lev obj)) ;;objが見つかった場合
                         (kind (decl-kind obj))
                         (dtype (decl-type obj)))
                     (cond ((equal? kind 'fun)
                            (if (equal? type dtype)
                                (let ((new-obj (decl name cur-lev 'proto type)))
                                  (cdr (cons (extend-delta env name new-obj) (stx:function-prototype type new-obj sem-parm-list pos (extend-delta env name new-obj)))))
                                (print-error "double declaration" pos)))
                           ((equal? kind 'proto)
                            (if (equal? type dtype)
                                (let ((new-obj (decl name cur-lev 'proto type)))
                                  (cdr (cons (extend-delta env name new-obj) (stx:function-prototype type new-obj sem-parm-list pos (extend-delta env name new-obj)))))
                                (print-error "double declaration" pos)))
                           ((equal? kind 'var)
                            (if (equal? cur-lev lev)
                                (print-error "double declaration" pos)
                                (let ((new-obj (decl name cur-lev 'proto type)))
                                  (cdr (cons (extend-delta env name new-obj) (stx:function-prototype type new-obj sem-parm-list pos (extend-delta env name new-obj)))))))
                           ((equal? kind 'parm)
                            (let ((new-obj (decl name cur-lev 'proto type)))
                              (cdr (cons (extend-delta env name new-obj) (stx:function-prototype type new-obj sem-parm-list pos (extend-delta env name new-obj))))))))
               (let ((new-obj (decl name cur-lev 'proto type))) ;;objが見つからなかった場合
                 (cdr (cons (extend-delta env name new-obj) (stx:function-prototype type new-obj sem-parm-list pos (extend-delta env name new-obj))))))))
        ((stx:function-definition? ast) ;;関数定義
         (let* ((parm-list (stx:function-definition-function-declarator ast))
                (name (stx:function-definition-function-name ast))
                (type (cons 'fun (cons (stx:function-definition-type-specifire ast) (map (lambda (x) (cadr x)) parm-list))))
                (stmt (stx:function-definition-compound-statement ast))
                (pos (stx:function-definition-pos ast))
                (obj (env name))
                (fenv
                 (if obj (let ((lev (decl-lev obj)) ;;objが見つかった場合
                         (kind (decl-kind obj))
                         (dtype (decl-type obj)))
                     (cond ((equal? kind 'fun)
                            (print-error "double declaration" pos))
                           ((equal? kind 'proto)
                            (if (equal? type dtype)
                                (let ((new-obj (decl name cur-lev 'fun type)))
                                  (car (cons (extend-delta env name new-obj) (stx:function-definition type new-obj 'hoge 'hoge pos (extend-delta env name new-obj)))))
                                (print-error "double declaration" pos)))
                           ((equal? kind 'var)
                            (if (equal? cur-lev lev)
                                (print-error "double declaration" pos)
                                (let ((new-obj (decl name cur-lev 'fun type)))
                                  (car (cons (extend-delta env name new-obj) (stx:function-definition type new-obj 'hoge 'hoge pos (extend-delta env name new-obj)))))))
                           ((equal? kind 'parm)
                            (let ((new-obj (decl name cur-lev 'fun type)))
                              (car (cons (extend-delta env name new-obj) (stx:function-definition type new-obj 'hoge 'hoge pos (extend-delta env name new-obj))))))))
               (let ((new-obj (decl name cur-lev 'fun type))) ;;objが見つからなかった場合
                 (car (cons (extend-delta env name new-obj) (stx:function-definition type new-obj 'hoge 'hoge pos (extend-delta env name new-obj)))))))
                (sem-parm-list-sub (foldr (lambda (x foldr-env)
                                                                 (let* ((ptype (cadr x))
                                                                        (pname (car x))
                                                                        (new-env (if (equal? foldr-env '()) fenv (caar foldr-env)))
                                                                        (pobj (new-env pname)))
                                                                   (if (equal? ptype 'void)
                                                                       (print-error "void type is used" pos) #f)
                                                                   (if (equal? ptype (list 'pointer 'void))
                                                                       (print-error "void type is used" pos) #f)
                                                                   (if (list? ptype)
                                                                       (if (and (equal? (car ptype) 'array) (equal? (cadr ptype) 'void))
                                                                           (print-error "void type is used" pos) #f) #f)
                                                                   (if (list? ptype)
                                                                       (if (and (equal? (car ptype) 'array) (equal? (cadr ptype) (list 'pointer 'void)))
                                                                           (print-error "void type is used" pos) #f) #f)
                                                                   (if pobj (let ((plev (decl-lev pobj)) ;;objが見つかった場合
                                                                                  (pkind (decl-kind pobj)))
                                                                              (if (equal? pkind 'parm)
                                                                                  (print-error "double declaration" pos)
                                                                                  (let ((new-obj (decl pname 1 'parm ptype))) 
                                                                                    (cons (cons (extend-delta new-env pname new-obj) (stx:declaration new-obj pos (extend-delta new-env pname new-obj))) foldr-env))))
                                                                       (let ((new-obj (decl pname 1 'parm ptype))) ;;objが見つからなかった場合
                                                                         (cons (cons (extend-delta new-env pname new-obj) (stx:declaration new-obj pos (extend-delta new-env pname new-obj))) foldr-env))))) '() parm-list))
                (new-env (if (equal? sem-parm-list-sub '()) fenv (caar sem-parm-list-sub)))
                (sem-parm-list (map (lambda(s) (cdr s)) sem-parm-list-sub))
                (sem-stmt (sem-ast new-env 1 type stmt notuse)))
           (if (equal? (stx:function-definition-type-specifire ast) (list 'pointer 'void))
               (print-error "void type is used" pos) #f)
           (if (list? (stx:function-definition-type-specifire ast))
               (if (and (equal? (car (stx:function-definition-type-specifire ast)) 'array) (equal? (cadr (stx:function-definition-type-specifire ast)) 'void))
                   (print-error "void type is used" pos) #f) #f)
           (if (list? (stx:function-definition-type-specifire ast))
               (if (and (equal? (car (stx:function-definition-type-specifire ast)) 'array) (equal? (cadr (stx:function-definition-type-specifire ast)) (list 'pointer 'void)))
                   (print-error "void type is used" pos) #f) #f)
           (if obj (let ((lev (decl-lev obj)) ;;objが見つかった場合
                         (kind (decl-kind obj))
                         (dtype (decl-type obj)))
                     (cond ((equal? kind 'fun)
                            (print-error "double declaration" pos))
                           ((equal? kind 'proto)
                            (if (equal? type dtype)
                                (let ((new-obj (decl name cur-lev 'fun type)))
                                  (cdr (cons (extend-delta new-env name new-obj) (stx:function-definition type new-obj sem-parm-list sem-stmt pos (extend-delta new-env name new-obj)))))
                                (print-error "double declaration" pos)))
                           ((equal? kind 'var)
                            (if (equal? cur-lev lev)
                                (print-error "double declaration" pos)
                                (let ((new-obj (decl name cur-lev 'fun type)))
                                  (cdr (cons (extend-delta new-env name new-obj) (stx:function-definition type new-obj sem-parm-list sem-stmt pos (extend-delta new-env name new-obj)))))))
                           ((equal? kind 'parm)
                            (let ((new-obj (decl name cur-lev 'fun type)))
                              (cdr (cons (extend-delta new-env name new-obj) (stx:function-definition type new-obj sem-parm-list sem-stmt pos (extend-delta new-env name new-obj))))))))
               (let ((new-obj (decl name cur-lev 'fun type))) ;;objが見つからなかった場合
                 (cdr (cons (extend-delta new-env name new-obj) (stx:function-definition type new-obj sem-parm-list sem-stmt pos (extend-delta new-env name new-obj))))))))
        ((stx:if-stmt? ast) ;;if文
         (if (equal? (typesearch (sem-ast env cur-lev f-name (stx:if-stmt-test ast) notuse)) 'int)
             (stx:if-stmt (sem-ast env cur-lev f-name (stx:if-stmt-test ast) notuse)
                          (sem-ast env cur-lev f-name (stx:if-stmt-tbody ast) notuse)
                          (sem-ast env cur-lev f-name (stx:if-stmt-ebody ast) notuse)
                          (stx:if-stmt-pos ast))
             (print-error "if-type is error" (stx:if-stmt-pos ast))))
        ((stx:while-stmt? ast) ;;while文
         (if (equal? (typesearch (sem-ast env cur-lev f-name (stx:while-stmt-test ast) notuse)) 'int)
             (stx:while-stmt (sem-ast env cur-lev f-name (stx:while-stmt-test ast) notuse)
                             (sem-ast env cur-lev f-name (stx:while-stmt-body ast) notuse)
                             (stx:while-stmt-pos ast))
             (print-error "while-type is error" (stx:while-stmt-pos ast))))
        ((stx:return-stmt? ast) ;;return文
         (let* ((f-type (cadr f-name))
                (return-type (if (equal? (stx:return-stmt-exp ast) '()) 'void
                                         (typesearch (sem-ast env cur-lev f-name (stx:return-stmt-exp ast) notuse))))) 
           (if (equal? f-type return-type) #f
               (print-error "return-type is error" (stx:return-stmt-pos ast)))                                 
           (stx:return-stmt return-type
                            (sem-ast env cur-lev f-name (stx:return-stmt-exp ast) notuse)
                            (stx:return-stmt-pos ast))))
        ((stx:cmpd-stmt? ast) ;;複文 
         (let* (
                (decls-sub (if (equal? (stx:cmpd-stmt-decls ast) '()) '()
                               (foldr (lambda (x foldr-env)
                                          (let* ((type (cadr x))
                                                 (name (car x))
                                                 (pos (stx:cmpd-stmt-pos ast))
                                                 (new-env (if (equal? foldr-env '()) env (caar foldr-env)))
                                                 (obj (new-env name)))
                                            (if (equal? type 'void)
                                                (print-error "void type is used" pos) #f)
                                            (if (equal? type (list 'pointer 'void))
                                                (print-error "void type is used" pos) #f)
                                            (if (list? type)
                                                (if (and (equal? (car type) 'array) (equal? (cadr type) 'void))
                                                    (print-error "void type is used" pos) #f) #f)
                                            (if (list? type)
                                                (if (and (equal? (car type) 'array) (equal? (cadr type) (list 'pointer 'void)))
                                                    (print-error "void type is used" pos) #f) #f) ;;void
                                            (if obj (let ((lev (decl-lev obj)) ;;objが見つかった場合
                                                          (kind (decl-kind obj)))
                                                      (cond ((or (equal? kind 'fun) (equal? kind 'proto))
                                                             (if (equal? cur-lev 0)
                                                                 (print-error "double declaration" pos)
                                         (let ((new-obj (decl name cur-lev 'var type)))
                                           (cons (cons (extend-delta new-env name new-obj) (stx:declaration new-obj pos (extend-delta new-env name new-obj))) foldr-env))))
                                                            ((equal? kind 'var)
                                                             (if (equal? (+ 1 cur-lev) lev)
                                                                 (print-error "double declaration" pos)
                                                                 (let ((new-obj (decl name (+ 1 cur-lev) 'var type)))
                                                                   (cons (cons (extend-delta new-env name new-obj) (stx:declaration new-obj pos (extend-delta new-env name new-obj))) foldr-env))))
                                                            ((equal? kind 'parm)
                                                             (print-warning "var hide parm" pos)
                                                             ((let ((new-obj (decl name (+ 1 cur-lev) 'var type)))
                                                                (cons (cons (extend-delta new-env name new-obj) (stx:declaration new-obj pos (extend-delta new-env name new-obj))) foldr-env))
                                                              ))))
                                                (let ((new-obj (decl name (+ 1 cur-lev) 'var type))) ;;objが見つからなかった場合
                                                  (cons (cons (extend-delta new-env name new-obj) (stx:declaration new-obj pos (extend-delta new-env name new-obj))) foldr-env))))) '() (stx:declaration-declaration-list (car (stx:cmpd-stmt-decls ast))))))
                (decls (if (equal? decls-sub '()) '() (map (lambda(s) (cdr s)) decls-sub)));;decls
                (new-env (if (equal? decls '()) env (caar decls-sub))) 
                (stmts (map (lambda (s) (sem-ast new-env (+ cur-lev 1) f-name s notuse)) (stx:cmpd-stmt-stmts ast))) ;;stmts
                (pos (stx:cmpd-stmt-pos ast))) 
           (stx:cmpd-stmt decls stmts pos)))
                               
        ((stx:assign-stmt? ast) ;;代入文
         (let* ((left-type (typesearch (sem-ast env cur-lev f-name (stx:assign-stmt-var ast) notuse)))
                (right-type (typesearch (sem-ast env cur-lev f-name (stx:assign-stmt-src ast) notuse)))
                (assign-type (if (equal? left-type right-type) left-type (print-error "type error in assign-stmt" (stx:assign-stmt-pos ast)))))
           (stx:assign-stmt assign-type
                            (sem-ast env cur-lev f-name (stx:assign-stmt-var ast) notuse)
                            (sem-ast env cur-lev f-name (stx:assign-stmt-src ast) notuse)
                            (stx:assign-stmt-pos ast))))
        ((stx:rop-exp? ast)
         (let* ((op (stx:rop-exp-op ast))
                (left (sem-ast env cur-lev f-name (stx:rop-exp-left ast) notuse))
                (right (sem-ast env cur-lev f-name (stx:rop-exp-right ast) notuse))
                (left-type (typesearch left))
                (right-type (typesearch right))
                (rop-type (cond ((or (equal? op '&&) (equal? op '||))
                                 (if (and (equal? left-type 'int) (equal? right-type 'int))
                                     'int
                                     (print-error "rop-type is not int" (stx:rop-exp-pos ast))))
                                ((or (equal? op '==) (equal? op '!=) (equal? op '<) (equal? op '>) (equal? op '<=)
                                     (equal? op '>=))
                                 (if (equal? left-type right-type)
                                     'int
                                     (print-error "rop-type is not int" (stx:rop-exp-pos ast)))))))
           (stx:rop-exp rop-type
                        (stx:rop-exp-op ast)
                        left
                        right
                        (stx:rop-exp-pos ast))))
        ((stx:aop-exp? ast)
         (let* ((op (stx:aop-exp-op ast))
                (left (sem-ast env cur-lev f-name (stx:aop-exp-left ast) notuse))
                (right (sem-ast env cur-lev f-name (stx:aop-exp-right ast) notuse))
                (left-type (typesearch left))
                (right-type (typesearch right))
                (aop-type
                 (cond ((and (or (equal? op '+) (equal? op '-) (equal? op '*) (equal? op '/))
                             (and (equal? left-type 'int) (equal? right-type 'int)))
                        'int)
                       ((and (equal? op '+) (or (and (equal? left-type (list 'pointer 'int)) (equal? right-type 'int))
                                               (and (equal? left-type 'int) (equal? right-type (list 'pointer 'int)))))
                        (list 'pointer 'int))
                       ((and (equal? op '+) (or (and (equal? left-type (list 'pointer 'pointer 'int)) (equal? right-type 'int))
                                                (and (equal? left-type 'int) (equal? right-type (list 'pointer 'pointer 'int)))))
                        (list 'pointer 'pointer 'int))
                       ((and (equal? op '-) (equal? left-type (list 'pointer 'int)) (equal? right-type 'int))
                        (list 'pointer 'int))
                       ((and (equal? op '-) (equal? left-type (list 'pointer 'pointer 'int)) (equal? right-type 'int))
                        (list 'pointer 'pointer 'int))
                       (else (print-error "aop type is error" (stx:aop-exp-pos ast))))))
           (if (and (or (equal? op '+) (equal? op '-)) (equal? left-type (list 'pointer 'int)) (equal? right-type 'int))
               (stx:aop-exp aop-type
                            (stx:aop-exp-op ast)
                            left
                            (stx:aop-exp 'int '* 4 right (stx:aop-exp-pos ast))
                            (stx:aop-exp-pos ast))
               (if (and (or (equal? op '+) (equal? op '-)) (equal? left-type 'int) (equal? right-type (list 'pointer 'int)))
                   (stx:aop-exp aop-type
                                (stx:aop-exp-op ast)
                                (stx:aop-exp 'int '* 4 left (stx:aop-exp-pos ast))
                                right
                                (stx:aop-exp-pos ast))
                   (stx:aop-exp aop-type
                                (stx:aop-exp-op ast)
                                left
                                right
                                (stx:aop-exp-pos ast))))))  
        ((stx:deref-exp? ast)
         (let* ((exp-type (typesearch (sem-ast env cur-lev f-name (stx:deref-exp-arg ast) notuse)))
                               (deref-type (cond ((equal? (cadr exp-type) (list 'pointer 'int)) (list 'pointer 'int))
                                                 ((equal? (cadr exp-type) 'int) 'int))))
           (stx:deref-exp deref-type
                          (sem-ast env cur-lev f-name (stx:deref-exp-arg ast) notuse)
                          (stx:deref-exp-pos ast))))
        ((stx:addr-exp? ast)
         (let* ((exp-type (typesearch (sem-ast env cur-lev f-name (stx:addr-exp-var ast) notuse)))
                (addr-type (if (equal? exp-type 'int) (list 'pointer 'int) (error "addr-exp is type error" (stx:addr-exp-pos ast)))))
           (stx:addr-exp addr-type
                         (sem-ast env cur-lev f-name (stx:addr-exp-var ast) notuse)
                         (stx:addr-exp-pos ast))))
        ((stx:call-exp? ast) 
         (let* ((tgt (stx:call-exp-tgt ast))
                (args (cond ((equal? (stx:call-exp-args ast) '()) '())
                            ((list? (stx:call-exp-args ast)) (map (lambda(x) (sem-ast env cur-lev f-name x notuse)) (stx:call-exp-args ast)))
                            (else (sem-ast env cur-lev f-name (stx:call-exp-args ast) notuse))))
                (args-type (if (equal? '() args) args
                               (if (list? args) (map (lambda (s) (typesearch s)) args) (list (typesearch args)))))
                (pos (stx:call-exp-pos ast))
                (obj (env tgt)))
           (if obj (let* ((kind (decl-kind obj)) ;;objが見つかった場合
                          (exp-type (decl-type obj))
                          (call-type (if (list? exp-type)  (if (equal? args-type (cddr exp-type))
                                                               (cadr exp-type) 
                                                               (print-error "call-exp is type error" (stx:call-exp-pos ast)))
                                         (if (equal? args-type exp-type) exp-type (print-error "call-exp is type error" (stx:call-exp-pos ast))))))
                     (if (or (equal? kind 'fun) (equal? kind 'proto))
                               (stx:call-exp call-type obj args pos)
                               (print-error "call-exp is var or parm" (stx:call-exp-pos ast))))
                     (print-error "call-exp is not declared" (stx:call-exp-pos ast))))) ;;objが見つからなかった場合
        ((stx:var-exp? ast) 
         (let* ((tgt (stx:var-exp-tgt ast))
                (pos (stx:var-exp-pos ast))
                (obj (env tgt)))
           (if obj (let* ((kind (decl-kind obj))
                          (exp-type (if (and (list? (decl-type obj)) (equal? (car (decl-type obj)) 'array))
                                        (cond ((and (list? (decl-type obj)) (equal? (cadr (decl-type obj)) (list 'pointer 'int)))
                                               (list 'pointer 'pointer 'int))
                                              ((and (list? (decl-type obj)) (equal? (cadr (decl-type obj)) 'int))
                                               (list 'pointer 'int)))
                                        (decl-type obj))))
                     (if (or (equal? kind 'var) (equal? kind 'parm))
                         (stx:var-exp exp-type obj pos)
                         (print-error "var-exp is fun or proto" pos)))
               (print-error "var-exp is not declared" pos))))
        ((stx:lit-exp? ast) ast)))

(define (typesearch x)
  (let* ((type (cond ((stx:massign-stmt? x) (stx:massign-stmt-exp-type x))
                     ((stx:assign-stmt? x) (stx:assign-stmt-exp-type x))
                     ((stx:var-exp? x) (stx:var-exp-exp-type x))       
                     ((stx:lit-exp? x) (stx:lit-exp-exp-type x))
                     ((stx:aop-exp? x) (stx:aop-exp-exp-type x)) 
                     ((stx:rop-exp? x) (stx:rop-exp-exp-type x))
                     ((stx:deref-exp? x) (stx:deref-exp-exp-type x))
                     ((stx:addr-exp? x) (stx:addr-exp-exp-type x))
                     ((stx:call-exp? x) (stx:call-exp-exp-type x)))))
    (if (equal? type 'void)
        (print-error "void type is used") #f)
    (if (equal? type (list 'pointer 'void))
        (print-error "void type is used") #f)
    (if (list? type)
        (if (and (equal? (car type) 'array) (equal? (cadr type) 'void))
            (print-error "void type is used") #f) #f)
    (if (list? type)
        (if (and (equal? (car type) 'array) (equal? (cadr type) (list 'pointer 'void)))
            (print-error "void type is used") #f) #f)
    type)) 
    
    


        
;; 文字列を受け取って意味解析
(define (sem-string str)
  (sem-port (open-input-string str)))


(define (sem-file fname)
  (sem-port (open-input-file fname)))

(define (sem-port port)
  (define (sem-port2 env port)
    (reverse (foldl (lambda (x result)
                      (let ((new-env (cond ((equal? result '()) env)
                                           ((stx:declaration? (car result)) (stx:declaration-env (car result)))
                                           ((stx:function-prototype? (car result)) (stx:function-prototype-env (car result)))
                                           ((stx:function-definition? (car result)) (stx:function-definition-env (car result))))))
                             (append (if (list? (sem-ast new-env 0 'first x result)) (sem-ast new-env 0 'first x result)
                                                          (list (sem-ast new-env 0 'first x result)))
                                                          result))) '() (par:parse-port port))))
  (sem-port2 delta port))
  















         