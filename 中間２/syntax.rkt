#lang racket
;; (require rackunit)
(provide (all-defined-out))

;; declaration/definition
;; (struct parm-decl (name ty pos)                #:transparent)
;; (struct fun-decl  (name ret-ty parm-tys pos)   #:transparent)
;; (struct fun-def   (name ret-ty parms body pos) #:transparent)

;; statement

;; メモリへの書き込み: *<exp> = <exp>;
(struct massign-stmt (exp-type dst src pos)              #:transparent)
;; 変数への代入: <var> = <exp>;
(struct assign-stmt  (exp-type var src pos)              #:transparent)
;; 条件分岐: if(<exp>) <cmpd-stmt> else <cmpd-stmt>
(struct if-stmt      (test tbody ebody pos)     #:transparent)
;; 繰り返し: while(<exp>) <cmpd-stmt>
(struct while-stmt   (test body pos)            #:transparent)
;; 値の出力: print(<exp>);
(struct print-stmt   (exp pos)                  #:transparent)

;; 複文: {<stmt>;...<stmt>;} 
(struct cmpd-stmt    (decls stmts pos)                #:transparent)

;; expression
;; 変数: <var>
(struct var-exp    (exp-type tgt pos)           #:transparent)
;; 偽を表す真偽値: true
(struct true-exp   (pos)               #:transparent)
;; 偽を表す真偽値: false
(struct false-exp  (pos)               #:transparent)
;; 整数即値: <num>
(struct lit-exp    (exp-type val pos)           #:transparent)
;; 算術演算: <left-exp> <op> <right-exp>
(struct aop-exp    (exp-type op left right pos) #:transparent)
;; 比較演算: <left-exp> <op> <right-exp>
(struct rop-exp    (exp-type op left right pos) #:transparent)
;; メモリ参照: *<exp>
(struct deref-exp   (exp-type arg pos)           #:transparent)
;; アドレスを取る: &<var>
(struct addr-exp  (exp-type var pos)           #:transparent)
;;関数呼び出し
(struct call-exp   (exp-type tgt args pos)      #:transparent)

;;変数宣言
(struct declaration (declaration-list pos env)  #:transparent)
;;関数プロトタイプ
(struct function-prototype (type-specifire  function-name function-declarator pos env) #:transparent)
;;関数定義
(struct function-definition (type-specifire  function-name function-declarator compound-statement pos env) #:transparent)
;;返り値
(struct return-stmt (exp-type exp pos) #:transparent)
;;パラメータ宣言
(struct parm-exp (tgt pos))
