#lang racket
(require (prefix-in stx: "syntax.rkt")
         ;; (prefix-in env: "env.rkt")
         ;; (prefix-in sem: "sem.rkt")
         (prefix-in sem: "sem.rkt")
         )
(provide (all-defined-out))

; ; プログラムは var-decl と fun-def のリスト
; 変数宣言
(struct var-decl (var) #:transparent)
; 関数定義
(struct fun-def (var parms body) #:transparent) ; parms は var-decl のリスト
 
; ; 文
; 変数への代入: <var> = <exp>;
(struct assign-stmt (var exp) #:transparent)
; メモリへの書き込み: *<dest> = <src>;
(struct write-stmt (dest src) #:transparent)
; メモリ参照: <dest> = *<src>;
(struct read-stmt (dest src) #:transparent)
; ラベル: <name>:
(struct label-stmt (name) #:transparent)
; 条件分岐: if(<var>){ goto <tlabel>; } else { goto <elabel>; }
(struct if-stmt (var tlabel elabel) #:transparent)
; 無条件分岐: goto <label>;
(struct goto-stmt (label) #:transparent)
; 関数呼出し: <dest> = <tgt>(<var1>, <var2>, <var3>, ...);
(struct call-stmt (dest tgt vars) #:transparent) ; vars は var のリスト
; リターン: return <var>;
(struct ret-stmt (var) #:transparent)
; 値の出力: print(<var>);
(struct print-stmt (var) #:transparent)
; 複文: {<decls> <stmts>}
(struct cmpd-stmt (decls stmts) #:transparent) ; decls は var-decl のリスト,stmts は文のリスト
 
; ; 式
; 変数参照
(struct var-exp (var) #:transparent)
; 整数即値
(struct lit-exp (val) #:transparent)
; 算術演算
(struct aop-exp (op left right) #:transparent)
; 比較演算
(struct rop-exp (op left right) #:transparent)
; アドレス取得: &<var>
(struct addr-exp (var) #:transparent)

;; Small-C の抽象構文木を中間命令に変換する関数
(define (ast->ir ast)
  (let ((var-maxid 0)
        (label-maxid 0))
    (define (fresh-symbol) ; 呼び出すたびに fresh な識別子を返す関数
      (let ([oldid var-maxid])
        (set! var-maxid (+ var-maxid 1))
        (string-append "_x" (number->string oldid))))
    (define (fresh-label) ; 呼び出すたびに fresh なラベルを返す関数
      (let ([oldid label-maxid])
        (set! label-maxid (+ label-maxid 1))
        (string-append "label" (number->string oldid)))) 
    (define (stmt->ir stmt) ; Small-C の文を表す抽象構文木を中間命令の
                            ; 文のリストに変換する関数
      (cond ((stx:cmpd-stmt? stmt) ;複文だった場合
             (cmpd-stmt (map stmt->ir (stx:cmpd-stmt-decls stmt))
                        (map stmt->ir (stx:cmpd-stmt-stmts stmt))))　;文それぞれにstmt->irを適用させて中間命令文にする
            ((stx:massign-stmt? stmt)　;stmtがメモリへの書き込み文だった場合
             (let* ((x1 (fresh-symbol))
                    (x2 (fresh-symbol)) ;変数にフレッシュな文字列を定義
                    (ir `(,@(exp->ir x1 (stx:massign-stmt-dst stmt)) ;左辺の式を評価してx1に代入
                          ,@(exp->ir x2 (stx:massign-stmt-src stmt)) ;右辺の式を評価してx2に代入
                          ,(write-stmt x1 x2)))) ;中間言語でのメモリへの書き込み分を実行
               ir)) ;irに定義された命令を実行し、メモリへの書き込み文の変換を終える
            ((stx:assign-stmt? stmt) ;stmtが変数への代入だった場合
             (let* ((x1 (fresh-symbol)) 
                    (x2 (fresh-symbol)) ;変数にフレッシュな文字列を定義
                    (ir `(,(exp->ir x1 (stx:assign-stmt-var stmt))
                          ,(exp->ir x2 (stx:assign-stmt-src stmt)) ;右辺の式を評価して値をx2に代入
                          ,(assign-stmt x1 x2)))) ;中間言語での変数への代入
               ir)) ;irに定義された命令を実行し、変数への代入文の変換を終える
            ((stx:if-stmt? stmt) ;stmtが条件分岐文だった場合
             (let* ((test-var (fresh-symbol))
                    (label1 (fresh-label))
                    (label2 (fresh-label))
                    (label3 (fresh-label)) ;変数にフレッシュな文字列を定義
                    (ir `(,(exp->ir test-var (stx:if-stmt-test stmt)) ;条件分岐文の条件判定部を評価し、それをtest-varに代入
                          ,(if-stmt test-var
                                    (goto-stmt label1)
                                    (goto-stmt label2)) ;中間言語で条件分岐を実行
                          ,(label-stmt label1) ;ラベル１。式が真だった場合にここへ飛ぶ
                          ,(stmt->ir (stx:if-stmt-tbody stmt)) ;式が真だった場合の文を再帰的に実行
                          ,(goto-stmt label3) ;ラベル３へ飛ぶ。つまり条件判定文を終える
                          ,(label-stmt label2) ;ラベル２。式がだった場合ここへ飛ぶ
                          ,(stmt->ir (stx:if-stmt-ebody stmt)) ;式が偽だった場合の文を再帰的に実行
                          ,(label-stmt label3) ;ラベル３。これ以降実行する文はないので、条件判定文を終了するためのラベル
                          )))
               ir)) ;irに定義された命令を実行し、条件分岐文の変換を終える
            ((stx:while-stmt? stmt) ;stmtが繰り返し文だった場合
             (let* ((test-var (fresh-symbol)) 
                    (label1 (fresh-label))
                    (label2 (fresh-label))
                    (label3 (fresh-label)) ;変数にフレッシュな文字列を定義
                    (test-ir (exp->ir test-var (stx:while-stmt-test stmt))) ;繰り返し文の条件判定部を評価し、それをtest-varに代入
                    (ir `(,(label-stmt label1) ;ラベル1。条件判定部が真だった場合ここに飛び、命令を繰り返す
                          ,test-ir ;条件判定部を評価する
                          ,(if-stmt test-var
                                    (goto-stmt label2)
                                    (goto-stmt label3)) ;中間言語で繰り返し文を実行
                          ,(label-stmt label2) ;ラベル２。条件判定部が真だった場合ここに飛ぶ
                          ,(stmt->ir (stx:while-stmt-body stmt)) ;条件判定部が真だった場合の文を再帰的に実行
                          ,(goto-stmt label1) ;繰り返し文のため、ラベル１に飛んで再び条件判定部を評価する
                          ,(label-stmt label3)))) ;ラベル３。条件判定部が偽になり、これ以上繰り返さない場合ここに飛ぶ
               ir)) ;irに定義された命令を実行し、繰り返し文の変換を終える
            ((stx:print-stmt? stmt) ;stmtが値の出力文だった場合
             (let* ((t (fresh-symbol)) ;変数にフレッシュな文字列を定義
                    (ir `(,@(exp->ir t (stx:print-stmt-exp stmt)) ;出力するための式を評価し、tに代入
                          ,(print-stmt t)))) ;tを出力する
               ir)) ;irに定義された命令を実行し、値の出力文の変換を終える
            ((stx:return-stmt? stmt) 
             (let* ((t (fresh-symbol)) ;変数にフレッシュな文字列を定義
                    (ir `(,(exp->ir t (stx:return-stmt-exp stmt)) 
                          ,(ret-stmt t)))) 
               ir))
            ((stx:declaration? stmt)
             (var-decl (stx:declaration-declaration-list stmt)))
            ((stx:function-definition? stmt)
             (fun-def (stx:function-definition-function-name stmt) (map stmt->ir (stx:function-definition-function-declarator stmt))
                       (stmt->ir (stx:function-definition-compound-statement stmt))))
            ((stx:call-exp? stmt)
             (let ((args
                    (if (list? (stx:call-exp-args stmt))
                        (append-map (lambda(x)
                               (let ((x1 (fresh-symbol)))
                                 (exp->ir x1 x))) (stx:call-exp-args stmt))
                        (let ((x1 (fresh-symbol)))
                          (exp->ir x1 (stx:call-exp-args stmt)))))
                   (call (fresh-symbol)))
               (if (equal? (sem:decl-name (stx:call-exp-tgt stmt)) 'print)
                   (print-stmt args)
                   (call-stmt call (stx:call-exp-tgt stmt) args))))
            ))

    (define (exp->ir dest exp) ; Small-C 式の抽象構文木を中間命令の文の
                               ; リストに変換する関数。ここには人為的
                               ; にはバグを仕込んでいない。（ただしコ
                               ; メントは付けること）
      (cond ((stx:var-exp? exp) ;expが変数だった場合
             (assign-stmt dest (var-exp (stx:var-exp-tgt exp)))) ;変数をそのまま変数destに代入
            ((stx:lit-exp? exp) ;expが整数即値だった場合
             (assign-stmt dest (lit-exp (stx:lit-exp-val exp)))) ;評価して整数即値にしたものを変数destに代入
            ((stx:aop-exp? exp) ;expが算術演算だった場合
             (let* ((op (stx:aop-exp-op exp)) ;算術演算子をopに定義
                    (left-exp (stx:aop-exp-left exp)) ;左の式をleft-expに定義
                    (right-exp (stx:aop-exp-right exp)) ;右の式をright-expに定義
                    (left-var (fresh-symbol)) 
                    (right-var (fresh-symbol)) ;変数にフレッシュな文字列を定義
                    (left-ir (exp->ir left-var left-exp)) ;left-expを評価して値にし、それをleft-varに代入する、というleft-irを定義
                    (right-ir (exp->ir right-var right-exp)) ;right-expを評価して値にし、それをright-varに代入する、というright-irを定義
                    (ir `(,left-ir ;left-irを実行
                          ,right-ir ;right-irを実行
                          ,(assign-stmt 
                            dest
                            (aop-exp op left-var right-var))))) ;変数destへ算術演算の結果を代入
               ir)) ;irに定義された命令を実行し、expが算術演算だった場合の変換を終える
            ((stx:rop-exp? exp) ;expが比較演算だった場合
             (let* ((op (stx:rop-exp-op exp)) ;比較演算子をopに定義
                    (left-exp (stx:rop-exp-left exp)) ;左の式をleft-expに定義
                    (right-exp (stx:rop-exp-right exp)) ;右の式をright-expに定義
                    (left-var (fresh-symbol)) 
                    (right-var (fresh-symbol)) ;変数にフレッシュな文字列を定義
                    (left-ir (exp->ir left-var left-exp)) ;left-expを評価して値にし、それをleft-varに代入する、というleft-irを定義
                    (right-ir (exp->ir right-var right-exp)) ;right-expを評価して値にし、それをright-varに代入する、というright-irを定義
                    (ir `(,left-ir ;left-irを実行
                          ,right-ir ;right-irを実行
                          ,(assign-stmt
                            dest
                            (rop-exp op left-var right-var))))) ;変数destへ比較演算の結果を代入
               ir)) ;irに定義された命令を実行し、expが比較演算だった場合の変換を終える
            ((stx:deref-exp? exp) 
             (let* ((arg (stx:deref-exp-arg exp))
                    (arg-var (fresh-symbol))
                    (ir `(,(exp->ir arg-var arg)
                          ,(read-stmt dest arg-var))))
               ir))
            ((stx:addr-exp? exp)
             (let* ((arg (stx:addr-exp-var exp))
                    (arg-var (fresh-symbol))
                    (ir `(,(exp->ir arg-var arg)
                          ,(addr-exp arg-var))))
               ir))
            ((stx:call-exp? exp)
             (let ((args
                    (if (list? (stx:call-exp-args exp))
                        (append-map (lambda(x)
                               (let ((x1 (fresh-symbol)))
                                 (exp->ir x1 x))) (stx:call-exp-args exp))
                        (let ((x1 (fresh-symbol)))
                          (exp->ir x1 (stx:call-exp-args exp))))))
               (call-stmt dest (stx:call-exp-tgt exp) args)))))
    (map stmt->ir (if (list? ast) ast (list ast)))))

;; 以下はデータを文字列に変換する関数である。デバッグに有用である。こ
;; こには人為的にはバグを仕込んでいない。コメントを付ける必要はない。

(define (stmt->string stmt)
  (let ((astmt
         (cond
          ((cmpd-stmt? stmt)
           (let ((astmts (foldr
                          (lambda (a b) (string-append a b))
                          ""
                          (map (lambda (stmt) (stmt->string stmt))
                               (cmpd-stmt-stmts stmt)))))
             (substring astmts 0 (- (string-length astmts) 1))))
          ((write-stmt? stmt)
           (let ((aexp (exp->string (write-stmt-src stmt)))
                 (avar (~a (write-stmt-dest stmt))))
             (string-append "*" avar " = " aexp ";")))
          ((assign-stmt? stmt)
           (let ((aexp (exp->string (assign-stmt-exp stmt)))
                 (avar (~a (assign-stmt-var stmt))))
             (string-append avar " = " aexp ";")))
          ((read-stmt? stmt)
           (let ((adst (~a (read-stmt-dest stmt)))
                 (asrc (~a (read-stmt-src stmt))))
             (string-append adst " = *" asrc ";")))
          ((label-stmt? stmt)
           (string-append (~a (label-stmt-name stmt)) ":"))
          ((if-stmt? stmt)
           (let ((avar (exp->string (if-stmt-var stmt)))
                 (atlabel (goto-stmt-label (if-stmt-tlabel stmt)))
                 (aelabel (goto-stmt-label (if-stmt-elabel stmt))))
             (string-append "if(" avar ") goto " atlabel " else goto " aelabel ";")))
          ((goto-stmt? stmt)
           (string-append "goto " (goto-stmt-label stmt) ";"))
          ((print-stmt? stmt)
           (string-append "print(" (exp->string (print-stmt-var stmt)) ");"))
          )))
        (string-append astmt "\n")))

(define (exp->string exp)
  (cond ((var-exp? exp) (~a (var-exp-var exp)))
        ((lit-exp? exp) (~a (lit-exp-val exp)))
        ((aop-exp? exp) (string-append (~a (aop-exp-left exp))
                                       " "
                                       (~a (aop-exp-op exp))
                                       " "
                                       (~a (aop-exp-right exp))
                                       ";"))
        ((rop-exp? exp) (string-append (~a (rop-exp-left exp))
                                       " "
                                       (~a (rop-exp-op exp))
                                       " "
                                       (~a (rop-exp-right exp))
                                       ";"))
        ))

(define (ir->string ir)
  (foldr
   (lambda (a b) (string-append a b))
   ""
   (map (lambda (stmt) (stmt->string stmt)) (sem:sem-string ir))))

(define (ir-string str)
  (ast->ir (sem:sem-string str)))
  
(define (ir-file fname)
  (ir-port (open-input-file fname)))

(define (ir-port port)
  (ast->ir (sem:sem-port port)))
