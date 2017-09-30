(ns pres.examples)

(def foo "(foo bar baz hello world this is a really long list)")

; code example from page 54 of BBN manual:
; https://github.com/shaunlebron/history-of-lisp-parens/blob/master/papers/656771.pdf
(def short-func (subs "
 (lambda (x y)
   (cond
     ((nul x) z)
     (t (cons
          (car x)
          (append (cdr x) y)))))
" 1))

; code example from page 38 of LISP/VM User's Guide:
; https://github.com/shaunlebron/history-of-lisp-parens/blob/master/papers/SH20-6477_LispVMUG_Jul84.pdf
(def long-func (subs "
 (lambda (ex)
   (prog (fcn args simpx)
     (cond ((atom ex) (return ex)))
     (setq fcn (car ex))
     (setq args (mapcar simplify (cdr ex)))
     (setq simpx
       (cond
         ((eq 0 (car args))
          (cond
            ((eq fcn 'plus) (cadr args))
            ((eq fcn 'times) 0)
            ((eq fcn 'minus) 0)
            ((eq fcn 'difference) (list 'minus (cadr args)))
            ((eq fcn 'quotient) 0)
            ((eq fcn 'exp) 1)
            ((eq fcn 'expt) 0)
            ((eq fcn 'sin) 0)
            ((eq fcn 'cos) 1)
            ('else (cons fcn args))))
         ((eq 0 (card args))
          (cond
            ((eq fcn 'plus) (car args))
            ((eq fcn 'times) 0)
            ((eq fcn 'difference) (car args))
            ((eq fcn 'expt) 1)
            ('else (cons fcn args))))
         ((and (eq 1 (car args)) (eq fcn 'times)) (cadr args))
         ((and (eq 1 (cadr args)) (eq fcn 'times)) (car args))
         ((and (eq 1 (car args)) (eq fcn 'expt)) 1)
         ((and (eq 1 (cadr args)) (eq fcn 'expt)) (car args))
         ((and (eq 1 (car args)) (eq fcn 'log)) 0)
         ((nump (car args))
          (cond
            ((eq fcn 'minus) (minus (car args)))
            ((and (eq fcn 'plus) (nump (cadr args)))
             (plus (car args) (cadr args)))
            ((and (eq fcn 'times) (nump (cadr args)))
             (times (car args) (cadr args)))
            ((and (eq fcn 'difference) (nump (cadr args)))
             (difference (car args) (cadr args)))
            ('else (cons fcn args))))
         ('else (cons fcn args))))
     (return simpx)))
" 1))
