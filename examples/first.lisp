(define {factorial x} {
    (cond {(> x 1) (* x (factorial (- x 1)))}
          {else 1})
})

(define {if x y e} {
    (cond {(eval x) (eval y)}
          {else (eval e)})
})

(print (cons {1} (cons {2} (cons {3} nil))))