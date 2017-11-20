(define (fibo n)
    (if(= n 0)
        0
        (if (<= n 2)
            1
            (+ (fibo(- n 1)) (fibo (- n 2))))))
    

(define (expo n m)
    (if(= m 0)
        1
        (* n (expo n (- m 1)))))
    
(define (minimo lista)
    (if(= (length lista) 1)
          (car lista)
          (if(<  (car lista) (minimo (cdr lista)))
                (car lista)
                (minimo (cdr lista)))))

(define (inserta x lista)
    (if(null? lista)
        (cons x '())
        (if(> (car lista) x)
            (cons x lista)
            (cons (car lista) (inserta x (cdr lista)))
            )))

(define (concatena lista1 lista2)
    (if (null? lista1)
        lista2
        (cons (car lista1) (concatena (cdr lista1) lista2))))

    
(define (invierte lista)
    (if(null? lista) 
        '()
        (concatena (invierte (cdr lista)) (list(car lista) )) ))
    
(define (repetidos lista)
    (cond
        ((null? lista) lista)
        ((esta? (car lista) (cdr lista))(repetidos(cdr lista)))
        (else(concatena(list (car lista))(repetidos (cdr lista))))))
    
(define (esta? x lista)
    (if(null? lista)
    #f
    (if(eq? x (car lista))
    #t
    (esta? x (cdr lista)))))

(define (elimina x lista)
    (if (null? lista)
        '()
        (if (equal? x (car lista))
        (elimina x (cdr lista))
        (cons (car lista)(elimina x (cdr lista))))))