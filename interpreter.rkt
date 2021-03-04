(define haha
    (lambda (a)
        0))


(define M_boolean
  (lambda (condition state)
    (cond
      ; cond 1: condition = error
      ((null? condition) #t)
      ((and (list? (car condition)) (eq? (cdr condition) '&&)) (and (M_boolean(car condition) state) (M_boolean(cdr (cdr condition)) state)))          
      ((and (list? (car condition)) (eq? (cdr condition) '||)) (or (M_boolean(car condition) state) (M_boolean(cdr (cdr condition)) state)))     
      ((and  (eq? (car condition) '!) (list? (cdr condition))) (cons [(not (M_boolean(cdr condition) state))] [(M_boolean(cdr (cdr condition)) state)]))
     
      ((eq? (car condition) '<) (< (M_value(cdr condition) state) (M_value(cdr(cdr condition)) state)))
      ((eq? (car condition) '>) (> (M_value(cdr condition) state) (M_value(cdr(cdr condition)) state)))
      ((eq? (car condition) '=) (= (M_value(cdr condition) state) (M_value(cdr(cdr condition)) state))))))