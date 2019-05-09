(defun append. (l1 l2) (cond ((eq l1 '()) l2) ('t (cons (car l1) (append. (cdr l1) l2)))))

(defun and. (x y) (cond ((eq x '()) '()) ((eq y '()) '()) ('t 't)))

(defun pair. (l1 l2) (cond ((and (eq l1 '()) (eq l2 '())) '()) ('t (cons (cons (car l1) (cons (car l2) '())) (pair. (cdr l1) (cdr l2))))))

(defun assoc. (a l) (cond ((eq l '()) '()) ((eq (caar l) a) (cadar l)) ('t (assoc. a (cdr l)))))

(defun subst. (o n e) (cond ((atom e) (cond ((eq e o) n) ('t e))) ('t (cons (subst. o n (car e)) (subst. o n (cdr e))))))

(defun eval. (e)
       (cond
        ((eq e '()) '())
        ((atom e) e)
        ((atom (car e))
         (cond
          ((eq (car e) 'quote) (cadr e))
          ((eq (car e) 'atom) (atom (eval. (cadr e))))
          ((eq (car e) 'eq) (eq (eval. (cadr e)) (eval. (caddr e))))
          ((eq (car e) 'car) (car (eval. (cadr e))))
          ((eq (car e) 'cdr) (cdr (eval. (cadr e))))
          ((eq (car e) 'cons) (cons (eval. (cadr e)) (eval. (caddr e))))
          ((eq (car e) 'cond) (evalcond. (cdr e)))
         )
        )
        ((eq (caar e) 'lambda) (eval. (evalsubst. (pair. (cadar e) (evallist. (cdr e))) (caddar e))))
       )
       )

(defun evalcond. (c)
       (cond
        ((eq c '()) '())
        ((eq (eval. (caar c)) 't) (eval. (cadar c)))
        ('t (evalcond. (cdr c)))
       )
       )

(defun evalsubst. (m e)
       (cond
        ((eq m '()) e)
        ('t (evalsubst. (cdr m) (subst. (caar m) (cadar m) e)))
       )
       )

(defun evallist. (l)
       (cond
        ((eq l '()) l)
        ('t (cons (eval. (car l)) (evallist. (cdr l))))
       )
       )
