(defun null (x) (eq x '()))

(defun subst (x y l)
  (cond ((null l) '())
	((eq x (car l)) (cons y (subst x y (cdr l))))
	(#t (cons (car l) (subst x y (cdr l))))))

(subst 'a 'b '(c a d a e))