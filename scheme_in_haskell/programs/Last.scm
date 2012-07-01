(defun null (x) (eq x '()))

(defun last (l)
  (cond ((null (cdr l)) (car l))
	(#t (last (cdr l)))))

(last '(1 2 3))