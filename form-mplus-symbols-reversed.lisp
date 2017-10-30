;; form-mplus-symbols-reversed.lisp -- reverse order of terms in "+" expressions
;; Usage:
;; load ("form-mplus-symbols-reversed.lisp") $
;; reverse_symbols_order : true $
;; a + b + z + b^2 + b^3;
;; => a+b^3+b^2+b+z
;; tex (a + b + z + b^2 + b^3);
;; => $$a+b^3+b^2+b+z$$

;; First part of this file is adapted from similarly-named functions in Maxima src/simp.lisp
;; which is released under terms of the GNU General Public License.
;; As a derivative work, this is therefore released under the same license.

(defun great-symbols-reversed (x y)
  (cond ((atom x)
	 (cond ((atom y)
		(cond ((numberp x)
		       (cond ((numberp y)
			      (setq y (- x y))
			      (cond ((zerop y) (floatp x)) (t (plusp y))))))
		      ((constant x)
		       (cond ((constant y) (alphalessp x y)) (t (numberp y))))
		      ((mget x '$scalar)
		       (cond ((mget y '$scalar) (alphalessp x y))
		             (t (maxima-constantp y))))
		      ((mget x '$mainvar)
		       (cond ((mget y '$mainvar) (alphalessp x y)) (t t)))
		      (t (or (maxima-constantp y) (mget y '$scalar)
			     (and (not (mget y '$mainvar)) (not (null (alphalessp x y))))))))
	       (t (not (ordfna-symbols-reversed y x)))))
	((atom y) (ordfna-symbols-reversed x y))
	((eq (caar x) 'rat)
	 (cond ((eq (caar y) 'rat)
		(> (* (caddr y) (cadr x)) (* (caddr x) (cadr y))))))
	((eq (caar y) 'rat))
	((member (caar x) '(mbox mlabox) :test #'eq) (great-symbols-reversed (cadr x) y))
	((member (caar y) '(mbox mlabox) :test #'eq) (great-symbols-reversed x (cadr y)))
	((or (member (caar x) '(mtimes mplus mexpt %del) :test #'eq)
	     (member (caar y) '(mtimes mplus mexpt %del) :test #'eq))
	 (ordfn-symbols-reversed x y))
	((and (eq (caar x) 'bigfloat) (eq (caar y) 'bigfloat)) (mgrp x y))
	((or (eq (caar x) 'mrat) (eq (caar y) 'mrat))
	 (error "GREAT: internal error: unexpected MRAT argument"))
	(t (do ((x1 (margs x) (cdr x1)) (y1 (margs y) (cdr y1))) (())
	     (cond ((null x1)
		    (return (cond (y1 nil)
				  ((not (alike1 (mop x) (mop y)))
				   (great-symbols-reversed (mop x) (mop y)))
				  ((member 'array (cdar x) :test #'eq) t))))
		   ((null y1) (return t))
		   ((not (alike1 (car x1) (car y1)))
		    (return (great-symbols-reversed (car x1) (car y1)))))))))

(defun ordfna-symbols-reversed (e a)			; A is an atom
  (cond ((numberp a)
	 (or (not (eq (caar e) 'rat))
	     (> (cadr e) (* (caddr e) a))))
        ((and (constant a)
              (not (member (caar e) '(mplus mtimes mexpt) :test #'eq)))
	 (not (member (caar e) '(rat bigfloat) :test #'eq)))
	((eq (caar e) 'mrat)) ;; all MRATs succeed all atoms
	((null (margs e)) nil)
	((eq (caar e) 'mexpt)
	 (cond ((and (maxima-constantp (cadr e))
		     (or (not (constant a)) (not (maxima-constantp (caddr e)))))
		(or (not (free (caddr e) a)) (great-symbols-reversed (caddr e) a)))
	       ((eq (cadr e) a) (great-symbols-reversed (caddr e) 1))
	       (t (great-symbols-reversed (cadr e) a))))
	((member (caar e) '(mplus mtimes) :test #'eq)
	 (let ((u (car (last e))))
	   (cond ((eq u a) (not (ordhack-symbols-reversed e))) (t (great-symbols-reversed u a)))))
	((eq (caar e) '%del))
	((prog2 (setq e (car (margs e)))	; use first arg of e
	     (and (not (atom e)) (member (caar e) '(mplus mtimes) :test #'eq)))
	 (let ((u (car (last e))))		; and compare using 
	   (cond ((eq u a) (not (ordhack-symbols-reversed e)))	; same procedure as above
		 (t (great-symbols-reversed u a)))))
	((eq e a))
	(t (great-symbols-reversed e a))))

(defun ordfn-symbols-reversed (x y)
  (let ((cx (caar x)) (cy (caar y)))
    (cond ((eq cx '%del) (if (eq cy '%del) (great-symbols-reversed (cadr x) (cadr y)) t))
	  ((eq cy '%del) nil)
	  ((or (eq cx 'mtimes) (eq cy 'mtimes))
	   (ordlist-symbols-reversed (factor-list x) (factor-list y) 'mtimes 'mtimes))
	  ((or (eq cx 'mplus) (eq cy 'mplus))
	   (ordlist-symbols-reversed (term-list x) (term-list y) 'mplus 'mplus))
	  ((eq cx 'mexpt) (ordmexpt-symbols-reversed x y))
	  ((eq cy 'mexpt) (not (ordmexpt-symbols-reversed y x))))))

(defun ordhack-symbols-reversed (x)
  (if (and (cddr x) (null (cdddr x)))
      (great-symbols-reversed (if (eq (caar x) 'mplus) 0 1) (cadr x))))

(defun ordmexpt-symbols-reversed (x y)
  (cond ((eq (caar y) 'mexpt)
	 (cond ((alike1 (cadr x) (cadr y)) (great-symbols-reversed (caddr x) (caddr y)))
	       ((maxima-constantp (cadr x))
		(if (maxima-constantp (cadr y))
		    (if (or (alike1 (caddr x) (caddr y))
			    (and (mnump (caddr x)) (mnump (caddr y))))
			(great-symbols-reversed (cadr x) (cadr y))
			(great-symbols-reversed (caddr x) (caddr y)))
		    (great-symbols-reversed x (cadr y))))
	       ((maxima-constantp (cadr y)) (great-symbols-reversed (cadr x) y))
	       ((mnump (caddr x))
		(great-symbols-reversed (cadr x) (if (mnump (caddr y)) (cadr y) y)))
	       ((mnump (caddr y)) (great-symbols-reversed x (cadr y)))
	       (t (let ((x1 (simpln1 x)) (y1 (simpln1 y)))
		    (if (alike1 x1 y1) (great-symbols-reversed (cadr x) (cadr y))
			(great-symbols-reversed x1 y1))))))
	((alike1 (cadr x) y) (great-symbols-reversed (caddr x) 1))
	((mnump (caddr x)) (great-symbols-reversed (cadr x) y))
	(t (great-symbols-reversed (simpln1 x) (simpln (list '(%log) y) 1 t)))))

(defun ordlist-symbols-reversed (a b cx cy)
  (prog (l1 l2 c d)
     (setq l1 (length a) l2 (length b))
     loop (cond ((= l1 0)
		 (return (cond ((= l2 0) (eq cx 'mplus))
			       ((and (eq cx cy) (= l2 1))
				(great-symbols-reversed (cond ((eq cx 'mplus) 0) (t 1)) (car b))))))
		((= l2 0) (return (not (ordlist-symbols-reversed b a cy cx)))))
     (setq c (nthelem l1 a) d (nthelem l2 b))
     (cond ((not (alike1 c d)) (return (great-symbols-reversed c d))))
     (setq l1 (1- l1) l2 (1- l2))
     (go loop)))

;; Remainder of this file, copyright 2017 by Robert Dodier.
;; I release this work under terms of the GNU General Public License.

(defmvar $reverse_symbols_order nil)

(defun form-mplus-symbols-reversed-maybe (form)
  (let
    ((op (car form))
     (args (if $reverse_symbols_order (stable-sort (cdr form) #'(lambda (a b) (great-symbols-reversed b a))) (cdr form))))
    (form-mplus (cons op args))))

;; Assigning FORM-MPLUS-SYMBOLS-REVERSED as the formatter for MPLUS
;; mostly doesn't cause trouble, but it does cause a number of test case
;; failures due to the unexpected ordering of terms in MPLUS expressions.
;; (Remember, although the formatter is not called in simplification,
;; it is called for any kind of part-hacking operation.)
(setf (get 'mplus 'formatter) #'form-mplus-symbols-reversed-maybe)

