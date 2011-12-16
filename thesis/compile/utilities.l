;; utilities

;tailrecursiveok
(defun any (n ls)
  (labels 
      ((do-any (n ls ac)
        (if (or (zerop n) (null ls)) 
            ac
          (do-any (- n 1) ls (cons (any-one ls) ac)))))
    (do-any n ls nil)))

(defun any-one (ls)
  (cond ((null ls) 
         nil) 
        (t 
         (nth (random-lo-hi 0 (1- (length ls))) 
              ls))))

;; from a list of structures create a list of evaluated structures 
(defun evaluate-all (ls)
  (mapcar 'eval ls))

(defun final (ls)
  (car (last ls)))

(defun pick (n ls)
  (let ((results nil) (rand 0))
    (do ((o n (- o 1)))
        ((or (zerop n) (null ls)))
      (setf rand (random-lo-hi 0 (1- (length ls))))
      (push (nth rand ls) results)
      (setf ls (remove-nth rand ls))
      (setf n (- n 1)))
    (values results ls)))
    
(defun random-lo-hi (lo hi)
  (+ lo (random (- (+ 1 hi) lo))))

(defun remove-nth (n thlist)
  (cond ((> n (length thlist))
         nil)
        (t
         (let (new-list)
           (repeat-fun  #'(lambda () (push (pop thlist) 
                                           new-list))
                        n)
           (append (reverse new-list) (cdr thlist))))))

;; remove final
(defun remove-final (l)
  (reverse (cdr (reverse l))))

(defun string-list-to-spaced-string (strls)
  (cond ((null (cadr strls)) (car strls))
        (t (concatenate 'string (car strls) 
                        (concatenate 'string " " 
                                     (string-list-to-spaced-string (cdr strls)))))))

;; print a number of strings and a newline character
(defun write-string-terpri (&rest strls) 
  (write-string (string-list-to-string strls)) (terpri) strls)             

;tailrecursiveok
(defun string-list-to-string (strls)
  (labels 
      ((do-string-list-to-string (strls ac)
         (cond ((null (cadr strls)) 
                (concatenate 
                 'string 
                 ac
                 (car strls)))
               (t
                (do-string-list-to-string (cdr strls)
                                      (concatenate 
                                       'string 
                                       ac 
                                       (car strls)))))))
    (do-string-list-to-string strls "")))

(defun elimenate-redundancy (l)
  (dolist (k l) (setf l (cons k (stripped-of-every-instance l k)))) (reverse l))
                                   
(defun stripped-of-every-instance (l e &aux newlist)
  (mapcar #'(lambda (x) (cond ((not (equal x e))
                               (push x newlist))))
          l) 
  (reverse newlist))

(defun group-list (n l)
  (let (chunkedlist)
    (dotimes (i (/ (length l) n))
      (setf chunkedlist (cons (n-car n l) chunkedlist))
      (setf l (n-cdr n l))) 
    (reverse chunkedlist)))

;tailrecursiveok
(defun n-car (n l)
  (labels 
      ((do-n-car 
        (n l ac) 
        (cond ((zerop n)
               ac)
              (t
               (do-n-car (- n 1) (cdr l) (endcons (car l) ac))))))
    (do-n-car n l nil)))
    
;tailrecursiveok
(defun n-cdr (n l)
  (cond ((zerop n)
         l)
        (t
         (n-cdr (- n 1) (cdr l)))))

;tailrecursiveok
(defun segment (d l)
  (labels 
      ((do-segment 
        (d l ac)
        (cond ((null l)
               ac)
              (t
               (do-segment d (div-cdr d l) 
                           (endcons (div-car d l) ac))))))
    (do-segment d l nil)))

;tailrecursiveok
(defun div-car (d l) 
  (labels 
      ((do-div-car 
        (d l ac)
        (cond ((null l)
               ac)
              ((equal (car l) d)
               ac)
              (t
               (do-div-car d (cdr l) (endcons (car l) ac))))))
    (do-div-car d l nil)))

;tailrecursive
(defun div-cdr (d l) 
  (cond ((null l)
         nil)
        ((equal (car l) d)
         (cdr l))
        (t
         (div-cdr d (cdr l)))))

(defun repeat-fun (fun n)
  (let (x)
    (do ((o 0 (+ o 1)))
	((equal o n))
        ( push (funcall fun) x )) x))

(defun percent-of (percent quantity)
  (* (/ percent 100) quantity))

(defun cdr-n (n list)
  (cond ((<= n 1) (cdr list))
        (t (cdr-n (1- n) (cdr list)))))

(defun car-n (n list)
  (cond ((null list) nil)
        ((zerop n) nil)
        (t (cons (car list) 
                 (car-n (1- n) (cdr list))))))

(defun make-list-of-lines (file &aux thisline linelist)
  (let ((str (open file)))
    (do ((j 0 (+ j 1))) 
        ((equal thisline "STOP"))   
      (push (setf thisline (read-line str)) linelist)) 
    (pop linelist) (close str)
    linelist))

(defun cat (strings)
  (eval (append '(concatenate 'string) strings)))

(defun add-progressively (ls)
  (cond ((null ls)
         nil)
        ((onep (length ls))
         ls)
        (t
         (cons (car ls)
               (add-progressively
                (cons (+ (car ls) 
                         (car (cdr ls)))    
                      (cdr (cdr ls))))))))

; setting the optional place value will add extra zeroes
(defun base-ten-number-to-binary-list (n &optional place)
  (cond (place
         (do-base-ten-number-to-binary-list place n))
        (t
         (do-base-ten-number-to-binary-list (find-default-place-value n) n))))

(defun do-base-ten-number-to-binary-list (place n)
  (cond ((zerop place) nil)
        ((and (< n (expt 2 place))
              (>= n (expt 2 (- place 1))))
         (cons 1 (do-base-ten-number-to-binary-list (- place 1) (- n (expt 2 (- place 1))))))
        (t 
         (cons 0 (do-base-ten-number-to-binary-list (- place 1) n)))))

(defun find-default-place-value (n)
  (do-find-default-place-value n 0))

;tailrecursiveok
(defun do-find-default-place-value (n cnt)
  (if (> (expt 2 cnt) n)
      cnt
    (do-find-default-place-value n (+ cnt 1))))

(defun num-to-string (n)
  (cond ((< n 0) 
         (setf n (- 0 n))
         (concatenate 'string "-" (cat (reverse (do-num-to-string n)))))
        (t (cat (reverse (do-num-to-string n))))))

(defun do-num-to-string (n)
  (cond ((< n 10) (list (string (+ 48 n))))
        (t (cons (string (+ 48 (- n (* 10 (floor (/ n 10))))))
                 (do-num-to-string (floor (/ n 10)))))))

;; return the elements in list als 
;; of all the indices specified in nls
(defun nths (nls als) 
  (cond ((null nls) nil) 
        (t (cons (nth (car nls) als) 
                 (nths (cdr nls) als)))))

(defun average (l)
  (float (/ (summation l) (length l))))

;; note: replace with tail recursion
(defun summation (integers)
  (let ((acc 0))
    (dotimes (i (length integers))
      (setf acc (+ acc (car integers)))
      (pop integers)) acc))

(defun onep (n)
  (eq n 1))
(defun twop (n) 
  (eq n 2))
(defun threep (n) 
  (eq n 3))
(defun fourp (n) 
  (eq n 4))
(defun fivep (n) 
  (eq n 5))
(defun sixp (n) 
  (eq n 6))

(defun construct-list (n s)
  (let (l)
    (dotimes (i n)
      (push s l)) l))

(defun random-val ()
  (if (zerop (random 2)) nil t))

(defun index-of-greatest (l &optional index windex winval)
  (if (null index) (setf index 1))
  (if (null windex) (setf windex 1))
  (if (null winval) (setf winval 0))
  (cond ((null l) windex)
        ((> (car l) winval) (index-of-greatest (cdr l) (1+ index) index (car l)))
        (t (index-of-greatest (cdr l) (1+ index) windex winval))))

(defun insert-items (inner outer where)
  (cond ((zerop where)
         (append inner outer))
        (t 
         (cons (car outer) 
               (insert-items inner (cdr outer) (- where 1))))))

(defun map-except-last-car (fun list)
  (cond 
   ((null (car list))
    nil) 
   ((null (car (cdr list)))
    list)
   (t
    (cons (apply fun (list (car list)))
          (map-except-last-car fun (cdr list))))))

(defun endcons (at li)
  (append li (list at)))

(proclaim '(inline 
             evaluate-all
             final
             random-lo-hi
             find-default-place-value
             write-string-terpri
             percent-of
             cat
             remove-final
             average
             onep twop 
             threep fourp 
             fivep sixp
             random-val
             endcons))