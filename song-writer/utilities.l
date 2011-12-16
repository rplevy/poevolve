;; utilities

;; return members of l1 that are not also in l2
(defun not-in (l1 l2)
  (cond ((null l1) nil)
        ((member (car l1) l2) (not-in (cdr l1) l2))
        (t (cons (car l1) (not-in (cdr l1) l2)))))

;; random number, possibilities are 1 thru n (not zero thru n-1)
(defun random-one (n)
   (+ 1 (random n)))

(defun any-one (ls)
  (cond ((null ls) nil) (t (do-any-one ls))))

(defun do-any-one (ls)
  (nth (random-lo-hi 0 (1- (length ls))) ls))

(defun any (n ls)
  (cond ((null ls) nil) 
        (t 
         (repeat-fun #'(lambda () (do-any-one ls)) n))))

(defun pick (n ls)
  (let ((results nil) (rand 0))
    (do ((o n (- o 1)))
        ((or (zerop n) (null ls)))
      (setf rand (random-lo-hi 0 (1- (length ls))))
      (push (nth rand ls) results)
      (setf ls (remove-nth rand ls))
      (setf n (- n 1)))
    (values results ls)))
    
(defun rotate (l)
  (let (tmp)
    (setf tmp (pop l))
    (reverse (cons tmp (reverse l)))))    

(defun remove-nth (n thlist)
  (cond ((> n (length thlist))
         nil)
        (t
         (let (new-list)
           (repeat-fun  #'(lambda () (push (pop thlist) 
                                           new-list))
                        n)
           (append (reverse new-list) (cdr thlist))))))

(defun endcons (b ls)
  (append ls (list b)))
             
;; true if one
(defun onep (n) 
  (zerop (- n 1)))

;; final
(defun final (ls)
  (car (last ls)))

;; from a list of structures create a list of evaluated structures 
(defun evaluate-all (ls)
  (mapcar 'eval ls))

;; evaluate a list of constructs
(defun evalist (ls)
  (cond ((null ls) nil)
        (t (eval (car ls))
           (evalist (cdr ls)))))

(defun write-strings-terpri (l) 
  (mapcar #'(lambda (arg) (write-string arg) (terpri)) l))

;; print a number of strings and a newline character
(defun write-string-terpri (&rest strls) 
  (write-string (string-list-to-string strls)) (terpri) strls)

;; convert a list of strings into one long string
(defun string-list-to-string (strls)
  (cond ((null (cadr strls)) (car strls))
        (t (concatenate 'string (car strls) (string-list-to-string (cdr strls)))))) 

;; above w/ spaces between words
(defun string-list-to-spaced-string (strls)
  (cond ((null (cadr strls)) (car strls))
        (t (concatenate 'string (car strls) 
                        (concatenate 'string " " 
                                     (string-list-to-spaced-string (cdr strls)))))))
  
;; atom at location n in list ls
(defun atom-n (n ls)
  (nth (1- n) ls))

;; alter atom at location n in list ls with replace
(defun replace-n (n ls replace)
  (cond ((null ls)
         nil)
        ((onep n)
         (cons replace (cdr ls)))
        (t
         (cons (car ls) (replace-n (- n 1) (cdr ls) replace)))))

(defun toggle (a)
  (cond ((null a) t)
        (t nil)))

(defun random-lo-hi (lo hi)
  (+ lo (random (- (+ 1 hi) lo))))

;; replace last element
(defun replace-last-element (l x)
  (cond ((null (car l))
         nil)
        ((null (car (cdr l)))
         (cons x nil))
        (t 
         (cons (car l) (replace-last-element (cdr l) x)))))

;; remove final
(defun remove-final (l)
  (reverse (cdr (reverse l))))

(defun summation (integers)
  (apply '+ integers))

(defun in-range (range n)
  (and (>= n (car range))
       (<= n (car (cdr range)))))

(defun rangep (a b n)
  (and (>= n a)
       (<= n b)))

(defun cluster-nums (l)
  (cond ((null l) 
	 nil)
	((numberp (car l)) 
	 (cons (cdr (next-nums l))
	       (cluster-nums (next-not-nums l))))
	(t
	 (cluster-nums (cdr l)))))

(defun next-nums (l)
  (cond ((null l) 
	 nil)
	((numberp (car l))
	 (cons (car l)
	       (next-nums (cdr l))))
	(t 
	 nil)))

(defun next-not-nums (l)
  (cond ((null l) 
	 nil)
	((numberp (car l))
	 (next-not-nums (cdr l)))
	(t 
	 l)))
	 
;; cut off the initial elements of l to give l a length of n
(defun truncate-to-length (l n)
  (cond ((null l) 
         nil)
        ((> (length l) n)
         (truncate-to-length (cdr l) n))
        (t 
         l)))

;; a function which binds symbols into new symbols
;;;; >(glob 'bob 'the 'bird)
;;;; #:BOBTHEBIRD

(defun glob (&rest l)
  (cond ((null l)
          (make-symbol ""))
        ((listp (car l))
         (join (apply 'glob (car l))
               (apply 'glob (cdr l))))
        (t
         (join (car l)
               (apply 'glob (cdr l))))))

(defun join (a b)
  (make-symbol (concatenate 'string
                            (symbol-name a)
                            (symbol-name b))))

;; a function which shatters lisp atoms
;; 
;;;; >(smash 'chillatteherder)
;;;; (#:C #:H #:I #:L #:L #:A #:T #:T #:E #:H #:E #:R #:D #:E #:R)

(defun smash (s)
  (mapcar 'make-symbol
          (mapcar #'(lambda (e) (coerce (list e) 'string))
                  (coerce (symbol-name s) 'list))))


(defun file-into-list (s eofmarker &aux st l c)
  (setf st (open s))
  (setf l nil)
  (setf c nil)
  (do ((j 0 (+ j 1))) 
      ((equal c eofmarker))   
    (push (setf c (read st)) l)) l)

(defun smash-every-element (l)
  (cond ((null l)
         nil)
        (t
         (princ (car l))
         (cons (smash (car l))
               (smash-every-element (cdr l))))))

;some sfunctions of the sform (glob (LIST-MANIPULATING-FUN (smash sym)))
(defun sreverse (sym)
  (glob (reverse (smash sym))))

(defun scdr (sym)
  (glob (cdr (smash sym))))

;and to generalize it all

(defun sapply (fun sym)
  (glob (apply fun (list (smash sym)))))

;;;; A demo example for car-x-cdr and sapply.
;;;;
;;;; >(sapply #'(lambda (e) (car-x-cdr e 'i)) 'star)
;;;; #:SITAR
;;;; 
;;;; >(sapply #'(lambda (e) (car-x-cdr e 'reenb)) 'grass)
;;;; #:GREENBRASS

;; (car l) x (cdr l)
(defun car-x-cdr (l x)
  (append (cons (car l) nil) (cons x nil) (cdr l)))

; no repitition in the list, please.

(defun elimenate-redundancy (l)
  (dolist (k l) (setf l (cons k (stripped-of-every-instance l k)))) (reverse l))
                                   
(defun stripped-of-every-instance (l e &aux newlist)
  (mapcar #'(lambda (x) (cond ((not (equal x e))
                               (push x newlist))))
          l) 
  (reverse newlist))

; list in groups of n

(defun group-list (n l)
  (let (chunkedlist)
    (dotimes (i (/ (length l) n))
      (setf chunkedlist (cons (n-car n l) chunkedlist))
      (setf l (n-cdr n l))) 
    (reverse chunkedlist)))

(defun n-car (n l)
  (cond ((zerop n)
         nil)
        (t
         (cons (car l)
               (n-car (- n 1) (cdr l))))))

(defun n-cdr (n l)
  (cond ((zerop n)
         l)
        (t
         (n-cdr (- n 1) (cdr l)))))

(defun flatten (l)
  (cond ((null l)
         nil)
        ((null (car l))
         (cons nil (flatten (cdr l))))
        ((listp (car l))
         (append (flatten (car l))
                 (flatten (cdr l))))
        (t
         (append (list (car l))
                 (flatten (cdr l))))))

(defun segment (l d)
  (cond ((null l)
         nil)
        (t
         (cons (div-car l d)
               (segment (div-cdr l d) d)))))

;; fun is functional
(defun fun-sort (fun ls)
  (cond ((null ls)
         nil)
        (t 
         (cons (funnest (cdr ls) (car ls) fun)
               (fun-sort fun (kill-funnest (cdr ls) fun))))))

;; size (largeness) is boring 
(defun boring-sort (ls)
  (cond ((null ls)
         nil)
        (t 
         (cons (largest (cdr ls) (car ls))
               (boring-sort (kill-largest ls))))))

(defun largest (ls largest)
  (cond ((null ls)
         largest)
        ((> (car ls) largest)
         (largest (cdr ls) (car ls)))
        (t 
         (largest (cdr ls) largest))))

(defun funnest (ls funnest fun)
  (cond ((null ls)
         funnest)
        ((funcall fun (car ls) funnest)
         (funnest (cdr ls) (car ls) fun))
        (t 
         (funnest (cdr ls) funnest fun ))))

(defun kill-largest (ls)
  (remove-nth ls (index-of-largest ls)))

(defun kill-funnest (ls fun)
  (remove-nth ls (index-of-funnest ls fun)))

(defun index-of-largest (ls)
  (get-index-of-largest ls 0 0 (car ls)))

(defun index-of-funnest (ls fun)
  (get-index-of-funnest ls 0 0 (car ls) fun))

(defun get-index-of-funnest (ls index index-of-funnest funnest fun)
  (cond ((null ls)
         index-of-funnest)
        ((funcall fun (car ls) funnest)
         (get-index-of-funnest (cdr ls) (+ 1 index) index (car ls) fun))
        (t 
         (get-index-of-funnest (cdr ls) (+ 1 index) index-of-funnest funnest fun))))

;; featurep can be any lambda list with two params (funcall (list feature b c))
(defun group (ls featurep)
  (cond ((null ls)
         nil)
        (t
         (cons (grp-car ls featurep)
               (group (grp-cdr ls featurep) featurep)))))

(defun grp-car (ls featurep)
  (cond ((null ls) ; if nothing is in the lst
         nil)
        ((null (car (cdr ls))) ; if one thing is in the lst
         (list (car ls)))
        ((not (apply featurep (list (car ls) (car (cdr ls)))))
         (list (car ls)))
        (t
         (cons (car ls) (grp-car (cdr ls) featurep)))))

(defun grp-cdr (ls featurep)
  (cond ((null ls) ; if nothing is in the lst
         nil)
        ((null (car (cdr ls))) ; if one thing is in the lst
         nil)
        ((not (apply featurep (list (car ls) (car (cdr ls)))))
         (cdr ls))
        (t
         (grp-cdr (cdr ls) featurep)))) 

(defun div-car (l d) 
  (cond ((null l)
         nil)
        ((equal (car l) d)
         nil)
        (t
         (cons (car l)
               (div-car (cdr l) d)))))

(defun div-cdr (l d) 
  (cond ((null l)
         nil)
        ((equal (car l) d)
         (cdr l))
        (t
         (div-cdr (cdr l) d))))

(defun car-and-cdr-cdr (ls)
  (list (car ls) (cdr (cdr ls))))

(defun 1st< (a b) (nth< 0 a b))
(defun 2nd< (a b) (nth< 1 a b))

(defun 1st= (a b) (nth= 0 a b))
(defun 2nd= (a b) (nth= 1 a b))

; is nth element of a less than nth element of b?
(defun nth< (n a b)
  (< (nth n a) (nth n b)))
(defun nth= (n a b)
  (= (nth n a) (nth n b)))

(defun place (icon y x)
  (mapcar #'(lambda (here) (list (+ (car here) x) (+ (car (cdr here)) y) (car (cdr (cdr here))))) icon))

(defun make-sequence-renderable (seq) 
  (setf seq (mapcar #'(lambda (arg) (- 0 arg)) seq))
  (mapcar #'(lambda (arg oig) (list oig arg ".")) 
	  (count-from-to 0 (length seq))
	  seq))


; specs args is a list of lists,
; each internal list starting with 
; coords, and then followed by content
(defun render (specs)  
  (terpri)
  (mapcar 'print-special
          (mapcar #'(lambda (arg) (sort arg #'2nd<)) 
                  (group (sort specs #'1st<) #'1st=))))

(defun print-special (line)
  (let ((x-locations (mapcar #'(lambda (item) (car (cdr item))) 
                             line))
        (x-values (mapcar #'(lambda (item) (car (cdr (cdr item))))
                          line))
        (pos 0)
        (step 0))
    (mapcar #'(lambda (loc val) 
                (setf step (- loc pos))
                (write-string (make-string step))
                (write-string val)
                (setf pos (+ 1 loc)))
            x-locations x-values))
    (terpri))

(defun string-length (stra)
  (length (coerce stra 'list)))

(defun repeat (n m)
  (cond ((zerop n)
	 nil)
	(t (cons m (repeat (- n 1) m)))))

(defun repeat-fun (fun n)
  (let (x)
    (do ((o 0 (+ o 1)))
	((equal o n))
        ( push (funcall fun) x )) x))

; use (display-time :international) to display internat, or no params for norm.
; in all honesty any true param will work, such as (display-time t)

(defun display-time (&optional international) 
  (let ((time (multiple-value-list (get-decoded-time))))
    (cond (international
	   (princ (third time)))
	  (t
	   (princ (- (third time) 12))))
    (write-string ":")
    (cond ((< (second time) 10) 
	   (princ 0)
	   (princ (second time)))
	  (t
	   (princ (second time))))
    (terpri)
    time))

(defun homogenousp (liz)
  (let (a n)
    (setf a (car liz))
    (setf n (remove-if #'(lambda (arg) (equal arg a)) liz))
    (null n)))

(defun frequency-of (s l)
  (let ((equal-s #'(lambda (arg) (equal s arg))))
    (- (length l) (length (remove-if equal-s l)))))

(defun count-from-to (a b)
  (cond ((equal a (+ b 1))
	 nil)
	(t
	 (cons a (count-from-to (+ a 1) b)))))

(defun ends-with (word ending)
  (cond ((> (length ending) (length word))
	    nil)
	 ((equal (length word)
		(length ending))
	 (equal word ending))
	(t
	 (ends-with (cdr word) ending))))

(defun either-or (afun bfun)
  (cond ((zerop (random-lo-hi 0 1))
         (funcall afun))
        (t
         (funcall bfun))))
     
(defun pick-fun (&rest args)
  (funcall (any-one args)))
                
(defun ok-prompt (message)
  (write-string (concatenate 'string message "   (options: yes/somethingotherthanyes)    >"))
  (cond ((equal 'yes (read))  t)  (t      nil)))

(defun get-first-n-elements ( n seq )
  (cond ((null seq)
         nil)
        ((zerop n)
         nil)
        (t
         (cons (car seq)
               (get-first-n-elements (1- n) (cdr seq))))))

(defun percent-of (percent quantity)
  (* (/ percent 100) quantity))

(defun my-round (n)
  (car (multiple-value-list
	(either-or #'(lambda () (floor n))  #'(lambda () (ceiling n))))))

(defun make-pattern (sequence size)
  (do-make-pattern sequence size (my-round (/ size (length sequence)))))
(defun do-make-pattern (sequence size sectionsize) 
  (cond ((null sequence) 
         nil)
        (t 
         (append (make-list sectionsize
                            :initial-element (car sequence))
                 (do-make-pattern (cdr sequence) size sectionsize)))))

(defun probablistic-decision (action-one-likeliness action-one action-two)
  (let ((decisive-move (random 100)))
    (cond ((<= decisive-move action-one-likeliness)
           (funcall action-one))
          (t 
           (funcall action-two)))))

(defun cdr-n (n list)
  (cond ((<= n 1) (cdr list))
        (t (cdr-n (1- n) (cdr list)))))

(defun car-n (n list)
  (cond ((null list) nil)
        ((zerop n) nil)
        (t (cons (car list) 
                 (car-n (1- n) (cdr list))))))

(defun random-part (l)
  (let (list-length start-with part-length)
    (setf list-length (length l))
    (setf start-with (random-lo-hi 0 (- list-length 1)))
    (setf part-length (random-lo-hi 1 (- list-length start-with)))
    (car-n part-length (cdr-n start-with l))))

(defun make-list-of-lines (file &aux thisline linelist)
  (setf str (open file)) 
  (do ((j 0 (+ j 1))) 
      ((equal thisline "STOP"))   
    (push (setf thisline (read-line str)) linelist)) 
  (pop linelist) (close str)
  linelist)

(defun binary-list-to-string (blist)
  (cat
   (mapcar #'(lambda (elem) (cond ((zerop elem) "0") ((onep elem) "1")))
           blist)))

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

(defun do-find-default-place-value (n cnt)
  (if (> (expt 2 cnt) n)
      cnt
    (do-find-default-place-value n (+ cnt 1))))

(defun random-word (n) (dotimes (i n) (princ (character (random-lo-hi 65  
91)))))   

; returns the index of the greatest number in a list
(defun index-of-greatest (l &optional index windex winval)
  (if (null index) (setf index 1))
  (if (null windex) (setf windex 1))
  (if (null winval) (setf winval 0))
  (cond ((null l) windex)
        ((> (car l) winval) (index-of-greatest (cdr l) (1+ index) index (car l)))
        (t (index-of-greatest (cdr l) (1+ index) windex winval))))

(defun date-string ()
  (let ((day (fourth (multiple-value-list (get-decoded-time))))
        (month (fifth (multiple-value-list (get-decoded-time))))
        (hour (third (multiple-value-list (get-decoded-time))))
        (minute (second (multiple-value-list (get-decoded-time)))))
    (concatenate 'string "on" 
                 (string (num-to-string day)) "of" 
                 (string (num-to-string month)) "at" 
                 (string (num-to-string hour)) 
                 (string (num-to-string minute)))))

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

(defun binary-string-to-binary-list (s)
  (remove-if-not 'numberp
                 (mapcar #'(lambda (arg) 
                             (cond ((equal arg '#\0) 0)
                                   ((equal arg '#\1) 1)))
                         (coerce s 'list))))