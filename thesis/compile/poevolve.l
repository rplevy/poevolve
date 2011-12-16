;; POEVOLVE                        ;;
;; COPYRIGHT ROBERT LEVY 2000      ;;

(in-package "PCL")

(load "lgtk.o")

;; THE POPULATION OBJECT CLASS     ;;
(defclass population ()
  ((individuals :accessor population-individuals :initarg :individuals)
   (size :accessor population-size :initarg :size)))

;; Global variables

( setf *population* () )   

(setf *population-counter* 0)

( setf *population-size* () )
( setf *nr-generations* () )
( setf *direct-copy-group-size* () )
( setf *crossover-group-size* () )
( setf *direct-copy-group-size* () )

;; the rating scale 

( setf *fittest* 6 )
( setf *highly-fit* 5 )
( setf *pretty-fit* 4 )
( setf *kindof-fit* 3 )
( setf *not-very-fit* 2 )
( setf *not-fit* 1 )

;; parameters with default settings

(setf *nr-generations-default* 60)
(setf *population-size-default* 5000)

(setf *percent-crossover* 70)
(setf *percent-mutation* 20)
(setf *percent-direct-copy* 10)

;; tournament selection process, 
;; as a rule, uses only seven
;; individuals from the population

(setf *select-few* 7)

;; contains what are currently the fittest individuals

(setf *fit-list* nil)

;;---------------------------------;;
;; establish POEVOLVE'S parameters
(defmethod establish-parameters (&optional pop-size num-gens)
  (let ((crossover-percent *percent-crossover*)
        (direct-copy-percent *percent-direct-copy*)
        (mutation-percent *percent-mutation*))
    (setf *population-counter* 0)
    (cond ((null pop-size) 
           (setf *population-size* *population-size-default*)) 
          (t (setf *population-size* pop-size)))
    (cond ((null num-gens) 
           (setf *nr-generations* *nr-generations-default*)) 
          (t (setf *nr-generations* num-gens)))
    (setf *mutation-group-size* 
          (ceiling (percent-of *population-size* mutation-percent)))
    (setf *direct-copy-group-size* 
          (ceiling (percent-of *population-size* direct-copy-percent)))
    (setf *crossover-group-size* 
          (- *population-size* 
             (+ *direct-copy-group-size* *mutation-group-size*)))
    (write-string "The program's parameters have been set to") 
    (terpri)
    (format t "~s% crossover, \(~s individuals\), "
            crossover-percent *crossover-group-size*) 
    (terpri)
    (format t "~s% direct copy, \(~s individuals\),"
            direct-copy-percent *direct-copy-group-size*)  
    (terpri)
    (format t "~s% mutation, \(~s individuals\),"  
            mutation-percent *mutation-group-size*)  
    (terpri)
    (format 
     t "to evolve a population of ~s individuals for ~s generations."
     *population-size* *nr-generations*)  (terpri)
     (list *population-size* *nr-generations*)))

(defmethod population-syllables ((p population))
  (let ((result nil) 
        (lims (mapcar 'limerick-poem-syllables 
                      (population-individuals *population*))))
    (dotimes (i (floor (/ (length lims) 2)))
      (setf result (append result (car lims) (car (cdr lims))))
      (setf lims (cdr (cdr lims)))) result))
            
;;---------------------------------;;
;; display a population
(defmethod display ((p population))
  (display (population-individuals p)))

;;---------------------------------;;
;; a method to set up population 0
(defmethod establish-initial-population ((size integer))
  (setf *population*
	(make-instance 'population 
		       :individuals (repeat-fun 'genlimerator size) 
		       :size size)))

;;---------------------------------;;
;; the evolution program
(defmethod generate-limericks ()
  (establish-parameters *population-size* *nr-generations*) 
  (establish-initial-population *population-size*)
  (dotimes (i *nr-generations*) 
    (evolve-one-generation)))

;;---------------------------------;;
;; produce the next generation
(defmethod evolve-one-generation ()
  (let ((mutation-group nil) (crossover-group nil) 
        (direct-copy-group nil) (most-fit nil))
    (evaluate *population*)
    (show-ratings)
    (display-frequency-table *population* *population-counter*)
    (setf *population-counter* (+ 1 *population-counter*))
    (establish-fit-list)
    (display-fit-list *fit-list*)
    (setf mutation-group 
          (mapcar 'mutate 
                  (any *mutation-group-size* *fit-list*)))
    (setf crossover-group 
          (mapcar 'crossover 
                  (any *crossover-group-size* *fit-list*) 
                  (any *crossover-group-size* *fit-list*)))
    (setf direct-copy-group (any *direct-copy-group-size* *fit-list*))
    (setf (population-individuals *population*)
          (append mutation-group crossover-group direct-copy-group))
    *population*))

;;---------------------------------;;
;; always display the seven most fit 
(defmethod display-fit-list (limerick-poems)
  (write-string-terpri "TOP 7 of the GENERATION")
  (display limerick-poems)
  (terpri))

(defmethod display-frequency-table ((p population) (n number))
  (let ((one (length (remove-if-not 
                      'onep 
                      (mapcar 'limerick-poem-rating 
                              (population-individuals p)))))
        (two (length (remove-if-not 
                      'twop 
                      (mapcar 'limerick-poem-rating 
                              (population-individuals p)))))
        (three (length (remove-if-not 
                        'threep 
                        (mapcar 'limerick-poem-rating 
                                (population-individuals p)))))
        (four (length (remove-if-not 
                       'fourp 
                       (mapcar 'limerick-poem-rating 
                               (population-individuals p)))))
        (five (length (remove-if-not 
                       'fivep (mapcar 'limerick-poem-rating 
                                      (population-individuals p)))))
        (six (length (remove-if-not 
                      'sixp (mapcar 'limerick-poem-rating 
                                    (population-individuals p))))))
    (terpri) (write-string "POPULATION ") (princ n) (terpri)
    (terpri) (write-string "FREQ TABLE") (terpri)
    (write-string "1        ") (princ one) (terpri)
    (write-string "2        ") (princ two) (terpri)
    (write-string "3        ") (princ three) (terpri)
    (write-string "4        ") (princ four) (terpri)
    (write-string "5        ") (princ five) (terpri)
    (write-string "6        ") (princ six) (terpri)
    (terpri) (write-string "AVERAGE = ") 
    (princ (average (append (construct-list one 1) 
                            (construct-list two 2) 
                            (construct-list three 3) 
                            (construct-list four 4) 
                            (construct-list five 5) 
                            (construct-list six 6)))) 
    (terpri)
    (list one two three four five six)))


;;---------------------------------;;
;; Mutation algorthm:
;; do nothing or something to the minilex
;; do nothing or something to the rhyme
;; do nothing or something to the lines
(defmethod mutate ((lim limerick-poem))
  (let ((newlim (clone lim)))
    (cond ((random-val) '( leave minilex alone ) nil)
          (t (setf newlim (mutate-minilex newlim))))
    (cond ((random-val) '( leave rhyming alone ) nil)
          (t (setf newlim (mutate-rhyming newlim))))
    (cond ((random-val) '( leave lines alone ) nil)
          (t (setf newlim (mutate-lines newlim)))) newlim))

;;---------------------------------;;
;; Mutate on minilex :
(defmethod mutate-minilex ((lim limerick-poem))
  (cond ((random-val) (weak-mutate-minilex lim))
        (t (strong-mutate-minilex lim))))

;;---------------------------------;;
;; Weak mutate on minilex:
;; replace 25 %  of minilex with new words 
(defmethod weak-mutate-minilex ((lim limerick-poem))
  (make-instance 'limerick-poem
                 :lines (limerick-poem-lines lim)
                 :minilex (append
                           (pick (* *working-lexicon-size* 0.75)
                                 (limerick-poem-minilex lim))
                           (pick (* *working-lexicon-size* 0.25)
                                 (lexicon-entries *lex*)))))

;;---------------------------------;;
;; Strong mutate on minilex:
;; replace 50 % of minilex with new words 
(defmethod strong-mutate-minilex ((lim limerick-poem))
  (make-instance 'limerick-poem
                 :lines (limerick-poem-lines lim)
                 :minilex (append
                           (pick (* *working-lexicon-size* 0.50)
                                 (limerick-poem-minilex lim))
                           (pick (* *working-lexicon-size* 0.50)
                                 (lexicon-entries *lex*)))))

;;---------------------------------;;
;; Mutate on rhyming :
(defmethod mutate-rhyming ((lim limerick-poem))
  (cond ((random-val) (weak-mutate-rhyming lim))
        (t (strong-mutate-rhyming lim))))

;;---------------------------------;;
;; Weak mutate on rhyming :
;; replace A rhymes with new A rhymes 
;; or replace B rhymes with new rhymes
(defmethod weak-mutate-rhyming ((lim limerick-poem))
  (cond ((random-val) (replace-A-rhymes lim))
        (t (replace-B-rhymes lim))))

(defmethod replace-A-rhymes ((lim limerick-poem))
  (make-instance 'limerick-poem
                 :lines 
                 (insert-items 
                  (list (third (limerick-poem-lines lim))
                        (fourth (limerick-poem-lines lim)))
                  (mapcar 
                   'endcons 
                   (get-three-rhyme)
                   (mapcar 'remove-final 
                           (list 
                            (first (limerick-poem-lines lim))
                            (second (limerick-poem-lines lim))
                            (fifth (limerick-poem-lines lim)))))
                  2)
                 :minilex (limerick-poem-minilex lim)))

(defmethod replace-B-rhymes ((lim limerick-poem))
  (make-instance 'limerick-poem
                 :lines
                 (insert-items 
                  (mapcar 
                   'endcons 
                   (get-two-rhyme)
                   (mapcar 'remove-final 
                           (list 
                            (third (limerick-poem-lines lim))
                            (fourth (limerick-poem-lines lim)))))
                  (list (first (limerick-poem-lines lim))
                        (second (limerick-poem-lines lim))
                        (fifth (limerick-poem-lines lim)))
                  2)
                 :minilex (limerick-poem-minilex lim)))

;;---------------------------------;;
;; Strong mutate on rhyming:
;; replace all rhymes with all new rhymes 
(defmethod strong-mutate-rhyming ((lim limerick-poem))
  (make-instance 'limerick-poem
                 :lines
                 (insert-items
                  (mapcar 
                   'endcons 
                   (get-two-rhyme)
                   (mapcar 'remove-final 
                           (list 
                            (third (limerick-poem-lines lim))
                            (fourth (limerick-poem-lines lim)))))
                  
                  (mapcar 
                   'endcons 
                   (get-three-rhyme)
                   (mapcar 'remove-final 
                           (list 
                            (first (limerick-poem-lines lim))
                            (second (limerick-poem-lines lim))
                            (fifth (limerick-poem-lines lim)))))
                  2)
                 :minilex (limerick-poem-minilex lim)))

;;---------------------------------;;
;; Mutate on lines :
(defmethod mutate-lines ((lim limerick-poem))
  (cond ((random-val) (weak-mutate-lines lim))
        (t (strong-mutate-lines lim))))

;;---------------------------------;;
;; Weak mutate on lines :
;; replace all rhymes with all new rhymes 
(defmethod weak-mutate-lines ((lim limerick-poem))
  (cond ((random-val) (modify-single-line lim))
        (t (modify-multiple-lines lim))))

;;---------------------------------;;
;; Modify single line 
;; either weakly or strongly
(defmethod modify-single-line ((lim limerick-poem))
  (cond ((random-val)
         (modify-single-line-weak lim))
        (t 
         (modify-single-line-strong lim))))

(defmethod modify-single-line-weak ((lim limerick-poem))
  (make-instance 'limerick-poem 
                 :lines
                 (weakly-modify-nth-line 
                  (random 4)
                  (limerick-poem-minilex lim)
                  (limerick-poem-lines lim))
                 :minilex (limerick-poem-minilex lim)))

(defmethod weakly-modify-nth-line ((n number) (minilex list) (lines list))
  (insert-items 
   (list (line-modifier-weak minilex (nth n lines)))
   (remove-nth n lines)
   n))

(defmethod modify-single-line-strong ((lim limerick-poem))
  (make-instance 'limerick-poem 
                 :lines
                 (strongly-modify-nth-line 
                  (random 4)
                  (limerick-poem-minilex lim)
                  (limerick-poem-lines lim))
                 :minilex (limerick-poem-minilex lim)))

(defmethod strongly-modify-nth-line ((n number) (minilex list) (lines list))
  (if (or (= n 0) (= n 1) (= n 4))
      (insert-items 
       (list (line-a-modifier-strong minilex (nth n lines)))
       (remove-nth n lines)
       n)
    (insert-items 
     (list (line-b-modifier-strong minilex (nth n lines)))
     (remove-nth n lines)
     n)))
    
;;---------------------------------;;
;; Modify multiple line 
;; modify A lines
;; or modify B lines
(defmethod modify-multiple-lines ((lim limerick-poem))
  (cond ((random-val) (modify-A-lines lim))
        (t (modify-B-lines lim))))

(defmethod modify-A-lines ((lim limerick-poem))
  (make-instance 'limerick-poem
                 :lines 
                 (list
                  (cond ((random-val)
                         (line-modifier-weak
                          (limerick-poem-minilex lim)
                          (first (limerick-poem-lines lim))))
                        (t 
                         (line-a-modifier-strong
                          (limerick-poem-minilex lim)
                          (first (limerick-poem-lines lim)))))
                  (cond ((random-val)
                         (line-modifier-weak
                          (limerick-poem-minilex lim)
                          (second (limerick-poem-lines lim))))
                        (t 
                         (line-a-modifier-strong
                          (limerick-poem-minilex lim)
                          (second (limerick-poem-lines lim)))))
                  (third (limerick-poem-lines lim))
                  (fourth (limerick-poem-lines lim))
                  (cond ((random-val)
                         (line-modifier-weak
                          (limerick-poem-minilex lim)
                          (fifth (limerick-poem-lines lim))))
                        (t 
                         (line-a-modifier-strong
                          (limerick-poem-minilex lim)
                          (fifth (limerick-poem-lines lim))))))
                 :minilex (limerick-poem-minilex lim)))  

(defmethod modify-B-lines ((lim limerick-poem))
  (make-instance 'limerick-poem
                 :lines 
                 (list
                  (first (limerick-poem-lines lim))
                  (second (limerick-poem-lines lim))
                  (cond ((random-val)
                         (line-modifier-weak
                          (limerick-poem-minilex lim)
                          (third (limerick-poem-lines lim))))
                        (t 
                         (line-b-modifier-strong
                          (limerick-poem-minilex lim)
                          (third (limerick-poem-lines lim)))))
                  (cond ((random-val)
                         (line-modifier-weak
                          (limerick-poem-minilex lim)
                          (fourth (limerick-poem-lines lim))))
                        (t 
                         (line-b-modifier-strong
                          (limerick-poem-minilex lim)
                          (fourth (limerick-poem-lines lim)))))
                  (fifth (limerick-poem-lines lim)))
                 :minilex (limerick-poem-minilex lim)))

;;---------------------------------;;
;; Strong mutate on lines:
;; replace all lines with all new lines 
(defmethod strong-mutate-lines ((lim limerick-poem))
  (make-instance 'limerick-poem
                 :lines 
                 (list
                  (cond ((random-val)
                         (line-modifier-weak
                          (limerick-poem-minilex lim)
                          (first (limerick-poem-lines lim))))
                        (t 
                         (line-a-modifier-strong
                          (limerick-poem-minilex lim)
                          (first (limerick-poem-lines lim)))))
                  (cond ((random-val)
                         (line-modifier-weak
                          (limerick-poem-minilex lim)
                          (second (limerick-poem-lines lim))))
                        (t 
                         (line-a-modifier-strong
                          (limerick-poem-minilex lim)
                          (second (limerick-poem-lines lim)))))
                  (cond ((random-val)
                         (line-modifier-weak
                          (limerick-poem-minilex lim)
                          (third (limerick-poem-lines lim))))
                        (t 
                         (line-b-modifier-strong
                          (limerick-poem-minilex lim)
                          (third (limerick-poem-lines lim)))))
                  (cond ((random-val)
                         (line-modifier-weak
                          (limerick-poem-minilex lim)
                          (fourth (limerick-poem-lines lim))))
                        (t 
                         (line-b-modifier-strong
                          (limerick-poem-minilex lim)
                          (fourth (limerick-poem-lines lim)))))
                  (cond ((random-val)
                         (line-modifier-weak
                          (limerick-poem-minilex lim)
                          (fifth (limerick-poem-lines lim))))
                        (t 
                         (line-a-modifier-strong
                          (limerick-poem-minilex lim)
                          (fifth (limerick-poem-lines lim))))))
                 :minilex (limerick-poem-minilex lim)))




;;---------------------------------;;
;; Crossover algorithm: 
;; merge with or exclude the other minilex
;; work with or ignore the other rhyming
;; work with or ignore the other lines
(defmethod crossover ((lim-i limerick-poem) 
                      (lim-ii limerick-poem))
  (let ((newlim (clone lim-i)))
    (cond ((random-val) '( exclude other minilex ) nil)
          (t (setf newlim (cross-minilex newlim lim-ii))))
    (cond ((random-val) '( ignore other rhyming ) nil)
          (t (setf newlim (cross-rhyming newlim lim-ii))))
    (cond ((random-val) '( ignore other lines ) nil)
          (t (setf newlim (cross-lines newlim lim-ii)))) newlim))

;;---------------------------------;;
;; Crossover minilex 
;; share 25 %
;; or share 50 % of other words
(defmethod cross-minilex ((lim-i limerick-poem) 
                          (lim-ii limerick-poem))
  (cond ((random-val)
         (weak-cross-minilex lim-i lim-ii))
        (t 
         (strong-cross-minilex lim-i lim-ii))))

(defmethod weak-cross-minilex ((lim-i limerick-poem)
                               (lim-ii limerick-poem))
  (make-instance 'limerick-poem
                 :lines (limerick-poem-lines lim-i)
                 :minilex (append
                           (pick (* *working-lexicon-size* 0.75)
                                 (limerick-poem-minilex lim-i))
                           (pick (* *working-lexicon-size* 0.25)
                                 (limerick-poem-minilex lim-ii)))))

(defmethod strong-cross-minilex ((lim-i limerick-poem)
                               (lim-ii limerick-poem))
  (make-instance 'limerick-poem
                 :lines (limerick-poem-lines lim-i)
                 :minilex (append
                           (pick (* *working-lexicon-size* 0.50)
                                 (limerick-poem-minilex lim-i))
                           (pick (* *working-lexicon-size* 0.50)
                                 (limerick-poem-minilex lim-ii)))))

;;---------------------------------;;
;; Crossover rhyming 
;; share A rhymes
;; or share B rhymes
(defmethod cross-rhyming ((lim-i limerick-poem) 
                          (lim-ii limerick-poem))
  (cond ((random-val) (cross-A-rhyming lim-i lim-ii))
        (t (cross-B-rhyming lim-i lim-ii))))

(defmethod cross-A-rhyming ((lim-i limerick-poem) 
                            (lim-ii limerick-poem))
  (make-instance 'limerick-poem
                 :lines 
                 (insert-items 
                  (list (third (limerick-poem-lines lim-i))
                        (fourth (limerick-poem-lines lim-i)))
                  (mapcar 
                   'endcons 
                   (list
                    (final (first (limerick-poem-lines lim-ii)))
                    (final (second (limerick-poem-lines lim-ii)))
                    (final (fifth (limerick-poem-lines lim-ii))))
                   (mapcar 'remove-final 
                           (list 
                            (first (limerick-poem-lines lim-i))
                            (second (limerick-poem-lines lim-i))
                            (fifth (limerick-poem-lines lim-i)))))
                  2)
                 :minilex (limerick-poem-minilex lim-i)))

(defmethod cross-B-rhyming ((lim-i limerick-poem) 
                            (lim-ii limerick-poem))
    (make-instance 'limerick-poem
                 :lines
                 (insert-items 
                  (mapcar 
                   'endcons 
                   (list
                    (final (third (limerick-poem-lines lim-ii)))
                    (final (fourth (limerick-poem-lines lim-ii))))
                   (mapcar 'remove-final 
                           (list 
                            (third (limerick-poem-lines lim-i))
                            (fourth (limerick-poem-lines lim-i)))))
                  (list (first (limerick-poem-lines lim-i))
                        (second (limerick-poem-lines lim-i))
                        (fifth (limerick-poem-lines lim-i)))
                  2)
                 :minilex (limerick-poem-minilex lim-i)))

;;---------------------------------;;
;; Crossover lines 
;; share lines
;; or merge lines
(defmethod cross-lines ((lim-i limerick-poem) 
                        (lim-ii limerick-poem))
  (cond ((random-val) (cross-share-lines lim-i lim-ii))
        (t (cross-merge-lines lim-i lim-ii))))

;;---------------------------------;;
;; Crossover share lines 
;; share A scheme lines
;; or share B scheme lines
(defmethod cross-share-lines ((lim-i limerick-poem) 
                              (lim-ii limerick-poem))
  (cond ((random-val) (share-A-lines lim-i lim-ii))
        (t (share-B-lines lim-i lim-ii))))

(defmethod share-A-lines ((lim-i limerick-poem) (lim-ii limerick-poem))
  (make-instance 'limerick-poem 
                 :lines (list 
                         (first (limerick-poem-lines lim-ii))
                         (second (limerick-poem-lines lim-ii))
                         (third (limerick-poem-lines lim-i))
                         (fourth (limerick-poem-lines lim-i))
                         (fifth (limerick-poem-lines lim-ii)))
                 :minilex (limerick-poem-minilex lim-i)))

(defmethod share-B-lines ((lim-i limerick-poem) (lim-ii limerick-poem))
  (make-instance 'limerick-poem 
                 :lines (list 
                         (first (limerick-poem-lines lim-i))
                         (second (limerick-poem-lines lim-i))
                         (third (limerick-poem-lines lim-ii))
                         (fourth (limerick-poem-lines lim-ii))
                         (fifth (limerick-poem-lines lim-i)))
                 :minilex (limerick-poem-minilex lim-i)))

;;---------------------------------;;
;; Crossover merge lines 
;; merge all lines 
;; or merge A or B scheme lines
(defmethod cross-merge-lines ((lim-i limerick-poem) 
                              (lim-ii limerick-poem))
  (cond ((random-val) (cross-merge-all-lines lim-i lim-ii))
        (t (cross-merge-A-B-lines lim-i lim-ii))))

;;---------------------------------;;
;; Crossover merge all lines 
(defmethod cross-merge-all-lines ((lim-i limerick-poem) 
                              (lim-ii limerick-poem))
  (make-instance 'limerick-poem 
                 :lines (list
                         (merge-lines
                          (first (limerick-poem-lines lim-i))
                          (first (limerick-poem-lines lim-ii)))
                         (merge-lines
                          (second (limerick-poem-lines lim-i))
                          (second (limerick-poem-lines lim-ii)))
                         (merge-lines
                          (third (limerick-poem-lines lim-i))
                          (third (limerick-poem-lines lim-ii)))
                         (merge-lines
                          (fourth (limerick-poem-lines lim-i))
                          (fourth (limerick-poem-lines lim-ii)))
                         (merge-lines
                          (fifth (limerick-poem-lines lim-i))
                          (fifth (limerick-poem-lines lim-ii))))

                 :minilex (limerick-poem-minilex lim-i)))
;;---------------------------------;;
;; Crossover merge A or B lines 
;; merge A lines together
;; or merge B lines together
(defmethod cross-merge-A-B-lines ((lim-i limerick-poem) 
                              (lim-ii limerick-poem))
  (cond ((random-val) (merge-A-lines lim-i lim-ii))
        (t (merge-A-lines lim-i lim-ii))))

(defmethod merge-A-lines ((lim-i limerick-poem)
                          (lim-ii limerick-poem))
  (make-instance 'limerick-poem 
                 :lines (list
                         (merge-lines
                          (first (limerick-poem-lines lim-i))
                          (first (limerick-poem-lines lim-ii)))
                         (merge-lines
                          (second (limerick-poem-lines lim-i))
                          (second (limerick-poem-lines lim-ii)))
                         (third (limerick-poem-lines lim-i))
                         (fourth (limerick-poem-lines lim-i))
                         (merge-lines
                          (fifth (limerick-poem-lines lim-i))
                          (fifth (limerick-poem-lines lim-ii))))
                 :minilex (limerick-poem-minilex lim-i)))

(defmethod merge-B-lines ((lim-i limerick-poem)
                          (lim-ii limerick-poem))
  (make-instance 'limerick-poem 
                 :lines (list
                         (first (limerick-poem-lines lim-i))
                         (second (limerick-poem-lines lim-i))
                         (merge-lines
                          (third (limerick-poem-lines lim-i))
                          (third (limerick-poem-lines lim-ii)))
                         (merge-lines
                          (fourth (limerick-poem-lines lim-i))
                          (fourth (limerick-poem-lines lim-ii)))
                         (fifth (limerick-poem-lines lim-i)))
                 :minilex (limerick-poem-minilex lim-i)))

;;---------------------------------;;
;; Here is the interface between CLOS 
;; and TLearn. It applies fitness 
;; ratings to the limericks
(defmethod evaluate ((p population))
  (make-tlearn-files p "./evaluator/evaluator") 
  (tlearn-script)
  (assign-ratings p "./evaluator/evaluator.ratings"))

(defmethod tlearn-script ()
  (let ((part-one "cd evaluator; ")
        (part-two "tlearn -f evaluator -l evaluator.wts -X -s ")
        (part-three (num-to-string (* 40 *population-size*)))
        (part-four " -V > evaluator.output; ")
        (part-five 
         "cat header evaluator.output footer > evaluator.ratings; ")
        (part-six "cd .."))
    (system (concatenate 'string 
                         part-one part-two part-three 
                         part-four part-five part-six))))

(defmethod make-tlearn-files ((p population) (prefix string))
  (let ((syllables (population-syllables p))
        (data-file-name (concatenate 'string prefix ".data"))
        (reset-file-name (concatenate 'string prefix ".reset")))
    (save-limericks-as-data (population-individuals p) 
                            data-file-name 
                            (length syllables))
    (save-limerick-reset-data (population-individuals p) 
                              reset-file-name 
                              (length syllables))))
    
(setf *stream* nil)
(defmethod save-limericks-as-data ((limericks list) (fn string) 
                                   (nr-of-syllables number))
  (setf *stream* (open fn :direction :output))
  (write-string "distributed" *stream*) (terpri *stream*)
  (princ nr-of-syllables *stream*) (terpri *stream*)
  (mapcar 'save-limerick-as-data 
          limericks)
  (terpri *stream*)
  (close *stream*))

(defmethod save-limerick-as-data ((l limerick-poem))
  (mapcar 'write-nn-inputs-to-disk (limerick-nn-input l)))

(defmethod write-nn-inputs-to-disk ((syllable list))
  (mapcar #'(lambda (arg) (princ arg *stream*) 
              (write-string " " *stream*)) syllable)
  (terpri *stream*))

;;---------------------------------;;
;; from limericks calculate reset 
;; points (inter-limerick borders) 
;; and construct reset file

(defmethod save-limerick-reset-data ((limerick-list list) (fn string) 
                                     (nr-of-syllables number))
  (let ((resets (find-tlearn-reset-points limerick-list)))
    (setf *stream* (open fn :direction :output)) 
    (princ nr-of-syllables *stream*) (terpri *stream*)
    (princ 0 *stream*) (terpri *stream*)
    (dotimes (i (1- (length resets)))
      (princ (car resets) *stream*) (terpri *stream*)
      (pop resets))
    (terpri *stream*)
    (close *stream*)))

(defmethod find-tlearn-reset-points ((limerick-list list))
  (add-progressively
   (mapcar 'limerick-poem-syllables-count limerick-list)))

;;---------------------------------;;
;; .ratings file contains the raw 
;; ratings within a single set of ()s
(defmethod assign-ratings ((p population) (ratings-file string))
  (mapcar #'(lambda (limerick rating) 
              (setf (limerick-poem-rating limerick) rating))
          (population-individuals p)
          (get-ratings-from-file ratings-file)))

(defmethod get-ratings-from-file ((ratings-file string))
  (let ((stream (open ratings-file)))
    (values  ; return stuff before closing file
     (mapcar 'index-of-greatest 
             (nths (find-tlearn-reset-points 
                    (population-individuals *population*))
                   (group-list 6 (read stream))))
     (close stream))))

(defmethod establish-fit-list ()
  (setf *fit-list*
        (car-n *select-few* 
               (sort-by-rating 
                (population-individuals 
                 *population*)))))

(defmethod sort-by-rating ((l list))
  (let ((one (remove-if-not 
              #'(lambda (arg) (= (limerick-poem-rating arg) 1)) l))
        (two (remove-if-not 
              #'(lambda (arg) (= (limerick-poem-rating arg) 2)) l))
        (three (remove-if-not 
                #'(lambda (arg) (= (limerick-poem-rating arg) 3)) l))
        (four (remove-if-not 
               #'(lambda (arg) (= (limerick-poem-rating arg) 4)) l))
        (five (remove-if-not 
               #'(lambda (arg) (= (limerick-poem-rating arg) 5)) l))
        (six (remove-if-not 
              #'(lambda (arg) (= (limerick-poem-rating arg) 6)) l)))
    (append six five four three two one)))


(defmethod show-ratings () 
  (mapcar #'(lambda (limmy) 
              (display limmy) (terpri) 
              (princ (limerick-poem-rating limmy)) (terpri)) 
          (population-individuals *population*)))