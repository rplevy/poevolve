#!/usr/bin/clisp -q

(load "lgtk.l")

(setf *song-writer-lexicon-size* 100)

; generate lyrics based on template file 
(defmethod compose-lyrics ((template-file string))
  (let ((lines (read-template-file template-file)))
    (dotimes (i (length lines))
      (terpri)
      (write-string (compose-line (car lines)))
      (pop lines))))
        
; compose yet another all purpose line
(defmethod compose-line ((scan list))
  (let ((mini-lex (pick *song-writer-lexicon-size* 
                       (lexicon-entries *lex*))))
    (line-to-text (make-poem-line mini-lex scan))))

(defmethod read-template-file ((template-file string))
  (mapcar 
   'binary-string-to-binary-list
   (reverse (make-list-of-lines template-file))))


(defun parse-line (line)
  (remove-if 'null (mapcar (lambda (x) (cond ((equalp x #\0) 0) ((equalp x #\1) 1)))  (coerce line 'list))))

(if (car *args*)
    (compose-lyrics (car *args*))
    (print "usage: ./song-writer.l example-template.sng"))
