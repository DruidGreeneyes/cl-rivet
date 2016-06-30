(in-package #:cl-rivet)

(defpackage #:mem-lex
  (:use :cl
        :iterate)
  (:export :make-lexicon
           :get-ind
           :get-lex
           :generate-riv
           :add-to-word-lex
           :ingest-text))
(in-package #:mem-lex)

(defstruct (lexicon (:conc-name nil) (:copier nil))
  (size 1000 :type fixnum :read-only t)
  (nnz 8 :type fixnum :read-only t)
  (words (make-hash-table :test #'equal) :type hash-table :read-only t))

(declaim (inline break-text
                 break-sentence
                 get-word-entry
                 new-word-entry
                 add-word-entry
                 generate-riv))

(defun get-word-entry (lexicon word)
  (gethash word (words lexicon)))

(defun new-word-entry ()
  (make-hash-table :test #'eq))

(defun add-word-entry (lexicon word)
  (setf (gethash word (words lexicon)) (new-word-entry)))

(defun generate-riv (lexicon word)
  (riv:generate-riv (size lexicon) (nnz lexicon) word))

(defun ensure-entry (lexicon word &optional (ind nil))
  (unless (get-word-entry lexicon word)
    (let ((entry (new-word-entry))
          (riv (or ind (generate-riv lexicon word))))
      (setf (gethash :lex entry) riv)
      (setf (gethash :ind entry) riv)
      (setf (gethash word (words lexicon)) entry))))

(defun get-ind (lexicon word)
  (ensure-entry lexicon word)
  (gethash :ind (get-word-entry lexicon word)))

(defun get-lex (lexicon word)
  (ensure-entry lexicon word)
  (gethash :lex (get-word-entry lexicon word)))

(defun set-ind (lexicon word riv)
  (ensure-entry lexicon word riv)
  (setf (gethash :ind (gethash word (words lexicon))) riv))

(defun set-lex (lexicon word riv)
  (ensure-entry lexicon word)
  (setf (gethash :lex (gethash word (words lexicon))) riv))

(defun add-to-word-lex (lexicon word riv)
  (ensure-entry lexicon word)
  (let ((orig (get-lex lexicon word)))
    (set-lex lexicon word (riv:add orig riv))))

(defun break-text (text)
  (cl-ppcre:split "\\.\\s+" text))

(defun break-sentence (sentence)
  (cl-ppcre:split "\\s+" sentence))

(defun ingest-broken-sentence (lexicon broken-sentence)
  (let ((cycle (rest (util::make-circular broken-sentence)))
        (len (1- (length broken-sentence))))
    (iterate:iter (for word in broken-sentence)
                  (for context on cycle) 
                  (add-to-word-lex lexicon word
                                   (riv:sum-rivs
                                    (mapcar (lambda (w) (get-ind lexicon w))
                                            (util::take len context)))))))

(defun ingest-broken-text (lexicon broken-text)
  (mapcar (lambda (s) (ingest-broken-sentence lexicon s)) broken-text))

(defun ingest-text (lexicon text)
  (let ((broken-text (mapcar #'break-sentence (break-text text))))
    (ingest-broken-text lexicon broken-text)))

(defun process-broken-sentence (lexicon broken-sentence)
  (riv:sum-rivs (mapcar (lambda (w) (get-lex lexicon w)) broken-sentence)))

(defun process-broken-text (lexicon broken-text)
  (riv:sum-rivs (mapcar (lambda (s) (process-broken-sentence lexicon s)) broken-text)))

(defun process-text (lexicon text)
  (let ((broken-text (mapcar #'break-sentence (break-text text))))
    (process-broken-text lexicon broken-text)))
