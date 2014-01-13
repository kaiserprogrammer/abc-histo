(defpackage :abc-histo
  (:use :cl :lisp-unit))

(in-package :abc-histo)

(defclass memory-db ()
  ((words :initform (make-hash-table :test 'equal)
          :accessor words)
   (topics :initform (make-hash-table :test 'equal)
           :accessor topics)))

(defun top (n alist)
  (subseq (sort alist #'> :key #'cdr) 0 n))

(defun top-histo (n db &key topic)
  (top n (histo db :topic topic)))

(defun add-dir (directory db)
  (when (cl-fad:directory-exists-p directory)
    (cl-fad:walk-directory
     directory
     (lambda (file)
       (with-open-file (in file)
         (let ((topic (string-downcase (read-line in nil nil))))
           (loop for line = (read-line in nil nil)
              while line
              do (add-line line topic db))))))))

(defun histo (db &key topic)
  (if topic
      (alexandria:hash-table-alist (gethash (string-downcase topic) (topics db)))
      (histogram db)))

(defun search-histo (prefix db &key topic)
  (let ((prefix (string-downcase prefix)))
    (remove-if-not (lambda (word-count)
                     (alexandria:starts-with-subseq prefix (car word-count)))
                   (histo db :topic topic))))

(defun add (abcstring db &key topic)
  (add-line abcstring topic db))

(defun add-line (abcstring topic db)
  (let ((words (subseq (cl-ppcre:split "\\s+" abcstring) 1)))
    (dolist (word words)
      (if topic
          (add-word (string-downcase word) (string-downcase topic) db)
          (add-word (string-downcase word) nil db)))))

(defmethod histogram ((db memory-db))
  (alexandria:hash-table-alist (words db)))

(defmethod add-word (word topic (db memory-db))
  (incf (gethash word (words db) 0))
  (when topic
    (unless (gethash topic (topics db))
      (setf (gethash topic (topics db)) (make-hash-table :test 'equal)))
    (incf (gethash word (gethash topic (topics db)) 0))))

(defmethod count-of (key (db memory-db))
  (gethash key (words db)))

(remove-tests :all)

(define-test abc-histo-add
  (let ((db (make-instance 'memory-db))
        (data "A Arnold  Annette"))
    (add data db)
    (assert-equal 1 (count-of "arnold" db))))

(define-test abc-histo-histogram
  (let ((db (make-instance 'memory-db))
        (data "A Arnold Annette annette"))
    (add data db)
    (assert-equal 1 (cdr (assoc "arnold" (histo db) :test #'string=)))
    (assert-equal 2 (cdr (assoc "annette" (histo db) :test #'string=)))))

(define-test abc-search-topic
  (let ((db (make-instance 'memory-db)))
    (add "A Arnold Alice" db :topic "Names")
    (add "A Anbei" db)
    (assert-true (assoc "arnold" (histo db :topic "Names") :test #'string=))
    (assert-false (assoc "anbei" (histo db :topic "Names") :test #'string=))
    (assert-true (search-histo "ar" db :topic "names"))
    (assert-true (search-histo "An" db))
    (assert-false (search-histo "an" db :topic "names"))))

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
