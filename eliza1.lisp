;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File eliza1.lisp: Basic version of the Eliza program

;;; The basic are in auxfns.lisp; look for "PATTERN MATCHING FACILITY"

;; New version of pat-match with segment variables

(provide :eliza1)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :elizaaux)
  )

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)                ; ***
         (segment-match pattern input bindings))    ; ***
        ((and (consp pattern) (consp input)) 
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) 
                               bindings)))
        (t fail)))

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?* var) . pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

;;; ==============================

;;;(defun segment-match (pattern input bindings &optional (start 0))
;;;  "Match the segment pattern ((?* var) . pat) against input."
;;;  (let ((var (second (first pattern)))
;;;        (pat (rest pattern)))
;;;    (if (null pat)
;;;        (match-variable var input bindings)
;;;        ;; We assume that pat starts with a constant
;;;        ;; In other words, a pattern can't have 2 consecutive vars
;;;        (let ((pos (position (first pat) input
;;;                             :start start :test #'equal)))
;;;          (if (null pos)
;;;              fail
;;;              (let ((b2 (pat-match pat (subseq input pos) bindings)))
;;;                ;; If this match failed, try another longer one
;;;                ;; If it worked, check that the variables match
;;;                (if (eq b2 fail)
;;;                    (segment-match pattern input bindings (+ pos 1))
;;;                    (match-variable var (subseq input 0 pos) b2))))))))

;;; ==============================

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        ;; We assume that pat starts with a constant
        ;; In other words, a pattern can't have 2 consecutive vars
        (let ((pos (position (first pat) input
                             :start start :test #'equal)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                          pat (subseq input pos)
                          (match-variable var (subseq input 0 pos)
                                          bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

;;; ==============================

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defvar *eliza-rules*)

;;; ==============================

(defun eliza1 ()
  "Respond to user input using pattern matching rules."
  (loop
    (print 'eliza>)(force-output)
    (write (flatten (use-eliza-rules (read))) :pretty t)))

(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result fail))
                  (sublis (switch-viewpoint result)
                          (random-elt (rule-responses rule))))))
        *eliza-rules*))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on."
  (sublis '((I . you) (you . I) (me . you) (am . are))
          words))