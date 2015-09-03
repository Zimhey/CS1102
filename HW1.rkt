#|
Authors:
 Corey Dixon
 Maryann O'Connell
Course: CS1102
Description: Homework #1
|#

;;Problem #1: Data definitions for patch, insert, and delete

;;*******QUESTION******* -
;;1)Is operation an acceptable type?
;;2)Does patch have a string parameter?
;;*******

;;A patch is (make-patch number op) 
(define-struct patch(pos op))

#|
(define (patch-fun a-patch)
  ...(patch-pos a-patch)...
  ...(patch-op a-patch)...
)
|#

;;An operation is either
;; - an insert, or
;; - a delete

;;*******QUESTION******* -
;;1)Appropriate place for template below?
;;*******

#|
(define (operation-fun an-operation)
 (cond [(insert? an-operation) (insert-fun an-operation)]
       [(delete? an-operation) (delete-fun an-operation)]))
|#

;;An insert is (make-insert str)
(define-struct insert(str))

;;Insert Template
#|
(define (insert-fun an-insert)
   ...(insert-str an-insert)...
)
|#


;;A delete is (make-insert num-char)
(define-struct delete(num-char))

#|
(define (delete-fun a-delete)
   ...(delete-num-char a-delete)
)
|#

;;num-char: number


;;Examples of insert
(define HELLO(make-insert "Hello"))
(define WORLD(make-insert "World"))


;;Examples of delete
(define DEL2(make-delete 2))
(define DEL5(make-delete 5))

;;Examples of patches
(make-patch 2 (make-insert "llo"))
(define DEL-1(make-patch 1 (make-delete 4)))
(define INS-HELLO(make-patch 0 HELLO))
(define DEL-2(make-patch 3 DEL2))
(define INS-DEF(make-patch 3 (make-insert "DEF")))
(define DEL-6(make-patch 6 (make-delete 2)))
(define INS-XYZ(make-patch 22 (make-insert "XYZ")))

;;Problem #2


;;***Contract - document input and output

;;***Purpose Statement - describe what input means
;;and what the function computes from input(s).
;;(i.e. if the first argument is a number, is it a height, speed, etc.?)

;;***Data definitions?

;;***Test Cases
(check-expect (apply-op (make-insert "World") "Hello " 6) "Hello World")
(check-expect (apply-op (make-insert "ats!") "Go" 2) "Goats!")
(check-expect (apply-op (make-delete 6) "Hello World" 5) "Hello")
(check-expect (apply-op (make-delete 2) "DrRacket" 0) "Racket")

(define (apply-op an-op document pos)
  (cond [(insert? an-op)
         (string-append (string-append (substring document 0 pos) (insert-str an-op)(substring document pos)))]
        [(delete? an-op)
         (string-append (substring document 0 pos) (substring document (+ pos (delete-num-char an-op))))]
        ))

;;Problem 3
#|
Write a function apply-patch that consumes a patch and a string
and produces the string resulting from applying the patch to the string.
You may assume that the string is long enough for the operation given in the patch.
|#

;;apply-patch: patch string -> string
;;consumes a patch and a string
;;produces the string resulting from applying the patch to the string

;;Test Cases
(check-expect (apply-patch INS-HELLO "World") "HelloWorld")
(check-expect (apply-patch DEL-2 "ABCEF") "ABC")

;;*****Definitions
(define (apply-patch a-patch str)
  (apply-op (patch-op a-patch) str (patch-pos a-patch)))

#| Problem 4: 
|#

;;helper function
;;in-range? number number number -> boolean
;;consumes three numbers
;;produces a boolean indicating if the number is between start and end (inclusive)

;;Test Cases
(check-expect (in-range? 0 4 2) true)
(check-expect (in-range? 3 5 0) false)

(define (in-range? start end num)
  (and (>= num start) (<= num end)))

;; Find end position of a delete operation in a patch
;; TODO comment me
(define (end-range a-patch)
  (+ (patch-pos a-patch) (delete-num-char(patch-op a-patch))))

(define (in-patch-range? a-patch pos)
  (in-range? (patch-pos a-patch) (end-range a-patch) pos))

(define (del-patch? a-patch)
  (delete? (patch-op a-patch)))

(define (ins-patch? a-patch)
  (insert? (patch-op a-patch)))

;;overlap?: patch patch -> boolean
;;consumes two patches
;;produces a boolean indidicating whether the two patches cannot be applied to the same string because they conflict

;;Test Cases
(check-expect (overlap? INS-HELLO INS-HELLO) true)
(check-expect (overlap? DEL-2 DEL-2) true)
(check-expect (overlap? DEL-1 INS-DEF) true)
(check-expect (overlap? INS-DEF DEL-1) true)
(check-expect (overlap? INS-HELLO INS-XYZ) false)
(check-expect (overlap? DEL-1 DEL-6) false)

;;Definition
(define (overlap? patch1 patch2)
  (cond[(and (ins-patch? patch1) (ins-patch? patch2))      ;;Two insertions start at same location?
       (= (patch-pos patch1) (patch-pos patch2))]
       
      [(and (del-patch? patch1) (del-patch? patch2))       ;;Two deletions whose ranges overlap?
       (or (in-patch-range? patch1 (patch-pos patch2))
           (in-patch-range? patch2 (patch-pos patch1)))]
      
      [(and (del-patch? patch1) (ins-patch? patch2))       ;;An insertion that starts inside the range of deletion?
       (in-patch-range? patch1 (patch-pos patch2))]       
      [(and (del-patch? patch2) (ins-patch? patch1))       
       (in-patch-range? patch2 (patch-pos patch1))]
      )
  )

;;merge: string -> patch patch
;;consumes a string
;;produces a string reflecting both patches or false if the patches do not overlap

;;Test Cases


;;Problem 6
#|
Returning false in the event of an overlap indicates to the user that
a merge did not occur.
|#

