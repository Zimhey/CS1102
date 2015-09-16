;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hwk3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Maryann O'Connell
;;Corey Dixon

;;CS 1102 - A15
;;Homework 3: Complex Data Definitions

;;*****FOLLOW THE TEMPLATES!!!*****

;;1. Write three examples of data created with the filesystem data definition.

;;a file is (make-file symbol number value)
(define-struct file (name size content))

(define hello_world (make-file 'hello_world 88 "Hello World"))

(define motd (make-file 'motd 256 "Work hard, play hard, make history."))

(define hwrk3.rkt (make-file 'hwrk3.rkt 128 "Homework 3"))

(define empty.rkt (make-file 'empty.rkt 0 ""))

;; A list-of-files is either
;; empty, or
;; (cons s lof) where s is a file and lof is a list of files

;; A list-of-directories is either
;; empty, or
;; (cons s lod)

;;a dir is (make-dir symbol list-of-directories list-of-files)
(define-struct dir (name dirs files))

(define cdixon (make-dir 'cdixon empty (list hello_world motd hwrk3.rkt)))

(define mjoconnell (make-dir 'mjoconnell empty (list motd hwrk3.rkt empty.rkt)))

(define home (make-dir 'home (list cdixon mjoconnell) empty))

(define root (make-dir '/ (list home) empty))


;;2. Write the template for functions over filesystems.

#|
file-fun : file -> ?
(define (file-fun a-file)
 ...(file-name a-file)...
 ...(file-size a-file)...
 ...(file-content a-file)...
)
|#

#| list-of-files-fun: list-of-files -> ?
(define (los-fun alos)
(cond [(empty? (los-fun alos)...]
      [(cons? alos) ...
          (file-fun (first alos))...
          (los-fun alos (rest alos)) ...]))
 |#

#| list-of-directories: dir -> ?
(define (lod-fun adir)
  (cond[(empty? alod) ...]
       [(cons? alod) ...
              (lod-fun (first (dir-dirs adir))...
              (lod-fun (rest (lod adir)))]))
|#


#|
3. Write a function any-huge-files? that consumes a filesystem and a number (a file size)
and returns a boolean indicating whether any file in the filesystem has size larger than the given size.
|#
;; any-huge-files? : filesystem number -> boolean
;;consumes a filesystem and a file size
;;produces a boolean indicating whether any file in the filesystem has a size larger than the give size.

(define (any-huge-files? alod size)
  (cond[(empty? alod) false]
       [(cons? alod)
               (or(> (length (filter (lambda (afile) (> (file-size afile) size)) (dir-files (first alod)))) 0)
                  (any-huge-files? (rest alod) size)
                  (any-huge-files? (dir-dirs (first alod)) size)
                  )]))
               


(check-expect (any-huge-files? (list) 10) false)
(check-expect (any-huge-files? (list cdixon) 100) true)
(check-expect (any-huge-files? (list root) 0) true)


#|
4. Write a function clean-directory that consumes a filesystem and an existing directory name,
 and returns a filesystem. In the returned filesystem, any files of size 0 in the named directory
 should have been removed. All other files and directories should be the same between the input
 and returned filesystems.

You may assume that the given directory name is only in the system once and that it has no subdirectories.
 You do not need to use these assumptions if you don't want to
 (i.e., no loss of points for not optimizing your code around this assumption).

|#

;;clean-directory : list-of-directories symbol -> list-of-directiories [excluding files of size 0]
;;consumes a filesystem and an existing directory name
;;produces a filesystem with no files of size 0

(define (clean-directory alod name)
  (cond[(empty? alod) false]
       [(cons? alod) ...

        (clean-directory (first alod)...
              (clean-directory (rest alod))...


#|
5. Write a function find-file-path that consumes a filesystem and a filename
 and returns either the path to that filename (a list of directory names,
 in order from root to the directory containing the file--but not including the filename)
 or false if the filename is not in the filesystem.

Assume that the given filename is only in the system once.
|#

#|
6. Write a function file-names-satisfying that consumes a filesystem and
 a function from file to boolean and returns a list of names of files for
 which the given function returns true (you don't need to include the
whole path, just the filename). You may find it easier to first write this
 function with a specific criterion (such as a file with a minimum size)
 instead of the function parameter, then generalize it.
|#

#|
7. Use file-names-satisfying to write a function files-containing that
 consumes a filesystem and a value and returns a list of names of files
 with the given value as its contents. You can compare arbitrary Racket
 values for equality using the primitive equal?.
|#