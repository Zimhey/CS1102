;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hwk3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Maryann O'Connell
;;Corey Dixon

;;CS 1102 - A15
;;Homework 3: Complex Data Definitions

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

;;clean-directory : list-of-directories symbol -> list-of-directiories [excluding files of size 0]
;;consumes a filesystem and an existing directory name
;;produces a filesystem with no files of size 0

(define (clean-directory alod name)
  (map (lambda (adir) (make-dir (dir-name adir)
                              (dir-dirs adir)
                              (cond[(symbol=? (dir-name adir) name)
                                    (filter (lambda (afile) (> (file-size afile) 0)) (dir-files adir))]
                                   [else (dir-files adir)]))) alod))



(check-expect (clean-directory (list mjoconnell) 'mjoconnell)
              (list (make-dir 'mjoconnell empty (list motd hwrk3.rkt))))
(check-expect (clean-directory (list mjoconnell) 'cdixon) (list mjoconnell))

;;find-file-path : list[filesystem] string -> list[directories] or false
;;consumes a filesystem and a filename
;;produces the path to that filename or false if the filename is not in the filesystem

#|
(define (find-file-path alod name)
  (cond [(> (length (filter (lambda (adir)
                              (filter
                               (> (length
                               (lambda (afile)
                                 (string=? (file-name afile) name))
                               (dir-files adir)) 0) alod))) 0)
         (filter (lambda (adir) (filter
                                 (lambda (afile)
                                   (string=? (file-name afile) name))
                                 (dir-files adir))))]
        [else false]))

|#