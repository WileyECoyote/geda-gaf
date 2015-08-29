;; -*-Scheme-*-
;; gEDA - GPL Electronic Design Automation
;; libgeda - gEDA's library - Scheme API
;;
;; Copyright (C) 2010-2015 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2011-2015 gEDA Contributors (see ChangeLog for details)
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA
;;

(use-modules (geda os) (ice-9 optargs) (ice-9 ftw))
(define path-sep separator)
(define geda-data-path (car (sys-data-dirs)))
(define geda-rc-path (car (sys-config-dirs)))

(define (build-path first . rest)
  (if (null? rest) first
      (apply build-path
	     (append (list (string-append first path-sep (car rest)))
		     (cdr rest)))))

;; Returns #t if the given path is a regular file, otherwise #f.
(define regular-file?
  (lambda (path)
    (eqv? (stat:type (stat path)) 'regular )
  ))

;; Returns #t if the given path is a directory file, otherwise #f.
(define directory?
  (lambda (path)
    (eqv? (stat:type (stat path)) 'directory )
  ))

;; Returns #t if the given string ends with the given suffix, otherwise or #f.
(define has-suffix?
  (lambda (str suf)
    (define len-str (string-length str))
    (define len-suf (string-length suf))
    (if (>= len-str len-suf)
      (string=? (substring str (- len-str len-suf) len-str) suf)
      #f
    )))

;; Execute any scheme files found in the given directory.
(define load-scheme-dir
  (lambda (scheme-dir)
  (if (and (file-exists? scheme-dir)
           (directory? scheme-dir)
           (access? scheme-dir R_OK))
    (let ((dir (opendir scheme-dir)))
      (do ((entry (readdir dir) (readdir dir)))
          ((eof-object? entry))
        (let ((path (build-path scheme-dir entry)))
          (if (and (regular-file? path)
                   (has-suffix? path ".scm")
                   (access? path R_OK))
            (eval-protected `(primitive-load ,path))
            #f
          )))
      (closedir dir))
    #f
  )))

;; Add all symbol libraries found below DIR to be searched for
;; components, naming them with an optional PREFIX.
(define* (component-library-search rootdir  #:optional prefix)
  (let ((dht (make-hash-table 31))
        (rootdir (expand-env-variables rootdir)))
    ;; Build symbol directory list
    (ftw rootdir
         (lambda (filename statinfo flags)
           (cond
            ((eq? 'invalid-stat flags)
             (error "Invalid path ~S." filename))
            ((or (eq? 'directory-not-readable flags)
                 (eq? 'symlink flags))
             (format #t "Warning: Cannot access ~S.\n" filename))
            (else
             (and (eq? 'regular flags)
                  (string-suffix-ci? ".sym" filename)
                  (hashq-set! dht
                              (string->symbol (dirname filename))
                              #t))))
           #t))

    ; Fill component library tree
    (for-each
     (lambda (dir)
       (let ((name (substring dir (string-length rootdir))))
         (component-library dir
                            (if prefix
                                (string-append prefix name)
                                name))))
     (sort-list! (hash-map->list (lambda (key val)
                                   (symbol->string key))
                                 dht)
                 string>?))))
