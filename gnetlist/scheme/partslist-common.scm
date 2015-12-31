; -*-scheme-*-
;
;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;
;;; Copyright (C) 2001-2015 MIYAMOTO Takanori
;;; gnet-partslist-common.scm
;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA, <http://www.gnu.org/licenses/>.

(define (get-parts-table packages)
  (if (null? packages)
     '()
      (let ((package (car packages)))
        (if (string=? (get-device package) "include")
            (get-parts-table (cdr packages))
            (cons (list package
               (get-device package)
               (get-value package)
               (get-package-attribute package "footprint")) ;; sdb change
               (get-parts-table (cdr packages)))))))

(define (write-one-row ls separator end-char)
  (if (null? ls)
     '()
      (begin (display (car ls))
        (for-each (lambda (st) (display separator)(display st)) (cdr ls))
        (display end-char))))

(define (get-sortkey-value ls key-column)
  (list-ref (car ls) key-column))

(define (merge-sort-sub ls1 ls2 key-column)
  (if (or (null? ls1) (null? ls2))
      (append ls1 ls2)
      (if (string-ci<=? (get-sortkey-value ls1  key-column) (get-sortkey-value ls2 key-column))
          (cons (car ls1) (merge-sort-sub (cdr ls1) ls2 key-column))
          (cons (car ls2) (merge-sort-sub ls1 (cdr ls2) key-column)))))

(define (merge-sort ls key-column)
  (let ((midpoint (inexact->exact (floor (/ (length ls) 2)))))
    (if (<= (length ls) 1)
        (append ls)
        (let ((top-half (reverse (list-tail (reverse ls) midpoint)))
              (bottom-half (list-tail ls (- (length ls) midpoint))))
             (set! top-half (merge-sort top-half key-column))
             (set! bottom-half (merge-sort bottom-half key-column))
             (merge-sort-sub top-half bottom-half key-column)))))

(define (merge-sort-with-multikey ls key-columns)
  (if (or (<= (length ls) 1) (null? key-columns))
      (append ls)
      (let* ((key-column (car key-columns))
             (sorted-ls (merge-sort ls key-column))
             (key-column-only-ls
                ((lambda (ls) (let loop ((l ls))
                 (if (null? l)
                '()
                 (cons (get-sortkey-value l key-column) (loop (cdr l))))))
                  sorted-ls))
             (first-value (get-sortkey-value sorted-ls key-column))
             (match-length (length (member first-value (reverse key-column-only-ls))))
             (first-ls (list-tail (reverse sorted-ls) (- (length sorted-ls) match-length)))
             (rest-ls (list-tail sorted-ls match-length)))
        (append (merge-sort-with-multikey first-ls (cdr key-columns))
             (merge-sort-with-multikey rest-ls key-columns)))))
