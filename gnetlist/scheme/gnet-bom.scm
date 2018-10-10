; -*-scheme-*-
;
;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
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


;; --------------------------------------------------------------------------
;;
;; Bill of Material backend written by Matt Ettus starts here
;;

;;; Bill Of Materials Generator
;;; Outputs a BOM to a file, the first line in the file is a header line
;;; containing the list of attribute names followed by lines with corresponding
;;; attribute values for all schematic components that do not have a "nobom=1"
;;; attribute. The following gnetlist options may be used to provide the list
;;; of attribute names:
;;;
;;;  - -Oattribs=<attrib-names>, where <attrib-names> is a list of attribute
;;;                              names separated by commas without spaces,
;;;  - -Oattrib_file=<filename>.
;;;
;;; The second option defines the file from which the attribute names will
;;; be read, if the first option was not given. If neither option is given,
;;; the procedure attempts to read the attribute names from file "attribs"
;;; in the current directory. If the attribute names are read from a source
;;; file, they must be separated by spaces, tabs, or newlines. An error will
;;; be displayed if no attribute name source is found.
;;; No comments are allowed in the file.
;;;
;;; Questions? Contact matt@ettus.com
;;; This software is released under the terms of the GNU GPL

(use-modules (ice-9 rdelim) ;; guile-1.8 fix
             (gnetlist backend-getopt))

;;--------------------------------------------------------------------
;;  Display attribute input error.
;;--------------------------------------------------------------------
(define (bom:error-no-attribs filename)
  "Prints an error when the bom backend cannot find an appropriate
source to read attributes and exits with return code 1."
  (format
   (current-error-port)
   "ERROR: Attribute file '~A' not found. You must do one of the following: ~%
         - Create an 'attribs' file ~%
         - Specify an attribute file using -O attrib_file=<filename> ~%
         - Specify which attributes to include using -O attribs=attrib1,attrib2,... (no spaces) ~%"
          filename)
  (primitive-exit 1))


;;--------------------------------------------------------------------
;;  Opens attribute file.
;;--------------------------------------------------------------------
(define bom:open-input-file
  (lambda (options)
    (let ((filename (backend-option-ref options 'attrib_file "attribs")))
      (if (file-exists? filename)
          (open-input-file filename)
          (if (not (backend-option-ref options 'attribs))
              (bom:error-no-attribs filename))))))


;;--------------------------------------------------------------------
;;  Read attributes from a file.
;;--------------------------------------------------------------------
(define (bom:read-attrib-list)
  "Translates text read from the standard input into a list of attribute
names. The attribute names must be delimited by spaces, tabs, or newlines."
  (let ((read-from-file (read-delimited " \n\t")))
    (cond ((eof-object? read-from-file)
           '())
          ((= 0 (string-length read-from-file))
           (bom:read-attrib-list))
          (else
           (cons read-from-file (bom:read-attrib-list))))))


;;--------------------------------------------------------------------
;;  Parses attrib file or argument. Returns a list of read attributes.
;;--------------------------------------------------------------------
(define (bom:parseconfig port options)
  "Reads attribute names from the list ATTRIBS which must be a CSV list.
If ATTRIBS is #f, reads the list of attributes standard input."
    (let ((attribs (backend-option-ref options 'attribs)))
      (if attribs (string-split attribs #\,)
          (and port
               (with-input-from-port port bom:read-attrib-list)))))


;;--------------------------------------------------------------------
;;  Write attribute values to output file.
;;--------------------------------------------------------------------
(define (bom:printlist ls)
  "Outputs the given list LS to the standard output as a tab separated list."
  (format #t "~A\n" (string-join ls "\t")))


;;--------------------------------------------------------------------
;;  Collects component output data
;;--------------------------------------------------------------------
(define (bom:components ls attriblist)
  "Outputs a tab separated list of attribute values for each attribute name
in ATTRIBLIST for components in LS not having an attribute \"nobom=1\""
  (define (no-bom-package? package)
    (string=? "1" (get-package-attribute package "nobom")))

  (define (component-attrib-values package attriblist)
    (map (lambda (attrib)
           (get-package-attribute package attrib))
         attriblist))

  (define (output-component-attrib-values package)
    (and (not (no-bom-package? package))
         (bom:printlist
          (cons package (component-attrib-values package attriblist)))))

  (for-each output-component-attrib-values ls))

;;--------------------------------------------------------------------
;;  Main Routine
;;--------------------------------------------------------------------
(define (bom output-filename)
  "Outputs BOM (Bill of Materials) to OUTPUT-FILENAME."
  (write-log "running gnet-bom backend")
  (let* ((options (backend-getopt (get-backend-arguments)
                    '((attrib_file (value #t)) (attribs (value #t)))))
         (port (bom:open-input-file options))
         (attriblist (bom:parseconfig port options)))
      (and attriblist
         (with-output-to-port (output-port output-filename)
           (lambda ()
             (bom:printlist (cons "refdes" attriblist))
             (bom:components packages attriblist))))))

;;
;; Bill of Material backend written by Matt Ettus ends here
;;
;; --------------------------------------------------------------------------
