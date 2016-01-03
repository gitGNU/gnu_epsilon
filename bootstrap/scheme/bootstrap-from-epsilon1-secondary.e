;;;;; This is -*- epsilon -*-.
;;;;; Bootstrapping code: generate REPL-generating image, from secondary:load.

;;;;; Copyright (C) 2015 Luca Saiu

;;;;; This file is part of GNU epsilon.

;;;;; GNU epsilon is free software: you can redistribute it and/or modify
;;;;; it under the terms of the GNU General Public License as published by
;;;;; the Free Software Foundation, either version 3 of the License, or
;;;;; (at your option) any later version.

;;;;; GNU epsilon is distributed in the hope that it will be useful,
;;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;; GNU General Public License for more details.

;;;;; You should have received a copy of the GNU General Public License
;;;;; along with GNU epsilon.  If not, see <http://www.gnu.org/licenses/>.


(e1:define bootstrap:unexec-repl-image-file-name
  (string:append configuration:abs_top_builddir
                 configuration:dir_separator
                 "dumps/unexec-repl-from-epsilon1.u"))
(e1:define bootstrap:unexec-repl-source-file-name
  (string:append configuration:abs_top_srcdir
                 configuration:dir_separator
                 "bootstrap/scheme/unexec-repl.e"))
(e1:define bootstrap:check-for-spills-source-file-name
  (string:append configuration:abs_top_srcdir
                 configuration:dir_separator
                 "bootstrap/scheme/check-for-spills.e"))
(e1:define bootstrap:repl-image-file-name
  (string:append configuration:abs_top_builddir
                 configuration:dir_separator
                 "dumps/repl-from-epsilon1.u"))

(fio:write "About to unexec an epsilon1 image loading the rest into "
           (st bootstrap:unexec-repl-image-file-name) "...\n")

;;; Invalidate the secondary symbol table, which shouldn't be visible after unexec
;;; and shouldn't be dumped.
(e1:define secondary:symbol-table 12345)

;; ------------------
;;(e1:begin
(fio:write "Still alive, working to unexec a REPL.  Hello from "
           (st bootstrap:unexec-repl-source-file-name)
           "\n")
(fio:write "Destroying procedure macro names (1)...\n")

;;; Unset macro procedure names for a few macros which I need to define
;;; within e1:toplevel for compatibility with my Scheme implementation;
;;; doing that is a little messy, and causes procedure macros to leak
;;; from the primary to the secondary symbol table, which I absolutely
;;; want to avoid.  I'm going to delete this horrible code later.
(e1:define (plug-spills)
  (e1:dolist (symbol (e1:value-list e1:define
                                    e1:define-macro
                                    e1:define-non-procedure
                                    e1:define-procedure
                                    e1:define-record
                                    e1:define-regexp
                                    e1:define-macro
                                    e1:define-sum
                                    e1:destructuring-bind
                                    e1:doiterator
                                    e1:dolist
                                    e1:unexec
                                    quote
                                    reader:atom-case
                                    reader:atom-item-list-box
                                    reader:define-simple-prefix
                                    reader:ignore-item-list-box
                                    reader:item-list-box
                                    sum:define
                                    unexec:unexec
                                    unexec:unexec-table
                                    unless-guile
                                    when-guile))
    (e1:when (state:macro? symbol)
      #;(fio:write (sy symbol) "\n")
      (state:invalidate-macro-procedure-name-cache-of! symbol))
    ;;(fio:write "...Still alive.  Rebuilding the macro cache...\n")
    #;(e1:dohash (_ symbol symbol:table)
      (e1:when (state:macro? symbol)
        (fio:write (sy symbol) "...\n")
        (state:macro-get-macro-procedure-name symbol) ;; ignore the result
        (fio:write "... done with " (sy symbol) ".\n")))
    (fio:write "...Still alive..\n")))

;;)
(e1:define-macro (when-guile . forms) `(e1:begin))
(e1:define-macro (unless-guile . forms) `(e1:begin ,@forms))
;; ------------------

(fio:write "Check that nothing spills from the original primary symbol table.")
(e1:load bootstrap:check-for-spills-source-file-name)
(plug-spills)
(check-for-spills)
(fio:write "Still alive after checking for spills.  Good!\n")

;;; I previously did this two-step bootstrapping, like for Guile: first I
;;; unexeced into a REPL generator, which when executed unexeced into a REPL.
;;; When bootstrapping from epsilon1 that's not really necessary, so instead of
;;; unexecing an e1:load form I just run it here, and unexec the REPL in a
;;; single step.  I'm keeping the old code here in this comment for the time
;;; being, but I mean to remove it.
;; (e1:unexec bootstrap:unexec-repl-image-file-name
;;   (fio:write "About to load " (st bootstrap:unexec-repl-source-file-name) "\n")
;;   (e1:load bootstrap:unexec-repl-source-file-name))
;; (fio:write "Now please run "
;;            (st bootstrap:unexec-repl-image-file-name)
;;            " to generate "
;;            (st bootstrap:repl-image-file-name)
;;            " .\n")
(e1:load bootstrap:unexec-repl-source-file-name)
(fio:write "Still alive after unexecing the REPL from epsilon1, using the secondary symbol table.  Good!\n")
