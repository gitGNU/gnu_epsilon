(e1:toplevel
;;;;; This is -*- epsilon -*- with just a little Scheme
;;;;; Tentative code

;;;;; Copyright (C) 2013  Jérémie Koenig

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


;;; Pretty-printing
;;;
;;; Reference:
;;;   John Hughes, The Design of a Pretty-printing Library.
;;;   In J. Jeuring and E. Meijer, editors, Advanced Functionnal Programming,
;;;   volume 925. Springer Verlag, 1995.
;;;
;;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.38.8777&rep=rep1&type=pdf
;;;
;;; TODO:
;;;  * format string interpreter
;;;  * abbreviation: we specify a width and height and the pretty-printer
;;;    writes out as much information as it can, replacing some nodes with
;;;    "..." to save space.
;;;  * define pp:fixed-point

;;;;; Documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Documents are built from the following combinators:
;;;  * text "string": builds the one-line document which is composed only of
;;;    the given string.
;;;  * nest /k/ /doc/: shifts the whole document to the right so as to
;;;    add k characters of indentation.
;;;  * /doc/ $$ /doc/ (vertical composition): the second document is laid out
;;;    below the first one, so that the lines of the composite document
;;;    consists of the concatenation of the lines of the parts.
;;;  * /doc/ <> /doc/ (horizontal composition): the second document is
;;;    translated so that its first non-indentation character appears
;;;    directly to the right document's last character, and so that its
;;;    two-dimensional structure is preserved.
;;;  * /doc/ <+> /doc/ is equivalent to '/doc/ <> text " " <> /doc/'.
;;;  * sep /doc/ .. /doc/: builds a documents which will be printed out
;;;    either as '/doc/ <+> .. <+> /doc/' if the result fits the requested
;;;    page width, or as '/doc/ $$ .. $$ /doc/' otherwise. The outermost
;;;    sep's switch to vertical composition first.
;;;
;;; We represent documents with the following sum-of-products structure,
;;; which mostly follows the combinators used to build it. However, to
;;; facilitate 'sep' decisions when the document is printed out, we sometimes
;;; store width information along with the combinator's arguments. The
;;; width corresponds to the width the document would have if all of its
;;; 'sep' combinators were to be laid out horizontally.
;;;
;;; Furthermore, we require that the following invariants hold for all
;;; documents:
;;;  * nest occurs only as the topmost layer or as the second argument of $$
;;;  * the first doc of a union always has the greater width (that is, it
;;;    corresponds to the horizontal branch of the corresponding 'sep').
;;;
(sum:define pp:doc
  (text s)
  (nest k doc)
  (<> width doc1 doc2)
  ($$ width doc1 doc2)
  (union width hdoc vdoc))

;;; Retreive (in constant time) the maximum width of the document
(e1:define (pp:width doc)
  (e1:match doc
    ((pp:doc-text s) (string:length s))
    ((pp:doc-nest k doc) (fixnum:+ k (pp:width doc)))
    ((pp:doc-<> width _ _) width)
    ((pp:doc-$$ width _ _) width)
    ((pp:doc-union width _ _) width)))

;;; Simple text docuemnt
(e1:define (pp:text s)
  (pp:doc-text s))

;;; Indent doc by k spaces
(e1:define (pp:nest k doc)
  (e1:match doc
    ((pp:doc-nest j subdoc)
     (pp:doc-nest (fixnum:+ j k) subdoc))
    (_
     (pp:doc-nest k doc))))

;;; Horizontal composition
(e1:define (pp:<> doc1 doc2)
  (e1:match doc1
    ; Keep nest outside
    ((pp:doc-nest k doc)
     (pp:doc-nest k (pp:<> doc doc2)))
    (_
     (e1:match doc2
       ; Remove any indentation in doc2
       ((pp:doc-nest _ doc)
        (pp:<> doc1 doc))
       (_
        (pp:doc-<> (fixnum:+ (pp:width doc1) (pp:width doc2)) doc1 doc2))))))

(variadic:define-associative pp:<> pp:<> (pp:text ""))

(e1:define (pp:<+> doc1 doc2)
  (pp:<> doc1 (pp:text " ") doc2))

(variadic:define-associative pp:<+> pp:<+>
  (e1:error "pp:<+>: at least one argument is required"))

(e1:define (pp:$$ doc1 doc2)
  (e1:match doc1
    ((pp:doc-nest k doc)
     (pp:doc-nest k (pp:$$ doc (pp:nest (fixnum:negate k) doc2))))
    (_
     (pp:doc-$$ (fixnum:max (pp:width doc1) (pp:width doc2)) doc1 doc2))))

(variadic:define-associative pp:$$ pp:$$
  (e1:error "pp:$$: at least one argument is required"))

;;; Forms the union of hdoc and vdoc: the resulting document will be
;;; rendered out as hdoc if we have enough space, or vdoc otherwise.
(e1:define (pp:union hdoc vdoc)
  (e1:match (tuple:make hdoc vdoc)
    ; Propagate equivalent nests upward
    ((tuple (pp:doc-nest hk hx) (pp:doc-nest vk vx))
     (e1:if (fixnum:= hk vk)
       (pp:nest hk (pp:union hx vx))
       (e1:error "pp:union: Unbalanced nests")))

    ; Nest should be in both or neither document
    ((or (tuple (pp:doc-nest _ _) _)
         (tuple _ (pp:doc-nest _ _)))
     (e1:error "pp:union: Unbalanced nest"))

    (_ (pp:doc-union (pp:width hdoc) hdoc vdoc))))

;;; Construct the 'sep' of a list of documents, using closure h for
;;; horizontal composition, and closure v for vertical composition.
(e1:define (pp:sephvl h v docs)
  (e1:let
    ((hdoc (list:fold h (list:head docs) (list:tail docs)))
     (vdoc (list:fold v (list:head docs) (list:tail docs))))
    (pp:union hdoc vdoc)))

;;; Usually, we want <+> as h and $$ as v
(e1:define (pp:sepl docs)
  (pp:sephvl (e1:lambda (x y) (pp:<+> x y))
             (e1:lambda (x y) (pp:$$ x y))
             docs))

;;; But sometimes we need <> for horizontal composition instead
(e1:define (pp:sepnl docs)
  (pp:sephvl (e1:lambda (x y) (pp:<> x y))
             (e1:lambda (x y) (pp:$$ x y))
             docs))

(e1:define-macro (pp:sep . docs)
  `(pp:sepl (list:list ,@docs)))

(e1:define-macro (pp:sepn . docs)
  `(pp:sepnl (list:list ,@docs)))

;;; Choose the best layout of a document and write it to the given i/o port
(e1:define (pp:write file w doc)
  (pp:write-pos file 0 w doc)
  (io:write-character file character:newline))

;;; In order to properly indent new lines, we need to keep track of the
;;; current horizontal position
(e1:define (pp:write-pos file pos w doc)
  (e1:match doc
    ((pp:doc-text s)
     (io:write-string file s))

    ((pp:doc-nest k doc)
     (pp:write-indent file k)
     (pp:write-pos file (fixnum:+ pos k) w doc))

    ((pp:doc-<> _ doc1 doc2)
     (pp:write-pos file pos (fixnum:- w (pp:width doc2)) doc1)
     (pp:write-pos file (fixnum:+ pos (pp:width doc1)) w doc2))

    ((pp:doc-$$ _ doc1 doc2)
     (pp:write-pos file pos w doc1)
     (io:write-character file character:newline)
     (pp:write-indent file pos)
     (pp:write-pos file pos w doc2))

    ((pp:doc-union width hdoc vdoc)
     (e1:if (fixnum:<= (fixnum:+ pos width) w)
       (pp:write-pos file pos w hdoc)
       (pp:write-pos file pos w vdoc)))))

(e1:define (pp:write-indent file k)
  (e1:cond
    ((fixnum:< k 0)
     (e1:error "pp:write-indent: attempted to write negative indentation"))
    ((fixnum:> k 0)
     (io:write-character file #\ )
     (pp:write-indent file (fixnum:1- k)))))

;;; Handy short hand
(e1:define (pp:print doc)
  (pp:write (io:standard-output) 72 doc))

;;;;; Creating doc's for various types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (pp:pp doc)
  (e1:match doc
    ((pp:doc-text s)
     (pp:sep (pp:text "(text")
             (pp:nest 2 (pp:string s))
	     (pp:nest 2 (pp:text ")"))))
    ((pp:doc-nest k doc)
     (pp:sep (pp:<+> (pp:text "(nest") (pp:fixnum k))
             (pp:nest 2 (pp:<> (pp:pp doc) (pp:text ")")))))
    ((pp:doc-<> _ doc1 doc2)
     (pp:sep (pp:text "(<>")
             (pp:nest 2 (pp:pp doc1))
	     (pp:nest 2 (pp:<> (pp:pp doc2) (pp:text ")")))))
    ((pp:doc-$$ _ doc1 doc2)
     (pp:sep (pp:text "($$")
             (pp:nest 2 (pp:pp doc1))
	     (pp:nest 2 (pp:<> (pp:pp doc2) (pp:text ")")))))
    ((pp:doc-union _ doc1 doc2)
     (pp:sep (pp:text "(union")
             (pp:nest 2 (pp:pp doc1))
	     (pp:nest 2 (pp:<> (pp:pp doc2) (pp:text ")")))))))

(e1:define (pp:empty-list value)
  (pp:text "()"))

(e1:define (pp:boolean value)
  (pp:text (e1:if value "#t" "#f")))

(e1:define (pp:fixnum-fullblown base minus digits value)
  (e1:if (fixnum:< value 0)
    (pp:<>
      (pp:character minus)
      (pp:fixnum-fullblown base minus digits (fixnum:negate value)))
    (e1:let
      ((q (fixnum:/ value base))
       (r (fixnum:% value base)))
      (pp:<>
        (e1:if (fixnum:= q 0)
          (pp:text "")
          (pp:fixnum-fullblown base minus digits q))
        (pp:character (string:get digits r))))))

(e1:define pp:regular-digits "0123456789abcdef")

(e1:define pp:subscript-digits
  (vector:list->vector
    (list:list #x2080 #x2081 #x2082 #x2083 #x2084
               #x2085 #x2086 #x2087 #x2088 #x2089)))

(e1:define (pp:fixnum value)
  (pp:fixnum-fullblown 10 #\- pp:regular-digits value))

(e1:define (pp:fixnum-subscript value)
  (pp:fixnum-fullblown 10 #x208b pp:subscript-digits value))

(e1:define (pp:fixnum-hex value)
  (pp:<> (pp:text "0x")
         (pp:fixnum-fullblown 16 #\- pp:regular-digits value)))

(e1:define (pp:scons value)
  (e1:let*
    ((docs (pp:scons:doclist value))
     (ndocs (e1:if (sexpression:list? (cons:car value))
              docs
              (list:cons
                (list:head docs)
                (list:map (e1:lambda (x) (pp:nest 1 x))
                  (list:tail docs))))))
    (pp:<> (pp:text "(") (pp:sepl ndocs) (pp:text ")"))))

(e1:define (pp:scons:doclist value)
  (e1:let ((car (cons:car value))
           (cdr (cons:cdr value)))
    (list:cons
      (pp:sexpression car)
      (e1:cond
        ((sexpression:cons? cdr)
         (pp:scons:doclist (sexpression:eject cdr)))
        ((sexpression:null? cdr)
         list:nil)
        (else
         (list:list (pp:text ".") (pp:sexpression cdr)))))))

(e1:define (pp:character value)
  (pp:text (string:character->string value)))

(e1:define (pp:string value)
  (pp:<> (pp:character #\")
         (pp:text value) ; FIXME: escape
         (pp:character #\")))

(e1:define (pp:symbol value)
  (pp:text (symbol:symbol->string value)))

;;; TODO: (e1:define (pp:fixed-point))

(e1:define (pp:expression value)
  (e1:match value
    ((e0:expression-variable handle name)
     (pp:<> (pp:symbol name) (pp:fixnum-subscript handle)))

    ((e0:expression-value handle content)
     (pp:<> (pp:fixnum content) (pp:fixnum-subscript handle)))

    ((e0:expression-bundle handle items)
     (pp:<> (pp:sepl
              (list:cons
                (pp:text "[bundle")
                (list:map
                  (e1:lambda (item) (pp:nest 2 (pp:expression item)))
                  items)))
            (pp:text "]")
            (pp:fixnum-subscript handle)))

    ((e0:expression-primitive handle name actuals)
     (pp:<>
       (pp:sepl
         (list:cons
           (pp:sep (pp:text "[primitive") (pp:nest 2 (pp:symbol name)))
           (list:map
             (e1:lambda (arg) (pp:nest 2 (pp:expression arg)))
             actuals)))
       (pp:text "]")
       (pp:fixnum-subscript handle)))

    ((e0:expression-let handle bound-variables bound-expression body)
     (pp:<>
       (pp:sep
         (pp:sep
           (pp:text "[let")
           (pp:nest 2
             (pp:<> (pp:text "[")
                    (pp:sepl (list:map (e1:lambda (var) (pp:symbol var))
                                       bound-variables))
                    (pp:text "] be")))
           (pp:nest 2 (pp:<+> (pp:expression bound-expression) (pp:text "in"))))
         (pp:nest 2 (pp:expression body)))
       (pp:text "]")
       (pp:fixnum-subscript handle)))

    ((e0:expression-call handle name actuals)
     (pp:<>
       (pp:sepl
         (list:cons
           (pp:sep (pp:text "[call") (pp:nest 2 (pp:symbol name)))
           (list:map
             (e1:lambda (arg) (pp:nest 2 (pp:expression arg)))
             actuals)))
       (pp:text "]")
       (pp:fixnum-subscript handle)))

    ((e0:expression-call-indirect handle name actuals)
     (pp:<>
       (pp:sepl
         (list:cons
           (pp:sep (pp:text "[call-indirect")
                   (pp:nest 2 (pp:expression name)))
           (list:map
             (e1:lambda (arg) (pp:nest 2 (pp:expression arg)))
             actuals)))
       (pp:text "]")
       (pp:fixnum-subscript handle)))

    ((e0:expression-if-in handle discriminand values then-branch else-branch)
     (pp:<>
       (pp:sep
         (pp:sep (pp:text "[if")
                 (pp:nest 2
                   (pp:expression discriminand))
                 (pp:nest 2
                   (pp:<> (pp:character #x2208)
                          (pp:text " {")
                          (pp:sepl (list:map (e1:lambda (val) (pp:value val))
                                             values))
                          (pp:text "} then"))))
         (pp:<+> (pp:nest 2 (pp:expression then-branch))
                 (pp:text "else"))
         (pp:nest 2 (pp:expression else-branch)))
       (pp:text "]")
       (pp:fixnum-subscript handle)))

    ((e0:expression-fork handle name actuals)
     (pp:<>
       (pp:sepl
         (list:cons
           (pp:sep (pp:text "[fork") (pp:nest 2 (pp:symbol name)))
           (list:map
             (e1:lambda (arg) (pp:nest 2 (pp:expression arg)))
             actuals)))
       (pp:text "]")
       (pp:fixnum-subscript handle)))

    ((e0:expression-join handle future)
     (pp:<>
       (pp:sep
         (pp:text "[join")
         (pp:nest 2 (pp:expression future)))
       (pp:text "]")
       (pp:fixnum-subscript handle)))

    (_
     (pp:<> (pp:text "[unknown expression ")
            (pp:fixnum-hex value)
            (pp:text "]")))))

(e1:define (pp:value value)
  (pp:value-up-to pp:value-maximum-depth value))

(e1:define pp:value-maximum-depth 10)

(e1:define (pp:value-up-to max-depth value)
  (e1:cond
    ((whatever:buffer? value)
     (pp:<>
       (pp:sepnl
         (list:list
           (pp:<> (pp:fixnum-hex value)
                  (pp:text "["))
           (pp:sepl
             (list:map
               (e1:lambda (elem) (pp:nest 2 elem))
               (pp:buffer-elements value (fixnum:1- max-depth) 0)))))
       (pp:text "]")))
    (else
     (pp:fixnum value))))

(e1:define (pp:buffer-elements buf max-depth from)
  (e1:cond
    ((fixnum:<= max-depth 0)
     (list:list (pp:text "...")))
    ((fixnum:>= from (buffer:length buf))
     list:nil)
    (else
     (list:cons
       (pp:value-up-to max-depth (buffer:get buf from))
       (pp:buffer-elements buf max-depth (fixnum:1+ from))))))

(e1:define (pp:sexpression sexpression)
  (e1:let*
    ((value (sexpression:eject sexpression))
     (tag (sexpression:get-tag sexpression))
     (printer (sexpression:type-tag->pretty-printer-procedure-name tag)))
    (e0:call-indirect printer value)))

)
