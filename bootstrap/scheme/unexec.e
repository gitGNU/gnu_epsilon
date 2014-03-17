;;;;; This is -*- epsilon -*-, possibly with some Scheme.
;;;;; Marshalling and unexecing facility

;;;;; Copyright (C) 2012 Université Paris 13
;;;;; Copyright (C) 2012, 2013 Luca Saiu
;;;;; Updated in 2014 by Luca Saiu
;;;;; Written by Luca Saiu

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


;;; This is written using only epsilonzero with e1:define, because we
;;; want to avoid depending on any particular extension: unexecing can
;;; be used very early while bootstrapping, or for personalities
;;; radically different from my one.


;;; FIXME: move away this implementation in epsilon0, and only keep
;;; the final part calling primitives.


;;;;; File format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Dump format
;;; The dump is a sequence of 32bit big-endian integers ("words" from now on).
;;; - buffer definition no (1 word)
;;; - buffer definitions: each one is:
;;;   - buffer length (1 word)
;;;   - elements (2 * buffer length words): each element is:
;;;     - either atom-tag or pointer-tag (1 word)
;;;     - the atom in case of atom, or identifier in case of pointer (1 word)
;;; - main object (1 word)
;;; Pointer identifiers are allocated sequentially starting from zero.

(e1:define-secondary marshal:atom-tag (e0:value 0))
(e1:define-secondary marshal:pointer-tag (e0:value 1))


;;;;; Marshalling: epsilon0 implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Create a new file with the given name, and dump the given object
;;; into it.
(e1:define-secondary (marshal:marshal file-name object)
  (e0:let (file) (io:open-file file-name io:write-mode)
    (e0:let () (marshal:marshal-to-open-file file object)
      (io:close-file file))))

(e1:define-secondary (marshal:marshal-to-open-file file object)
  (e0:let (pointer-map) (unboxed-hash:make)
    (e0:let (pointers) (marshal:fill-pointer-map! pointer-map (list:singleton object))
      (e0:let () (io:write-32-bit-big-endian file (unboxed-hash:element-no pointer-map))
        (e0:let () (marshal:marshal-buffer-definitions! file pointer-map pointers)
          (marshal:marshal-object! file pointer-map object))))))

;;; While dumping we use an auxiliary data structure called the
;;; pointer map: it's a hash table using pointers as keys.  Each
;;; pointer is mapped into a unique identifier.

;;; Return the given worklist with the pointer elements of the given
;;; buffer prepended.  No side effects.
(e1:define-secondary (marshal:add-buffer-elements-to-worklist buffer worklist)
  (marshal:add-buffer-elements-to-worklist-aux buffer worklist (e0:value 0) (boxedness:buffer-length buffer)))
(e1:define-secondary (marshal:add-buffer-elements-to-worklist-aux buffer worklist from-index length)
  (e0:if-in (fixnum:= from-index length) (#f)
    (e0:let (candidate) (buffer:get buffer from-index)
      (e0:let (new-worklist) (e0:if-in (boxedness:buffer? candidate) (#f)
                               worklist
                               (list:cons candidate worklist))
        (marshal:add-buffer-elements-to-worklist-aux buffer
                                                    new-worklist
                                                    (fixnum:1+ from-index)
                                                    length)))
    worklist))

;;; Add all pointers reachable from the objects in the worklist to the
;;; pointer map.  Return the list of pointers stored in the hash, in
;;; the same order they were added to the table.
(e1:define-secondary (marshal:fill-pointer-map! pointer-map worklist)
  (marshal:fill-pointer-map-acc! pointer-map worklist list:nil))
(e1:define-secondary (marshal:fill-pointer-map-acc! pointer-map worklist acc)
  (e0:if-in (list:null? worklist) (#f)
    ;; worklist is not empty
    (e0:let (object) (list:head worklist)
      (e0:let (worklist-rest) (list:tail worklist)
        (e0:if-in (boxedness:thread? object) (#f)
          ;; object is not a thread: check for all the non-error cases:
          (e0:if-in (boxedness:buffer? object) (#f)
            ;; object is a non-thread non-buffer, hence an atom:
            (marshal:fill-pointer-map-acc! pointer-map worklist-rest acc)
            ;; object is a pointer
            (e0:if-in (unboxed-hash:has? pointer-map object) (#f)
              ;; object is a new pointer
              (e0:let (new-identifier) (unboxed-hash:element-no pointer-map)
                (e0:let (new-worklist) (marshal:add-buffer-elements-to-worklist object worklist-rest)
                  (e0:let () (unboxed-hash:set! pointer-map object new-identifier)
                    (marshal:fill-pointer-map-acc! pointer-map new-worklist (list:cons object acc)))))
              ;; object is a pointer we already know
              (marshal:fill-pointer-map-acc! pointer-map worklist-rest acc)))
          ;; object is a thread 
          (e1:error "marshal:fill-pointer-map-acc!: can't dump threads"))))
    ;; worklist is empty
    (list:reverse acc)))

;;; Dump the given object into the file, assuming the pointer map has
;;; already been filled.
(e1:define-secondary (marshal:marshal-object! file pointer-map object)
  (e0:if-in (boxedness:thread? object) (#f)
    ;; object is not a thread
    (e0:if-in (boxedness:buffer? object) (#f)
      ;; object is a non-thread non-buffer, hence an atom:
      (marshal:marshal-atom! file object)
      ;; object is a pointer:
      (marshal:marshal-pointer! file (unboxed-hash:get pointer-map object)))
    ;; object is a thread
    (e1:error "marshal:marshal: can't dump threads")))

;;; How to dump a word: first a tag, then the content.  In case of
;;; pointer, of course, the content is a pointer identifier, not thexs
;;; address.
(e1:define-secondary (marshal:marshal-atom! file atom)
  (e0:let () (io:write-32-bit-big-endian file marshal:atom-tag)
    (io:write-32-bit-big-endian file atom)))
(e1:define-secondary (marshal:marshal-pointer! file identifier)
  (e0:let () (io:write-32-bit-big-endian file marshal:pointer-tag)
    (io:write-32-bit-big-endian file identifier)))

;;; Dump a buffer definition into the file, assuming the pointer map has
;;; already been filled.
(e1:define-secondary (marshal:marshal-buffer-definition! file pointer-map pointer)
  (e0:let (length) (boxedness:buffer-length pointer)
    (e0:let () (io:write-32-bit-big-endian file length)
      (marshal:marshal-buffer-content! file pointer-map pointer (e0:value 0) length))))
(e1:define-secondary (marshal:marshal-buffer-content! file pointer-map buffer from-index length)
  (e0:if-in (fixnum:= from-index length) (#f)
    (e0:let () (marshal:marshal-object! file pointer-map (buffer:get buffer from-index))
      (marshal:marshal-buffer-content! file pointer-map buffer (fixnum:1+ from-index) length))
    ;; from-index is equal to length
    (e0:bundle)))

;;; Dump all buffer definitions into the file.
(e1:define-secondary (marshal:marshal-buffer-definitions! file pointer-map pointers)
  (e0:if-in (list:null? pointers) (#f)
    (e0:let () (marshal:marshal-buffer-definition! file pointer-map (list:head pointers))
      (marshal:marshal-buffer-definitions! file pointer-map (list:tail pointers)))
    ;; pointers is empty
    (e0:bundle)))


;;;;; Unmarshalling: epsilon0 implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Read an object from the file whose name is given, and return the
;;; object.
(e1:define-secondary (marshal:unmarshal file-name)
  (e0:let (file) (io:open-file file-name io:read-mode)
    (e0:let (result) (marshal:unmarshal-from-open-file file)
      (e0:let () (io:close-file file)
        result))))

;;; While undumping we use an auxiliary data structure called the
;;; identifier buffer: it's a buffer indexed by fixnum identifiers.
;;; Each buffer element contains a buffer.  This has conceptually the
;;; opposite role of the pointer map.

(e1:define-secondary (marshal:unmarshal-from-open-file file)
  (e0:let (buffer-no) (io:read-32-bit-big-endian file)
    (e0:let (identifier-buffer) (buffer:make buffer-no)
      ;; the tag buffer is a buffer of vectors (so that we store
      ;; its length) of which each element represents the tag of
      ;; the corresponding identifier buffer element:
      (e0:let (tag-buffer) (buffer:make buffer-no)
        (e0:let () (marshal:fill-identifier-and-tag-buffers! file identifier-buffer tag-buffer (e0:value 0) buffer-no)
          (e0:let () (marshal:resolve-pointers! identifier-buffer tag-buffer (e0:value 0) buffer-no)
            ;; Read the final object
            (e0:let (trivial-buffer) (buffer:make (e0:value 1))
              (e0:let (trivial-tag-vector) (vector:make (e0:value 1))
                (e0:let () (marshal:set-buffer-definition! file trivial-buffer trivial-tag-vector (e0:value 0) (e0:value 1))
                  (e0:let () (marshal:resolve-pointers-in-buffer! identifier-buffer trivial-buffer trivial-tag-vector (e0:value 0) (e0:value 1))
                    (e0:let (result) (buffer:get trivial-buffer (e0:value 0))
                      (e0:let () (buffer:destroy identifier-buffer)
                        (e0:let () (marshal:destroy-tag-buffer tag-buffer (e0:value 0) buffer-no)
                          (e0:let () (buffer:destroy trivial-buffer)
                            (e0:let () (vector:destroy trivial-tag-vector)
                              result)))))))))))))))

(e1:define-secondary (marshal:fill-identifier-and-tag-buffers! file identifier-buffer tag-buffer from-index length)
  (e0:if-in (fixnum:= from-index length) (#f)
    (e0:let (this-buffer-length) (io:read-32-bit-big-endian file)
      (e0:let (this-tag-vector) (vector:make this-buffer-length)
        (e0:let (this-buffer) (buffer:make this-buffer-length)
          (e0:let () (buffer:set! identifier-buffer from-index this-buffer)
            (e0:let () (buffer:set! tag-buffer from-index this-tag-vector)
              (e0:let () (marshal:set-buffer-definition! file this-buffer this-tag-vector (e0:value 0) this-buffer-length)
                (marshal:fill-identifier-and-tag-buffers! file identifier-buffer tag-buffer (fixnum:1+ from-index) length)))))))
    ;; from-index is equal to length
    (e0:bundle)))
(e1:define-secondary (marshal:set-buffer-definition! file buffer tag-vector from-index length)
  (e0:if-in (fixnum:= from-index length) (#f)
    (e0:let (tag) (io:read-32-bit-big-endian file)
      (e0:let (element) (io:read-32-bit-big-endian file)
        (e0:let () (vector:set! tag-vector from-index tag)
          (e0:let () (buffer:set! buffer from-index element)
            (marshal:set-buffer-definition! file buffer tag-vector (fixnum:1+ from-index) length)))))
    ;; from-index is equal to length
    (e0:bundle)))

;;; Scan the tag buffer: for each pointer tag, fix the corresponding
;;; element of the identifier buffer, replacing the identifier with
;;; the correct pointer.
(e1:define-secondary (marshal:resolve-pointers! identifier-buffer tag-buffer from-index length)
  (e0:if-in (fixnum:= from-index length) (#f)
    (e0:let (this-tag-vector) (buffer:get tag-buffer from-index)
      (e0:let () (marshal:resolve-pointers-in-buffer! identifier-buffer
                                                     (buffer:get identifier-buffer from-index)
                                                     this-tag-vector
                                                     (e0:value 0)
                                                     (vector:length this-tag-vector))
        (marshal:resolve-pointers! identifier-buffer tag-buffer (fixnum:1+ from-index) length)))
    ;; from-index is equal to length
    (e0:bundle)))
(e1:define-secondary (marshal:resolve-pointers-in-buffer! identifier-buffer buffer tag-vector from-index length)
  (e0:if-in (fixnum:= from-index length) (#f)
    (e0:let () (e0:if-in (fixnum:= (vector:get tag-vector from-index) marshal:pointer-tag) (#f)
                 ;; an atom: nothing to do
                 (e0:bundle)
                 ;; a pointer
                 (buffer:set! buffer from-index (buffer:get identifier-buffer (buffer:get buffer from-index))))
      (marshal:resolve-pointers-in-buffer! identifier-buffer buffer tag-vector (fixnum:1+ from-index) length))
    ;; from-index is equal to length
    (e0:bundle)))

;;; Destroy the given buffer of vectors, including its elements:
(e1:define-secondary (marshal:destroy-tag-buffer tag-buffer from-index length)
  (e0:if-in (fixnum:= from-index length) (#f)
    ;; not the end yet
    (e0:let () (vector:destroy (buffer:get tag-buffer from-index))
      (marshal:destroy-tag-buffer tag-buffer (fixnum:1+ from-index) length))
    ;; we arrived at the end: destroy the vector-containing buffer
    (buffer:destroy tag-buffer)))


;;;;; Use C primitives to replace the code above
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: this makes all the previous code obsolete: even if I want
;;;        to keep it as a proof of concept, I should move it away.

;;; Replace the versions above with primitive wrappers:
(e1:define-secondary (marshal:marshal-to-open-file file object)
  (e0:primitive marshal:marshal-to-open-file file object))
(e1:define-secondary (marshal:unmarshal-from-open-file file)
  (e0:primitive marshal:unmarshal-from-open-file file))


;;;;; Marshalling: user interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Make a file with the given name and dump the given object into it:
(e1:define-secondary (marshal:marshal file-name object)
  (e0:let (file) (io:open-file file-name io:write-mode)
    (e0:let () (marshal:marshal-to-open-file file object)
      (io:close-file file))))


;;;;; Unmarshalling: user interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return the object contained in the file with the given name:
(e1:define-secondary (marshal:unmarshal file-name)
  (e0:let (file) (io:open-file file-name io:read-mode)
    (e0:let (result) (marshal:unmarshal-from-open-file file)
      (e0:let () (io:close-file file)
        result))))


;;;;; Unexec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-secondary unexec:default-file
  (e0:value "default-unexecing.m"))

;;; The user-friendly versions require a syntactic extension so that
;;; the expression can be expressed in concrete syntax.  These are
;;; the core procedures our extended syntax will reduce to calling.
(e1:define-secondary (unexec:unexec-table-procedure file-name table expression)
  (marshal:marshal file-name (cons:make table
                                        expression)))
(e1:define-secondary (unexec:quick-unexec-table-procedure table expression)
  (unexec:unexec-table-procedure unexec:default-file
                                 table
                                 expression))


;;;;; Exec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-secondary (unexec:exec file-name)
  ;; We can't use e1:define here: this may be executed before its epsilon1 definition
  (e0:let (pair) (marshal:unmarshal file-name)
    (e0:let (new-symbol-table) (cons:car pair)
      (e0:let (main-expression) (cons:cdr pair)
        (e0:let () (state:global-set! (e0:value symbol:table) new-symbol-table)
          (e0:eval main-expression alist:nil))))))
(e1:define-secondary (unexec:quick-exec)
  (unexec:exec unexec:default-file))
