;;;;; This is -*- epsilon -*-
;;;;; Sieve of Eratosthenes

;;;;; Copyright (C) 2016 Luca Saiu

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


;;;;; Global definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: ensure that the value matches the target machine.
(e1:define sieve:bits-per-cell
  (fixnum:- configuration:bits-per-word 3))


;;;;; Bit twiddling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (sieve:cellindex-bitmask buffer bit-index)
  (e1:let* (((cell-index bit-offset)
             (fixnum:/% bit-index sieve:bits-per-cell))
            (bit-mask (fixnum:left-shift 1 bit-offset)))
    (e1:bundle cell-index bit-mask)))

(e1:define (sieve:get-bit buffer bit-index)
  (e1:let* (((cell-index bit-mask) (sieve:cellindex-bitmask buffer bit-index)))
    (fixnum:bitwise-and (buffer:get buffer cell-index)
                        bit-mask)))

(e1:define (sieve:set-bit! buffer bit-index value)
  (e1:let* (((cell-index bit-mask) (sieve:cellindex-bitmask buffer bit-index))
            (old-cell (buffer:get buffer cell-index))
            (new-cell (e1:if value
                        (fixnum:bitwise-or old-cell bit-mask)
                        (fixnum:bitwise-and old-cell (fixnum:bitwise-not bit-mask)))))
    (buffer:set! buffer cell-index new-cell)))


;;;;; Sieve
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (sieve:make-buffer bit-no quiet)
  ;; FIXME: define (and use here) a fixnum:/-rounded-up procedure.
  (e1:let* ((buffer-cell-no (fixnum:1+ (fixnum:/ bit-no sieve:bits-per-cell)))
            (buffer (buffer:make buffer-cell-no)))
    (e1:dotimes (i buffer-cell-no)
      (buffer:initialize! buffer i -1))
    (sieve:set-bit! buffer 0 #f)
    (sieve:set-bit! buffer 1 #f)
    (e1:let outer-loop ((i 2))
      (e1:when (fixnum:< i bit-no)
        (e1:unless quiet
          (fio:write (i i) "\n"))
        ;; FIXME: comment.  The commented-out line coming next isn't needed as
        ;; long as I print primes on the fly, as I'm doing now.
        #;(sieve:set-bit! buffer i #t)
        (e1:let inner-loop ((j (fixnum:* i 2)))
          (e1:when (fixnum:< j bit-no)
            (sieve:set-bit! buffer j #f)
            (inner-loop (fixnum:+ j i))))
        (outer-loop (sieve:next-prime-from buffer bit-no (fixnum:1+ i) ))))
    buffer))

(e1:define (sieve:next-prime-from buffer bit-no i)
  (e1:cond ((fixnum:>= i bit-no)
            bit-no)
           ((sieve:get-bit buffer i)
            i)
           (else
            (sieve:next-prime-from buffer bit-no (fixnum:1+ i)))))

#;(e1:define (sieve:prime? buffer n)
  (sieve:get-bit buffer n))
#;(e1:define (sieve:print-primes buffer bit-no)
  (e1:let loop ((i 0))
    (e1:when (fixnum:< i bit-no)
      (e1:when (sieve:prime? buffer i)
        (fio:write (i i) "\n"))
      (loop (fixnum:1+ i)))))


;;;;; Command line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Of course we want --help, --version and -- to finalize option processing.
(command-line:set-common-options)

;;; Add the other options we need
(command-line:add-options (("--quiet" "-q")
                           "sieve but don't print the list of primes"))

;;; Set the information displayed by --version and --help .
(command-line:set-info!
    #:program-name "sieve (GNU epsilon)"
    #:usage "sieve [option] LIMIT"
    #:program-version configuration:package_version
    #:bug-email configuration:package_bugreport
    #:copyright "Copyright (C) Luca Saiu 2016"
    #:authors "Luca Saiu <http://ageinghacker.net>"
    #:introduction "Compute primes up to and not including the given limit."
    #:closing "This program is a benchmark, not really meant to be practical.")


;;;;; Main driver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (sieve:main)
  (command-line:process-args)
  (e1:let* ((arguments (command-line:non-option-list))
            (limit (e1:if (fixnum:= (list:length arguments) 1)
                     (reader:string->fixnum (list:first arguments))
                     (e1:error "There should be exactly one argument, the limit"))))
    (sieve:make-buffer limit
                       (command-line:option-supplied? "--quiet"))))
