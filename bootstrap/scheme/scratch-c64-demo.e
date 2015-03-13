;;;;; This is -*- epsilon -*- with some Scheme
;;;;; Tentative code

;;;;; Copyright (C) 2014 Luca Saiu

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

;; (e1:define-macro (fio:write . stuff) '(e1:bundle))

(e1:define-6502-procedures)

(e1:define-c64-multi-color-sprite sprite-stand "
. aaa
 bbbb
baacac
baaccc
baccc
 ccc
 bccb
bbbcb
bbbbbc
bbbbb
 bbbbbbc
 bbbbbb
 aaaaa
 bbbbaa
 bbbabba
 bb abb
 bb  bb
 bb  bb
 bb  bb
 c   c
 ccc ccc
")

(e1:define sprite-stand-flipped
  (c64:flip-multi-color-sprite sprite-stand))


(e1:define-c64-multi-color-sprite sprite-walk-1 "
. aaa
 bbbb
baacac
baaccc
baccc
 ccc
 bccb
 bbcb
bbbbbbc
bbbbbb
 bbbbbc
 bbbbb
 aaaaa
 bbbbaa
  bbaba
 bbbab
 bb bb
 bb bb
 c  bb
 c  c
    ccc
")
(e1:define sprite-walk-1-flipped
  (c64:flip-multi-color-sprite sprite-walk-1))

(e1:define-c64-multi-color-sprite sprite-walk-2 "
. aaa
 bbbb
baacac
baaccc
baccc
 ccc
 bccb
bbbcb
bbbbbc
 bbbb
 bbbbbbc
 bbbbbb
 aaaaa
 bbbbaa
  bbaba
   bab
 cbbbbb
 cbbbb
 c bb
   c
   ccc
")
(e1:define sprite-walk-2-flipped
  (c64:flip-multi-color-sprite sprite-walk-2))

(e1:define-c64-multi-color-sprite sprite-walk-3 "
. aaa
 bbbb
baacac
baaccc
baccc
 ccc
 bccb
 bbcb
bbbbbbc
bbbbbb
 bbbbbc
 bbbbb
 aaaaa
 bbbbaa
 bbbaba
  bbabb
  bb bb
  bb  c
  bb  c
  c
  ccc
")
(e1:define sprite-walk-3-flipped
  (c64:flip-multi-color-sprite sprite-walk-3))

(e1:define-c64-multi-color-sprite sprite-punch-1 "
. aaa
 bbbb
baacac
baaccc
baccc
 ccc
 bccb
bbbcb
bbbbbbbbc
bbbbcbbb
 bbbbbb
 bbbb
 aaaaa
 bbbbaa
 bbbabba
 bbbabb
  bb bb
  bb bb
 bbb bb
 c   c
 ccc ccc
")
(e1:define sprite-punch-1-flipped
  (c64:flip-multi-color-sprite sprite-punch-1))

(e1:define sprite-low-punch-1 sprite-punch-1)
(e1:define sprite-low-punch-1-flipped sprite-punch-1-flipped)

;; (e1:define-c64-multi-color-sprite sprite-low-punch-2 "
;; .  aaa
;;   bbbb
;;  baacac
;; b aaccc
;; b accc
;;   ccc
;;   bccb
;;   bbcbb
;;  bbbbbbbbbc
;;  bbbbbbbbb
;;  bbbcbbb
;;   bbbb
;;   aaaaa
;;   bbbbaa
;;   bbbabb
;;   bbbabbb
;;    bb  bb
;;    bb bbb
;;   bbb bb
;;   c   c
;;   ccc ccc
;; ")

;; (e1:define-c64-multi-color-sprite sprite-low-punch-2 "
;; .  aaa
;;   bbbb
;;  baacac
;; b aaccc
;; b accc
;;   ccc
;;   bccb
;;   bbcbb
;;  bbbbbbb
;;  bbbbbbbbbc
;;  bbbcbbbbb
;;   bbbbb
;;   aaaaa
;;   bbbbaa
;;   bbbabb
;;   bbbabbb
;;    bb  bb
;;    bb bbb
;;   bbb bb
;;   c   c
;;   ccc ccc
;; ")

;; (e1:define-c64-multi-color-sprite sprite-low-punch-2 "
;; .  aaa
;;   bbbb
;;  baacac
;; b aaccc
;; b accc
;;   ccc
;;   bccb
;;   bbcb
;;  bbbbbb
;;  bbbbbbb
;;  bbbcbbbbbc
;;   bbbbbbbb
;;   aaaaa
;;   bbbbaa
;;   bbbabb
;;   bbbabbb
;;    bb  bb
;;    bb bbb
;;   bbb bb
;;   c   c
;;   ccc ccc
;; ")

(e1:define-c64-multi-color-sprite sprite-low-punch-2 "
.  aaa
  bbbb
 baacac
b aaccc
b accc
  ccc
  bccb
  bbcb
 bbbbb
 bbbbbb
 bbbcbbb
  bbbbbbbbc
  aaaaabbb
  bbbba
  bbbabb
  bbbabbb
   bb  bb
   bb bbb
  bbb bb
  c   c
  ccc ccc
")
(e1:define sprite-low-punch-2-flipped
  (c64:flip-multi-color-sprite sprite-low-punch-2))

(e1:define sprite-medium-punch-1 sprite-punch-1)
(e1:define sprite-medium-punch-1-flipped sprite-punch-1-flipped)

(e1:define-c64-multi-color-sprite sprite-medium-punch-2 "
.  aaa
  bbbb
 baacac
b aaccc
b accc
  ccc
  bccbbbbbc
  bbcbbbbb
 bbbbbbb
 bbbbbb
 bbbcb
  bbbb
  aaaaa
  bbbbaa
  bbbabb
  bbbabbb
   bb  bb
   bb bbb
  bbb bb
  c   c
  ccc ccc
")
(e1:define sprite-medium-punch-2-flipped
  (c64:flip-multi-color-sprite sprite-medium-punch-2))

;; (e1:define-c64-multi-color-sprite sprite-low-punch-2 "
;; .  aaa
;;   bbbb
;;  baacac
;; b aaccc
;; b accc
;;   ccc
;;   bccb
;;   bbcbbbbbc
;;  bbbbbbbbb
;;  bbbbbbb
;;  bbbcb
;;   bbbb
;;   aaaaa
;;   bbbbaa
;;   bbbabb
;;   bbbabbb
;;    bb  bb
;;    bb bbb
;;   bbb bb
;;   c   c
;;   ccc ccc
;; ")

(e1:define sprite-high-punch-1 sprite-punch-1)
(e1:define sprite-high-punch-1-flipped sprite-punch-1-flipped)

(e1:define-c64-multi-color-sprite sprite-high-punch-2 "
.  aaa
  bbbb
 baacac
b aaccc
b accc  bbc
  ccc  bbb
  bcc bbb
  bbcbbb
 bbbbbb
 bbbbbb
 bbbcb
  bbbb
  aaaaa
  bbbbaa
  bbbabb
  bbbabbb
   bb  bb
   bb bbb
  bbb bb
  c   c
  ccc ccc
")
(e1:define sprite-high-punch-2-flipped
  (c64:flip-multi-color-sprite sprite-high-punch-2))

(e1:define-c64-multi-color-sprite sprite-kick-1 "
.
  aaa
 bbbb
baacac
baaccc
baccc
 ccc
 bccb
bbbcb
bbbbbc
bbbbbb
 bbbbbc
 aaaaa
 bbbbaa
 bbbabbb
  bb  bbb
  bb   bb
 bbb   bb
 bb    ccc
 c
 ccc
")
(e1:define sprite-kick-1-flipped
  (c64:flip-multi-color-sprite sprite-kick-1))

(e1:define-c64-multi-color-sprite sprite-kick-2 "
.
  aaa
 bbbb
baacac
baaccc
baccc
 ccc
 bccb
bbbcb
bbbbbc
bbbbb
 bbbbca    c
 aaaaabb   c
 bbbbabbbbbc
 bbbabbbbbb
 bbb
  bb
 bbb
 bb
 c
 ccc
")
(e1:define sprite-kick-2-flipped
  (c64:flip-multi-color-sprite sprite-kick-2))

;;; !!!! EXPERIMENTAL !!!!
;(e1:define-macro (set-sprite-block! n b) `(set-sprite-configuration! ,n ,b))

(e1:define current-block
  (box:make 0))

(e1:define (update-block)
  (e1:let* ((old (box:get current-block))
            (old+1 (fixnum:1+ old))
            (new (e1:if (fixnum:= old+1 (vector:length (box:get all-sprite-blocks)))
                   0
                   old+1)))
    (box:set! current-block new)
    new))

(e1:define (update-sprite-0-block)
  (set-sprite-block! 0 (vector:get (box:get all-sprite-blocks)
                                   (update-block))))

;;; FIXME: this is useful. move to epsilon1.scm.
(e1:define-macro (vector:vector . elements)
  `(vector:list->vector (list:list ,@elements)))

(e1:define-sum state
  (stand)
  (walk)
  (low-punch)
  (medium-punch)
  (high-punch)
  (kick))


;; (e1:define state-stand        0)
;; (e1:define state-walk         1)
;; (e1:define state-low-punch    2)
;; (e1:define state-medium-punch 3)
;; (e1:define state-high-punch   4)
;; (e1:define state-kick         5)

(e1:define state
  (box:make (state-stand)))

;;; I actually initialize sprite-blocks with sprite configurations, and then
;;; convert configurations to blocks at initialization time.  This is to avoid
;;; an unexec problem: I can't reliably conserve the invariant according to which
;;; a given number is equal to a given *address* divided by 64 thru unexec.
(e1:define sprite-blocks
  (vector:vector ;; 0: stand.
                 (vector:vector sprite-stand)
                 ;; 1: walk.
                 (vector:vector sprite-walk-1 sprite-walk-2 sprite-walk-3 sprite-stand
                                ;;sprite-walk-1-flipped sprite-walk-2-flipped sprite-walk-3-flipped sprite-stand-flipped
                                )
                 ;; 2: low-punch.
                 (vector:vector sprite-low-punch-1 sprite-low-punch-2 sprite-low-punch-1 sprite-stand
                                sprite-low-punch-1-flipped sprite-low-punch-2-flipped sprite-low-punch-1-flipped sprite-stand-flipped)
                 ;; 2: medium-punch.
                 (vector:vector sprite-medium-punch-1 sprite-medium-punch-2 sprite-medium-punch-1 sprite-stand
                                sprite-medium-punch-1-flipped sprite-medium-punch-2-flipped sprite-medium-punch-1-flipped sprite-stand-flipped)
                 ;; 3: high-punch.
                 (vector:vector sprite-high-punch-1 sprite-high-punch-2 sprite-high-punch-1 sprite-stand
                                sprite-high-punch-1-flipped sprite-high-punch-2-flipped sprite-high-punch-1-flipped sprite-stand-flipped)
                 ;; 4: kick.
                 (vector:vector sprite-kick-1 sprite-kick-2 sprite-kick-1 sprite-stand
                                sprite-kick-1-flipped sprite-kick-2-flipped sprite-kick-1-flipped sprite-stand-flipped)
                 ))

(e1:define (convert-sprite-configurations-to-sprite-blocks!)
  (convert-sprite-configurations-to-sprite-blocks!-helper
      (fixnum:1- (vector:length sprite-blocks))))
(e1:define (convert-sprite-configurations-to-sprite-blocks!-helper i)
  (e1:unless (fixnum:< i 0)
    ;; (fio:write "i: " (i i)
    ;;            ", sprites: " (i (vector:get sprite-blocks i))
    ;;            ", length:" (i (vector:length (vector:get sprite-blocks i)))
    ;;            "\n")
    (convert-sprite-configuration-vector-to-sprite-blocks!-helper
        (vector:get sprite-blocks i)
        (fixnum:1- (vector:length (vector:get sprite-blocks i))))
    (convert-sprite-configurations-to-sprite-blocks!-helper (fixnum:1- i))))
(e1:define (convert-sprite-configuration-vector-to-sprite-blocks!-helper sprites j)
  (e1:unless (fixnum:< j 0)
    ;; (fio:write "sprites: " (i sprites) ", j: " (i j) ", sprite: " (i (vector:get sprites j)) "\n")
    ;; (fio:write "Updating element #" (i j) " of " (i sprites) ":\n")
    ;; (fio:write "* was:    " (i (vector:get sprites j)) "\n")
    (vector:set! sprites j (sprite->block (vector:get sprites j)))
    ;; (fio:write "* is now: " (i (vector:get sprites j)) "\n")
    (convert-sprite-configuration-vector-to-sprite-blocks!-helper sprites (fixnum:1- j))))

(e1:define all-sprite-blocks
  (box:make 0))

(e1:define (test-sprites-interactively)
  (convert-sprite-configurations-to-sprite-blocks!)
  ;; FIXME: avoid to append vectors at run time (mostly for generated code size).
  (box:set! all-sprite-blocks
            (vector:append (vector:get sprite-blocks (state-stand))
                           (vector:get sprite-blocks (state-low-punch))
                           (vector:get sprite-blocks (state-medium-punch))
                           (vector:get sprite-blocks (state-high-punch))
                           (vector:get sprite-blocks (state-kick))
                           ;; (vector:get sprite-blocks (state-walk))
                           ;; (vector:get sprite-blocks (state-walk))
                           ;; (vector:get sprite-blocks (state-walk))
                           ;; (vector:get sprite-blocks (state-walk))
                           ;; (vector:get sprite-blocks (state-walk))
                           (vector:get sprite-blocks (state-walk))
                           ;; (vector:get sprite-blocks (state-walk))
                           ;; (vector:get sprite-blocks (state-walk))
                           ;; (vector:get sprite-blocks (state-walk))
                           ;; (vector:get sprite-blocks (state-walk))
                           ;; (vector:get sprite-blocks (state-walk))
                           ;; (vector:get sprite-blocks (state-walk))
                           ;; (vector:get sprite-blocks (state-walk))
                           ;; (vector:get sprite-blocks (state-walk))
                           ))
  (set-sprite-multi-color! 0 #t)
  (set-sprite-block! 0 (vector:get (vector:get sprite-blocks (state-stand)) 0))
  (set-sprite-xy-expandedness! 0 #f #f)
  ;;(set-sprite-xy-expandedness! 0 #t #t)
  (show-sprite! 0)
  (set-sprite-multi-color! 1 #t)
  (set-sprite-block! 1 (vector:get (box:get (vector:get sprite-blocks (state-stand))) 0))
  (set-sprite-xy-expandedness! 1 #f #t)
  (show-sprite! 1)
  (set-sprite-multi-color! 2 #t)
  (set-sprite-block! 2 (vector:get (box:get (vector:get sprite-blocks (state-stand))) 0))
  (set-sprite-xy-expandedness! 2 #t #f)
  (show-sprite! 2)
  (set-sprite-multi-color! 3 #t)
  (set-sprite-block! 3 (vector:get (box:get (vector:get sprite-blocks (state-stand))) 0))
  (set-sprite-xy-expandedness! 3 #t #t)
  (show-sprite! 3)

  (set-sprite-multi-color! 4 #t)
  (set-sprite-block! 4 (vector:get (box:get (vector:get sprite-blocks (state-stand))) 0))
  (set-sprite-xy-expandedness! 4 #f #f)
  (show-sprite! 4)
  (set-sprite-multi-color! 5 #t)
  (set-sprite-block! 5 (vector:get (box:get (vector:get sprite-blocks (state-stand))) 0))
  (set-sprite-xy-expandedness! 5 #f #t)
  (show-sprite! 5)
  (set-sprite-multi-color! 6 #t)
  (set-sprite-block! 6 (vector:get (box:get (vector:get sprite-blocks (state-stand))) 0))
  (set-sprite-xy-expandedness! 6 #t #f)
  (show-sprite! 6)
  (set-sprite-multi-color! 7 #t)
  (set-sprite-block! 7 (vector:get (box:get (vector:get sprite-blocks (state-stand))) 0))
  (set-sprite-xy-expandedness! 7 #t #t)
  (show-sprite! 7)

  (set-sprite-multi-color-color-0! c64:color-black)
  (set-sprite-multi-color-color-1! c64:color-pink)
  ;;(set-sprite-color! 0             c64:color-light-grey)
  (set-sprite-color! 0             c64:color-light-grey)

  (io:store-byte! 53281 c64:color-dark-grey)

  ;;(move-sprite! 0 50 160)
  (test-sprites-interactively-loop 50 160))

(e1:define joystick 1)
(e1:define (test-sprites-interactively-loop x y)
  ;;(update-sprite-0-block)
  (move-sprite! 0 x y)
  (move-sprite! 1 (fixnum:+ x 30)  (fixnum:- y 21))
  (move-sprite! 2 (fixnum:+ x 60)  y)
  (move-sprite! 3 (fixnum:+ x 110) (fixnum:- y 21))
  (move-sprite! 4 x                (fixnum:- y 49))
  (move-sprite! 5 (fixnum:+ x 30)  (fixnum:- y 70))
  (move-sprite! 6 (fixnum:+ x 60)  (fixnum:- y 49))
  (move-sprite! 7 (fixnum:+ x 110) (fixnum:- y 70))
  (e1:let* ((joystick-state (joystick-state joystick))
            (dx (e1:cond ((joystick-left? joystick-state)  -1)
                         ((joystick-right? joystick-state) 1)
                         (else                             0)))
            (dy (e1:cond ((joystick-up? joystick-state)    -1)
                         ((joystick-down? joystick-state)  1)
                         (else                             0))))
    (e1:if (joystick-fire? joystick-state)
      (e1:begin
        (delay)
        (update-sprite-0-block)
        (e1:let ((b (vector:get (box:get all-sprite-blocks) (box:get current-block))))
          ;;(fio:write "b is now " (i b) "\n")
          (set-sprite-block! 1 b)
          (set-sprite-block! 2 b)
          (set-sprite-block! 3 b)

          (set-sprite-block! 4 b)
          (set-sprite-block! 5 b)
          (set-sprite-block! 6 b)
          (set-sprite-block! 7 b)
          )
        (test-sprites-interactively-loop (fixnum:+ x dx) (fixnum:+ y dy)))
      (test-sprites-interactively-loop (fixnum:+ x dx) (fixnum:+ y dy)))))

(e1:define (delay)
  (delay-helper 100))
(e1:define (delay-helper n)
  (e1:if (fixnum:zero? n)
    (e1:bundle)
    (delay-helper (fixnum:1- n))))

;; (e1:define c64:jiffy-mask
;;   (fixnum:bitwise-not (fixnum:left-shift 1 15)))
(e1:define (c64:get-jiffies)
  ;; The jiffy count is stored in three bytes.  I ignore the most significant one at 160.
  (e1:let* ((high-byte (fixnum:bitwise-and (io:load-byte 161)
                                           127)) ;; clear sign bit
            (low-byte  (io:load-byte 162)))
    (fixnum:bitwise-or (fixnum:256* high-byte)
                       low-byte)))

;;(e1:define (fixnum:negate fixnum)
;;  (fixnum:- 0 fixnum))

(e1:define (go)
  ;;(fio:write (i (c64:get-jiffies)) "\n")
  ;;(fio:write "The timer is now: " (i (e1:primitive c64:read-timer)) "\n")
  ;;(fio:write (b (fixnum:< -50 50)) "\n")
  ;; (e1:for1 (i -2000 -1980 1)
  ;;   (fio:write "negate(" (i i) ") = " (i (fixnum:negate i)) "\n"))
  (fio:write (i (c64:get-jiffies)) #\space)
  (go)
  )

(e1:define (move-sprite! n x y)
  (move-sprite-fast! n x y))

;;; Local Variables:
;;; show-trailing-whitespace: t
;;; End:
