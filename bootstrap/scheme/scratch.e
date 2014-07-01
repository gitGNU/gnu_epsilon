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


(e1:define-c64-multi-color-sprite sprite-stand "
. aaa
 aacc
 aacac
 aaccc
 accc
 ccc
 bccb
bbbcb
bbbbbc
 bbbb
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

(e1:define-c64-multi-color-sprite sprite-walk-1 "
. aaa
 aacc
 aacac
 aaccc
 accc
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
 bb bbb
 bb bbb
 c  bbb
 c  c
    ccc
")

(e1:define-c64-multi-color-sprite sprite-walk-2 "
. aaa
 aacc
 aacac
 aaccc
 accc
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

(e1:define-c64-multi-color-sprite sprite-walk-3 "
. aaa
 aacc
 aacac
 aaccc
 accc
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
  bbab
  bbbb
  bb c
  bb c
  c
  ccc
")

(e1:define-c64-multi-color-sprite sprite-punch-1 "
. aaa
 aacc
 aacac
 aaccc
 accc
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

(e1:define-c64-multi-color-sprite sprite-punch-2 "
.  aaa
  aacc
  aacac
  aaccc
  accc
  ccc
  bccbbbbbc
  bbcbbbbb
 bbbbbbb
 bbbbb
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

(e1:define-c64-multi-color-sprite sprite-kick-1 "
.
  aaa
 aacc
 aacac
 aaccc
 accc
 ccc
 bccb
bbbcb
bbbbbc
 bbbbb
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

(e1:define-c64-multi-color-sprite sprite-kick-2 "
.
  aaa
 aacc
 aacac
 aaccc
 accc
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

(e1:define current-configuration
  (box:make 0))

(e1:define (update-configuration)
  (e1:let* ((old (box:get current-configuration))
            (old+1 (fixnum:1+ old))
            (new (e1:if (fixnum:= old+1 (vector:length all-sprite-configurations))
                   0
                   old+1)))
    (box:set! current-configuration new)
    new))

(e1:define (update-sprite-0-configuration)
  (set-sprite-configuration! 0 (vector:get all-sprite-configurations
                                           (update-configuration))))

;;; FIXME: this is useful. move to epsilon1.scm.
(e1:define-macro (vector:vector . elements)
  `(vector:list->vector (list:list ,@elements)))

(e1:define-sum state
  (stand)
  (walk)
  (punch)
  (kick))

;; (e1:define state-stand 0)
;; (e1:define state-walk  1)
;; (e1:define state-punch 2)
;; (e1:define state-kick  3)

(e1:define state
  (box:make (state-stand)))

(e1:define sprite-configurations
  (tuple:make ;; 0: stand.
              (vector:vector sprite-stand)
              ;; 1: walk.
              (vector:vector sprite-walk-1 sprite-walk-2 sprite-walk-3 sprite-stand)
              ;; 2: punch.
              (vector:vector sprite-punch-1 sprite-punch-2 sprite-punch-1 sprite-stand)
              ;; 3: kick.
              (vector:vector sprite-kick-1 sprite-kick-2 sprite-kick-1 sprite-stand)))

(e1:define all-sprite-configurations
  (vector:append (buffer:get sprite-configurations (state-punch))
                 (buffer:get sprite-configurations (state-punch))
                 (buffer:get sprite-configurations (state-kick))
                 (buffer:get sprite-configurations (state-walk))
                 (buffer:get sprite-configurations (state-walk))
                 (buffer:get sprite-configurations (state-walk))
                 (buffer:get sprite-configurations (state-walk))
                 (buffer:get sprite-configurations (state-walk))
                 ))

(e1:define (test-sprites-interactively)
  ;; (set-sprite-xy-expandedness! 0 #f #f)
  (set-sprite-multi-color! 0 #t)
  (set-sprite-configuration! 0 sprite-stand)
  (set-sprite-xy-expandedness! 0 #f #f)
  (show-sprite! 0)
  (set-sprite-multi-color! 1 #t)
  (set-sprite-configuration! 1 sprite-stand)
  (set-sprite-xy-expandedness! 1 #f #t)
  (show-sprite! 1)
  (set-sprite-multi-color! 2 #t)
  (set-sprite-configuration! 2 sprite-stand)
  (set-sprite-xy-expandedness! 2 #t #f)
  (show-sprite! 2)
  (set-sprite-multi-color! 3 #t)
  (set-sprite-configuration! 3 sprite-stand)
  (set-sprite-xy-expandedness! 3 #t #t)
  (show-sprite! 3)

  (set-sprite-multi-color! 4 #t)
  (set-sprite-configuration! 4 sprite-stand)
  (set-sprite-xy-expandedness! 4 #f #f)
  (show-sprite! 4)
  (set-sprite-multi-color! 5 #t)
  (set-sprite-configuration! 5 sprite-stand)
  (set-sprite-xy-expandedness! 5 #f #t)
  (show-sprite! 5)
  (set-sprite-multi-color! 6 #t)
  (set-sprite-configuration! 6 sprite-stand)
  (set-sprite-xy-expandedness! 6 #t #f)
  (show-sprite! 6)
  (set-sprite-multi-color! 7 #t)
  (set-sprite-configuration! 7 sprite-stand)
  (set-sprite-xy-expandedness! 7 #t #t)
  (show-sprite! 7)

  (set-sprite-multi-color-color-0! c64:color-black)
  (set-sprite-multi-color-color-1! c64:color-pink)
  (set-sprite-color! 0             c64:color-light-grey)

  (show-sprite! 4)
  (show-sprite! 5)
  (show-sprite! 6)
  (show-sprite! 7)

  (test-sprites-interactively-loop 50 200))

(e1:define joystick 1)
(e1:define (test-sprites-interactively-loop x y)
  ;;(update-sprite-0-configuration)
  (move-sprite! 0 x y)
  (move-sprite! 1 (fixnum:+ x 30) (fixnum:- y 21))
  (move-sprite! 2 (fixnum:+ x 60) y)
  (move-sprite! 3 (fixnum:+ x 110) (fixnum:- y 21))
  (move-sprite! 4 x (fixnum:- y 100))
  (move-sprite! 5 (fixnum:+ x 30) (fixnum:- y 121))
  (move-sprite! 6 (fixnum:+ x 60) (fixnum:- y 100))
  (move-sprite! 7 (fixnum:+ x 110) (fixnum:- y 121))
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
        (update-sprite-0-configuration)
        (e1:let ((c (vector:get all-sprite-configurations (box:get current-configuration))))
          (set-sprite-configuration! 1 c)
          (set-sprite-configuration! 2 c)
          (set-sprite-configuration! 3 c)

          (set-sprite-configuration! 4 c)
          (set-sprite-configuration! 5 c)
          (set-sprite-configuration! 6 c)
          (set-sprite-configuration! 7 c)
          )
        (test-sprites-interactively-loop (fixnum:+ x dx #;4) (fixnum:+ y dy)))
      (test-sprites-interactively-loop (fixnum:+ x dx) (fixnum:+ y dy)))))

(e1:define (delay)
  (delay-helper 100))
(e1:define (delay-helper n)
  (e1:if (fixnum:zero? n)
    (e1:bundle)
    (delay-helper (fixnum:1- n))))

(e1:define c64:jiffy-mask
  (fixnum:bitwise-not (fixnum:left-shift 1 15)))
#;(e1:define (c64:get-jiffies)
  ;; The jiffy count is stored in three bytes.  I ignore the most significant one at 160.
  (e1:let* ((high-byte (io:load-byte 161))
            (low-byte  (io:load-byte 162))
            (two-bytes (fixnum:bitwise-or (fixnum:256* high-byte)
                                          low-byte)))
    (fixnum:logic-right-shift-1-bit two-bytes)))

(e1:define (fixnum:negate fixnum)
  (fixnum:- 0 fixnum))
(e1:define (io:write-fixnum file fixnum)
  (e0:if-in (fixnum:< fixnum (e0:value 0)) (#f)
    (io:write-non-negative-fixnum file fixnum)
    (e0:let () (io:write-character file (e0:value #\-))
      ;(e1:if (fixnum:< (fixnum:negate fixnum) 0)
        ;(e1:error "negating a negative yields another negative")
        (io:write-non-negative-fixnum file (fixnum:negate fixnum)))));)

(e1:define (go)
  ;;(fio:write (i (c64:get-jiffies)) "\n")
  ;;(fio:write "The timer is now: " (i (e1:primitive c64:read-timer)) "\n")
  ;;(fio:write (b (fixnum:< -50 50)) "\n")
  #;(e1:for1 (i -2000 -1980 1)
    (fio:write "negate(" (i i) ") = " (i (fixnum:negate i)) "\n"))
  (fio:write (i (e1:primitive c64:read-timer)) #\space)
  (go)
  )

(e1:define (move-sprite! n x y)
  (move-sprite-fast! n x y))

;;; Local Variables:
;;; show-trailing-whitespace: t
;;; End:
