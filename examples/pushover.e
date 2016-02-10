;;;;; This is -*- epsilon -*-
;;;;; Pushover game

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


;;;;; Temporary, to ease compilation: simplify assertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Do not print s-expressions: that currently doesn't work in compiled
;;; programs, relying as it does on the symbol table.
(e1:define (printer:write-sexpression port arg)
  (io:write-string port "[omitted s-expression]"))

;; (e1:define assertion-no
;;   (box:make 0))
;; (e1:define-macro (e1:assert expression . stuff)
;;   (e1:let ((i (box:bump-and-get! assertion-no)))
;;     (fio:write "** The " (i i) "-th assertion is " (se expression) "\n")
;;     `(e1:unless ,expression
;;        (fio:write "About the " (i ,(sexpression:inject-fixnum i)) "-th assertion (1-based)\n")
;;        (e1:error "Assertion failed"))))


;;;;; Board cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is also used to represent the current player in a state;
;;; of course the empty case is invalid in that context.
(e1:define-sum po:case
  (empty)
  (black)
  (white))

(e1:define (po:player->string case)
  (e1:match case
    ((po:case-empty) (e1:assert #f))
    ((po:case-black) "Black")
    ((po:case-white) "White")
    (else
     (fio:write "About ")
     (fio:write (i case))
     ;;(e0:primitive debug:dump case)
     (fio:write ": not a player (or even an empty case)\n")
     (e1:assert #f))))

(e1:define (po:flip-player case)
  (e1:match case
    ((po:case-black) (po:case-white))
    ((po:case-white) (po:case-black))
    ((po:case-empty) (e1:assert #f))))


;;;;; Board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The direction expresses the side the pawn is pushed from:
;;; "left" stands for "left-to-right".
(e1:define-sum po:direction
  (left)
  (right)
  (top)
  (bottom))

(e1:define-record po:board
  size
  matrix) ;; the matrix is a buffer of row buffers

(e1:define (po:make-row size)
  (e1:let ((result (buffer:make size)))
    (e1:dotimes (i size)
      (buffer:initialize! result i (po:case-empty)))
    result))

(e1:define (po:make-board size)
  (e1:let ((matrix (buffer:make size)))
    (e1:dotimes (i size)
      (buffer:set! matrix i (po:make-row size)))
    (po:board size matrix)))

(e1:define (po:matrix-get matrix row-index column-index)
  (e1:let ((row (buffer:get matrix row-index)))
    (buffer:get row column-index)))

(e1:define (po:board-get board row-index column-index)
  (e1:let ((matrix (po:board-get-matrix board)))
    (po:matrix-get matrix row-index column-index)))

(e1:define (po:matrix-set! matrix row-index column-index new-case)
  (e1:let ((row (buffer:get matrix row-index)))
    (buffer:set! row column-index new-case)))

(e1:define (po:board-set! board row-index column-index new-case)
  (e1:let* ((matrix (po:board-get-matrix board)))
    (po:matrix-set! matrix row-index column-index new-case)))

(e1:define (po:clone-board board)
  (e1:let* ((size (po:board-get-size board))
            (source-matrix (po:board-get-matrix board))
            (result (po:make-board size))
            (result-matrix (po:board-get-matrix result)))
    (e1:dotimes (row-index size)
      (e1:dotimes (column-index size)
        (e1:let ((case (po:matrix-get source-matrix row-index column-index)))
          (po:matrix-set! result-matrix row-index column-index case))))
    result))


;;;;; Terminal escape sequences (this should be generalized...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This can be set to disable terminal escape sequences.
(e1:define po:ansi-terminal-escapes
  (box:make #t))

(e1:define po:escape-character-string
  (e1:let ((s (vector:make 1)))
    (vector:set! s 0 27)
    s))
(e1:define po:terminal-noattr-string
  (string:append po:escape-character-string "[0m"))
(e1:define po:terminal-black-string
  (string:append po:escape-character-string "[1m"
                 po:escape-character-string "[34m"))
(e1:define po:terminal-white-string
  (string:append po:escape-character-string "[1m"
                 ;;po:escape-character-string "[37m" ;; white
                 po:escape-character-string "[33m" ;; yellow
                 ))
(e1:define (po:terminal-noattr)
  (e1:if (box:get po:ansi-terminal-escapes)
    po:terminal-noattr-string
    ""))
(e1:define (po:terminal-black)
  (e1:if (box:get po:ansi-terminal-escapes)
    po:terminal-black-string
    ""))
(e1:define (po:terminal-white)
  (e1:if (box:get po:ansi-terminal-escapes)
    po:terminal-white-string
    ""))

(e1:define (po:player->escape-string case)
  (e1:match case
    ((po:case-empty) "")
    ((po:case-black) (po:terminal-black))
    ((po:case-white) (po:terminal-white))))

(e1:define (po:player->escaped-string case)
  (string:append (po:player->escape-string case)
                 (po:player->string case)
                 (po:terminal-noattr)))


;;;;; Output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (po:print-board board)
  (e1:let ((size (po:board-get-size board)))
    (fio:write "  ")
    (e1:dotimes (column-index size)
      (fio:write (i (fixnum:1+ column-index)) " "))
    (fio:write "\n")
    (e1:dotimes (row-index size)
      (fio:write (i (fixnum:1+ row-index)) " ")
      (e1:dotimes (column-index size)
        (e1:match (po:board-get board row-index column-index)
          ((po:case-empty)
           (fio:write (st ". ")))
          ((po:case-black)
           (fio:write (st (po:terminal-black))
                      "B"
                      (st (po:terminal-noattr))
                      " "))
          ((po:case-white)
           (fio:write (st (po:terminal-white))
                      "W"
                      (st (po:terminal-noattr))
                      " "))))
      (fio:write "\n"))))


;;;;; Access to rows and columns as lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (po:get-row board row-index)
  (list:reverse (po:get-reversed-row board row-index)))

(e1:define (po:get-reversed-row board row-index)
  (e1:let* ((size (po:board-get-size board))
            (matrix (po:board-get-matrix board))
            (row-as-buffer (buffer:get matrix row-index)))
    (po:buffer-and-size->reversed-list row-as-buffer size)))

(e1:define (po:buffer-and-size->reversed-list row-as-buffer size)
  (e1:let ((result-box (box:make list:nil)))
    (e1:dotimes (i size)
      (box:set! result-box (list:cons (buffer:get row-as-buffer i)
                                      (box:get result-box))))
    (box:get result-box)))

(e1:define (po:get-column board column-index)
  (list:reverse (po:get-reversed-column board column-index)))

(e1:define (po:get-reversed-column board column-index)
  (e1:let* ((size (po:board-get-size board))
            (matrix (po:board-get-matrix board))
            (result-box (box:make list:nil)))
    (e1:dotimes (i size)
      (box:set! result-box (list:cons (po:matrix-get matrix i column-index)
                                      (box:get result-box))))
    (box:get result-box)))

(e1:define (po:set-row! board row-index row-as-list)
  (e1:let loop ((rest row-as-list)
                (column-index 0))
    (e1:unless (list:null? rest)
      (po:board-set! board row-index column-index (list:head rest))
      (loop (list:tail rest)
            (fixnum:1+ column-index)))))
(e1:define (po:set-reversed-row! board row-index row-as-list)
  (po:set-row! board row-index (list:reverse row-as-list)))

(e1:define (po:set-column! board column-index column-as-list)
  (e1:let loop ((rest column-as-list)
                (row-index 0))
    (e1:unless (list:null? rest)
      (po:board-set! board row-index column-index (list:head rest))
      (loop (list:tail rest)
            (fixnum:1+ row-index)))))
(e1:define (po:set-reversed-column! board column-index column-as-list)
  (po:set-column! board column-index (list:reverse column-as-list)))


;;;;; Moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-record po:move
  direction
  index) ;; row or column index, 0-based.


;;;;; Moves on lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (po:can-push-into-list? list case)
  (e1:or (list:has? list (po:case-empty))
         (whatever:eq? (list:last list) case)))

(e1:define (po:push-into-list list case)
  (e1:if (list:has? list (po:case-empty))
    (po:push-into-list-without-spilling list case)
    (list:without-last (list:cons case list))))

(e1:define (po:push-into-list-without-spilling list case)
  (e1:let ((head (list:head list))
           (tail (list:tail list)))
    (e1:match head
      ((po:case-empty)
       (list:cons case tail))
      (else
       (list:cons case (po:push-into-list-without-spilling tail head))))))


;;;;; Moves on boards (destructive version)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (po:board-move! board case move)
  (e1:let ((direction (po:move-get-direction move))
           (index (po:move-get-index move)))
    (e1:match direction
      ((po:direction-left)
       (e1:let ((list (po:get-row board index)))
         (po:set-row! board index (po:push-into-list list case))))
      ((po:direction-right)
       (e1:let ((list (po:get-reversed-row board index)))
         (po:set-reversed-row! board index (po:push-into-list list case))))
      ((po:direction-top)
       (e1:let ((list (po:get-column board index)))
         (po:set-column! board index (po:push-into-list list case))))
      ((po:direction-bottom)
       (e1:let ((list (po:get-reversed-column board index)))
         (po:set-reversed-column! board index (po:push-into-list list case)))))))


;;;;; Move possibility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (po:board-can-move? board case move)
  (e1:let ((size (po:board-get-size board))
           (direction (po:move-get-direction move))
           (index (po:move-get-index move)))
    (e1:and (fixnum:>= index 0)
            (fixnum:< index size)
            (e1:match direction
              ((po:direction-left)
               (e1:let ((list (po:get-row board index)))
                 (po:can-push-into-list? list case)))
              ((po:direction-right)
               (e1:let ((list (po:get-reversed-row board index)))
                 (po:can-push-into-list? list case)))
              ((po:direction-top)
               (e1:let ((list (po:get-column board index)))
                 (po:can-push-into-list? list case)))
              ((po:direction-bottom)
               (e1:let ((list (po:get-reversed-column board index)))
                 (po:can-push-into-list? list case)))))))

(e1:define po:all-directions
  (list:list (po:direction-left)
             (po:direction-right)
             (po:direction-top)
             (po:direction-bottom)))

(e1:define (po:board-possible-moves board case)
  (e1:let ((size (po:board-get-size board))
           (result-box (box:make list:nil)))
    (e1:dolist (direction po:all-directions)
      (e1:dotimes (index size)
        (e1:let ((move (po:move direction index)))
          (e1:when (po:board-can-move? board case move)
            (box:set! result-box
                      (list:cons move (box:get result-box)))))))
    (box:get result-box)))


;;;;; Move printing and parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Notice that the user notation is 1-based, differently from
;;; the internal notation.

(e1:define (po:move->string move)
  (e1:let ((direction (po:move-get-direction move))
           (index (po:move-get-index move))
           (result (vector:make 2)))
    (vector:set! result 0 (e1:match direction
                            ((po:direction-left)   #\L)
                            ((po:direction-right)  #\R)
                            ((po:direction-top)    #\T)
                            ((po:direction-bottom) #\B)))
    (vector:set! result 1 (fixnum:+ #\1 index))
    result))

(e1:define (po:print-move move)
  (fio:write (st (po:move->string move))))

;;; This checks the syntactic validity of a move, which might still be or not be
;;; possible on a given board.
(e1:define (po:valid-move-as-string? string)
  (e1:and (fixnum:= (string:length string) 2)
          (list:has? (e1:value-list #\L #\l #\R #\r #\T #\t #\B #\b)
                     (string:get string 0))
          (fixnum:>= (string:get string 1) #\1)
          (fixnum:<= (string:get string 1) #\9)))

;;; This assumes the string to encode a valid move.
(e1:define (po:string->move string)
  (e1:let ((index (fixnum:- (string:get string 1) #\1)))
    (e1:case (string:get string 0)
      ((#\L #\l) (po:move (po:direction-left) index))
      ((#\R #\r) (po:move (po:direction-right) index))
      ((#\T #\t) (po:move (po:direction-top) index))
      ((#\B #\b) (po:move (po:direction-bottom) index)))))


;;;;; Winning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum po:outcome
  (in-progress)
  (draw)
  (black-wins)
  (white-wins))

(e1:define (po:flip-outcome outcome)
  (e1:match outcome
    ((or (po:outcome-in-progress)
         (po:outcome-draw))
     outcome)
    ((po:outcome-black-wins)
     (po:outcome-white-wins))
    ((po:outcome-white-wins)
     (po:outcome-black-wins))))

(e1:define (po:all-equal? list element)
  (e1:cond ((list:null? list)
            #t)
           ((whatever:eq? element (list:head list))
            (po:all-equal? (list:tail list) element))
           (else
            #f)))

(e1:define (po:update-score! list black-box white-box)
  (e1:cond ((po:all-equal? list (po:case-black))
            (box:bump! black-box))
           ((po:all-equal? list (po:case-white))
            (box:bump! white-box))))

(e1:define (po:board->outcome board)
  (e1:let ((size (po:board-get-size board))
           (black-box (box:make 0))
           (white-box (box:make 0)))
    (e1:dotimes (i size)
      (e1:let ((row (po:get-reversed-row board i))
               (column (po:get-reversed-column board i)))
        (po:update-score! row black-box white-box)
        (po:update-score! column black-box white-box)))
    (e1:let ((black-score (box:get black-box))
             (white-score (box:get white-box)))
      (e1:cond ((e1:and (fixnum:zero? black-score)
                        (fixnum:zero? white-score))
                (po:outcome-in-progress))
               ((fixnum:= black-score white-score)
                (po:outcome-draw))
               ((fixnum:> black-score white-score)
                (po:outcome-black-wins))
               (else
                (po:outcome-white-wins))))))

(e1:define (po:outcome->string outcome)
  (e1:match outcome
    ((po:outcome-in-progress)
     "Game in progress")
    ((po:outcome-draw)
     "Draw")
    ((po:outcome-black-wins)
     (string:append (po:player->escaped-string (po:case-black)) " wins"))
    ((po:outcome-white-wins)
     (string:append (po:player->escaped-string (po:case-white)) " wins"))))


;;;;; Game state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-record po:state
  board
  player) ;; a case, either black or white -- empty is not allowed.

(e1:define (po:make-state size)
  (e1:let ((board (po:make-board size)))
    (po:state board
              (po:case-black))))

(e1:define (po:state->outcome state)
  (e1:let ((board (po:state-get-board state)))
    (po:board->outcome board)))

(e1:define (po:possible-moves state)
  (e1:let ((board (po:state-get-board state))
           (player (po:state-get-player state)))
    (po:board-possible-moves board player)))

(e1:define (po:print-possible-moves state)
  (e1:let ((board (po:state-get-board state))
           (player (po:state-get-player state)))
    (fio:write "The possible moves for "
               (st (po:player->escaped-string player))
               " are:\n  ")
    (e1:let ((moves (po:board-possible-moves board player)))
      (e1:dolist (move moves)
        (po:print-move move)
        (fio:write " "))
      (fio:write "\n"))))

(e1:define (po:can-move? state move)
  (e1:let ((board (po:state-get-board state))
           (player (po:state-get-player state)))
    (po:board-can-move? board player move)))

(e1:define (po:move! state move)
  (e1:let ((board (po:state-get-board state))
           (player (po:state-get-player state)))
    (po:board-move! board player move)
    (po:state-set-player! state (po:flip-player player))))

(e1:define (po:print-state state)
  (e1:let ((board (po:state-get-board state))
           (player (po:state-get-player state))
           (outcome (po:state->outcome state)))
    (po:print-board board)
    (e1:match outcome
      ((po:outcome-in-progress)
       (po:print-possible-moves state))
      (else
       (fio:write (st (po:outcome->string outcome)) ".\n\n")))))

(e1:define (po:clone-state state)
  (e1:let ((board (po:state-get-board state))
           (player (po:state-get-player state)))
    (po:state (po:clone-board board)
              player)))


;;;;; Interactive play
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (po:read-move state)
  (e1:let* ((player (po:state-get-player state))
            (string-from-user (fio:write (st (po:player->escaped-string player)))
                              (io:readline)))
    (e1:cond ((fixnum:zero? string-from-user)
              (fio:write "\nGoodbye.\n")
              (unix:exit 0))
             (bind (string-from-user (string:trim string-from-user)))
             ((po:valid-move-as-string? string-from-user)
              (e1:let ((move (po:string->move string-from-user)))
                (e1:if (po:can-move? state move)
                  move
                  (e1:begin
                    (fio:write "Impossible move.\n")
                    (po:print-state state)
                    (po:read-move state)))))
             (else
              (fio:write "Invalid move " (S string-from-user) ".\n")
              (po:print-state state)
              (po:read-move state)))))

(e1:define (po:read-and-move! state)
  (e1:let ((move (po:read-move state)))
    (po:move! state move)))


;;;;; Computer play utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return a new state instead of modifying the given one.
(e1:define (po:non-destructive-move state move)
  (e1:let ((new-state (po:clone-state state)))
    (po:move! new-state move)
    new-state))


;;;;; Computer play: dumb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (po:best-dumb-move state randomize)
  (e1:assert (e1:not (po:terminal-state? state)))
  (e1:let* ((all-moves (po:possible-moves state))
            (all-moves (e1:if randomize
                         (list:shuffle all-moves)
                         all-moves)))
    (list:head all-moves)))


;;;;; Computer play: minimax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (po:better-outcome-for? outcome-1 outcome-2 player)
  (e1:match player
    ((po:case-white)
     (po:better-outcome-for? outcome-2 outcome-1 (po:case-black)))
    ((po:case-black)
     (e1:match outcome-1
       ((po:outcome-in-progress)
        (e1:match outcome-2
          ((po:outcome-in-progress) #f)
          ((po:outcome-draw)        #t)
          ((po:outcome-black-wins)  #f)
          ((po:outcome-white-wins)  #t)))
       ((po:outcome-draw)
        (e1:match outcome-2
          ((po:outcome-in-progress) #f)
          ((po:outcome-draw)        #f)
          ((po:outcome-black-wins)  #f)
          ((po:outcome-white-wins)  #t)))
       ((po:outcome-black-wins)
        (e1:match outcome-2
          ((po:outcome-in-progress) #t)
          ((po:outcome-draw)        #t)
          ((po:outcome-black-wins)  #f)
          ((po:outcome-white-wins)  #t)))
       ((po:outcome-white-wins)
        #f)))
    ((po:case-empty)
     (e1:assert #f))))

(e1:define (po:better-outcome-for outcome-1 outcome-2 player)
  (e1:if (po:better-outcome-for? outcome-1 outcome-2 player)
    outcome-1
    outcome-2))

(e1:define (po:terminal-state? state)
  (e1:match (po:state->outcome state)
    ((po:outcome-in-progress) #f)
    (else                     #t)))

;;; Return a list of the states directly reachable from the given states with the
;;; given moves, in some unspecified order.
(e1:define (po:children-states state)
  (e1:let ((moves (po:possible-moves state)))
    (po:children-states-acc state moves list:nil)))
(e1:define (po:children-states-acc state moves acc)
  (e1:if (list:null? moves)
    acc
    (e1:let ((child (po:non-destructive-move state (list:head moves))))
      (po:children-states-acc state
                              (list:tail moves)
                              (list:cons child acc)))))

(e1:define (po:evaluate-state state depth)
  (e1:if (e1:or (fixnum:zero? depth)
                (po:terminal-state? state))
    (po:state->outcome state)
    (e1:let ((children-states (po:children-states state)))
      (po:evaluate-states children-states
                          (fixnum:1- depth)))))

(e1:define (po:evaluate-states states depth)
  (e1:let* ((first-state (list:head states))
            (remaining-states (list:tail states))
            (player-to-play (po:state-get-player first-state))
            (player-to-choose (po:flip-player player-to-play)))
    (po:evaluate-states-acc remaining-states
                            player-to-choose
                            depth
                            (po:evaluate-state first-state depth))))
(e1:define (po:evaluate-states-acc states player depth current-best-outcome)
  (e1:if (list:null? states)
    current-best-outcome
    (e1:let* ((first-state (list:head states))
              (first-state-outcome (po:evaluate-state first-state depth))
              (remaining-states (list:tail states))
              (new-best-outcome (po:better-outcome-for first-state-outcome
                                                       current-best-outcome
                                                       player)))
      (po:evaluate-states-acc remaining-states
                              player
                              depth
                              new-best-outcome))))

(e1:define (po:children-moves-and-states state)
  (e1:let ((moves (po:possible-moves state)))
    (e1:let loop ((remaining-moves moves)
                  (acc list:nil))
      (e1:if (list:null? remaining-moves)
        acc
        (e1:let* ((first-move (list:head remaining-moves))
                  (reached-state (po:non-destructive-move state first-move)))
          (loop (list:tail remaining-moves)
                (list:cons (cons:make first-move reached-state)
                           acc)))))))

;; Return a two-element bundle: best move, best outcome.
(e1:define (po:best-minimax-move state depth randomize)
  (e1:assert (e1:not (po:terminal-state? state)))
  (e1:assert (fixnum:> depth 0))
  (e1:let* ((moves-and-states (po:children-moves-and-states state))
            (moves-and-states (e1:if randomize
                                (list:shuffle moves-and-states)
                                moves-and-states))
            (player (po:state-get-player state))
            (children-depth (fixnum:1- depth))
            (best-move (box:make 0))
            (best-outcome (box:make 0)))
    (e1:dolist (ms moves-and-states)
      (e1:let* ((move (cons:get-car ms))
                (child (cons:get-cdr ms))
                (outcome (po:evaluate-state child children-depth)))
        (e1:when (e1:or (fixnum:zero? (box:get best-move))
                        (po:better-outcome-for? outcome
                                                (box:get best-outcome)
                                                player))
          (box:set! best-move move)
          (box:set! best-outcome outcome))))
    (e1:bundle (box:get best-move) (box:get best-outcome))))


;;;;; Player type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum po:player-type
  (dumb randomize)
  (minimax depth randomize)
  (human suggestion-depth))

(e1:define (po:player-type->string player-type)
  (e1:match player-type
    ((po:player-type-dumb randomize)
     (string:append "dumb"
                    (e1:if randomize "" " deterministic")))
    ((po:player-type-minimax depth randomize)
     (string:append "minimax "
                    (string:fixnum->string depth) "-plies deep"
                    (e1:if randomize "" " deterministic")))
    ((po:player-type-human suggestion-depth)
     (string:append "human"
                    (e1:if (fixnum:zero? suggestion-depth)
                      ""
                      (string:append " with "
                                     (string:fixnum->string suggestion-depth)
                                     "-plies deep suggestions"))))))


;;;;; Game loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the outcome.
(e1:define (po:play size black-player-type white-player-type)
  (e1:let ((state (po:make-state size)))
    (e1:while (e1:not (po:terminal-state? state))
      (po:print-state state)
      (e1:let* ((current-player (po:state-get-player state))
                (current-player-type (e1:match current-player
                                       ((po:case-black) black-player-type)
                                       ((po:case-white) white-player-type))))
        (fio:write "It's now "(st (po:player->escaped-string current-player))
                   " ("
                   (st (po:player-type->string current-player-type))
                   ")'s turn.\n")
        (po:play-one-move! state current-player-type)
        (fio:write "\n")))
    (po:print-state state)
    (po:state->outcome state)))

(e1:define (po:play-one-move! state player-type)
  (e1:let ((player (po:state-get-player state)))
    (e1:match player-type
      ((po:player-type-dumb randomize)
       (e1:let ((move (po:best-dumb-move state randomize)))
         (fio:write (st (po:player->escaped-string player))
                    " dumbly plays "
                    (st (po:move->string move))
                    ".\n")
         (po:move! state move)))
      ((po:player-type-minimax depth randomize)
       (fio:write "The computer is thinking...\n")
       (e1:let (((move outcome) (po:best-minimax-move state depth randomize)))
         (fio:write (st (po:player->escaped-string player))
                    " plays "
                    (st (po:move->string move))
                    " (its worst possible outcome is \""
                    (st (po:outcome->string outcome))
                    "\" as far as it can see).\n")
         (po:move! state move)))
      ((po:player-type-human suggestion-depth)
       (e1:when (fixnum:> suggestion-depth 0)
         (fio:write "The computer is searching for a suggestion...\n")
         (e1:let (((move outcome) (po:best-minimax-move state suggestion-depth #t)))
           (fio:write "  (the computer suggests "
                      (st (po:move->string move))
                      " for outcome \""
                      (st (po:outcome->string outcome))
                      "\")\n")))
       (e1:let ((move (po:read-move state)))
         (po:move! state move)))
      (else
       (e1:assert #f)))))


;;;;; Tournament: play a specified number of games
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (po:tournament size black-player-type white-player-type game-no)
  (e1:let loop ((black-victories 0)
                (white-victories 0)
                (draws 0)
                (remaining-games game-no))
    (e1:if (fixnum:zero? remaining-games)
      (e1:let ((black-percentage (fixnum:/ (fixnum:* 100 black-victories)
                                           game-no))
               (white-percentage (fixnum:/ (fixnum:* 100 white-victories)
                                           game-no))
               (draw-percentage (fixnum:/ (fixnum:* 100  draws)
                                          game-no)))
        (fio:write "Score over " (i game-no) " games:\n"
                   "* Black ("
                   (st (po:player-type->string black-player-type))
                   "): " (i black-percentage) "%\n"
                   "* White ("
                   (st (po:player-type->string white-player-type))
                   "): " (i white-percentage) "%\n"
                   "* Draws: " (i draw-percentage) "%\n"
                   "\n"))
      (e1:let* ((game-outcome (po:play size black-player-type
                                       white-player-type))
                ((black-victories
                  white-victories
                  draws) (e1:match game-outcome
                           ((po:outcome-black-wins)
                            (e1:bundle (fixnum:1+ black-victories)
                                       white-victories
                                       draws))
                           ((po:outcome-white-wins)
                            (e1:bundle black-victories
                                       (fixnum:1+ white-victories)
                                       draws))
                           ((po:outcome-draw)
                            (e1:bundle black-victories
                                       white-victories
                                       (fixnum:1+ draws)))
                           (else
                            (e1:assert #f)))))
        (loop black-victories
              white-victories
              draws
              (fixnum:1- remaining-games))))))

;;;;; Command line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Of course we want --help, --version and -- to finalize option processing.
(command-line:set-common-options)

;;; Add the other options we need
(command-line:add-options (("--no-color")
                           "don't use ANSI terminal sequences for color")
                          (("--size")
                           fixnum 4
                           "board size")
                          (("--black")
                           string "h3"
                           "specify black player")
                          (("--white")
                           string "m5"
                           "specify white player")
                          (("--tournament")
                           fixnum 0
                           "play a tournament of N games"))

;;; Set the information displayed by --version and --help .
(command-line:set-info!
    #:program-name "pushover (GNU epsilon)"
    #:usage "pushover [option]..."
    #:program-version configuration:package_version
    #:bug-email configuration:package_bugreport
    #:copyright (string:append "Copyright (C) Luca Saiu 2016")
    #:authors "Luca Saiu <http://ageinghacker.net>"
    #:introduction "Play the Pushover game."
    #:closing "Player specifications may be h (human player), hN (human player
with minimax suggestions), mN (minimax player), mNd (minimax deterministic
player), d (dumb player), or dd (dumb deterministic player).  N is the
one-digit depth in plies.

The default is h3 for black and m5 for white.")

(e1:define (po:require-nonzero-digit c)
  (e1:when (fixnum:= c #\0)
    (e1:error "zero digit not allowed"))
  (e1:unless (e1:and (fixnum:>= c #\1)
                     (fixnum:<= c #\9))
    (e1:error "invalid decimal digit " (c c))))

(e1:define (po:string->player-type s)
  (e1:case (string:length s)
    ((1)
     (e1:case (string:get s 0)
       ((#\h) (po:player-type-human 0))
       ((#\d) (po:player-type-dumb #t))
       (else (e1:error "invalid player specification " (S s)))))
    ((2)
     (e1:let ((c (string:get s 1)))
       (e1:case (string:get s 0)
         ((#\h)
          (po:require-nonzero-digit c)
          (po:player-type-human (fixnum:- c #\0)))
         ((#\d)
          (e1:unless (fixnum:= c #\d)
            (e1:error "invalid player specification " (S s)))
          (po:player-type-dumb #f))
         ((#\m)
          (po:require-nonzero-digit c)
          (po:player-type-minimax (fixnum:- c #\0) #t))
         (else
          (e1:error "invalid player specification " (S s))))))
    ((3)
     (e1:let ((c0 (string:get s 0))
              (c1 (string:get s 1))
              (c2 (string:get s 2)))
       (e1:unless (e1:and (fixnum:= c0 #\m)
                          (fixnum:= c2 #\d))
         (e1:error "invalid player specification " (S s)))
       (po:require-nonzero-digit c1)
       (po:player-type-minimax (fixnum:- c1 #\0) #f)))
    (else
     (e1:error "invalid player specification " (S s)))))


;;;;; Main program, for execution or compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (po:main)
  (command-line:process-args)
  (e1:unless (list:null? (command-line:non-option-list))
    (e1:error "this program has no non-option arguments"))
  (box:set! po:ansi-terminal-escapes
            (e1:not (command-line:option-supplied? "--no-color")))
  (e1:let ((size (command-line:option-value "--size"))
           (black-player-type
            (po:string->player-type (command-line:option-value "--black")))
           (white-player-type
            (po:string->player-type (command-line:option-value "--white")))
           (tournament (command-line:option-value "--tournament")))
    (e1:unless (e1:and (fixnum:>= size 3)
                       (fixnum:<= size 9))
      (e1:error "Invalid board size " (i size)
                ": size should be between 3 and 9 included"))
    (e1:if (fixnum:zero? tournament)
      (po:play size black-player-type white-player-type)
      (po:tournament size black-player-type white-player-type tournament))))


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(po:play 4 (po:player-type-minimax 5 #f) (po:player-type-minimax 5 #f))
;;(e1:compile "/tmp/po-h" (po:play 4 (po:player-type-human 5) (po:player-type-minimax 5 #f)))
