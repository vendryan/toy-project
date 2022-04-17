(defpackage #:tic-tac-toe
  (:use #:cl)
  (:export #:run-game))
(in-package #:tic-tac-toe)

(defmacro once-only ((&rest names) &body body)
  "Macro for used in another macro, for making the argument
  evaluate only once"
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let (,@ (loop for n in names collect `(,n (gensym))))
     ,@body))

(defmacro dolist-no-last ((var list)
                          (&body always-run-body)
                          (&optional &body not-run-at-last-body))
  "Dolist but `not-run-at-last-body is not evaluated at last element"
  (with-gensyms (count list-length)
    (once-only (list)
      `(let ((,count 1)
-             (,list-length (length ,list)))
         (dolist (,var ,list)
           ,@always-run-body
           (when (/= ,count ,list-length)
             ,@not-run-at-last-body
             (incf ,count)))))))

(defparameter *empty-board* (list (list #\Space #\Space #\Space)
                                  (list #\Space #\Space #\Space)
                                  (list #\Space #\Space #\Space))
  "Empty board representation for tic-tac-toe")

(defvar *board* (copy-list *empty-board*)
  "Current board for tic-tac-toe")

(defun print-board ()
  "Print the current tic-tac-toe board"
  (dolist-no-last (row *board*)
      ((format t " ~a | ~a | ~a ~%"
                (nth 0 row)
                (nth 1 row)
                (nth 2 row)))
      ((format t "---+---+---~%")))
  (format t "~%"))

(defun x-or-o (choice)
  "Return O if given X and return X if given O"
  (if (string-equal "X" choice)
      "O"
      "X"))

(defun get-board-choice ()
  "Get the player board choice between X and O"
  (format t "Your choice: ")
  (let ((player-choice (read-line)))
    (if (or (string-equal "x" player-choice)
            (string-equal "o" player-choice))
        (progn
          (format t "Player 1 symbol is ~a~%" player-choice)
          (format t "Player 2 symbol is ~a~%~%" (x-or-o player-choice))
          (list (string-upcase player-choice) (string-upcase (x-or-o player-choice))))
        (progn
          (format t "Please enter either `X` or `O`~%~%")
          (get-board-choice)))))

(defun valid-move-p (move)
  "Check whether move is valid or not"
  (and (or (string= "1" move)
           (string= "2" move)
           (string= "3" move)
           (string= "4" move)
           (string= "5" move)
           (string= "6" move)
           (string= "7" move)
           (string= "8" move)
           (string= "9" move))))

(defun random-first-turn ()
  (if (= (random 2) 0)
      "Player 1"
      "Player 2"))

(defun get-move (player)
  "Get a current player move"
  (format t "~a turn please enter your move between 1 and 9~%" player)
  (format t "Your move: ")
  (let ((player-move (read-line)))
    (if (valid-move-p player-move)
        (if (free-space-p player-move)
            player-move
            (progn
              (format t "~a is not a free space~%~%" player-move)
              (get-move player)))
        (progn
          (format t "~a is not within 1-9~%~%" player-move)
          (get-move player)))))

(defun write-board (pos symbol)
  "Write the board based on given number"
  (let ((pos-num (- (parse-integer pos) 1)))
    (let ((row (floor (/ pos-num 3)))
          (col (rem pos-num 3)))
      (setf (nth col (nth row *board*)) symbol))))

(defun free-space-p (pos)
  "Check if chosen position is free or not"
  (let ((pos-num (- (parse-integer pos) 1)))
    (let ((row (floor (/ pos-num 3)))
          (col (rem pos-num 3)))
      (string= (nth col (nth row *board*)) " "))))

(defun winning-p (choice)
  "Check if its winning or not"
  (let ((pos1 (nth 0 (nth 0 *board*)))
        (pos2 (nth 1 (nth 0 *board*)))
        (pos3 (nth 2 (nth 0 *board*)))
        (pos4 (nth 0 (nth 1 *board*)))
        (pos5 (nth 1 (nth 1 *board*)))
        (pos6 (nth 2 (nth 1 *board*)))
        (pos7 (nth 0 (nth 2 *board*)))
        (pos8 (nth 1 (nth 2 *board*)))
        (pos9 (nth 2 (nth 2 *board*))))
    (or (and (string-equal choice pos1)
             (string-equal choice pos2)
             (string-equal choice pos3))
        (and (string-equal choice pos4)
             (string-equal choice pos5)
             (string-equal choice pos6))
        (and (string-equal choice pos7)
             (string-equal choice pos8)
             (string-equal choice pos9))
        (and (string-equal choice pos1)
             (string-equal choice pos4)
             (string-equal choice pos7))
        (and (string-equal choice pos2)
             (string-equal choice pos5)
             (string-equal choice pos8))
        (and (string-equal choice pos3)
             (string-equal choice pos6)
             (string-equal choice pos9))
        (and (string-equal choice pos1)
             (string-equal choice pos5)
             (string-equal choice pos9))
        (and (string-equal choice pos3)
             (string-equal choice pos5)
             (string-equal choice pos7)))))

(defun board-full-p ()
  "Check if board is full"
  (every #'(lambda (row)
             (every #'(lambda (col) (string/= col " ")) row))
         *board*))

(defun clear-board ()
  "Clear the tic-tac-toe board"
  (setf *board* (copy-list *empty-board*)))

(defun run-game ()
  "Run the tic-tac-toe game"
  (clear-board)
  (format t "Welcome to the tic-tac-toe game~%")
  (print-board)
  (let ((choice (get-board-choice)))
    (labels ((reset-game ()
               ;; Reset the game
               (clear-board)
               (run (random-first-turn)))
             (run (player)
               (print-board)
               (if (board-full-p)
                   (progn
                     (format t "DRAW! Board is full~%")
                     (if (y-or-n-p "Do you want to play again?")
                         (reset-game)))
                   (let ((move (get-move player)))
                     (cond ((string= player "Player 1")
                            (write-board move (nth 0 choice))
                            (print-board)
                            (if (winning-p (nth 0 choice))
                                (progn
                                  (format t "~a win!!~%" player)
                                  (if (y-or-n-p "Do you want to play again?")
                                      (reset-game)))
                                (run "Player 2")))
                           ((string= player "Player 2")
                            (write-board move (nth 1 choice))
                            (print-board)
                            (if (winning-p (nth 1 choice))
                                (progn
                                  (format t "Player 2 win!!~%")
                                  (if (y-or-n-p "Do you want to play again?")
                                      (reset-game)))
                                (run "Player 1"))))))))
      (run (random-first-turn))
      (format t "Good bye :)"))))
