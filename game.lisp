(defvar *pool*) ; Pool of numbers

(defun print-status (player)
  "Prints the status of a player on the screen"
  (format t "~%~%~S" (player-name player))
  (format t "~%~S Numbers Left" (left-numbers (player-card player)))
  (print-card (player-card player)))

(defun welcome-screen (p1 p2)
  "Shows an awesome welcome screen to the player"
  (format t "~%###########################################")
  (format t "~%##    WELCOME TO THE GREAT CASINO NIGHT  ##")
  (format t "~%###########################################")
  (format t "~%##         WE ARE PLAYING BINGO!         ##")
  (format t "~%###########################################")
  (format t "~%###########################################")
  (format t "~%##   'denigrante' version by fuzzyalej   ##")
  (format t "~%###########################################")
  (print-status p1)
  (print-status p2))

(defun make-pool (size)
  "Make a pool of numbers of size `size`"
  (loop for i from 1 to size collect i))

(defun generate-random-number-from-pool ()
  "Gets and deletes a random number from the pool and updates `*pool*`"
  (when (eql (length *pool*) 0)
    (format t "The pool is empty, something is very wrong...")
    (exit))
  (let ((num (nth (random (length *pool*)) *pool*)))
    (setf *pool* (delete num *pool*))
    num))

(defun start-game (p1 p2)
  "Plays one round of the game"
  (let* ((num (generate-random-number-from-pool))
         (p1-card (try-for-number num (player-card p1)))
         (p2-card (try-for-number num (player-card p2))))
    ; how should I proceed to make this more functional-like?
     (setf (player-card p1) p1-card)
     (setf (player-card p2) p2-card)
    ;--
    (format t "~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%Press a key for next number...~%")
    (force-output t)
    (read-char *terminal-io*) 
    (format t "~&THE BOMBO GIRA...")
    (format t "~&ITS A ~S" num)
    (print-status p1)
    (print-status p2)
    (cond ((is-bingo-p p1-card)
           (format t "~%~%~%~S WINS" (player-name p1)))
          ((is-bingo-p p2-card)
           (format t "~%~%~%~S WINS" (player-name p2)))
          (t (start-game p1 p2)))))

(defun start ()
  "Sets up and starts the game"
  (let ((human (make-player :name "You" :card (make-card)))
        (computer (make-player :name "Evil Computer" :card (make-card))))
    (setf *pool* (make-pool 90))
    (welcome-screen human computer)
    (start-game human computer)))
