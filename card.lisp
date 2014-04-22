(defvar *max-per-decene* 3) ;Maximum numbers per decene in a card

(defun size-of-decene-in-card (decene card)
  "Counts the number of elements in the given decene in a card"
  (or (count-if #'(lambda (num)
                    (eql decene (floor num 10)))
                card)
      0))

(defun is-legal-p (num card)
  "Checks if a number is legal to be added to a card"
  (let* ((decene (floor num 10))
        (size-of-decene (size-of-decene-in-card decene card)))
    (and (not (member num card))
         (< size-of-decene *max-per-decene*))))

(defun add-legal-number-to-card (num card)
  "Adds a legal number to a card"
  (cond ((is-legal-p num card)
         (cons num card))
        (t card)))

(defun fill-card (card size limit)
  "Fills an empty card with legal numbers"
  (let* ((n (1+ (random limit)))
         (new-card (add-legal-number-to-card n card)))
    (cond ((eql (length new-card) size)
           new-card)
          (t (fill-card new-card size limit)))))

(defun format-card (card)
  "Groups the numbers in the card by decene"
  (let ((formatted-card '()))
    (dotimes (i 10)
      (let ((numbers (remove-if #'(lambda (num)
                                    (or (null num)
                                        (not (eql i (floor num 10)))))
                                card)))
        (push numbers formatted-card)))
    (reverse formatted-card)))

(defun left-numbers (card)
  (or (count-if #'(lambda (num)
                    (not (null num)))
                card)
      0))

(defun is-bingo-p (card)
  "Checks if the card is a bingo"
  (every #'null card))

(defun try-for-number (num card)
  "Tries to check a number from a card"
  (if (member num card)
    (substitute nil num card)
    card))

(defun make-card (&optional (size 15) (limit 90))
  "Creates a card, ready to be playable"
  (let* ((empty-card '())
         (filled-card (fill-card empty-card size limit))
         (ordered-card (sort filled-card #'<)))
    ordered-card))

(defun print-card (card)
  "Prints a card to the screen, formatted"
  (let ((formatted-card (format-card card)))
    (format t "~&+-----------------------------------------------------------+~%")
    (dotimes (decene-i *max-per-decene*)
      (dotimes (group-i (length formatted-card))
        (format t "| ~4S" (nth decene-i (nth group-i formatted-card))))
      (format t "|")
      (format t "~&+-----------------------------------------------------------+~%"))))
