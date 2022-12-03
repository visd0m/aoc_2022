;;; package --- solution day 1 aoc2022
;;; Commentary:
;;; reference https://adventofcode.com/2022/day/2
;;; Code:

(load-file "../input.el")

(defun higher-priority-symbol (symbol)
  "Given `SYMBOL, get symbol with higher priority."
  (pcase symbol
    ("A" "B")
    ("B" "C")
    ("C" "A")))

(defun replace-opponent-symbol (strategy-guide)
  "Replace oppontent symbols with player symbols in `STRATEGY-GUIDE."
  (string-replace "Z" "C" (string-replace "Y" "B" (string-replace "X" "A" strategy-guide))))

(defun part-1 (input)
  "Solve part 1 of advent of code day 2 for the given `INPUT."
  (seq-reduce '+
              (seq-map 'round-score
                       (seq-filter (lambda (round) (not (string-empty-p round)))
                                   (split-string (replace-opponent-symbol input) "\n")))
              0))

;; (defun part-2 )

(defun round-score (round)
  "Get the score of `ROUND."
  (let* ((round-moves (split-string round " "))
         (opponent-move (nth 0 round-moves))
         (your-move (nth 1 round-moves)))
  (+ (result-score (cons your-move opponent-move)) (your-move-score your-move))))

(defun decode (to-decode opponent-choice strategy)
  "Decode `TO-DECODE symbol based on `STRATEGY and `OPPONENT-CHOICE."
  (pcase (cons strategy to-decode opponent-move)
    ('('your-move . "A" ) )))

(defun result-score (choices)
  "Get the result score based on round `CHOICES."
  (if (equal (car choices) (cdr choices))
      3
    (if (equal (car choices) (higher-priority-symbol (cdr choices)))
        6
      0)))

(defun your-move-score (move)
  "Get the score of your move based on round `MOVE."
  (pcase move
    ("A" 1)
    ("B" 2)
    ("C" 3)))

(result-score (cons "A" "A"))

(part-1 (get-file-content "./input.txt"))
(part-1 "A Y\nB X\nC Z\n")

(provide 'solution)
;;; solution.el ends here
