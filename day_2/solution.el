;;; package --- solution day 2 aoc2022
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

(defun lower-priority-symbol (symbol)
  "Given `SYMBOL, get symbol with lower priority."
  (pcase symbol
    ("B" "A")
    ("C" "B")
    ("A" "C")))

(defun decode-as-your-move (to-decode _)
  "Decode `TO-DECODE symbol as your move."
  (pcase to-decode
    ("X" "A")
    ("Y" "B")
    ("Z" "C")))

(defun round-score (round decoding-function)
  "Given a 'DECODING-FUNCTION, get the score of `ROUND."
  (let* ((round-moves (split-string round " "))
         (opponent-move (nth 0 round-moves))
         (your-move (funcall decoding-function (nth 1 round-moves) opponent-move)))
  (+ (result-score (cons your-move opponent-move)) (your-move-score your-move))))

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

(defun solution (input decoding-function)
  "Given `INPUT and a `DECODING-FUNCTION solve day 2 problem."
  (seq-reduce '+
              (seq-map (lambda (round) (round-score round decoding-function))
                       (seq-filter (lambda (round) (not (string-empty-p round)))
                                   (split-string input "\n")))
              0))

(defun part-1 (input)
  "Solve part 1 of advent of code day 2 for the given `INPUT."
  (solution input 'decode-as-your-move))

(part-1 (get-file-content "./input.txt"))
(part-1 "A Y\nB X\nC Z\n")

(defun decode-as-expected-result (to-decode opponent-move)
  "Decode `TO-DECODE as expected result, return your move given `OPPONENT-MOVE."
  (pcase to-decode
    ("X" (lower-priority-symbol opponent-move))
    ("Y" opponent-move)
    ("Z" (higher-priority-symbol opponent-move))))

(defun part-2 (input)
  "Solve part 1 of advent of code day 2 for the given `INPUT."
  (solution input 'decode-as-expected-result))

(part-2 (get-file-content "./input.txt"))

(provide 'solution)
;;; solution.el ends here
