;;; package --- solution day 6 aoc2022
;;; Commentary:
;;; reference https://adventofcode.com/2022/day/6
;;; Code:

(load-file "../input.el")

(defun solution (input unique-count)
  "Given `INPUT and `UNIQUE-COUNT provide the solution at the problem presented in day 6."
  (seq-reduce (lambda (acc elem)
                (let ((unique-chars (cdr acc)))
                  (if (equal (length (seq-uniq unique-chars)) unique-count)
                      acc
                    (let* ((count (+ 1 (car acc)))
                           (unique-chars (cons elem unique-chars))
                           (unique-chars (if (> (length unique-chars) unique-count)
                                             (remove-last unique-chars)
                                           unique-chars)))
                      (cons count unique-chars)))))
              (seq-filter (lambda (elem) (not (string-empty-p elem))) (split-string input ""))
              (cons 0 (list))))

(defun remove-last (list)
  "Remove last element of `LIST."
  (if (not (cdr list))
      nil
    (cons (car list) (remove-last (cdr list)))))

(defun part-1 (input)
  "Solve problem presented in day 6 part 1 with given `INPUT."
  (solution input 4))

(part-1 (get-file-content "./input.txt"))

(defun part-2 (input)
  "Solve problem presented in day 6 part 2 with given `INPUT."
  (solution input 14))

(part-2 (get-file-content "./input.txt"))

(provide 'solution)
;;; solution.el ends here
