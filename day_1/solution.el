;;; package --- solution day 1 aoc2022
;;; Commentary:
;;; reference https://adventofcode.com/2022/day/1
;;; Code:

(load-file "../input.el")

(defun part-1 ()
  "Solve part 1 of problem statement."
  (sum-of-top-nth 1))

(defun part-2 ()
  "Solve part 2 of problem statement."
  (sum-of-top-nth 3))

(defun sum-of-top-nth (nth)
  "Get the sum of the top `NTH elements."
  (seq-reduce '+
              (seq-take (seq-sort '>=
                                  (seq-map (lambda (elf-kcals)
                                             (seq-reduce '+ (seq-map 'string-to-number elf-kcals) 0))
                                           (seq-map (lambda (elf-kcal-as-string)
                                                      (split-string elf-kcal-as-string "\n"))
                                                    (split-string (get-file-content "./input.txt") "^\n"))))
                        nth)
              0))

(part-1) ; 70116
(part-2) ; 206582

(provide 'solution)
;;; solution.el ends here

