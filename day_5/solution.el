;;; package --- solution day 5 aoc2022
;;; Commentary:
;;; reference https://adventofcode.com/2022/day/5
;;; Code:

(load-file "../input.el")

(defun solution (input to-reverse)
  "Solve problem presented in day 5 with given `INPUT and `TO-REVERSE flag."
  (let* ((input-tokens (split-string (get-file-content "./input.txt") "^\n"))
         (stacks (parse-stacks (nth 0 input-tokens)))
         (moves (seq-map 'parse-instruction
                         (seq-filter (lambda (line) (not (string-empty-p line)))
                                     (split-string (nth 1 input-tokens) "\n"))))
         (stacks
          (seq-reduce
           (lambda (acc move)
             (move-n (nth 0 move) (- (nth 1 move) 1) (- (nth 2 move) 1) acc to-reverse)
             acc)
           moves
           stacks)))
    (string-replace "]" ""(string-replace "[" "" (seq-reduce
                                                  (lambda (acc stack)
                                                    (string-join (list acc (or (pop stack) " "))))
                                                  stacks
                                                  "")))))

(defun move-n (n from to stacks to-reverse)
  "Move `N elements from `FROM to `TO in `STACKS.
Elements are reversed if `TO-REVERSE is set to t."
  (let* ((elems-to-push (seq-map
                         (lambda (_i) (pop (nth from stacks)))
                         (number-sequence 1 n)))
         (elems-to-push (if to-reverse
                            (seq-reverse elems-to-push)
                          elems-to-push)))
    (seq-reduce (lambda (acc elem)
                  (if elem
                      (progn
                        (push elem (nth to acc))
                        acc)
                    acc))
                elems-to-push
                stacks)))

(defun parse-instruction (str)
  "Parse instruction from `STR."
  (when (string-match "^.* \\([0-9]+\\) .* \\([0-9]+\\) .* \\([0-9]+\\).*$" str)
    (list (string-to-number (match-string 1 str))  (string-to-number (match-string 2 str)) (string-to-number (match-string 3 str)))))

(defun parse-stacks (stacks-input)
  "Parse `STACKS-INPUT and return a list of stacks."
  (let* ((lines (seq-filter (lambda (line) (not (string-empty-p line))) (seq-reverse (split-string stacks-input "\n"))))
         (stacks (seq-map (lambda (_i) (list)) (number-sequence 0 (/ (length (nth 0 lines)) 4 )))))
    (dotimes (i (length lines))
      (let ((elems-to-push (seq-map
                            (lambda (elems)
                              (string-join elems))
                            (seq-partition (split-string (nth i lines) "") 4))))
        (dotimes (j (length elems-to-push))
          (when (string-match "\\[\\(.\\)\\]" (nth j elems-to-push))
            (push (match-string 0 (nth j elems-to-push)) (nth j stacks))))))
    stacks))

(defun part-1 (input)
  "Solve part 1 of day 5 with given `INPUT."
  (solution input nil))

(part-1 (get-file-content "./input.txt"))

(defun part-2 (input)
  "Solve part 2 of day 5 with given `INPUT."
  (solution input t))

(part-2 (get-file-content "./input.txt"))

(provide 'solution)
;;; solution.el ends here
