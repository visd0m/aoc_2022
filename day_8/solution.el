;;; package --- solution day 8 aoc2022
;;; Commentary:
;;; reference https://adventofcode.com/2022/day/8
;;; Code:

(load-file "../input.el")

(defun solution (input)
  "Parse `INPUT representing it as a matrix."
  (let* ((lines (seq-filter (lambda (line) (not (string-empty-p line))) (split-string input "\n")))
         (lines-count (length lines))
         (columns-count (length (seq-filter (lambda (token) (not (string-empty-p token))) (split-string (nth 0 lines) ""))))
         (indexes-line (number-sequence 0 (- lines-count 1)))
         (indexes-column (number-sequence 0 (- columns-count 1)))
         (matrix (make-hash-table))
         (matrix (seq-reduce (lambda (matrix index) (parse-line (nth index lines) index matrix))
                             indexes-line
                             matrix))
         (visible-elements-count (seq-reduce (lambda (acc i)
                                               (seq-reduce (lambda (acc j)
                                                             (if (is-visible (cons i j) matrix lines-count columns-count)
                                                                 (+ acc 1)
                                                               acc))
                                                           indexes-column acc))
                                             indexes-line 0)))
    ;; (message (format "%s" matrix))
    visible-elements-count))

(defun part-1 (input)
  "Solve problem of day 8 part 1 given `INPUT."
  (solution input))

(part-1 (get-file-content "./input.txt"))

(part-1 "30373
25512
65332
33549
35390")

(defun is-visible (coordinates matrix lines-count columns-count)
  "Check if tree with `COORDINATES is visible in `MATRIX with #`LINES-COUNT lines and #`COLUMNS-COUNT columns."
  (or (is-border coordinates lines-count columns-count)
      (has-lower-neighbor coordinates lines-count columns-count matrix)))

(defun is-border (coordinates lines-count columns-count)
  "Check if `COORDINATES is in border accordingly to `LINES-COUNT and `COLUMNS-COUNT."
  (or (or (= (car coordinates) 0) (= (cdr coordinates) 0))
      (or (= (car coordinates) (- lines-count 1)) (= (cdr coordinates) (- columns-count 1)))))

(defun has-lower-neighbor (coordinates lines-count columns-count matrix)
  "Check if `COORDINATES has a lower neighbor given `LINES-COUNT `COLUMNS-COUNT and `MATRIX."
  (or (has-lower-neighbors-in-line coordinates columns-count matrix)
      (has-lower-neighbors-in-column coordinates lines-count matrix)))

(defun has-lower-neighbors-in-line (coordinates columns-count matrix)
  "Check if `COORDINATES has a lower neighbor in line given `COLUMNS-COUNT and `MATRIX."
  (let* ((value (gethash (to-key coordinates) matrix))
         (is-visible (or (seq-every-p (lambda (column-index) (< (gethash (to-key (cons (car coordinates) column-index)) matrix) value)) (number-sequence 0 (- (cdr coordinates) 1)))
                         (seq-every-p (lambda (column-index) (< (gethash (to-key (cons (car coordinates) column-index)) matrix) value)) (number-sequence (+ (cdr coordinates) 1) (- columns-count 1))))))
    is-visible))

(defun has-lower-neighbors-in-column (coordinates lines-count matrix)
  "Check if `COORDINATES has a lower neighbor in column given `LINES-COUNT and `MATRIX."
  (let* ((value (gethash (to-key coordinates) matrix))
         (is-visible (or (seq-every-p (lambda (line-index) (< (gethash (to-key (cons line-index (cdr coordinates))) matrix) value)) (number-sequence 0 (- (car coordinates) 1)))
                         (seq-every-p (lambda (line-index) (< (gethash (to-key (cons line-index (cdr coordinates))) matrix) value)) (number-sequence (+ (car coordinates) 1) (- lines-count 1))))))
    is-visible))

(defun parse-line (line line_number matrix)
  "Parse `LINE updating `MATRIX accordinlgy to `LINE_NUMBER."
  (let* ((tokens (seq-filter (lambda (token) (not (string-empty-p token))) (split-string line "")))
         (columns-count (length tokens))
         (indexes (number-sequence 0 (- columns-count 1))))
    (seq-reduce (lambda (matrix index)
                  (puthash (to-key (cons line_number index)) (string-to-number (nth index tokens)) matrix)
                  matrix)
                indexes matrix)))

(defun to-key (coordinates)
  "Get hash table key from given `COORDINATES."
  (intern (format "%s%s" (car coordinates) (cdr coordinates))))

(provide 'solution)
;;; solution.el ends here
