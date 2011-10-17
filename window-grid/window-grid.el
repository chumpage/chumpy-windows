;;; window-grid.el -- Create grids of windows.
;;
;; Author: Steven Thomas
;;
;;; Commentary:
;;
;; Window-grid lets you easily create a grid of windows. Just specify
;; the rows and columns. The core function is window-grid.
;;
;;; Code:

(require 'cl)

(defun wg-clamp (val min max)
  (max (min val max) min))

(defun wg-pad-list (lst min-length value)
  (if (>= (length lst) min-length)
      lst
      (append lst (make-list (- min-length (length lst)) value))))

(defun wg-ndsort (lst predicate)
  (sort (copy-sequence lst) predicate))

(defun wg-flatten (lst)
  (mapcan (lambda (x) (if (listp x) x nil)) lst))

(defun wg-split-impl (split-fn size-fn num-windows)
  (let* ((size (/ (funcall size-fn) num-windows))
         (leftover (- (funcall size-fn) (* size num-windows))))
    (dotimes (n (- num-windows 1))
      (funcall split-fn (+ size (wg-clamp leftover 0 1)))
      (other-window 1)
      (setq leftover (- leftover 1)))))

(defun wg-split-horizontally (num-windows)
  (wg-split-impl 'split-window-horizontally 'window-width num-windows))

(defun wg-split-vertically (num-windows)
  (wg-split-impl 'split-window-vertically 'window-height num-windows))

(defun wg-window-sort-predicate (w1 w2)
  (let* ((w1-x (nth 0 (window-pixel-edges w1)))
         (w1-y (nth 1 (window-pixel-edges w1)))
         (w2-x (nth 0 (window-pixel-edges w2)))
         (w2-y (nth 1 (window-pixel-edges w2))))
    (if (not (= w1-y w2-y))
        (< w1-y w2-y)
        (< w1-x w2-x))))

(defun wg-window-in-row (row-anchor-window test-window)
  (destructuring-bind (rl rt rr rb) (window-pixel-edges row-anchor-window)
    (destructuring-bind (wl wt wr wb) (window-pixel-edges test-window)
        (not (or (<= wr rl) (<= wb rt) (>= wt rb))))))

(defun wg-partition-list (lst fn)
  (let* ((non-matching-elts (remove-if fn lst))
         (matching-elts (set-difference lst non-matching-elts)))
    (list matching-elts non-matching-elts)))

(defun wg-order-windows ()
  "Provides a top-left to bottom-right window ordering. The return value is a 
list of lists, where each sub-list represents a row of windows. Not every row 
is guaranteed to have the same number of windows."
  (let ((windows (window-list))
        (rows))
    (while windows
      (let ((row-anchor-window (car (wg-ndsort windows 'wg-window-sort-predicate))))
        (setq windows (remove row-anchor-window windows))
        (let* ((partitioned-windows
                (wg-partition-list windows
                                   (lambda (window) (wg-window-in-row row-anchor-window window))))
               (row-windows (wg-ndsort (nth 0 partitioned-windows) 'wg-window-sort-predicate)))
          (setq windows (nth 1 partitioned-windows))
          (setq rows (cons (cons row-anchor-window row-windows) rows)))))
    (nreverse rows)))

(defun wg-load-buffer-in-window (buffer window)
  (display-buffer-reuse-window buffer (list window nil nil)))

(defun wg-maybe-load-buffer-in-window (buffer window)
  (if (and buffer (get-buffer buffer) (wg-load-buffer-in-window buffer window))
      t
      nil))

(defun window-grid (&optional rows cols buffers default-buffer)
  "Splits the frame into a grid of evenly sized windows of ROWS x
COLS dimensions. BUFFERS is a list of buffers to assign to the
newly created windows. The windows are ordered from top left to
bottom right, so that the first buffer will go in the top left
window and the last buffer will go in the bottom right window. If
the BUFFERS list is nil or too short, or if a specified buffer
doesn't exist, DEFAULT-BUFFER is loaded into the window instead.

Called interactively, window-grid prompts the user for the rows
and columns."
  (interactive)
  (when (not (or rows cols))
    (let* ((input-str (read-string "window grid layout (e.g. '2 3'): "))
           (strs (or (split-string input-str) '("0" "0")))
           (r (string-to-number (or (elt strs 0) "0")))
           (c (string-to-number (or (elt strs 1) "0"))))
      (setq rows r cols c)))
  (when (<= (min rows cols) 0)
    (error "window-grid: invalid rows/cols"))
  (delete-other-windows)
  (wg-split-vertically rows)
  (let ((windows (wg-ndsort (window-list) 'wg-window-sort-predicate)))
    (assert (= (length windows) rows))
    (mapc (lambda (window)
            (select-window window)
            (wg-split-horizontally cols))
          windows))
  (let* ((windows (wg-flatten (wg-order-windows)))
         (buffers (wg-pad-list buffers (length windows) nil)))
    (map 'list (lambda (window buffer) (or (wg-maybe-load-buffer-in-window buffer window)
                                           (wg-maybe-load-buffer-in-window default-buffer window)))
         windows buffers))
  (select-window (car (wg-ndsort (window-list) 'wg-window-sort-predicate))))

(defun wg-split-% (h-or-v win-1-%)
  "Used to split windows unevenly. H-OR-V should be 'h or 'v to
specify the direction in which to split, and WIN-1-% specifies
the size percentage to give to the first window, i.e. the left
window in a horizontal split or the top window in a vertical
split.

For example, to get a 75%-25% vertical split, you would do
  (wg-split-% 'v 75)"
  (case h-or-v
    ('h (wg-split-horizontally (round (* (/ (float win-1-%) 100) (window-width)))))
    ('v (wg-split-vertically (round (* (/ (float win-1-%) 100) (window-height)))))))

(provide 'window-grid)
