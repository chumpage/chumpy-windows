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

(defun wg-filter (filter-list predicate)
  (let ((result))
    (dolist (filter-list-val filter-list)
      (when (funcall predicate filter-list-val)
        (push filter-list-val result)))
    (nreverse result)))

(defun wg-partition (partition-list predicate)
  (let ((success-list) (fail-list))
    (dolist (partition-list-val partition-list)
      (if (funcall predicate partition-list-val)
          (push partition-list-val success-list)
          (push partition-list-val fail-list)))
    (list (nreverse success-list) (nreverse fail-list))))

(defun wg-split-impl (split-fn size-fn num-windows)
  (let* ((size (/ (funcall size-fn) num-windows))
         (fudge (if (eq split-fn 'split-window-horizontally) 1 0))
         (leftover (- (funcall size-fn) (* size num-windows))))
    (dotimes (n (- num-windows 1))
      (funcall split-fn (+ size (wg-clamp leftover 0 1) fudge))
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
  (set-window-buffer window buffer)
  (eq (window-buffer window) (get-buffer buffer)))

(defun wg-maybe-load-buffer-in-window (buffer window)
  (and buffer (get-buffer buffer) (wg-load-buffer-in-window buffer window)))

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

(defun wg-sorted-window-list (&optional windows)
  (let ((windows (or windows (window-list nil 'no-minibuf))))
    (wg-ndsort windows 'wg-window-sort-predicate)))

(defun wg-left-edge (window) (nth 0 (window-pixel-edges window)))
(defun wg-top-edge (window) (nth 1 (window-pixel-edges window)))
(defun wg-right-edge (window) (nth 2 (window-pixel-edges window)))
(defun wg-bottom-edge (window) (nth 3 (window-pixel-edges window)))

(defun wg-windows-bounding-box (windows)
  (if (null windows)
      nil
      (let ((left most-positive-fixnum)
            (top most-positive-fixnum)
            (right most-negative-fixnum)
            (bottom most-negative-fixnum))
        (dolist (w windows)
          (when (< (wg-left-edge w) left) (setq left (wg-left-edge w)))
          (when (< (wg-top-edge w) top) (setq top (wg-top-edge w)))
          (when (> (wg-right-edge w) right) (setq right (wg-right-edge w)))
          (when (> (wg-bottom-edge w) bottom) (setq bottom (wg-bottom-edge w))))
        (list left top right bottom))))

(defun wg-get-windows-sharing-edge (windows edge-fn val)
  (wg-filter windows (lambda (w) (= (funcall edge-fn w) val))))

(defun wg-get-window-from-top-left (left top)
  (car (wg-filter (wg-sorted-window-list)
                  (lambda (w) (and (= (wg-left-edge w) left)
                                   (= (wg-top-edge w) top))))))

(defun wg-check-split-continuation (windows
                                    continuing-edge-fn
                                    continuing-edge-val
                                    next-window-edge-fn
                                    next-window-edge-val
                                    boundary-edge-fn
                                    boundary-edge-val)
  (block nil
    (while t
      (let ((next-window (car (wg-get-windows-sharing-edge
                               (wg-get-windows-sharing-edge windows next-window-edge-fn next-window-edge-val)
                               continuing-edge-fn continuing-edge-val))))
        (when (null next-window) (return nil))
        (setq next-window-edge-val (funcall boundary-edge-fn next-window))
        (when (= next-window-edge-val boundary-edge-val) (return t))))))

(defun wg-partition-windows-along-edge (windows h-or-v edge)
  (case h-or-v
    ('h (wg-partition windows (lambda (w) (<= (wg-right-edge w) edge))))
    ('v (wg-partition windows (lambda (w) (<= (wg-bottom-edge w) edge))))
    (t (error "wg-partition: need to specify 'h or 'v"))))

(defun wg-split-% (h-or-v win-1-%)
  "Used to split windows unevenly. H-OR-V should be 'h or 'v to
specify the direction in which to split, and WIN-1-% specifies
the size percentage to give to the first window, i.e. the left
window in a horizontal split or the top window in a vertical
split.

For example, to get a 75%-25% vertical split, you would do
  (wg-split-% 'v .75)"
  (destructuring-bind (wl wt wr wb) (window-pixel-edges)
    (case h-or-v
      ;; The 1+ call below is a total bullshit fudge factor to account for the fact
      ;; that the split functions don't actually give me windows of the size I
      ;; request. They seem to leave the left window a little smaller than they're
      ;; supposed to be when I split horizontally for some reason. I think they factor
      ;; in the size of the fringe incorrecty.
      ('h (progn (split-window-horizontally (1+ (round (* (float win-1-%) (window-width)))))
                 (list (wg-get-window-from-top-left wl wt)
                       (wg-get-window-from-top-left (wg-right-edge (selected-window)) wt))))
      ('v (progn (split-window-vertically (round (* (float win-1-%) (window-height))))
                 (list (wg-get-window-from-top-left wl wt)
                       (wg-get-window-from-top-left wl (wg-bottom-edge (selected-window)))))))))

(defun wg-get-split-% (low high split-loc)
  (/ (float (- split-loc low)) (- high low)))

(defun wg-determine-window-layout-recursive (windows)
  (if (<= (length windows) 1)
      nil
      (block 'layout
        (let ((windows (wg-sorted-window-list windows))
              (box (wg-windows-bounding-box windows)))
          (dolist (w windows)
            (when (and (= (wg-left-edge w) (nth 0 box))
                       (/= (wg-bottom-edge w) (nth 3 box))
                       (wg-check-split-continuation windows
                                                    'wg-bottom-edge (wg-bottom-edge w)
                                                    'wg-left-edge (wg-left-edge w)
                                                    'wg-right-edge (nth 2 box)))
              (let ((partitioned-windows (wg-partition-windows-along-edge
                                          windows 'v (wg-bottom-edge w))))
                (return-from 'layout (list 'v (wg-get-split-% (wg-top-edge w) (nth 3 box) (wg-bottom-edge w))
                                           (wg-determine-window-layout-recursive (first partitioned-windows))
                                           (wg-determine-window-layout-recursive (second partitioned-windows))))))
            (when (and (= (wg-top-edge w) (nth 1 box))
                       (/= (wg-right-edge w) (nth 2 box))
                       (wg-check-split-continuation windows
                                                    'wg-right-edge (wg-right-edge w)
                                                    'wg-top-edge (wg-top-edge w)
                                                    'wg-bottom-edge (nth 3 box)))
              (let ((partitioned-windows (wg-partition-windows-along-edge
                                          windows 'h (wg-right-edge w))))
                (return-from 'layout (list 'h (wg-get-split-% (wg-left-edge w) (nth 2 box) (wg-right-edge w))
                                           (wg-determine-window-layout-recursive (first partitioned-windows))
                                           (wg-determine-window-layout-recursive (second partitioned-windows)))))))
          (error "wg-determine-window-layout-recursive: i shouldn't ever get here")))))

(defun wg-determine-window-layout ()
  (list (wg-determine-window-layout-recursive (wg-sorted-window-list))
        ;; !!! preserve window-start?
        ;(mapcar (lambda (w) (window-buffer w)) (wg-sorted-window-list))
        (mapcar (lambda (w) (list (window-buffer w) (window-start w)))
                (wg-sorted-window-list))
        ))

(defun wg-apply-window-layout-recursive (layout window)
  (when layout
    (select-window window)
    (destructuring-bind (h-or-v split-% layout-1 layout-2) layout
      (destructuring-bind (win-1 win-2) (wg-split-% h-or-v split-%)
        (wg-apply-window-layout-recursive layout-1 win-1)
        (wg-apply-window-layout-recursive layout-2 win-2)))))

(defun wg-apply-window-layout (layout window)
  (destructuring-bind (wl wt wr wb) (window-pixel-edges window)
    (wg-apply-window-layout-recursive (first layout) window)
    (let ((windows (wg-filter (wg-sorted-window-list)
                              (lambda (w) (and (>= (wg-left-edge w) wl)
                                               (>= (wg-top-edge w) wt)
                                               (<= (wg-right-edge w) wr)
                                               (<= (wg-bottom-edge w) wb))))))
      (assert (= (length windows) (length (second layout))))
      ;; !!! preserve window-start?
      ;; (map 'list (lambda (window buffer) (wg-maybe-load-buffer-in-window buffer window))
      ;;    windows (second layout))
      (map 'list (lambda (window window-info)
                   (when (wg-maybe-load-buffer-in-window (first window-info) window)
                     (set-window-start window (second window-info))))
         windows (second layout))
      nil
      )))

;; (defun wg-delete-other-windows ()
;;   (if (not (eq (selected-window) (minibuffer-window)))
;;       (delete-other-windows)
;;       (save-selected-window
;;         (select-window (car (window-list nil 'no-minibuf)))
;;         (delete-other-windows))))

(defun wg-select-non-minibuffer-window ()
  (select-window (car (window-list nil 'no-minibuf))))

(defun wg-shrink-window-layout (h-or-v shrink-%)
  (save-selected-window
    (wg-select-non-minibuffer-window)
    (let ((layout (wg-determine-window-layout)))
      (delete-other-windows)
      (destructuring-bind (win-1 win-2) (wg-split-% h-or-v shrink-%)
        (wg-apply-window-layout layout win-1)
        win-2))))

(provide 'window-grid)
