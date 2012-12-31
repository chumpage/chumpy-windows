;;; window-layout.el --- Modify window layout, create grids of windows, etc.
;;
;; Author: Steven Thomas
;;
;;; Commentary:
;;
;; Window-layout lets you easily declare and apply sophisticated window
;; layouts, create grids of windows, and intelligently shrink a window
;; layout to create room for a new window.
;;
;; See https://github.com/chumpage/chumpy-windows for more
;; documentation, and to submit patches.
;;
;;; Code:

(require 'cl)

(defun wl-clamp (val min max)
  (max (min val max) min))

(defun wl-pad-list (lst min-length value)
  (if (>= (length lst) min-length)
      lst
      (append lst (make-list (- min-length (length lst)) value))))

(defun wl-ndsort (lst predicate)
  (sort (copy-sequence lst) predicate))

(defun wl-flatten (lst)
  (mapcan (lambda (x) (if (listp x) x nil)) lst))

(defun wl-filter (filter-list predicate)
  (let ((result))
    (dolist (filter-list-val filter-list)
      (when (funcall predicate filter-list-val)
        (push filter-list-val result)))
    (nreverse result)))

(defun wl-partition (partition-list predicate)
  (let ((success-list) (fail-list))
    (dolist (partition-list-val partition-list)
      (if (funcall predicate partition-list-val)
          (push partition-list-val success-list)
          (push partition-list-val fail-list)))
    (list (nreverse success-list) (nreverse fail-list))))

(defun wl-window-size-for-split-direction (window direction)
  (if (eq direction 'h)
      (window-width window)
      (window-height window)))

(defun wl-split-direction-horizontal-p (direction)
  (if (eq direction 'h)
      t
      nil))

(defun wl-normalize-window-size (size window-size &optional round-fn)
  (when (null round-fn) (setq round-fn 'round))
  (let ((size-in-chars (if (and (> size -1) (< size 1))
                           (funcall round-fn (* (float size) window-size))
                           size)))
    (if (< size-in-chars 0)
        (- window-size size-in-chars)
        size-in-chars)))

(defun wl-tweak-window-size (direction size windows)
  (let* ((total-size
          (reduce '+ windows :key
                  (lambda (w) (wl-window-size-for-split-direction w direction))))
         (win-1-desired-size (wl-normalize-window-size size total-size)))
    (adjust-window-trailing-edge
     (car windows)
     (- win-1-desired-size
        (wl-window-size-for-split-direction (car windows) direction))
     (wl-split-direction-horizontal-p direction))))

(defun wl-split (direction size &optional window)
  "Used to split windows. DIRECTION should be 'h or 'v to specify
the direction in which to split.

SIZE specifies the size of the windows. If SIZE is positive, it
applies to the left window in a horizontal split, and the top
window in a vertical split. If SIZE is negative, it applies to
the right window in a horizontal split, and the bottom window in
a vertical split. In addition, SIZE can be given as either a
float value between -1 and 1, in which case it's interpreted as a
percentage. If SIZE is a value outside that range, it's
interpreted as the number of characters to give to the window.

WINDOW is the window to split, and defaults to the selected
window.

For example, to get a 75%-25% vertical split, you would do
  (wl-split 'v .75)

To get a vertical split where the bottom window is sized to 10
characters, you would do
  (wl-split 'v -10)

The return value is a list of the two windows, left/right in a
horizontal split and top/bottom in a vertical split."
  (when (null window) (setq window (selected-window)))
  (destructuring-bind (wl wt wr wb) (window-pixel-edges window)
    (split-window window nil
                  (wl-split-direction-horizontal-p direction))
    (let ((windows (list (wl-get-window-from-top-left wl wt)
                         (wl-get-window-from-bottom-right wr wb))))
      (wl-tweak-window-size direction size windows)
      windows)))

(defun wl-verify-split*-sizes (num-windows sizes)
  (setq sizes (copy-tree sizes))
  (when (or (< (length sizes) (1- num-windows))
            (> (length sizes) num-windows))
    (error (format "wrong number of sizes given. num sizes = %d, num windows = %d."
                   (length sizes) num-windows)))
  (dolist (size sizes)
    (when (or (< size 0) (> size 1))
      (error (format "invalid size given: %f. should be between 0 and 1." size))))
  (let ((sum (apply '+ sizes)))
    (when (> sum (+ 1 1e-6))
      (error (format "sum of sizes (%f) too high. should be 1." sum)))
    (if (< (length sizes) num-windows)
      (setq sizes (append sizes (list (- 1 sum))))
      (when (< sum (- 1 1e-6))
        (error (format "sum of sizes (%f) too low. should be 1." sum)))))
  sizes)

(defun wl-tweak-window-size-multiple (direction sizes windows)
  (let* ((available-size
          (reduce '+ windows :key
                  (lambda (w) (wl-window-size-for-split-direction w direction))))
         (normalized-sizes
          (mapcar (lambda (size) (wl-normalize-window-size size available-size 'floor))
                  sizes))
         (used-size (reduce '+ normalized-sizes))
         (leftover (- available-size used-size)))
    (dotimes (n (- (length windows) 1))
      (adjust-window-trailing-edge
       (nth n windows)
       (+ (wl-clamp leftover 0 1)
          (- (nth n normalized-sizes)
             (wl-window-size-for-split-direction (nth n windows) direction)))
       (wl-split-direction-horizontal-p direction))
      (setq leftover (1- leftover)))))

(defun wl-split* (direction num-windows &optional sizes window)
  (when (null sizes) (setq sizes (make-list num-windows (/ 1.0 num-windows))))
  (setq sizes (wl-verify-split*-sizes num-windows sizes))
  (when (null window) (setq window (selected-window)))
  (let ((windows (list window)))
    (dotimes (n (- num-windows 1))
      (destructuring-bind (win-1 win-2)
          (wl-split direction window-min-height window)
        (setq window win-2)
        (setq windows (cons win-2 windows))))
    (setq windows (nreverse windows))
    (wl-tweak-window-size-multiple direction sizes windows)
    windows))

(defun wl-window-sort-predicate (w1 w2)
  (let* ((w1-x (nth 0 (window-pixel-edges w1)))
         (w1-y (nth 1 (window-pixel-edges w1)))
         (w2-x (nth 0 (window-pixel-edges w2)))
         (w2-y (nth 1 (window-pixel-edges w2))))
    (if (not (= w1-y w2-y))
        (< w1-y w2-y)
        (< w1-x w2-x))))

(defun wl-partition-list (lst fn)
  (let* ((non-matching-elts (remove-if fn lst))
         (matching-elts (set-difference lst non-matching-elts)))
    (list matching-elts non-matching-elts)))

(defun wl-sorted-window-list (&optional windows)
  "Provides a list of windows sorted from top-left to bottom-right."
  (when (null windows) (setq windows (window-list nil 'no-minibuf)))
  (wl-ndsort windows 'wl-window-sort-predicate))

(defun wl-load-buffer-in-window (buffer window)
  (if (and buffer (get-buffer buffer))
      (progn (set-window-buffer window buffer)
             (eq (window-buffer window) (get-buffer buffer)))
      nil))

(defun window-grid (&optional rows cols buffers default-buffer)
  "Splits the frame into a grid of evenly sized windows of ROWS x
COLS dimensions. BUFFERS is a list of buffers to assign to the
newly created windows. The windows are ordered from top-left to
bottom-right, so that the first buffer will go in the top left
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
  (wl-split* 'v rows)
  (let ((windows (wl-sorted-window-list)))
    (assert (= (length windows) rows))
    (mapc (lambda (window)
            (select-window window)
            (wl-split* 'h cols))
          windows))
  (let* ((windows (wl-sorted-window-list))
         (buffers (wl-pad-list buffers (length windows) nil)))
    (map 'list (lambda (window buffer) (or (wl-load-buffer-in-window buffer window)
                                           (wl-load-buffer-in-window default-buffer window)))
         windows buffers))
  (select-window (car (wl-sorted-window-list))))

(defun wl-left-edge (window) (nth 0 (window-pixel-edges window)))
(defun wl-top-edge (window) (nth 1 (window-pixel-edges window)))
(defun wl-right-edge (window) (nth 2 (window-pixel-edges window)))
(defun wl-bottom-edge (window) (nth 3 (window-pixel-edges window)))

(defun wl-windows-bounding-box (windows)
  (if (null windows)
      nil
      (let ((left most-positive-fixnum)
            (top most-positive-fixnum)
            (right most-negative-fixnum)
            (bottom most-negative-fixnum))
        (dolist (w windows)
          (when (< (wl-left-edge w) left) (setq left (wl-left-edge w)))
          (when (< (wl-top-edge w) top) (setq top (wl-top-edge w)))
          (when (> (wl-right-edge w) right) (setq right (wl-right-edge w)))
          (when (> (wl-bottom-edge w) bottom) (setq bottom (wl-bottom-edge w))))
        (list left top right bottom))))

(defun wl-get-windows-sharing-edge (windows edge-fn val)
  (wl-filter windows (lambda (w) (= (funcall edge-fn w) val))))

(defun wl-get-window-from-top-left (left top)
  (car (wl-filter (wl-sorted-window-list)
                  (lambda (w) (and (= (wl-left-edge w) left)
                                   (= (wl-top-edge w) top))))))

(defun wl-get-window-from-bottom-right (right bottom)
  (car (wl-filter (wl-sorted-window-list)
                  (lambda (w) (and (= (wl-right-edge w) right)
                                   (= (wl-bottom-edge w) bottom))))))

(defun wl-check-split-continuation (windows
                                    continuing-edge-fn
                                    continuing-edge-val
                                    next-window-edge-fn
                                    next-window-edge-val
                                    boundary-edge-fn
                                    boundary-edge-val)
  (block nil
    (while t
      (let ((next-window (car (wl-get-windows-sharing-edge
                               (wl-get-windows-sharing-edge windows next-window-edge-fn next-window-edge-val)
                               continuing-edge-fn continuing-edge-val))))
        (when (null next-window) (return nil))
        (setq next-window-edge-val (funcall boundary-edge-fn next-window))
        (when (= next-window-edge-val boundary-edge-val) (return t))))))

(defun wl-partition-windows-along-edge (windows direction edge)
  (case direction
    ('h (wl-partition windows (lambda (w) (<= (wl-right-edge w) edge))))
    ('v (wl-partition windows (lambda (w) (<= (wl-bottom-edge w) edge))))
    (t (error "wl-partition: direction should be 'h or 'v"))))

(defun wl-get-split-% (low high split-loc)
  (/ (float (- split-loc low)) (- high low)))

;; specifying the shrink-direction is a nasty hack. when i know i'm shrinking
;; vertically, i record the horizontal split sizes as character values rather than
;; percentages (and similarly for the vertical split sizes when i'm shrinking
;; horizontally). this is because when i apply the layout, the widths of the windows
;; aren't exactly the same as the way they were in the original layout, because i'm not
;; accounting for the fringes and divider lines when i split. i should fix this and
;; then get rid of the shrink-direction parameter. one way to fix it would be to have a
;; post-layout-application window size tweak process, that tries to get the window
;; sizes to account for the window decorations. another way to fix it would be to
;; detect multi-splits (i.e. wl-split* splits) and record those when i determine the
;; window layout. the wl-split* function already handles the window sizing issue i'm
;; trying to work around here.
(defun wl-determine-window-layout-recursive (windows &optional shrink-direction)
  (if (<= (length windows) 1)
      nil
      (block 'layout
        (let ((windows (wl-sorted-window-list windows))
              (box (wl-windows-bounding-box windows)))
          (dolist (w windows)
            (when (and (= (wl-left-edge w) (nth 0 box))
                       (/= (wl-bottom-edge w) (nth 3 box))
                       (wl-check-split-continuation windows
                                                    'wl-bottom-edge (wl-bottom-edge w)
                                                    'wl-left-edge (wl-left-edge w)
                                                    'wl-right-edge (nth 2 box)))
              (let ((partitioned-windows (wl-partition-windows-along-edge
                                          windows 'v (wl-bottom-edge w))))
                (return-from 'layout (list 'v
                                           (if (or (null shrink-direction) (eq shrink-direction 'v))
                                               (wl-get-split-% (wl-top-edge w) (nth 3 box) (wl-bottom-edge w))
                                               (window-height w))
                                           (wl-determine-window-layout-recursive (first partitioned-windows) shrink-direction)
                                           (wl-determine-window-layout-recursive (second partitioned-windows) shrink-direction)))))
            (when (and (= (wl-top-edge w) (nth 1 box))
                       (/= (wl-right-edge w) (nth 2 box))
                       (wl-check-split-continuation windows
                                                    'wl-right-edge (wl-right-edge w)
                                                    'wl-top-edge (wl-top-edge w)
                                                    'wl-bottom-edge (nth 3 box)))
              (let ((partitioned-windows (wl-partition-windows-along-edge
                                          windows 'h (wl-right-edge w))))
                (return-from 'layout (list 'h
                                           (if (or (null shrink-direction) (eq shrink-direction 'h))
                                               (wl-get-split-% (wl-left-edge w) (nth 2 box) (wl-right-edge w))
                                               (window-width w))
                                           (wl-determine-window-layout-recursive (first partitioned-windows) shrink-direction)
                                           (wl-determine-window-layout-recursive (second partitioned-windows) shrink-direction))))))
          (error "wl-determine-window-layout-recursive: i shouldn't ever get here")))))

(defun wl-determine-window-layout (&optional shrink-direction)
  (list (wl-determine-window-layout-recursive (wl-sorted-window-list) shrink-direction)
        ;; we need to explicitly preserve and reapply the window-start and
        ;; window-hscroll, otherwise redisplay will reposition those values so that
        ;; point is in the middle of the window
        (mapcar (lambda (w) (list (window-buffer w)
                                  (window-start w)
                                  (window-hscroll w)))
                (wl-sorted-window-list))))

(defun wl-apply-window-layout-recursive (layout window)
  (when layout
    (destructuring-bind (direction size layout-1 layout-2) layout
      (destructuring-bind (win-1 win-2)
          (wl-split direction size window)
        (wl-apply-window-layout-recursive layout-1 win-1)
        (wl-apply-window-layout-recursive layout-2 win-2)))))

(defun wl-apply-window-layout (layout window)
  (destructuring-bind (wl wt wr wb) (window-pixel-edges window)
    (wl-apply-window-layout-recursive (first layout) window)
    (let ((windows (wl-filter (wl-sorted-window-list)
                              (lambda (w) (and (>= (wl-left-edge w) wl)
                                               (>= (wl-top-edge w) wt)
                                               (<= (wl-right-edge w) wr)
                                               (<= (wl-bottom-edge w) wb))))))
      (assert (= (length windows) (length (second layout))))
      (map 'list (lambda (window window-info)
                   (when (wl-load-buffer-in-window (first window-info) window)
                     (set-window-start window (second window-info))
                     (set-window-hscroll window (third window-info))))
         windows (second layout))
      nil)))

(defun wl-select-non-minibuffer-window ()
  (select-window (car (window-list nil 'no-minibuf))))

(defun wl-shrink-window-layout (direction size)
  (save-selected-window
    (wl-select-non-minibuffer-window)
    (let ((layout (wl-determine-window-layout direction)))
      (delete-other-windows)
      (destructuring-bind (win-1 win-2) (wl-split direction size)
        (wl-apply-window-layout layout win-1)
        win-2))))

(provide 'window-layout)

;;; window-layout.el ends here
