(defun window-resizer nil
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (=
                 (nth 0
                      (window-edges))
                 0)
                1 -1))
        (dy (if (=
                 (nth 1
                      (window-edges))
                 0)
                1 -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width)
                 (window-height))
        (setq c (read-char))
        (cond
         ((= c 108)
          (enlarge-window-horizontally dx))
         ((= c 104)
          (shrink-window-horizontally dx))
         ((= c 106)
          (enlarge-window dy))
         ((= c 107)
          (shrink-window dy))
         (t
          (message "Quit")
          (throw 'end-flag t)))))))

(provide 'window-resizer)
