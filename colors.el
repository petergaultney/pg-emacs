(defun my-derive-hsl-color (str min-saturation)
  "Generate color deterministically with configurable minimum saturation.
MIN-SATURATION should be 0.0 to 1.0, where:
  0.0 = allows pure gray
  0.5 = moderately colorful minimum
  0.8 = very colorful minimum"
  (let* ((hash (md5 (downcase str)))
         ;; Use golden angle for even hue distribution (0-360°)
         (hue (mod (* (string-to-number (substring hash 0 8) 16) 137.5) 360))
         ;; Saturation from min-saturation to 1.0
         (sat (+ min-saturation
                 (* (- 1.0 min-saturation)
                    (/ (mod (string-to-number (substring hash 8 16) 16) 1000) 1000.0))))
         ;; Full lightness range: 0.2 to 0.8 (avoid pure black/white)
         (light (+ 0.2 (* 0.6 (/ (mod (string-to-number (substring hash 16 24) 16) 1000) 1000.0)))))
	(list hue sat light)))

(defun rgb-to-hsl (r g b)
  "Convert RGB (0-255) to HSL (H: 0-360, S: 0-1, L: 0-1)."
  (let* ((r (/ r 255.0))
         (g (/ g 255.0))
         (b (/ b 255.0))
         (max-val (max r g b))
         (min-val (min r g b))
         (diff (- max-val min-val))
         (l (/ (+ max-val min-val) 2.0))
         (s (if (= diff 0) 0
              (if (< l 0.5)
                  (/ diff (+ max-val min-val))
                (/ diff (- 2.0 max-val min-val)))))
         (h (cond ((= diff 0) 0)
                  ((= max-val r) (mod (/ (* 60 (- g b)) diff) 360))
                  ((= max-val g) (+ 120 (/ (* 60 (- b r)) diff)))
                  ((= max-val b) (+ 240 (/ (* 60 (- r g)) diff))))))
    (list h s l)))

(defun hsl-to-rgb (h s l)
  "Convert HSL to RGB (0-255)."
  (let* ((h (/ h 360.0))
         (c (* (- 1 (abs (- (* 2 l) 1))) s))
         (x (* c (- 1 (abs (- (mod (* h 6) 2) 1)))))
         (m (- l (/ c 2)))
         (rgb-prime (cond ((< h (/ 1 6.0)) (list c x 0))
                          ((< h (/ 2 6.0)) (list x c 0))
                          ((< h (/ 3 6.0)) (list 0 c x))
                          ((< h (/ 4 6.0)) (list 0 x c))
                          ((< h (/ 5 6.0)) (list x 0 c))
                          (t (list c 0 x)))))
    (mapcar (lambda (val) (round (* 255 (+ val m)))) rgb-prime)))

(defun relative-luminance (r g b)
  "Calculate relative luminance using sRGB formula."
  (let ((rs (if (<= r 0.03928) (/ r 12.92) (expt (/ (+ r 0.055) 1.055) 2.4)))
        (gs (if (<= g 0.03928) (/ g 12.92) (expt (/ (+ g 0.055) 1.055) 2.4)))
        (bs (if (<= b 0.03928) (/ b 12.92) (expt (/ (+ b 0.055) 1.055) 2.4))))
    (+ (* 0.2126 rs) (* 0.7152 gs) (* 0.0722 bs))))

(defun contrast-ratio (rgb1 rgb2)
  "Calculate WCAG contrast ratio between two RGB colors."
  (let* ((l1 (apply #'relative-luminance (mapcar (lambda (x) (/ x 255.0)) rgb1)))
         (l2 (apply #'relative-luminance (mapcar (lambda (x) (/ x 255.0)) rgb2)))
         (lighter (max l1 l2))
         (darker (min l1 l2)))
    (/ (+ lighter 0.05) (+ darker 0.05))))

(defun best-text-color (bg-rgb)
  "Choose white or black text based on contrast with background."
  (let* ((white-contrast (contrast-ratio bg-rgb '(255 255 255)))
         (black-contrast (contrast-ratio bg-rgb '(0 0 0))))
    (if (> white-contrast black-contrast) "white" "black")))

(defun my-pick-fg-bg-color-from-hsl (str min-saturation)
  "Generate RGB color and text color from HSL derivation."
  (let* ((hsl (my-derive-hsl-color str min-saturation))
         (rgb (apply #'hsl-to-rgb hsl))
         (text-color (best-text-color rgb)))
    (list (format "#%02X%02X%02X" (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)) text-color)))
