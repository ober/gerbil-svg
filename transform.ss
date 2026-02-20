(import :std/srfi/13
        :gerbil-svg/attributes)
(export
  translate-transform
  rotate-transform
  scale-transform
  skew-x-transform
  skew-y-transform
  matrix-transform
  compose-transforms)

;; (translate-transform tx ty) => "translate(tx,ty)"
(def (translate-transform tx (ty 0))
  (string-append "translate(" (n->s tx) "," (n->s ty) ")"))

;; (rotate-transform angle (cx #f) (cy #f)) => "rotate(angle)" or "rotate(angle,cx,cy)"
(def (rotate-transform angle (cx #f) (cy #f))
  (if (and cx cy)
    (string-append "rotate(" (n->s angle) "," (n->s cx) "," (n->s cy) ")")
    (string-append "rotate(" (n->s angle) ")")))

;; (scale-transform sx (sy #f)) => "scale(sx)" or "scale(sx,sy)"
(def (scale-transform sx (sy #f))
  (if sy
    (string-append "scale(" (n->s sx) "," (n->s sy) ")")
    (string-append "scale(" (n->s sx) ")")))

(def (skew-x-transform angle)
  (string-append "skewX(" (n->s angle) ")"))

(def (skew-y-transform angle)
  (string-append "skewY(" (n->s angle) ")"))

;; (matrix-transform a b c d e f) => "matrix(a,b,c,d,e,f)"
(def (matrix-transform a b c d e f)
  (string-append "matrix("
    (string-join (map n->s [a b c d e f]) ",") ")"))

;; Compose multiple transform strings
;; (compose-transforms "translate(10,20)" "rotate(45)") => "translate(10,20) rotate(45)"
(def (compose-transforms . transforms)
  (string-join (filter (lambda (s) (not (string-empty? s))) transforms) " "))
