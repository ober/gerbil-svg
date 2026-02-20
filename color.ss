(import :gerbil-svg/attributes)
(export
  rgb rgba hsl hsla
  color->string
  color-black color-white color-red color-green color-blue
  color-gray color-none color-transparent)

;; (rgb r g b) => "rgb(r,g,b)" where r,g,b are 0-255
(def (rgb r g b)
  (string-append "rgb(" (n->s r) "," (n->s g) "," (n->s b) ")"))

;; (rgba r g b a) => "rgba(r,g,b,a)" where a is 0.0-1.0
(def (rgba r g b a)
  (string-append "rgba(" (n->s r) "," (n->s g) "," (n->s b) "," (n->s a) ")"))

;; (hsl h s l) => "hsl(h,s%,l%)"
(def (hsl h s l)
  (string-append "hsl(" (n->s h) "," (n->s s) "%," (n->s l) "%)"))

;; (hsla h s l a) => "hsla(h,s%,l%,a)"
(def (hsla h s l a)
  (string-append "hsla(" (n->s h) "," (n->s s) "%," (n->s l) "%," (n->s a) ")"))

;; Generic color->string: pass-through for strings, convert numbers
(def (color->string c)
  (if (string? c) c (n->s c)))

;; Named color constants
(defconst color-black "black")
(defconst color-white "white")
(defconst color-red "red")
(defconst color-green "green")
(defconst color-blue "blue")
(defconst color-gray "#808080")
(defconst color-none "none")
(defconst color-transparent "transparent")
