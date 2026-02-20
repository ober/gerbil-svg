(import :std/srfi/13
        :gerbil-svg/attributes)
(export
  path-builder? path-builder-commands
  make-path-builder
  path-move-to path-line-to path-close
  path-cubic-to path-quadratic-to
  path-arc-to
  path-horizontal-to path-vertical-to
  path-rel-move-to path-rel-line-to
  path-rel-cubic-to path-rel-quadratic-to
  path-rel-horizontal-to path-rel-vertical-to
  path-build)

;; Path builder accumulates command strings in reverse
(defstruct path-builder (commands)
  transparent: #t)

;; Internal helper to create path-builder with given commands
(def (new-pb commands)
  (##structure path-builder::t commands))

;; Each command appends to the list and returns a new builder (immutable)
(def (path-move-to pb x y)
  (new-pb
    (cons (string-append "M" (n->s x) " " (n->s y))
          (path-builder-commands pb))))

(def (path-line-to pb x y)
  (new-pb
    (cons (string-append "L" (n->s x) " " (n->s y))
          (path-builder-commands pb))))

(def (path-close pb)
  (new-pb
    (cons "Z" (path-builder-commands pb))))

(def (path-cubic-to pb x1 y1 x2 y2 x y)
  (new-pb
    (cons (string-append "C" (n->s x1) " " (n->s y1) " "
                         (n->s x2) " " (n->s y2) " "
                         (n->s x) " " (n->s y))
          (path-builder-commands pb))))

(def (path-quadratic-to pb x1 y1 x y)
  (new-pb
    (cons (string-append "Q" (n->s x1) " " (n->s y1) " "
                         (n->s x) " " (n->s y))
          (path-builder-commands pb))))

(def (path-arc-to pb rx ry x-rotation large-arc-flag sweep-flag x y)
  (new-pb
    (cons (string-append "A" (n->s rx) " " (n->s ry) " "
                         (n->s x-rotation) " "
                         (n->s large-arc-flag) " "
                         (n->s sweep-flag) " "
                         (n->s x) " " (n->s y))
          (path-builder-commands pb))))

(def (path-horizontal-to pb x)
  (new-pb
    (cons (string-append "H" (n->s x))
          (path-builder-commands pb))))

(def (path-vertical-to pb y)
  (new-pb
    (cons (string-append "V" (n->s y))
          (path-builder-commands pb))))

;; Relative variants use lowercase letters
(def (path-rel-move-to pb dx dy)
  (new-pb
    (cons (string-append "m" (n->s dx) " " (n->s dy))
          (path-builder-commands pb))))

(def (path-rel-line-to pb dx dy)
  (new-pb
    (cons (string-append "l" (n->s dx) " " (n->s dy))
          (path-builder-commands pb))))

(def (path-rel-cubic-to pb dx1 dy1 dx2 dy2 dx dy)
  (new-pb
    (cons (string-append "c" (n->s dx1) " " (n->s dy1) " "
                         (n->s dx2) " " (n->s dy2) " "
                         (n->s dx) " " (n->s dy))
          (path-builder-commands pb))))

(def (path-rel-quadratic-to pb dx1 dy1 dx dy)
  (new-pb
    (cons (string-append "q" (n->s dx1) " " (n->s dy1) " "
                         (n->s dx) " " (n->s dy))
          (path-builder-commands pb))))

(def (path-rel-horizontal-to pb dx)
  (new-pb
    (cons (string-append "h" (n->s dx))
          (path-builder-commands pb))))

(def (path-rel-vertical-to pb dy)
  (new-pb
    (cons (string-append "v" (n->s dy))
          (path-builder-commands pb))))

;; Finalize: join all commands into a single path data string
(def (path-build pb)
  (string-join (reverse (path-builder-commands pb)) " "))
