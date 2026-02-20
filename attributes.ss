(import :std/srfi/13)
(export
  n->s
  opt-attr
  opt-attrs
  points->string
  style-string)

;; Convert number or string to string
(def (n->s v)
  (if (string? v) v (number->string v)))

;; Build a single optional attribute pair, returns () if val is #f
(def (opt-attr name val)
  (if val `((,name ,(n->s val))) '()))

;; Strip trailing colon from keyword to get attribute name symbol
(def (keyword->attr-name kw)
  (let* ((s (keyword->string kw))
         (len (string-length s)))
    (string->symbol s)))

;; Build filtered attribute list from keyword pairs
;; Returns list of (name val) pairs, filtering out #f values
(def (opt-attrs . kvs)
  (let loop ((kvs kvs) (acc '()))
    (if (null? kvs)
      (reverse acc)
      (let ((k (car kvs))
            (v (cadr kvs)))
        (if v
          (loop (cddr kvs) (cons (list (keyword->attr-name k) (n->s v)) acc))
          (loop (cddr kvs) acc))))))

;; Convert ((x1 y1) (x2 y2) ...) to "x1,y1 x2,y2 ..."
(def (points->string pts)
  (string-join
    (map (lambda (p) (string-append (n->s (car p)) "," (n->s (cadr p))))
         pts)
    " "))

;; Build inline CSS style string from keyword pairs
;; (style-string fill: "red" stroke: "black") => "fill:red;stroke:black"
(def (style-string . kvs)
  (let loop ((kvs kvs) (acc '()))
    (if (null? kvs)
      (string-join (reverse acc) ";")
      (let ((k (car kvs))
            (v (cadr kvs)))
        (if v
          (loop (cddr kvs)
                (cons (string-append (keyword->string k) ":" (n->s v)) acc))
          (loop (cddr kvs) acc))))))
