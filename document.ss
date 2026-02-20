(import :std/xml
        :gerbil-svg/attributes)
(export
  svg-document svg-document? svg-document-width svg-document-height
  svg-document-viewbox svg-document-elements
  make-svg-document
  svg-document->sxml
  write-svg
  write-svg-to-file
  svg->string)

;; Document wrapper struct
(defstruct svg-document (width height viewbox elements)
  transparent: #t)

;; Build the full SXML tree with xmlns
(def (svg-document->sxml doc)
  (let ((w (n->s (svg-document-width doc)))
        (h (n->s (svg-document-height doc))))
    `(svg (@ (xmlns "http://www.w3.org/2000/svg")
             (width ,w)
             (height ,h)
             ,@(if (svg-document-viewbox doc)
                 `((viewBox ,(svg-document-viewbox doc)))
                 '()))
          ,@(svg-document-elements doc))))

;; Write to port (default: current-output-port)
(def (write-svg doc (port (current-output-port)))
  (parameterize ((current-output-port port))
    (write-xml (svg-document->sxml doc))))

;; Write to file
(def (write-svg-to-file doc filename)
  (call-with-output-file filename
    (lambda (port) (write-svg doc port))))

;; Convert to string
(def (svg->string doc)
  (call-with-output-string
    (lambda (port) (write-svg doc port))))
