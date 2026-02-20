(import :std/test
        :std/xml
        :gerbil-svg/document
        :gerbil-svg/elements)
(export document-test)

(def document-test
  (test-suite "SVG document"
    (test-case "write-svg produces valid XML string"
      (let* ((doc (make-svg-document 100 100 #f
                    [(svg-rect 0 0 100 100 fill: "white")]))
             (xml-str (svg->string doc)))
        (check (string? xml-str) ? values)
        (check (string-contains xml-str "xmlns") ? values)
        (check (string-contains xml-str "<rect") ? values)))

    (test-case "viewBox attribute"
      (let* ((doc (make-svg-document 800 600 "0 0 800 600" []))
             (xml-str (svg->string doc)))
        (check (string-contains xml-str "viewBox") ? values)))

    (test-case "svg-document->sxml structure"
      (let* ((doc (make-svg-document 200 100 #f []))
             (sxml (svg-document->sxml doc)))
        (check (car sxml) => 'svg)))))
