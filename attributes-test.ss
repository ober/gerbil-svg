(import :std/test
        :gerbil-svg/attributes)
(export attributes-test)

(def attributes-test
  (test-suite "SVG attributes"
    (test-case "n->s converts numbers"
      (check (n->s 42) => "42")
      (check (n->s 3.14) => "3.14")
      (check (n->s "hello") => "hello"))

    (test-case "opt-attr with value"
      (check (opt-attr 'fill "red") => '((fill "red"))))

    (test-case "opt-attr with #f"
      (check (opt-attr 'fill #f) => '()))

    (test-case "opt-attrs filters #f values"
      (check (opt-attrs fill: "red" stroke: #f id: "test")
        => '((fill "red") (id "test"))))

    (test-case "opt-attrs all #f"
      (check (opt-attrs fill: #f stroke: #f) => '()))

    (test-case "points->string"
      (check (points->string '((0 0) (10 20) (30 40)))
        => "0,0 10,20 30,40"))

    (test-case "style-string"
      (check (style-string fill: "red" stroke: "black")
        => "fill:red;stroke:black"))

    (test-case "style-string filters #f"
      (check (style-string fill: "red" stroke: #f)
        => "fill:red"))))
