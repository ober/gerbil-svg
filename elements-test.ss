(import :std/test
        :gerbil-svg/elements
        :gerbil-svg/attributes)
(export elements-test)

(def elements-test
  (test-suite "SVG elements"
    (test-case "svg-rect generates correct SXML"
      (check (svg-rect 10 20 100 50)
        => '(rect (@ (x "10") (y "20") (width "100") (height "50")))))

    (test-case "svg-rect with optional attributes"
      (let ((r (svg-rect 0 0 50 50 fill: "red" stroke: "black" stroke-width: 2)))
        (check (car r) => 'rect)))

    (test-case "svg-circle"
      (check (svg-circle 50 50 25)
        => '(circle (@ (cx "50") (cy "50") (r "25")))))

    (test-case "svg-ellipse"
      (check (svg-ellipse 50 50 30 20)
        => '(ellipse (@ (cx "50") (cy "50") (rx "30") (ry "20")))))

    (test-case "svg-line"
      (let ((l (svg-line 0 0 100 100 stroke: "blue")))
        (check (car l) => 'line)))

    (test-case "svg-text"
      (let ((t (svg-text 10 20 "Hello" font-size: 14)))
        (check (car t) => 'text)))

    (test-case "svg-g groups elements"
      (let ((g (svg-g [(svg-rect 0 0 10 10) (svg-circle 5 5 3)])))
        (check (car g) => 'g)))

    (test-case "svg-polyline"
      (let ((pl (svg-polyline '((0 0) (10 20) (30 40)))))
        (check (car pl) => 'polyline)))

    (test-case "svg-polygon"
      (let ((pg (svg-polygon '((0 0) (50 0) (50 50) (0 50)))))
        (check (car pg) => 'polygon)))

    (test-case "svg-path"
      (let ((p (svg-path "M0 0 L100 100 Z" fill: "none" stroke: "red")))
        (check (car p) => 'path)))

    (test-case "svg-defs"
      (let ((d (svg-defs '(stop (@ (offset "0") (stop-color "red"))))))
        (check (car d) => 'defs)))

    (test-case "svg-stop"
      (check (svg-stop 0 "red")
        => '(stop (@ (offset "0") (stop-color "red") (stop-opacity "1.")))))

    (test-case "svg-linear-gradient"
      (let ((g (svg-linear-gradient "grad1" 0 0 1 0
                [(svg-stop 0 "red") (svg-stop 1 "blue")])))
        (check (car g) => 'linearGradient)))))
