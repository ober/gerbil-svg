(import :std/test :gerbil-svg/path)
(export path-test)

(def path-test
  (test-suite "SVG path builder"
    (test-case "simple line path"
      (let* ((pb (make-path-builder []))
             (pb (path-move-to pb 0 0))
             (pb (path-line-to pb 100 100))
             (pb (path-close pb)))
        (check (path-build pb) => "M0 0 L100 100 Z")))

    (test-case "cubic bezier"
      (let* ((pb (make-path-builder []))
             (pb (path-move-to pb 0 0))
             (pb (path-cubic-to pb 10 20 30 40 50 60)))
        (check (path-build pb) => "M0 0 C10 20 30 40 50 60")))

    (test-case "quadratic bezier"
      (let* ((pb (make-path-builder []))
             (pb (path-move-to pb 0 0))
             (pb (path-quadratic-to pb 50 50 100 0)))
        (check (path-build pb) => "M0 0 Q50 50 100 0")))

    (test-case "horizontal and vertical"
      (let* ((pb (make-path-builder []))
             (pb (path-move-to pb 0 0))
             (pb (path-horizontal-to pb 100))
             (pb (path-vertical-to pb 50)))
        (check (path-build pb) => "M0 0 H100 V50")))

    (test-case "relative move and line"
      (let* ((pb (make-path-builder []))
             (pb (path-move-to pb 10 10))
             (pb (path-rel-line-to pb 20 30)))
        (check (path-build pb) => "M10 10 l20 30")))

    (test-case "empty path"
      (let ((pb (make-path-builder [])))
        (check (path-build pb) => "")))))
