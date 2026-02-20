(import :std/test :gerbil-svg/color)
(export color-test)

(def color-test
  (test-suite "SVG colors"
    (test-case "rgb format"
      (check (rgb 255 0 0) => "rgb(255,0,0)"))
    (test-case "rgba format"
      (check (rgba 255 0 0 0.5) => "rgba(255,0,0,.5)"))
    (test-case "hsl format"
      (check (hsl 120 50 50) => "hsl(120,50%,50%)"))
    (test-case "hsla format"
      (check (hsla 120 50 50 0.8) => "hsla(120,50%,50%,.8)"))
    (test-case "named colors are strings"
      (check (string? color-black) ? values)
      (check (string? color-none) ? values))
    (test-case "color->string passthrough"
      (check (color->string "red") => "red")
      (check (color->string 42) => "42"))))
