(import :gerbil-svg/attributes)
(export
  svg-rect svg-circle svg-ellipse svg-line
  svg-polyline svg-polygon
  svg-path
  svg-text
  svg-g svg-defs svg-use svg-symbol
  svg-linear-gradient svg-radial-gradient svg-stop
  svg-clip-path
  svg-image
  svg-marker)

;; (svg-rect x y width height . attrs) => SXML
(def (svg-rect x y width height
               fill: (fill #f)
               stroke: (stroke #f)
               stroke-width: (stroke-width #f)
               rx: (rx #f) ry: (ry #f)
               opacity: (opacity #f)
               transform: (transform #f)
               class: (class #f)
               id: (id #f))
  `(rect (@ (x ,(n->s x)) (y ,(n->s y))
            (width ,(n->s width)) (height ,(n->s height))
            ,@(opt-attrs fill: fill stroke: stroke
                         stroke-width: stroke-width
                         rx: rx ry: ry opacity: opacity
                         transform: transform class: class id: id))))

;; (svg-circle cx cy r . attrs) => SXML
(def (svg-circle cx cy r
                 fill: (fill #f)
                 stroke: (stroke #f)
                 stroke-width: (stroke-width #f)
                 opacity: (opacity #f)
                 transform: (transform #f)
                 class: (class #f)
                 id: (id #f))
  `(circle (@ (cx ,(n->s cx)) (cy ,(n->s cy)) (r ,(n->s r))
              ,@(opt-attrs fill: fill stroke: stroke
                           stroke-width: stroke-width
                           opacity: opacity transform: transform
                           class: class id: id))))

;; (svg-ellipse cx cy rx ry . attrs) => SXML
(def (svg-ellipse cx cy rx ry
                  fill: (fill #f)
                  stroke: (stroke #f)
                  stroke-width: (stroke-width #f)
                  opacity: (opacity #f)
                  transform: (transform #f)
                  class: (class #f)
                  id: (id #f))
  `(ellipse (@ (cx ,(n->s cx)) (cy ,(n->s cy))
               (rx ,(n->s rx)) (ry ,(n->s ry))
               ,@(opt-attrs fill: fill stroke: stroke
                            stroke-width: stroke-width
                            opacity: opacity transform: transform
                            class: class id: id))))

;; (svg-line x1 y1 x2 y2 . attrs) => SXML
(def (svg-line x1 y1 x2 y2
               stroke: (stroke "black")
               stroke-width: (stroke-width #f)
               stroke-dasharray: (stroke-dasharray #f)
               opacity: (opacity #f)
               transform: (transform #f)
               class: (class #f)
               id: (id #f))
  `(line (@ (x1 ,(n->s x1)) (y1 ,(n->s y1))
            (x2 ,(n->s x2)) (y2 ,(n->s y2))
            ,@(opt-attrs stroke: stroke stroke-width: stroke-width
                         stroke-dasharray: stroke-dasharray
                         opacity: opacity transform: transform
                         class: class id: id))))

;; (svg-polyline points . attrs) => SXML
;; points is a list of (x y) pairs: ((0 0) (10 20) (30 40))
(def (svg-polyline points
                   fill: (fill "none")
                   stroke: (stroke "black")
                   stroke-width: (stroke-width #f)
                   stroke-dasharray: (stroke-dasharray #f)
                   opacity: (opacity #f)
                   transform: (transform #f)
                   class: (class #f)
                   id: (id #f))
  `(polyline (@ (points ,(points->string points))
                ,@(opt-attrs fill: fill stroke: stroke
                             stroke-width: stroke-width
                             stroke-dasharray: stroke-dasharray
                             opacity: opacity transform: transform
                             class: class id: id))))

;; (svg-polygon points . attrs) => SXML
(def (svg-polygon points
                  fill: (fill #f)
                  stroke: (stroke #f)
                  stroke-width: (stroke-width #f)
                  opacity: (opacity #f)
                  transform: (transform #f)
                  class: (class #f)
                  id: (id #f))
  `(polygon (@ (points ,(points->string points))
               ,@(opt-attrs fill: fill stroke: stroke
                            stroke-width: stroke-width
                            opacity: opacity transform: transform
                            class: class id: id))))

;; (svg-path d . attrs) => SXML
;; d is a path data string (see path.ss for builder)
(def (svg-path d
               fill: (fill #f)
               stroke: (stroke #f)
               stroke-width: (stroke-width #f)
               stroke-dasharray: (stroke-dasharray #f)
               opacity: (opacity #f)
               transform: (transform #f)
               class: (class #f)
               id: (id #f))
  `(path (@ (d ,d)
            ,@(opt-attrs fill: fill stroke: stroke
                         stroke-width: stroke-width
                         stroke-dasharray: stroke-dasharray
                         opacity: opacity transform: transform
                         class: class id: id))))

;; (svg-text x y content . attrs) => SXML
;; content is a string
(def (svg-text x y content
               font-family: (font-family #f)
               font-size: (font-size #f)
               font-weight: (font-weight #f)
               text-anchor: (text-anchor #f)
               dominant-baseline: (dominant-baseline #f)
               fill: (fill #f)
               opacity: (opacity #f)
               transform: (transform #f)
               class: (class #f)
               id: (id #f))
  `(text (@ (x ,(n->s x)) (y ,(n->s y))
            ,@(opt-attrs font-family: font-family
                         font-size: font-size
                         font-weight: font-weight
                         text-anchor: text-anchor
                         dominant-baseline: dominant-baseline
                         fill: fill opacity: opacity
                         transform: transform class: class id: id))
         ,content))

;; (svg-g elements . attrs) => SXML
;; elements is a list of SXML fragments
(def (svg-g elements
            transform: (transform #f)
            class: (class #f)
            id: (id #f)
            opacity: (opacity #f))
  `(g (@ ,@(opt-attrs transform: transform class: class
                      id: id opacity: opacity))
      ,@elements))

;; (svg-defs . elements) => SXML
(def (svg-defs . elements)
  `(defs ,@elements))

;; (svg-use href . attrs) => SXML
(def (svg-use href
              x: (x #f) y: (y #f)
              width: (width #f) height: (height #f)
              transform: (transform #f))
  `(use (@ (href ,href)
           ,@(opt-attrs x: x y: y width: width height: height
                        transform: transform))))

;; (svg-symbol id elements . attrs) => SXML
(def (svg-symbol id elements
                 viewBox: (viewBox #f))
  `(symbol (@ (id ,id)
              ,@(opt-attrs viewBox: viewBox))
           ,@elements))

;; (svg-linear-gradient id x1 y1 x2 y2 stops) => SXML
(def (svg-linear-gradient id x1 y1 x2 y2 stops)
  `(linearGradient (@ (id ,id)
                      (x1 ,(n->s x1)) (y1 ,(n->s y1))
                      (x2 ,(n->s x2)) (y2 ,(n->s y2)))
                   ,@stops))

;; (svg-radial-gradient id cx cy r stops . attrs) => SXML
(def (svg-radial-gradient id cx cy r stops
                          fx: (fx #f) fy: (fy #f))
  `(radialGradient (@ (id ,id)
                      (cx ,(n->s cx)) (cy ,(n->s cy)) (r ,(n->s r))
                      ,@(opt-attrs fx: fx fy: fy))
                   ,@stops))

;; (svg-stop offset color (opacity 1.0)) => SXML
(def (svg-stop offset color (opacity 1.0))
  `(stop (@ (offset ,(n->s offset))
            (stop-color ,color)
            (stop-opacity ,(n->s opacity)))))

;; (svg-clip-path id . elements) => SXML
(def (svg-clip-path id . elements)
  `(clipPath (@ (id ,id)) ,@elements))

;; (svg-image href x y width height . attrs) => SXML
(def (svg-image href x y width height
                transform: (transform #f)
                opacity: (opacity #f)
                class: (class #f)
                id: (id #f))
  `(image (@ (href ,href)
             (x ,(n->s x)) (y ,(n->s y))
             (width ,(n->s width)) (height ,(n->s height))
             ,@(opt-attrs transform: transform opacity: opacity
                          class: class id: id))))

;; (svg-marker id viewBox refX refY width height elements . attrs) => SXML
(def (svg-marker id viewBox refX refY width height elements
                 orient: (orient #f)
                 markerUnits: (markerUnits #f))
  `(marker (@ (id ,id)
              (viewBox ,viewBox)
              (refX ,(n->s refX)) (refY ,(n->s refY))
              (markerWidth ,(n->s width)) (markerHeight ,(n->s height))
              ,@(opt-attrs orient: orient markerUnits: markerUnits))
           ,@elements))
