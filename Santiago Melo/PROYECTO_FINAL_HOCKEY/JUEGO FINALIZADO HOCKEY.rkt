#lang racket
(displayln (current-directory))
(require graphics/graphics)

(require racket/gui/base)

(open-graphics)

(define ventana (open-viewport "AIR HOCKEY" 500 700))
(define ya-sonido-menu? #f)

(define FX 70)
(define FY 80)
(define FW 360)
(define FH 540)

(define FRICCION 0.985)
(define RADIO-DISCO 12)
(define RADIO-PALETA 30)

(define-struct juego
  (dificultad
   disco-x disco-y disco-vx disco-vy
   disco-x-prev disco-y-prev
   rival-x rival-y rival-x-prev rival-y-prev
   jugador-x jugador-y jugador-x-prev jugador-y-prev
   score-j score-r
   cancha-dibujada))

;======================
;TEXTURAS
;======================

(define (textura-y-vp vp x y)
  (when (< y (+ FY FH -30))
    ((draw-solid-ellipse vp) (make-posn x y) 2 2 "darkgray")
    ((draw-ellipse vp) (make-posn (+ x 1) (+ y 1)) 1 1 "gray")
    (textura-y-vp vp x (+ y 35))))

(define (textura-x-vp vp x)
  (when (< x (+ FX FW -30))
    (textura-y-vp vp x (+ FY 30))
    (textura-x-vp vp (+ x 35))))

(define (decor-top-vp vp x)
  (when (<= x (+ FX FW -40))
    ((draw-solid-ellipse vp) (make-posn x (+ FY 4)) 10 10 "purple")
    ((draw-ellipse vp) (make-posn x (+ FY 4)) 10 10 "violet")
    ((draw-ellipse vp) (make-posn x (+ FY 4)) 6 6 "white")
    (decor-top-vp vp (+ x 36))))

(define (decor-bot-vp vp x)
  (when (<= x (+ FX FW -40))
    ((draw-solid-ellipse vp) (make-posn x (- (+ FY FH) 12)) 10 10 "purple")
    ((draw-ellipse vp) (make-posn x (- (+ FY FH) 12)) 10 10 "violet")
    ((draw-ellipse vp) (make-posn x (- (+ FY FH) 12)) 6 6 "white")
    (decor-bot-vp vp (+ x 36))))

(define (decor-left-vp vp y)
  (when (<= y (+ FY FH -40))
    ((draw-solid-ellipse vp) (make-posn (+ FX 4) y) 10 10 "purple")
    ((draw-ellipse vp) (make-posn (+ FX 4) y) 10 10 "violet")
    ((draw-ellipse vp) (make-posn (+ FX 4) y) 6 6 "white")
    (decor-left-vp vp (+ y 36))))

(define (decor-right-vp vp y)
  (when (<= y (+ FY FH -40))
    ((draw-solid-ellipse vp) (make-posn (- (+ FX FW) 12) y) 10 10 "purple")
    ((draw-ellipse vp) (make-posn (- (+ FX FW) 12) y) 10 10 "violet")
    ((draw-ellipse vp) (make-posn (- (+ FX FW) 12) y) 6 6 "white")
    (decor-right-vp vp (+ y 36))))

(define (lineas-laterales-vp vp y)
  (when (< y (+ FY FH -60))
    ((draw-solid-rectangle vp) (make-posn (+ FX 20) y) 8 2 "purple")
    ((draw-solid-rectangle vp) (make-posn (+ FX FW -28) y) 8 2 "purple")
    (lineas-laterales-vp vp (+ y 50))))

;==============================
;CANCHA ORIGINAL 
;==============================

(define (textura-y x y)
  (when (< y (+ FY FH -30))
    ((draw-solid-ellipse ventana) (make-posn x y) 2 2 "darkgray")
    ((draw-ellipse ventana) (make-posn (+ x 1) (+ y 1)) 1 1 "gray")
    (textura-y x (+ y 35))))

(define (textura-x x)
  (when (< x (+ FX FW -30))
    (textura-y x (+ FY 30))
    (textura-x (+ x 35))))

(define (decor-top x)
  (when (<= x (+ FX FW -40))
    ((draw-solid-ellipse ventana) (make-posn x (+ FY 4)) 10 10 "purple")
    ((draw-ellipse ventana) (make-posn x (+ FY 4)) 10 10 "violet")
    ((draw-ellipse ventana) (make-posn x (+ FY 4)) 6 6 "white")
    (decor-top (+ x 36))))

(define (decor-bot x)
  (when (<= x (+ FX FW -40))
    ((draw-solid-ellipse ventana) (make-posn x (- (+ FY FH) 12)) 10 10 "purple")
    ((draw-ellipse ventana) (make-posn x (- (+ FY FH) 12)) 10 10 "violet")
    ((draw-ellipse ventana) (make-posn x (- (+ FY FH) 12)) 6 6 "white")
    (decor-bot (+ x 36))))

(define (decor-left y)
  (when (<= y (+ FY FH -40))
    ((draw-solid-ellipse ventana) (make-posn (+ FX 4) y) 10 10 "purple")
    ((draw-ellipse ventana) (make-posn (+ FX 4) y) 10 10 "violet")
    ((draw-ellipse ventana) (make-posn (+ FX 4) y) 6 6 "white")
    (decor-left (+ y 36))))

(define (decor-right y)
  (when (<= y (+ FY FH -40))
    ((draw-solid-ellipse ventana) (make-posn (- (+ FX FW) 12) y) 10 10 "purple")
    ((draw-ellipse ventana) (make-posn (- (+ FX FW) 12) y) 10 10 "violet")
    ((draw-ellipse ventana) (make-posn (- (+ FX FW) 12) y) 6 6 "white")
    (decor-right (+ y 36))))

(define (lineas-laterales y)
  (when (< y (+ FY FH -60))
    ((draw-solid-rectangle ventana)
     (make-posn (+ FX 20) y) 8 2 "purple")
    ((draw-solid-rectangle ventana)
     (make-posn (+ FX FW -28) y) 8 2 "purple")
    (lineas-laterales (+ y 50))))

(define (arco-superior ang)
  (when (<= ang 180)
    ((draw-solid-ellipse ventana)
     (make-posn
      (- (+ (+ FX (/ FW 2))
            (* 70 (cos (/ (* ang 3.1416) 180)))) 2)
      (- (+ (+ FY 120)
            (* 70 (sin (/ (* ang 3.1416) 180)))) 90))
     5 5 "violet")
    ((draw-ellipse ventana)
     (make-posn
      (- (+ (+ FX (/ FW 2))
            (* 60 (cos (/ (* ang 3.1416) 180)))) 2)
      (- (+ (+ FY 120)
            (* 60 (sin (/ (* ang 3.1416) 180)))) 90))
     3 3 "white")
    (arco-superior (+ ang 4))))

(define (arco-inferior ang)
  (when (<= ang 180)
    ((draw-solid-ellipse ventana)
     (make-posn
      (- (+ (+ FX (/ FW 2))
            (* 70 (cos (/ (* (+ ang 180) 3.1416) 180)))) 2)
      (- (+ (- (+ FY FH) 120)
            (* 70 (sin (/ (* (+ ang 180) 3.1416) 180)))) -90))
     5 5 "violet")
    ((draw-ellipse ventana)
     (make-posn
      (- (+ (+ FX (/ FW 2))
            (* 60 (cos (/ (* (+ ang 180) 3.1416) 180)))) 2)
      (- (+ (- (+ FY FH) 120)
            (* 60 (sin (/ (* (+ ang 180) 3.1416) 180)))) -90))
     3 3 "white")
    (arco-inferior (+ ang 4))))

(define (dientes x)
  (let loop ([i x])
    (when (< i (+ FX FW -80))
      ((draw-solid-ellipse ventana)
       (make-posn i (+ FY 20)) 6 16 "purple")
      ((draw-ellipse ventana)
       (make-posn i (+ FY 20)) 4 14 "violet")
      (loop (+ i 12)))))

(define (dibujar-estrella x y size color)
  ((draw-solid-ellipse ventana)
   (make-posn (- x (/ size 2)) (- y (/ size 2)))
   size size color)
  ((draw-solid-rectangle ventana)
   (make-posn (- x 1) (- y size))
   2 (* size 2) color)
  ((draw-solid-rectangle ventana)
   (make-posn (- x size) (- y 1))
   (* size 2) 2 color))

(define (decorar-esquinas)
  (dibujar-estrella (+ FX 30) (+ FY 30) 8 "violet")
  (dibujar-estrella (+ FX FW -30) (+ FY 30) 8 "violet")
  (dibujar-estrella (+ FX 30) (+ FY FH -30) 8 "violet")
  (dibujar-estrella (+ FX FW -30) (+ FY FH -30) 8 "violet"))


(define (dibujar-cancha-fija-vp vp)
  ((draw-solid-rectangle vp) (make-posn 0 0) 500 700 "black")
  ;Fondo negro
  ((draw-solid-rectangle vp) (make-posn FX FY) FW FH "black")
  (textura-x-vp vp (+ FX 30))

  ;Bordes
  
  ((draw-solid-rectangle vp) (make-posn FX FY) FW 14 "purple")
  ((draw-solid-rectangle vp) (make-posn FX (+ FY 2)) FW 8 "violet")
  ((draw-solid-rectangle vp) (make-posn FX (+ FY FH -14)) FW 14 "purple")
  ((draw-solid-rectangle vp) (make-posn FX (+ FY FH -10)) FW 8 "violet")

  ((draw-solid-rectangle vp) (make-posn FX FY) 14 FH "purple")
  ((draw-solid-rectangle vp) (make-posn (+ FX 2) FY) 8 FH "violet")
  ((draw-solid-rectangle vp) (make-posn (+ FX FW -14) FY) 14 FH "purple")
  ((draw-solid-rectangle vp) (make-posn (+ FX FW -10) FY) 8 FH "violet")

  ; Línea interna
  ((draw-rectangle vp) (make-posn (+ FX 12) (+ FY 12)) (- FW 24) (- FH 24) "violet")
  ((draw-rectangle vp) (make-posn (+ FX 14) (+ FY 14)) (- FW 28) (- FH 28) "white")

  (decor-top-vp vp (+ FX 40))
  (decor-bot-vp vp (+ FX 40))
  (decor-left-vp vp (+ FY 40))
  (decor-right-vp vp (+ FY 40))
  (lineas-laterales-vp vp (+ FY 80))

  ;LÍNEA CENTRAL
  ((draw-solid-rectangle vp)
   (make-posn (+ FX 40) (+ FY (- (/ FH 2) 4))) (- FW 80) 8 "red")
  ((draw-solid-rectangle vp)
   (make-posn (+ FX 42) (+ FY (- (/ FH 2) 3))) (- FW 84) 6 "white")

  ;CÍRCULO CENTRAL
  ((draw-ellipse vp)
   (make-posn (- (+ FX (/ FW 2)) 65) (- (+ FY (/ FH 2)) 65)) 130 130 "red")
  ((draw-ellipse vp)
   (make-posn (- (+ FX (/ FW 2)) 60) (- (+ FY (/ FH 2)) 60)) 120 120 "violet")
  ((draw-ellipse vp)
   (make-posn (- (+ FX (/ FW 2)) 55) (- (+ FY (/ FH 2)) 55)) 110 110 "white")

  ((draw-solid-ellipse vp)
   (make-posn (- (+ FX (/ FW 2)) 8) (- (+ FY (/ FH 2)) 8)) 16 16 "red")
  ((draw-solid-ellipse vp)
   (make-posn (- (+ FX (/ FW 2)) 5) (- (+ FY (/ FH 2)) 5)) 10 10 "violet")

  ;ZONAS DE GOL
  ((draw-rectangle vp)
   (make-posn (+ FX (/ FW 2) -60) (+ FY 25)) 120 70 "cyan")
  ((draw-solid-rectangle vp)
   (make-posn (+ FX (/ FW 2) -58) (+ FY 27)) 116 66 "darkblue")

  ((draw-rectangle vp)
   (make-posn (+ FX (/ FW 2) -60) (+ FY FH -95)) 120 70 "cyan")
  ((draw-solid-rectangle vp)
   (make-posn (+ FX (/ FW 2) -58) (+ FY FH -93)) 116 66 "darkblue")

  ;LÍNEAS AZULES
  ((draw-solid-rectangle vp)
   (make-posn (+ FX 40) (+ FY 140)) (- FW 80) 5 "blue")
  ((draw-solid-rectangle vp)
   (make-posn (+ FX 42) (+ FY 141)) (- FW 84) 3 "cyan")

  ((draw-solid-rectangle vp)
   (make-posn (+ FX 40) (+ FY FH -145)) (- FW 80) 5 "blue")
  ((draw-solid-rectangle vp)
   (make-posn (+ FX 42) (+ FY FH -144)) (- FW 84) 3 "cyan")

  ; CÍRCULOS DE FACE-OFF
  ((draw-ellipse vp)
   (make-posn (+ FX 70) (+ FY 90)) 40 40 "red")
  ((draw-solid-ellipse vp)
   (make-posn (+ FX 85) (+ FY 105)) 10 10 "red")

  ((draw-ellipse vp)
   (make-posn (+ FX FW -110) (+ FY 90)) 40 40 "red")
  ((draw-solid-ellipse vp)
   (make-posn (+ FX FW -95) (+ FY 105)) 10 10 "red")

  ((draw-ellipse vp)
   (make-posn (+ FX 70) (+ FY FH -130)) 40 40 "red")
  ((draw-solid-ellipse vp)
   (make-posn (+ FX 85) (+ FY FH -115)) 10 10 "red")

  ((draw-ellipse vp)
   (make-posn (+ FX FW -110) (+ FY FH -130)) 40 40 "red")
  ((draw-solid-ellipse vp)
   (make-posn (+ FX FW -95) (+ FY FH -115)) 10 10 "red"))

;============================================
; NUMEROS DEL MARCADOR  (SOMBRA)
;============================================

(define (dibujar-numero-sombra num x y)
  (case num
    [(0) ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 1)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 1) (+ y 5)) 4 20 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 15) (+ y 5)) 4 20 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 25)) 14 4 "purple")]
    [(1) ((draw-solid-rectangle ventana) (make-posn (+ x 15) (+ y 1)) 4 28 "purple")]
    [(2) ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 1)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 15) (+ y 5)) 4 10 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 13)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 1) (+ y 15)) 4 10 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 25)) 14 4 "purple")]
    [(3) ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 1)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 15) (+ y 5)) 4 10 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 13)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 15) (+ y 15)) 4 10 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 25)) 14 4 "purple")]
    [(4) ((draw-solid-rectangle ventana) (make-posn (+ x 1) (+ y 1)) 4 14 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 13)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 15) (+ y 1)) 4 28 "purple")]
    [(5) ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 1)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 1) (+ y 5)) 4 10 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 13)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 15) (+ y 15)) 4 10 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 25)) 14 4 "purple")]
    [(6) ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 1)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 1) (+ y 5)) 4 20 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 13)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 15) (+ y 15)) 4 10 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 25)) 14 4 "purple")]
    [(7) ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 1)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 15) (+ y 1)) 4 28 "purple")]
    [(8) ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 1)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 1) (+ y 5)) 4 20 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 13)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 15) (+ y 5)) 4 20 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 25)) 14 4 "purple")]
    [(9) ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 1)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 1) (+ y 5)) 4 10 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 13)) 14 4 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 15) (+ y 5)) 4 20 "purple")
         ((draw-solid-rectangle ventana) (make-posn (+ x 3) (+ y 25)) 14 4 "purple")]))

;============================================
;NUMEROS PRINCIPALES (VIOLETA)
;============================================

(define (dibujar-numero num x y)
  (dibujar-numero-sombra num x y)
  (case num
    [(0) ((draw-solid-rectangle ventana) (make-posn (+ x 2) y) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn x (+ y 4)) 4 20 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 14) (+ y 4)) 4 20 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 2) (+ y 24)) 14 4 "violet")]
    [(1) ((draw-solid-rectangle ventana) (make-posn (+ x 14) y) 4 28 "violet")]
    [(2) ((draw-solid-rectangle ventana) (make-posn (+ x 2) y) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 14) (+ y 4)) 4 10 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 2) (+ y 12)) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn x (+ y 14)) 4 10 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 2) (+ y 24)) 14 4 "violet")]
    [(3) ((draw-solid-rectangle ventana) (make-posn (+ x 2) y) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 14) (+ y 4)) 4 10 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 2) (+ y 12)) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 14) (+ y 14)) 4 10 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 2) (+ y 24)) 14 4 "violet")]
    [(4) ((draw-solid-rectangle ventana) (make-posn x y) 4 14 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 2) (+ y 12)) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 14) y) 4 28 "violet")]
    [(5) ((draw-solid-rectangle ventana) (make-posn (+ x 2) y) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn x (+ y 4)) 4 10 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 2) (+ y 12)) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 14) (+ y 14)) 4 10 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 2) (+ y 24)) 14 4 "violet")]
    [(6) ((draw-solid-rectangle ventana) (make-posn (+ x 2) y) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn x (+ y 4)) 4 20 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 2) (+ y 12)) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 14) (+ y 14)) 4 10 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 2) (+ y 24)) 14 4 "violet")]
    [(7) ((draw-solid-rectangle ventana) (make-posn (+ x 2) y) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 14) y) 4 28 "violet")]
    [(8) ((draw-solid-rectangle ventana) (make-posn (+ x 2) y) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn x (+ y 4)) 4 20 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 2) (+ y 12)) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 14) (+ y 4)) 4 20 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 2) (+ y 24)) 14 4 "violet")]
    [(9) ((draw-solid-rectangle ventana) (make-posn (+ x 2) y) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn x (+ y 4)) 4 10 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 2) (+ y 12)) 14 4 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 14) (+ y 4)) 4 20 "violet")
         ((draw-solid-rectangle ventana) (make-posn (+ x 2) (+ y 24)) 14 4 "violet")]))

(define (dibujar-score score x y)
  (if (< score 10)
      (dibujar-numero score x y)
      (begin
        (dibujar-numero (quotient score 10) x y)
        (dibujar-numero (remainder score 10) (+ x 22) y))))

;============================================
; CREAR FONDO (PIXMAP) eliminar parpadeo 
;============================================

(define fondo
  (let ([vp (open-viewport "fondo-temp" 500 700)])
    (dibujar-cancha-fija-vp vp)
    (close-viewport vp)
    vp))

;============================================
;DIBUJAR ESCENA COMPLETA (cada fotograma)
;============================================

(define (dibujar-escena j)
  ;Copia fondo
  ((draw-solid-rectangle ventana) (make-posn 0 0) 500 700 "black")
  (copy-viewport fondo ventana)

  ;Dibujar marcador
  ((draw-solid-rectangle ventana) (make-posn (+ FX FW -90) (+ FY 185)) 80 100 "purple")
  ((draw-solid-rectangle ventana) (make-posn (+ FX FW -85) (+ FY 190)) 70 90 "black")
  ((draw-rectangle ventana) (make-posn (+ FX FW -85) (+ FY 190)) 70 90 "violet")
  (dibujar-score (juego-score-r j) (+ FX FW -80) (+ FY 200))
  (dibujar-score (juego-score-j j) (+ FX FW -80) (+ FY 245))

  ;RIVAL — ROJO
  ((draw-solid-ellipse ventana)
   (make-posn (- (juego-rival-x j) 32) (- (juego-rival-y j) 32)) 64 64 "purple")
  ((draw-solid-ellipse ventana)
   (make-posn (- (juego-rival-x j) 30) (- (juego-rival-y j) 30)) 60 60 "red")
  ((draw-solid-ellipse ventana)
   (make-posn (- (juego-rival-x j) 22) (- (juego-rival-y j) 22)) 44 44 "white")

  ;PELOTA — BLANCA
  ((draw-solid-ellipse ventana)
   (make-posn (- (juego-disco-x j) (+ RADIO-DISCO 3))
              (- (juego-disco-y j) (+ RADIO-DISCO 3)))
   (* 2 (+ RADIO-DISCO 3)) (* 2 (+ RADIO-DISCO 3)) "violet")
  ((draw-solid-ellipse ventana)
   (make-posn (- (juego-disco-x j) RADIO-DISCO) (- (juego-disco-y j) RADIO-DISCO))
   (* 2 RADIO-DISCO) (* 2 RADIO-DISCO) "white")

  ;JUGADOR — CYAN
  ((draw-solid-ellipse ventana)
   (make-posn (- (juego-jugador-x j) 32) (- (juego-jugador-y j) 32)) 64 64 "purple")
  ((draw-solid-ellipse ventana)
   (make-posn (- (juego-jugador-x j) 30) (- (juego-jugador-y j) 30)) 60 60 "cyan")
  ((draw-solid-ellipse ventana)
   (make-posn (- (juego-jugador-x j) 22) (- (juego-jugador-y j) 22)) 44 44 "white")

  ;ACTUALIZAR PREVIOS
  (make-juego
   (juego-dificultad j)
   (juego-disco-x j) (juego-disco-y j)
   (juego-disco-vx j) (juego-disco-vy j)
   (juego-disco-x j) (juego-disco-y j)
   (juego-rival-x j) (juego-rival-y j)
   (juego-rival-x j) (juego-rival-y j)
   (juego-jugador-x j) (juego-jugador-y j)
   (juego-jugador-x j) (juego-jugador-y j)
   (juego-score-j j) (juego-score-r j)
   #t))

;=====================================================
;MOVIMIENTO — JUGADOR
;=====================================================

(define (limitar-jugador-x x)
  (cond
    [(< x (+ FX 42)) (+ FX 42)]
    [(> x (+ FX FW -42)) (+ FX FW -42)]
    [else x]))

(define (limitar-jugador-y y)
  (cond
    [(< y (+ FY (/ FH 2) 20)) (+ FY (/ FH 2) 20)]
    [(> y (+ FY FH -42)) (+ FY FH -42)]
    [else y]))

(define (mover-jugador j)
  (let ([pos (query-mouse-posn ventana)])
    (if pos
        (make-juego
         (juego-dificultad j)
         (juego-disco-x j)
         (juego-disco-y j)
         (juego-disco-vx j)
         (juego-disco-vy j)
         (juego-disco-x-prev j)
         (juego-disco-y-prev j)
         (juego-rival-x j)
         (juego-rival-y j)
         (juego-rival-x-prev j)
         (juego-rival-y-prev j)

         (limitar-jugador-x (posn-x pos))
         (limitar-jugador-y (posn-y pos))

         (juego-jugador-x j)
         (juego-jugador-y j)

         (juego-score-j j)
         (juego-score-r j)
         (juego-cancha-dibujada j))
        j)))

;=====================================================
;MOVIMIENTO PELOTA
;=====================================================

(define (mover-pelota j)
  (make-juego
   (juego-dificultad j)
   (+ (juego-disco-x j) (juego-disco-vx j))
   (+ (juego-disco-y j) (juego-disco-vy j))
   (* (juego-disco-vx j) FRICCION)
   (* (juego-disco-vy j) FRICCION)
   (juego-disco-x j) (juego-disco-y j)
   (juego-rival-x j) (juego-rival-y j)
   (juego-rival-x-prev j) (juego-rival-y-prev j)
   (juego-jugador-x j) (juego-jugador-y j)
   (juego-jugador-x-prev j) (juego-jugador-y-prev j)
   (juego-score-j j) (juego-score-r j)
   (juego-cancha-dibujada j)))

;=====================================================
;REBOTES PARED 
; =====================================================
(define (rebote-pelota j)
  (cond
    [(< (juego-disco-x j) (+ FX 32))
     (make-juego
      (juego-dificultad j)
      (+ FX 32) (juego-disco-y j)
      (- (juego-disco-vx j)) (juego-disco-vy j)
      (juego-disco-x-prev j) (juego-disco-y-prev j)
      (juego-rival-x j) (juego-rival-y j)
      (juego-rival-x-prev j) (juego-rival-y-prev j)
      (juego-jugador-x j) (juego-jugador-y j)
      (juego-jugador-x-prev j) (juego-jugador-y-prev j)
      (juego-score-j j) (juego-score-r j)
      (juego-cancha-dibujada j))]

    [(> (juego-disco-x j) (+ FX FW -32))
     (make-juego
      (juego-dificultad j)
      (+ FX FW -32) (juego-disco-y j)
      (- (juego-disco-vx j)) (juego-disco-vy j)
      (juego-disco-x-prev j) (juego-disco-y-prev j)
      (juego-rival-x j) (juego-rival-y j)
      (juego-rival-x-prev j) (juego-rival-y-prev j)
      (juego-jugador-x j) (juego-jugador-y j)
      (juego-jugador-x-prev j) (juego-jugador-y-prev j)
      (juego-score-j j) (juego-score-r j)
      (juego-cancha-dibujada j))]
    [else j]))

; ============================
; VELOCIDAD DINÁMICA DEL DISCO
;============================

(define (vel-disco dificultad)
  (case dificultad
    [(easy) 9]
    [(medium) 12]
    [(hard) 15]
    [else 10]))

;============================
; SISTEMA DE GOLES
;============================

(define (gol-jugador j)
  (play-sound "gol.wav.wav" #t)
  (sleep 0.05)
  (let ([cx (+ FX (/ FW 2))]
        [cy (+ FY (/ FH 2))])
    (make-juego
     (juego-dificultad j)
     cx cy
     0 (- (vel-disco (juego-dificultad j)))
     cx cy
     (juego-rival-x j) (juego-rival-y j)
     (juego-rival-x j) (juego-rival-y j)
     (juego-jugador-x j) (juego-jugador-y j)
     (juego-jugador-x j) (juego-jugador-y j)
     (+ (juego-score-j j) 1) (juego-score-r j)
     #f)))

(define (gol-rival j)
   (play-sound "gol.wav.wav" #t)
  (sleep 0.5)
  (let ([cx (+ FX (/ FW 2))]
        [cy (+ FY (/ FH 2))])
    (make-juego
     (juego-dificultad j)
     cx cy
     0 (vel-disco (juego-dificultad j))
     cx cy
     (juego-rival-x j) (juego-rival-y j)
     (juego-rival-x j) (juego-rival-y j)
     (juego-jugador-x j) (juego-jugador-y j)
     (juego-jugador-x j) (juego-jugador-y j)
     (juego-score-j j) (+ (juego-score-r j) 1)
     #f)))

(define (goles j)
  (cond
    [(< (juego-disco-y j) (+ FY 24)) (gol-jugador j)]
    [(> (juego-disco-y j) (+ FY FH -24)) (gol-rival j)]
    [else j]))


;============================
;DISTANCIA ENTRE DOS PUNTOS
;============================

(define (distancia x1 y1 x2 y2)
  (sqrt (+ (* (- x2 x1) (- x2 x1))
           (* (- y2 y1) (- y2 y1)))))


(define (separar-objetos pelota-x pelota-y objeto-x objeto-y distancia-min)
  (let* ([dx (- pelota-x objeto-x)]  
         [dy (- pelota-y objeto-y)]   
         [distancia (sqrt (+ (* dx dx) (* dy dy)))]  
         [proporcion (/ distancia-min distancia)])   

    (values (+ objeto-x (* dx proporcion))
            (+ objeto-y (* dy proporcion)))))


;===========================================
; REBOTE: PELOTA CONTRA EL JUGADOR (AZUL)
;===========================================

(define (pelota-jugador j)
  (if (< (distancia (juego-disco-x j) (juego-disco-y j)
                    (juego-jugador-x j) (juego-jugador-y j))
         42)
      (make-juego
       (juego-dificultad j)
       (juego-disco-x j) (juego-disco-y j)
       (* (- (juego-disco-x j) (juego-jugador-x j)) 0.5)
       (* (- (juego-disco-y j) (juego-jugador-y j)) 0.5)
       (juego-disco-x-prev j) (juego-disco-y-prev j)
       (juego-rival-x j) (juego-rival-y j)
       (juego-rival-x-prev j) (juego-rival-y-prev j)
       (juego-jugador-x j) (juego-jugador-y j)
       (juego-jugador-x-prev j) (juego-jugador-y-prev j)
       (juego-score-j j) (juego-score-r j)
       (juego-cancha-dibujada j))
      j))


;===========================================
; REBOTE: PELOTA CONTRA EL RIVAL (ROJO)
;===========================================

(define (pelota-rival j)
  (let* ([dx (juego-disco-x j)]
         [dy (juego-disco-y j)]
         [rx (juego-rival-x j)]
         [ry (juego-rival-y j)])
    
    (if (< (distancia dx dy rx ry) 42)
        (let-values ([(nx ny) (separar-objetos dx dy rx ry 42)])
          (make-juego
           (juego-dificultad j)
           nx ny
           (* (- nx rx) 0.5)
           (* (- ny ry) 0.5)
           (juego-disco-x j) (juego-disco-y j)
           rx ry
           (juego-rival-x-prev j) (juego-rival-y-prev j)
           (juego-jugador-x j) (juego-jugador-y j)
           (juego-jugador-x-prev j) (juego-jugador-y-prev j)
           (juego-score-j j) (juego-score-r j)
           (juego-cancha-dibujada j)))
        j)))

(define (sgn n)
  (cond
    [(> n 0) 1]
    [(< n 0) -1]
    [else 0]))
(define (mover-rival j)
  (let* ([rx (juego-rival-x j)]
         [ry (juego-rival-y j)]
         [dx (juego-disco-x j)]
         [dy (juego-disco-y j)]

         ;; Velocidad base según dificultad
         [vel (case (juego-dificultad j)
       [(easy) 1]
       [(medium) 3]
       [(hard) 5]
       [else 4])]

         ;; Movimiento hacia la pelota
         [nx (+ rx (* vel (sgn (- dx rx))))]
         [ny (+ ry (* vel (sgn (- dy ry))))]

         ;; Límites: rival solo se mueve en la mitad superior
         [lim-x (min (max nx (+ FX 40)) (+ FX FW -40))]
         [lim-y (min (max ny (+ FY 40)) (+ FY (/ FH 2) -40))])

    (make-juego
     (juego-dificultad j)
     (juego-disco-x j) (juego-disco-y j)
     (juego-disco-vx j) (juego-disco-vy j)
     (juego-disco-x-prev j) (juego-disco-y-prev j)
     
     ;; Nueva posición del rival
     lim-x lim-y
     (juego-rival-x-prev j) (juego-rival-y-prev j)

     ;; Jugador queda igual
     (juego-jugador-x j) (juego-jugador-y j)
     (juego-jugador-x-prev j) (juego-jugador-y-prev j)

     (juego-score-j j) (juego-score-r j)
     (juego-cancha-dibujada j))))

;============================
; ACTUALIZAR ESTADO COMPLETO
;============================

(define (actualizar-estado j)
  (goles
   (pelota-rival
    (pelota-jugador
     (rebote-pelota
      (mover-rival
       (mover-pelota j)))))))


; =====================================================
; JUEGO PRINCIPAL
; =====================================================

(define (jugar j)
  ;; dibujar escena devuelve j con previos actualizados
  (let ([j2 (dibujar-escena j)])
    (sleep 0.016)
    (jugar (actualizar-estado (mover-jugador j2)))))


; =====================================================
; MENÚ DIFICULTAD
; =====================================================
(define (estado-inicial dificultad)
  (make-juego
   dificultad
   ; disco posición inicial
   (+ FX (/ FW 2))
   (+ FY (/ FH 2))

   ; velocidad inicial del disco: hacia arriba, según la dificultad
   0 (- (vel-disco dificultad))

   ; disco prev (igual al actual)
   (+ FX (/ FW 2))
   (+ FY (/ FH 2))

   ; rival posición inicial
   (+ FX (/ FW 2))
   (+ FY 90)

   ; prev rival
   (+ FX (/ FW 2))
   (+ FY 90)

   ; jugador posición inicial
   (+ FX (/ FW 2))
   (+ FY FH -70)

   ; prev jugador
   (+ FX (/ FW 2))
   (+ FY FH -70)

   ; score (jugador-rival)
   0 0
   #f))


;-----------------------------------
;; MENU PRINCIPAL
;------------------------------------


(define (menu-loop)
  (pantalla-menu)

   (when (not ya-sonido-menu?)
    (set! ya-sonido-menu? #t)
    (play-sound "menu.wav.wav" #t))
  (let ([ev (ready-mouse-click ventana)])
    (if ev
        (let ([mx (posn-x (mouse-click-posn ev))]
              [my (posn-y (mouse-click-posn ev))])
          (cond
            [(and (>= mx 150) (<= mx 350) (>= my 300) (<= my 360))
             (jugar (estado-inicial 'easy))]
            [(and (>= mx 150) (<= mx 350) (>= my 380) (<= my 440))
             (jugar (estado-inicial 'medium))]
            [(and (>= mx 150) (<= mx 350) (>= my 460) (<= my 520))
             (jugar (estado-inicial 'hard))]
            [else (menu-loop)]))
        (begin (sleep 0.05) (menu-loop)))))

;=========================
;INICIO DEL JUEGO
;=========================
(define (pantalla-menu)
  ;Fondo negro COMPLETO
  ((draw-solid-rectangle ventana) (make-posn 0 0) 500 700 "black")
  ((draw-string ventana) (make-posn 150 140) "AIR  HOCKEY" "WHITE")
((draw-string ventana) (make-posn 148 138) "AIR  HOCKEY" "WHITE") ;; sombra
  
  ;Fondo decorativo con gradiente simulado
  ((draw-solid-rectangle ventana) (make-posn 50 50) 400 600 "purple")
  ((draw-solid-rectangle ventana) (make-posn 55 55) 390 590 "black")
    ((draw-solid-rectangle ventana) (make-posn 100 80) 300 80 "purple")
  ((draw-solid-rectangle ventana) (make-posn 105 85) 290 70 "violet")
  ((draw-solid-rectangle ventana) (make-posn 110 90) 280 60 "black")

  ;Decoración del título
  ((draw-solid-ellipse ventana) (make-posn 120 110) 20 20 "red")
  ((draw-solid-ellipse ventana) (make-posn 360 110) 20 20 "cyan")
  
  ;Líneas NEÓN decorativas
  ((draw-line ventana)
   (make-posn 0 400)
   (make-posn 200 800)
   "purple") ((draw-line ventana)
   (make-posn 0 200)
   (make-posn 140 800)"purple") ((draw-line ventana)(make-posn 0 200)(make-posn 140 500)   "purple")
  ((draw-line ventana) (make-posn 420 60) (make-posn 500 500) "purple")
   ((draw-line ventana) (make-posn 0 200) (make-posn 900 800) "purple")
  ((draw-line ventana) (make-posn 0 200) (make-posn 420 800)   "purple")
  ((draw-line ventana) (make-posn 0 200) (make-posn 900 800) "purple")
  ((draw-line ventana)  (make-posn 0 22) (make-posn 500 450) "purple")
  ((draw-line ventana) (make-posn 0 20)(make-posn 900 120) "purple")
  
  ((draw-line ventana) (make-posn 0 20)(make-posn 123 15) "orange")
  ((draw-line ventana) (make-posn 0 20)(make-posn 819 182) "orange")
  ((draw-line ventana) (make-posn 0 20)(make-posn 728 232) "orange")
  ((draw-line ventana) (make-posn 0 20)(make-posn 640 783) "orange")

    ((draw-line ventana) (make-posn 0 20)(make-posn 123 15) "red")
  ((draw-line ventana) (make-posn 0 20)(make-posn 120 142) "red")
  ((draw-line ventana) (make-posn 0 24)(make-posn 5604 235) "red")
  ((draw-line ventana) (make-posn 0 23)(make-posn -232 783) "red")

  (sleep 0.000000001)

  ;Título  AIR HOCKEY
  ((draw-string ventana) (make-posn 150 120) "AIR " "red")
    ((draw-string ventana) (make-posn 180 120) "HOCKEY" "cyan")
  
;Botón EASY 
  ((draw-solid-rectangle ventana) (make-posn 145 295) 210 70 "CYAN")
  ((draw-solid-rectangle ventana) (make-posn 150 300) 200 60 "CYAN")
  ((draw-rectangle ventana) (make-posn 150 300) 200 60 "white")
  ((draw-solid-ellipse ventana) (make-posn 160 320) 29 29 "green")
  ((draw-solid-ellipse ventana) (make-posn 170 330) 15 15 "cyan")
  ((draw-string ventana) (make-posn 213 333) "EASY" "BLACK")
  ((draw-string ventana) (make-posn 900 330) "EASY" "black")

  ;; Botón MEDIUM 
  ((draw-solid-rectangle ventana) (make-posn 145 375) 210 70 "ORANGE")
  ((draw-solid-rectangle ventana) (make-posn 150 380) 200 60 "ORANGE")
  ((draw-rectangle ventana) (make-posn 150 380) 200 60 "white")
    ((draw-solid-ellipse ventana) (make-posn 160 400) 29 29 "yellow")
  ((draw-solid-ellipse ventana) (make-posn 170 410) 15 15 "brown")
  ((draw-string ventana) (make-posn 198 413) "MEDIUM" "BLACK")
  ((draw-string ventana) (make-posn 900 410) "MEDIUM" "BLACK")

  ;Botón HARD 
  ((draw-solid-rectangle ventana) (make-posn 145 455) 210 70 "purple")
  ((draw-solid-rectangle ventana) (make-posn 150 460) 200 60 "red")
  ((draw-rectangle ventana) (make-posn 150 460) 200 60 "white")
  ((draw-solid-ellipse ventana) (make-posn 160 480) 29 29 "Purple")
  ((draw-solid-ellipse ventana) (make-posn 170 490) 15 15 "red")
  ((draw-string ventana) (make-posn 213 493) "HARD")
  ((draw-string ventana) (make-posn 900 490) "HARD")
  
  ;Decoración inferior
  ((draw-solid-ellipse ventana) (make-posn 240 580) 30 30 "purple")
  ((draw-solid-ellipse ventana) (make-posn 245 585) 20 20 "violet")
  ((draw-string ventana) (make-posn 140 620) "Mueve el mouse para jugar" "white"))
;Líneas NEÓN decorativas

((draw-solid-rectangle ventana)
 (make-posn 120 260) 260 1 "violet")
((draw-solid-rectangle ventana)
 (make-posn 120 262) 260 1 "white")

;Línea inferior
((draw-solid-rectangle ventana)
 (make-posn 120 530) 260 1 "violet")
((draw-solid-rectangle ventana)
 (make-posn 120 532) 260 1 "white")

(menu-loop)




