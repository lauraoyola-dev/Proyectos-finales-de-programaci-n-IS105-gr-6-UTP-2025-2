#lang racket

(require 2htdp/universe
         2htdp/image)

;; ============================================================
;; CONSTANTES
;; ============================================================

;; Tamaño ventana
(define WIDTH 500)
(define HEIGHT 500)

;; Jugador
(define LADO 50)

;; Suelo (altura donde “se para” el cuadro)
(define PISO (- HEIGHT 80)) ; Y de la línea del suelo

;; Física general
(define GRAVEDAD 1.2)
(define OBSTACLE-SPEED 6)

;; Tiempo entre ticks
(define TICK-RATE 0.03)

;; Tiempo que debe sobrevivir por nivel (segundos)
(define NIVEL-DURACION 10)

;; Máximo de obstáculos en pantalla
(define MAX-OBSTACLES 5)

;; Fondos para cada nivel
(define FONDO1 (bitmap "fondo1.jpg"))
(define FONDO2 (bitmap "fondo2.jpg"))
(define FONDO3 (bitmap "fondo3.jpg"))

;; ============================================================
;; JUGADOR
;; ============================================================

;; Jugador
(struct player (x y vy ang can-jump?) #:transparent)

;; Y del centro del jugador cuando está sobre el piso
(define GROUND-Y (- PISO (/ LADO 2)))

;; Crear jugador inicial
(define (make-player)
  (player 200 GROUND-Y 0 0 #t))

;; Actualizar física del jugador
(define (player-update-physics p)
  (let* ((vy (+ (player-vy p) GRAVEDAD))
         (y  (+ (player-y p) vy))
         ;; limitar al suelo (centro del jugador en GROUND-Y)
         (y  (if (> y GROUND-Y) GROUND-Y y))
         (vy (if (= y GROUND-Y) 0 vy))
         ;; limitar que no salga demasiado arriba
         (y  (if (< y 40) 40 y))
         (vy (if (< y 40) 0 vy))
         (can? (if (= y GROUND-Y) #t (player-can-jump? p)))
         (ang (+ (player-ang p) 4)))
    (player (player-x p) y vy ang can?)))

;; Salto al presionar espacio
(define (player-jump-press p)
  (if (and (player-can-jump? p))
      (player (player-x p)
              (player-y p)
              -20            ; fuerza de salto
              (player-ang p)
              #f)
      p))

;; Ajuste al soltar espacio (salto un poco más largo)
(define (player-jump-release p)
  (if (not (player-can-jump? p))
      (player (player-x p)
              (player-y p)
              (+ (player-vy p) -6)
              (player-ang p)
              (player-can-jump? p))
      p))

;; Dibujar jugador sobre una escena
(define (draw-player p img)
  (place-image
   (rotate (player-ang p)
           (square LADO "solid" "orange"))
   (player-x p)
   (player-y p)
   img))

;; ============================================================
;; OBSTÁCULOS (estructura + movimiento + dibujo)
;; ============================================================

;; kind: 'square o 'triangle
(struct obstacle (x y w h kind) #:transparent)

;; Creadores
(define (make-square-obstacle x y w h)
  (obstacle x y w h 'square))

(define (make-triangle-obstacle x y w h)
  (obstacle x y w h 'triangle))

;; Mover obstáculo hacia la izquierda
(define (move-obstacle o)
  (obstacle (- (obstacle-x o) OBSTACLE-SPEED)
            (obstacle-y o)
            (obstacle-w o)
            (obstacle-h o)
            (obstacle-kind o)))

;; Dibujar obstáculo
(define (draw-obstacle o img)
  (define shape
    (cond [(eq? (obstacle-kind o) 'square)
           (rectangle (obstacle-w o)
                      (obstacle-h o)
                      "solid"
                      "green")]
          [(eq? (obstacle-kind o) 'triangle)
            (let* ((size (max (obstacle-w o)
                              (obstacle-h o)))
                  (tri  (triangle size "solid" "red")))
              ;; si el centro del triángulo está en la mitad superior de la pantalla,
              ;; lo dibujamos boca abajo
              (if (< (obstacle-y o) (/ HEIGHT 2))
                  (rotate 180 tri)
                  tri))]
          [else
           (rectangle (obstacle-w o)
                      (obstacle-h o)
                      "outline"
                      "white")]))
  (place-image shape
               (obstacle-x o)
               (obstacle-y o)
               img))

;; ============================================================
;; GENERACIÓN DE OBSTÁCULOS POR NIVEL
;; ============================================================

;; -------- Nivel 1: cuadrados --------
(define (spawn-obstacle-level1)
  ;; elegimos si será pequeño o grande
  (let* ((small? (zero? (random 2)))
         (base   (if small? 35 90))   ; 35–65 pequeño, 90–120 grande
         (w      (+ base (random 30)))
         (h      (+ base (random 30)))
         (x      (+ WIDTH 50)))
    (if (< (random 100) 70)
        ;; Obstáculo desde el suelo
        (make-square-obstacle x (- PISO (/ h 2)) w h)
        ;; Plataforma elevada
        (make-square-obstacle x (- PISO 250) w h))))

;; -------- Nivel 2: triángulos --------
(define (spawn-obstacle-level2)
  (let* ((small? (zero? (random 2)))
         (base   (if small? 30 90)) ; 30–60 vs 90–120
         (size   (+ base (random 30)))
         (x      (+ WIDTH 50)))
    (if (zero? (random 2))
        ;; púa desde el suelo
        (make-triangle-obstacle x (- PISO (/ size 2)) size size)
        ;; púa desde arriba
        (make-triangle-obstacle x (/ size 2) size size))))

;; -------- Nivel 3: mezcla --------
(define (spawn-obstacle-level3)
  (let* ((choice (random 2)))
    (cond
      ;; Opción 0: cuadrados
      ((= choice 0)
       (let* ((small? (zero? (random 2)))
              (base   (if small? 35 95))
              (w      (+ base (random 30)))
              (h      (+ base (random 30)))
              (x      (+ WIDTH 50)))
         (if (< (random 100) 60)
             (make-square-obstacle x (- PISO (/ h 2)) w h)
             (make-square-obstacle x (- PISO 200) w h))))
      ;; Opción 1: triángulos
      (else
       (let* ((small? (zero? (random 2)))
              (base   (if small? 30 90))
              (size   (+ base (random 30)))
              (x      (+ WIDTH 50)))
         (if (zero? (random 2))
             (make-triangle-obstacle x (- PISO (/ size 2)) size size)
             (make-triangle-obstacle x (/ size 2) size size)))))))

;; ============================================================
;; MUNDO / UTILIDADES
;; ============================================================

;; Mundo global:
;; player: jugador
;; obstacles: lista de obstáculos
;; level: 1, 2 o 3
;; time: tiempo acumulado en el nivel
;; attempts: lista '(intentos-n1 intentos-n2 intentos-n3)
;; game-over?: #t si perdió
(struct world (player obstacles level time attempts game-over?)
  #:transparent)

;; Mundo inicial
(define (make-initial-world)
  (world (make-player)
         '()
         1
         0
         (list 0 0 0)
         #f))

;; Incrementar intentos del nivel actual
(define (increment-attempt w)
  (let* ((lvl (world-level w))
         (atts (world-attempts w))
         (n1 (list-ref atts 0))
         (n2 (list-ref atts 1))
         (n3 (list-ref atts 2))
         (atts2 (cond [(= lvl 1) (list (add1 n1) n2 n3)]
                      [(= lvl 2) (list n1 (add1 n2) n3)]
                      [else       (list n1 n2 (add1 n3))])))
    (struct-copy world w [attempts atts2])))

;; Reiniciar mundo en el mismo nivel (para reintentar)
(define (restart-world w)
  (world (make-player)
         '()
         1          ; <-- aquí lo fuerzas a volver a nivel 1
         0
         (list 0 0 0)   ; opcional: borrar intentos, si quieres dejarlos pon (world-attempts w)
         #f))


;; Helper de solapamiento 1D
(define (overlap? a1 a2 b1 b2)
  (and (< a1 b2) (< b1 a2)))

;; ============================================================
;; FÍSICAS DEL MUNDO (TICK, COLISIONES, SPAWN)
;; ============================================================

;; Tick del mundo (se llama en on-tick)
(define (world-tick w)
  (if (world-game-over? w)
      w
      (let* (;; actualizar jugador
             (p1 (player-update-physics (world-player w)))

             ;; mover obstáculos y filtrar los que siguen en pantalla
             (obs-movidos (map move-obstacle (world-obstacles w)))
             (obs-vivos (filter (lambda (o) (> (obstacle-x o) -50))
                                obs-movidos))

             ;; spawnear según nivel
             (obs-finales (maybe-spawn (world-level w) obs-vivos))

             ;; sumar tiempo
             (t1 (+ (world-time w) TICK-RATE))

             ;; construir mundo intermedio
             (w1 (struct-copy world w
                               [player p1]
                               [obstacles obs-finales]
                               [time t1]))

             ;; checar colisión
             (dead? (collides-any? p1 obs-finales))

             ;; si muere, marcar game-over y aumentar intentos
             (w2 (if dead?
                     (let ((w-inc (increment-attempt w1)))
                       (struct-copy world w-inc [game-over? #t]))
                     w1))

             ;; pasar de nivel si sobrevivió
             (w3 (if (and (not dead?)
                          (>= (world-time w2) NIVEL-DURACION)
                          (< (world-level w2) 3))
                     ;; subir de nivel
                     (world (make-player)
                            '()
                            (add1 (world-level w2))
                            0
                            (world-attempts w2)
                            #f)
                     w2)))
        w3)))

;; Distancia mínima entre obstáculos (en píxeles)
(define MIN-OBSTACLE-GAP 120)

;; devuelve la posición X más grande entre todos los obstáculos
(define (rightmost-obstacle-x obs-list)
  (if (null? obs-list)
      -1000
      (apply max (map obstacle-x obs-list))))

;; Spawnear según nivel
(define (maybe-spawn lvl obs-list)
  (let* ((max-x (rightmost-obstacle-x obs-list)))
    (if (and (< (length obs-list) MAX-OBSTACLES)
             ;; solo spawnear si el último obstáculo ya se alejó lo suficiente
             (< max-x (- WIDTH MIN-OBSTACLE-GAP)))
        (if (< (random 100) 10) ; 4% prob de crear uno por tick
            (cons (case lvl
                    [(1) (spawn-obstacle-level1)]
                    [(2) (spawn-obstacle-level2)]
                    [else (spawn-obstacle-level3)])
                  obs-list)
            obs-list)
        obs-list)))


;; ¿Jugador choca con algún obstáculo?
(define (collides-any? p obs-list)
  (ormap (lambda (o) (collide p o)) obs-list))

;; Colisión aproximada usando cajas
(define (collide p o)
  ;; =============================
  ;; HITBOX DEL JUGADOR (más pequeña)
  ;; =============================
  (define px (player-x p))
  (define py (player-y p))

  (define p-half (* LADO 0.35)) ; jugador 70% del tamaño

  (define left      (- px p-half))
  (define right     (+ px p-half))
  (define top       (- py p-half))
  (define bottom    (+ py p-half))

  ;; =============================
  ;; HITBOX DEL OBSTÁCULO
  ;; =============================
  (define ox (obstacle-x o))
  (define oy (obstacle-y o))

  ;; si es triángulo => reducir caja
  (define kind (obstacle-kind o))
  (define scale (if (eq? kind 'triangle)
                    0.45   ; <-- AJUSTA ESTE NÚMERO SI SIGUE MUY SENSIBLE
                    1.0))  ; cuadrados normales

  (define hw (* scale (/ (obstacle-w o) 2)))
  (define hh (* scale (/ (obstacle-h o) 2)))

  (define o-left   (- ox hw))
  (define o-right  (+ ox hw))
  (define o-top    (- oy hh))
  (define o-bottom (+ oy hh))

  ;; =============================
  ;; COLISIÓN FINAL
  ;; =============================
  (and (overlap? left right o-left o-right)
       (overlap? top bottom o-top o-bottom)))

;; ============================================================
;; DECORACIÓN: picos no-colisionables (solo dibujo)
;; ============================================================

(define DEC-SPIKE-W 20)   ; ancho “aparente” de cada pico
(define DEC-SPIKE-H 16)   ; alto de cada pico
(define DEC-SPIKE-GAP 4)  ; espacio entre picos

(define (draw-decor img)
  ;; Triángulos base (equiláteros) y versiones escaladas para que se vean angostos
  (define tri-base (triangle DEC-SPIKE-H "solid" "dimgray"))
  (define tri-up   (scale/xy (/ DEC-SPIKE-W DEC-SPIKE-H) 1 tri-base)) ; punta hacia arriba
  (define tri-down (rotate 180 tri-up))                                ; punta hacia abajo

  (define step (+ DEC-SPIKE-W DEC-SPIKE-GAP))
  (define start-x (/ DEC-SPIKE-W 2))

  ;; Dibuja una fila a lo largo del ancho
  (define (place-strip y tri acc)
    (for/fold ([im acc])
              ([x (in-range start-x WIDTH step)])
      (place-image tri x y im)))

  ;; Fila superior (picos hacia abajo)
  (define img1 (place-strip (/ DEC-SPIKE-H 2) tri-down img))
  ;; Fila inferior, justo sobre la línea del suelo (picos hacia arriba)
  (place-strip (+ PISO 4) tri-up img1))


;; ============================================================
;; DIBUJO DEL MUNDO Y MANEJO DE TECLAS
;; ============================================================

(define (get-background lvl)
  (cond
    [(= lvl 1) FONDO1]
    [(= lvl 2) FONDO2]
    [(= lvl 3) FONDO3]
    [else FONDO1]))

;; Dibujo del mundo
(define (draw-world w)
  ;; Obtener el fondo correspondiente
  (define bg (scale/xy (/ WIDTH (image-width (get-background (world-level w))))
                       (/ HEIGHT (image-height (get-background (world-level w))))
                       (get-background (world-level w))))

  ;; Colocar el fondo como escena base
  (define base (place-image bg (/ WIDTH 2) (/ HEIGHT 2)
                            (empty-scene WIDTH HEIGHT)))


    ;; Suelo (línea base donde se apoyan jugador y obstáculos)
  (define con-suelo
    (place-image
     (rectangle WIDTH 8 "solid" "white")
     (/ WIDTH 2)
     PISO
     base))

  ;; >>> NUEVO: agregar decoración de picos sobre la imagen con suelo
  (define con-decor (draw-decor con-suelo))

  ;; Obstáculos (ahora parten de con-decor en vez de con-suelo)
  (define con-obstaculos
    (foldl (lambda (o img) (draw-obstacle o img))
           con-decor
           (world-obstacles w)))

  ;; Jugador
  (define con-jugador
    (draw-player (world-player w) con-obstaculos))

  ;; Textos de nivel e intentos
  (define lvl (world-level w))
  (define atts (world-attempts w))
  (define current-att (list-ref atts (sub1 lvl)))

  (define txt-level
    (text (string-append "Nivel: " (number->string lvl))
          16
          "white"))

  (define txt-attempts
    (text (string-append "Intentos: "
                         (number->string current-att))
          16
          "white"))

    ;; tiempo del nivel con 1 decimal
  (define txt-time
    (text (string-append "Tiempo: "
                         (real->decimal-string (world-time w) 1))
          16
          "white"))
  
  ;; Si está en game over, mensaje
  (define final-img
    (if (world-game-over? w)
        (place-image
         (text "GAME OVER - Presiona ESPACIO" 20 "white")
         (/ WIDTH 2)
         60
         con-jugador)
        con-jugador))

  ;; Colocar textos
  (define img1
    (place-image txt-level
                 60
                 20
                 final-img))

  (define img2
    (place-image txt-time
                 (/ WIDTH 2)
                 20
                 img1))

  (place-image txt-attempts
               (- WIDTH 80)
               20
               img2))


;; Manejo de teclas (keydown)
(define (handle-key w k)
  (cond
    ;; si perdió y pulsa espacio → reinicia mismo nivel
    [(and (world-game-over? w)
          (string=? k " "))
     (restart-world w)]

    ;; salto (si no ha perdido)
    [(and (string=? k " ") (not (world-game-over? w)))
     (struct-copy world w
                  [player (player-jump-press (world-player w))])]

    [else w]))

;; Al soltar espacio -> extender un poco salto
(define (handle-release w k)
  (cond
    [(and (string=? k " ") (not (world-game-over? w)))
     (struct-copy world w
                  [player (player-jump-release (world-player w))])]
    [else w]))

;; ============================================================
;; MAIN
;; ============================================================

(define initial-world (make-initial-world))

(big-bang initial-world
  [to-draw draw-world]
  [on-tick world-tick TICK-RATE]
  [on-key handle-key]
  [on-release handle-release])

