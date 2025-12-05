
(require 2htdp/image)
(require 2htdp/universe)
(require racket/gui)
(require racket/runtime-path)
;valores del menu
(define WIDTH 600)
(define HEIGHT 400)

; datos para el menu
(define-struct menu-data (selected-option time))

; opciones del menu
(define OPTION-PLAY 'play)
(define OPTION-OPTIONS 'options)
(define OPTION-QUIT 'quit)

; estado incial del menu
(define INITIAL-MENU-STATE (make-menu-data OPTION-PLAY 0))

; valores de animacion de la pelota y el menu
(define-struct platform(x y w h color))
(define-struct coin (x y))
(define COIN-RADIUS 10)
(define COIN-COLOR "gold")
(define BALL-RADIUS 20)
(define BOUNCE-AMPLITUDE 30) 
(define ANIMATION-SPEED 2)   
(define HOVER-COLOR "red")       
(define NORMAL-COLOR "black")    
(define BACKGROUND-COLOR "lightgray")
;definir la ruta al archivo de musica
(define-runtime-path musica-fondo-path"musica.wav.wav")


;valores de posicion y velocidad
; Estructura del Estado del Juego
(define-struct game-state (ball-pos ball-vel is-on-ground? obstacles score coins level-num current-goal))

; valores de Física y Nivel
(define GRAVITY 0.5)           
(define JUMP-VELOCITY -12)     
(define MOVEMENT-SPEED 6)      
(define GROUND-HEIGHT 50)      

; El nivel donde la bola se detiene
(define FLOOR-LEVEL (- HEIGHT BALL-RADIUS GROUND-HEIGHT))

;posicion de plataformas nivel 1
(define LEVEL-1
  (list
   ;el suelo
   (make-platform(/ WIDTH 2);que esta al centro
                 (- HEIGHT 25)WIDTH 50 "forestgreen")
   ;una plataforma que flote
   (make-platform 250 280 120 20
                  "saddlebrown")
   ;una plataforma mas alta
   (make-platform 560 190 60 20"saddlebrown")))

;ahora nivel 2
(define LEVEL-2
  (list
   ; FÍJATE EN EL PARÉNTESIS AL FINAL DE "forestgreen" )
   (make-platform 100 350 150 20 "forestgreen") 
   (make-platform 300 280 100 20 "saddlebrown")
   (make-platform 500 200 100 20 "saddlebrown")
   (make-platform 300 120 100 20 "saddlebrown")
   (make-platform 100 80 150 20 "saddlebrown"))) 

; Monedas del Nivel 2
(define COINS-LEVEL-2
  (list
   (make-coin 300 230)
   (make-coin 500 150)
   (make-coin 300 70)
   (make-coin 100 30)))
  ;ahora nivel 3
  (define LEVEL-3
    (list
     (make-platform 50 350 80 20
                    "forestgreen");el inicio
     (make-platform 200 350 60 20
                    "saddlebrown")
     (make-platform 350 300 60 20
                    "saddlebrown")
     (make-platform 500 250 60 20
                    "saddlebrown")
     (make-platform 350 180 60 20
                    "saddlebrown")
     (make-platform 200 120 60 20
                    "saddlebrown")
     (make-platform 550 100 100 20
                    "forestgreen")));la meta
 
;posicion de las monedas
(define COINS-LEVEL-1
  (list
   (make-coin 100 330)
   (make-coin 250 240)
   (make-coin 450 150)
   (make-coin 350 330)))

  ;ahora monedas del nivel 2
(define COINS-LEVEL-2
  (list
   (make-coin 300 230)
   (make-coin 500 150)
   (make-coin 300 70)
   (make-coin 100 30)))

   ;ahora monedas del nivel 3
   (define COINS-LEVEL-3
     (list
      (make-coin 200 300)
      (make-coin 350 250)
      (make-coin 350 130)
      (make-coin 200 70)))
  ;puerta para avanzar al siguiente nivel
  ;estructura de la puerta , x y el ancho y color
  (define-struct goal(x y w h color))
  ;meta de nivel 1
  (define GOAL-LEVEL-1
    (make-goal 560 (- FLOOR-LEVEL 155)30 50 "blue"))
  ;la meta del nivel 2
  (define GOAL-LEVEL-2
           (make-goal 100 50 40 60 "blue"))
  ;meta nivel 3
  (define GOAL-LEVEL-3
    (make-goal 550 70 40 60 "blue"))

  
; Estado Inicial del Juego
(define INITIAL-GAME-STATE
(make-game-state
 (make-posn 50 FLOOR-LEVEL)
 (make-posn 0 0)
 #t
 LEVEL-1 ;las plataformas del nivel 1
 0
 COINS-LEVEL-1; las monedas del nivel 1
 1
 GOAL-LEVEL-1;la puerta del nivel 1
 ))


; funcion tick con el menu
(define (tick-menu md)
  (make-menu-data 
   (menu-data-selected-option md)
   (+ (menu-data-time md) 1/28)))

; velocidad de la animacion
(define (bouncing-offset t)
  (* BOUNCE-AMPLITUDE (sin (* t ANIMATION-SPEED))))

; funcion dibujo
(define (draw-bouncing-ball t)
  (let* ([base-y (/ HEIGHT 2)]
         [offset (bouncing-offset t)]
         [ball-y (- base-y offset)]) 
    
    (place-image 
     (circle BALL-RADIUS "solid" HOVER-COLOR) 
     (/ WIDTH 4) 
     ball-y 
     (empty-scene WIDTH HEIGHT))))

; draw-option: String Symbol Symbol -> Image
(define (draw-option label current-option expected-option)
  (if (symbol=? current-option expected-option)
      (text (string-append "> " label " <") 40 HOVER-COLOR) 
      (text label 30 NORMAL-COLOR))) 

; draw-menu: menu-data -> Image
(define (draw-menu md)
  (let* ([t (menu-data-time md)]
         [selected (menu-data-selected-option md)]
         
         [background (empty-scene WIDTH HEIGHT BACKGROUND-COLOR)] 
         [ball-layer (draw-bouncing-ball t)] 
         
         ; CORRECCIÓN: Aseguramos todos los argumentos en above/align
         [menu-layer (above/align "center" 
                       (draw-option "JUGAR" selected OPTION-PLAY)
                       (empty-scene 1 20)
                       (draw-option "OPCIONES" selected OPTION-OPTIONS)
                       (empty-scene 1 20)
                       (draw-option "SALIR" selected OPTION-QUIT))])

    (overlay/align "center" "center" 
                   menu-layer 
                   (overlay ball-layer background)))) 

; Función handle-menu-key CORREGIDA
; controles del menu
(define (handle-menu-key md key)
  (let ([current-option (menu-data-selected-option md)]
        [current-time (menu-data-time md)])
    (cond
      ; 1. NAVEGACIÓN (DOWN)
      [(key=? key "down")
       (make-menu-data 
        (case current-option
          [(play) OPTION-OPTIONS]
          [(options) OPTION-QUIT]
          [(quit) OPTION-PLAY]
          [else current-option])
        current-time)] 
      
      ; 2. NAVEGACIÓN (UP)
      [(key=? key "up")
       (make-menu-data 
        (case current-option
          [(play) OPTION-QUIT]
          [(options) OPTION-PLAY]
          [(quit) OPTION-OPTIONS]
          [else current-option])
        current-time)]
      
      ; 3. SELECCIÓN (ENTER / RETURN)
      [(string=? key "\r") 
       (case current-option
         [(play) 'GAME-START]      
         [(options) 'OPTIONS-SCREEN] ; Devolver solo el símbolo
         [(quit) (exit)]                             
         [else md])]
      
      ; 4. CLÁUSULA FINAL (OTRAS TECLAS)
      [else md])))

       ;fisicas de las plataformas nivel 1
(define(find-ground-collision x y vy platforms)
  (cond
    [(empty? platforms)#f];si se acabo la lista y no se choca no hay suelo
    [else(let* ([p (first platforms)] ; <-- CORREGIDO
                [p-top(-(platform-y p)
                        (/ (platform-h p)2))];borde alto de la plataforma <-- CORREGIDO
                [p-left(-(platform-x p)(/(platform-w p)2))];borde izquierdo
                [p-right(+(platform-x p)(/(platform-w p)2))];borde derecho
                [ball-bottom(+ y BALL-RADIUS)]);parte baja de la pelota
           (if(and(>= x p-left)(<= x p-right);mira sa la pelota esta en el ancho de la plataforma
                  (>= ball-bottom p-top);si la pelota choca con el borde alto de la plataforma
                  (<=(- ball-bottom vy)
                     (+ p-top 5))
                  (>= vy 0)); verifica si esta cayendo
              (- p-top BALL-RADIUS);se devuelve la posicion y y se debe poner en el centro de la pelota
              (find-ground-collision x y vy(rest platforms))))]));revisa la otra plataforma

; Distancia entre dos puntos
(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))

; Filtro: Devuelve la lista de monedas MENOS las que tocamos
(define (remove-touched-coins ball-x ball-y coins)
  (filter (lambda (c) 
            ; Mantenemos la moneda si está lejos de la bola
            (> (distance ball-x ball-y (coin-x c) (coin-y c))
               (+ BALL-RADIUS COIN-RADIUS)))
          coins))

;verifica si tocamos la puerta
(define(touching-goal? ball-x ball-y g)
  (and(<= (abs (- ball-x(goal-x g)))
          (/(goal-w g)2)); ddentro del ancho
      (<= (abs (- ball-y (goal-y g)))
          (/(goal-h g)2))));dentro del alto

;funcion para reiniciar
(define(reset-level nivel score)
  (cond
    [(= nivel 1)INITIAL-GAME-STATE];reinicia al 1

    [(= nivel 2);reinicia al nivel 2
     (make-game-state (make-posn 50 300)(make-posn 0 0)#t
                      LEVEL-2 score COINS-LEVEL-2 2 GOAL-LEVEL-2)]
    [(= nivel 3);reinicia al nivel 3
     (make-game-state (make-posn 50 300)(make-posn 0 0)#t
                      LEVEL-3 score
                      COINS-LEVEL-3 3 GOAL-LEVEL-3)]
    [else INITIAL-GAME-STATE]))
;fisicas de la pelota
(define (tick-game gs)
  (let* ([old-pos (game-state-ball-pos gs)]
         [old-vel (game-state-ball-vel gs)]
         [nivel-actual (game-state-level-num gs)])
    
    ; SI ESTAMOS EN NIVEL 4 (VICTORIA), EL JUEGO SE CONGELA
    (if (= nivel-actual 4)
        gs ; No hacemos nada, solo mostramos la pantalla de victoria
        
        ; SI NO, CALCULAMOS FÍSICA NORMAL:
        (let* ([monedas-actuales (game-state-coins gs)]
               [puntaje-actual (game-state-score gs)]
               [meta-actual (game-state-current-goal gs)]
               
               ; 1. Gravedad y Movimiento
               [new-vy (+ (posn-y old-vel) GRAVITY)] 
               [vel-after-gravity (make-posn (posn-x old-vel) new-vy)]
               [new-x (+ (posn-x old-pos) (posn-x vel-after-gravity))]
               [new-y (+ (posn-y old-pos) (posn-y vel-after-gravity))]
               
               ; 2. Monedas
               [monedas-restantes (remove-touched-coins new-x new-y monedas-actuales)]
               [monedas-agarradas (- (length monedas-actuales) (length monedas-restantes))]
               [nuevo-puntaje (+ puntaje-actual (* monedas-agarradas 10))] 
               
               ; 3. Colisiones
               [ground-y (find-ground-collision new-x new-y new-vy (game-state-obstacles gs))]
               [gano-nivel? (touching-goal? new-x new-y meta-actual)])
          
          (cond
            ; CASO A: ¡GANÓ NIVEL!
            [gano-nivel?
             (cond
 [(= nivel-actual 1) (make-game-state (make-posn 50 300)
     (make-posn 0 0) #true LEVEL-2 nuevo-puntaje COINS-LEVEL-2 2 GOAL-LEVEL-2)]
               [(= nivel-actual 2) (make-game-state (make-posn 50 300)
         (make-posn 0 0) #true LEVEL-3 nuevo-puntaje COINS-LEVEL-3 3 GOAL-LEVEL-3)]
               ; SI TERMINA EL NIVEL 3 -> PASA AL NIVEL 4 (VICTORIA)
[else (make-game-state (make-posn 0 0) (make-posn 0 0)
                #t empty nuevo-puntaje empty 4 (make-goal 0 0 0 0 "white"))])]
            
            ; CASO B: ATERRIZAJE
            [(number? ground-y)
             (make-game-state (make-posn new-x ground-y) (make-posn (posn-x vel-after-gravity) 0)
      #true (game-state-obstacles gs) nuevo-puntaje monedas-restantes nivel-actual meta-actual)]
            
            ; CASO C: CAÍDA AL VACÍO (MUERTE)
            [(> new-y (+ HEIGHT 50)) 
             (reset-level nivel-actual puntaje-actual)] ; <--- Usamos la función de reinicio
            
            ; CASO D: AIRE
            [else
             (make-game-state (make-posn new-x new-y) vel-after-gravity
   #false (game-state-obstacles gs) nuevo-puntaje monedas-restantes nivel-actual meta-actual)])))))
     

;Controlador del Juego
; CONTROLADOR DE MOVIMIENTO (ACTUALIZADO CON MONEDAS)
(define (handle-game-key gs key)
  (let ([old-vel (game-state-ball-vel gs)]
        [on-ground? (game-state-is-on-ground? gs)]
        [mis-monedas (game-state-coins gs)]
        [nivel-actual (game-state-level-num gs)]   ; <--- Guardar nivel
        [meta-actual (game-state-current-goal gs)]) ; <--- Guardar meta
    (cond
      ; IZQUIERDA
      [(key=? key "left")
       (make-game-state (game-state-ball-pos gs)
                        (make-posn (- 0 MOVEMENT-SPEED) (posn-y old-vel))
                        on-ground? (game-state-obstacles gs) (game-state-score gs)
                        mis-monedas nivel-actual meta-actual)] ; <--- Pasamos todo
      
      ; DERECHA
      [(key=? key "right")
       (make-game-state (game-state-ball-pos gs)
                        (make-posn MOVEMENT-SPEED (posn-y old-vel))
                        on-ground? (game-state-obstacles gs) (game-state-score gs)
                        mis-monedas nivel-actual meta-actual)]
      
      ; SALTO
      [(and (key=? key "up") on-ground?)
       (make-game-state (game-state-ball-pos gs)
                        (make-posn (posn-x old-vel) JUMP-VELOCITY)
                        #false (game-state-obstacles gs) (game-state-score gs)
                        mis-monedas nivel-actual meta-actual)]
      
      [else gs])))


;si se suelta una tecla
(define (handle-game-key-up gs key)
  (let ([old-vel (game-state-ball-vel gs)])
    (cond
      [(or (key=? key "left") (key=? key "right"))
       (make-game-state (game-state-ball-pos gs)
                        (make-posn 0 (posn-y old-vel))
                        (game-state-is-on-ground? gs)
                        (game-state-obstacles gs)
                        (game-state-score gs)
                        (game-state-coins gs)
                        (game-state-level-num gs)     ; <--- Nuevo
                        (game-state-current-goal gs))] ; <--- Nuevo
      [else gs])))   

;ahora funcion para que dibuje las plataformas
(define(draw-platforms platforms scene)
  (cond
   {(empty? platforms)scene}; si la lista esta vacio la muestra igual
   {else
    (let({p (first platforms)});toma la primera plataforma de la lista
      (place-image
       (rectangle(platform-w p)
                 (platform-h p)"solid"(platform-color p))
       (platform-x p)
       (platform-y p);recursividad
       (draw-platforms(rest platforms)scene)))}))
  
; Función auxiliar para dibujar lista de monedas
(define (draw-coins coins scene)
  (cond
    [(empty? coins) scene]
    [else
     (let ([c (first coins)])
       (place-image
        (circle COIN-RADIUS "solid" COIN-COLOR)
        (coin-x c)
        (coin-y c)
        (draw-coins (rest coins) scene)))]))

  ;funcion auxiliar para la puerta
  (define(draw-goal g scene)
    (place-image(rectangle (goal-w g)
                           (goal-h g)"solid"(goal-color g))
                (goal-x g)(goal-y g)scene))
;diseño de niveles
;dibuja la pelota el suelo y la puntuacion
; DIBUJO DEL JUEGO (ACTUALIZADO CON MONEDAS)
(define (draw-game gs)
  (if (= (game-state-level-num gs) 4)
      
      ; SI ES NIVEL 4 -> DIBUJAR PANTALLA DE VICTORIA
      (overlay (above (text "¡HAS GANADO!" 50 "green")
                      (text (string-append "Puntaje Final: " (number->string (game-state-score gs))) 30 "black"))
               (empty-scene WIDTH HEIGHT "white"))
      
      ; SI NO -> DIBUJAR JUEGO NORMAL
      (let* ([pos (game-state-ball-pos gs)] 
             [score-text (text (string-append "Nivel " (number->string (game-state-level-num gs)) 
                                              " - Puntos: " (number->string (game-state-score gs)))
                               20 "white")]
             [sky (empty-scene WIDTH HEIGHT "skyblue")]
             [scene-platforms (draw-platforms (game-state-obstacles gs) sky)]
             [scene-goal (draw-goal (game-state-current-goal gs) scene-platforms)]
             [scene-coins (draw-coins (game-state-coins gs) scene-goal)]
             [scene-final (place-image (circle BALL-RADIUS "solid" "red") (posn-x pos) (posn-y pos) scene-coins)])
        
        (overlay/align "left" "top" score-text scene-final))))
  
        
; funcion para iniciar el menu
; funcion para iniciar el menu CORREGIDA
(define (start-menu)
  (big-bang INITIAL-MENU-STATE 
    (name "Red Ball Menu")
    (to-draw draw-menu) 
    (on-key handle-menu-key) 
    (on-tick tick-menu 1/28) 
    (stop-when symbol?))) ; <--- ¡BORRA LA PARTE DE (lambda...)! SOLO DEJA (stop-when symbol?)

;ahora que se ejecute el juego
(define (start-game-loop gs)
  (big-bang gs
    (name "Red Ball Game")
    (to-draw draw-game) 
    (on-tick tick-game 1/28) 
    (on-key handle-game-key) 
    (on-release handle-game-key-up)));on-release funcion de libreria para que la pelota pare
;funcion para musica de fondo
(define (play-background-music)
  (if (file-exists? musica-fondo-path)
      (play-sound musica-fondo-path #t) ; #t para que no congele el juego
      (displayln "Advertencia: No se encontró musica.wav")))
; principal
(define (main)
  (play-background-music)
  (let ([next-scene (start-menu)]) 
    (cond
      ;si presiona game start inicia el juego
      [(symbol=? next-scene 'GAME-START)
       (start-game-loop INITIAL-GAME-STATE)]
      [(symbol=? next-scene 'OPTIONS-SCREEN)
       (display "¡Pasamos a opciones!")]
[else (void)])))
(main)