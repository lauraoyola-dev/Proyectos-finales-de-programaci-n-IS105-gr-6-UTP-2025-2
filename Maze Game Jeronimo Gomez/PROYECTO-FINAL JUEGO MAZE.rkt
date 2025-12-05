(require 2htdp/image)
(require 2htdp/universe)

; 1. ESTRUCTURA DEL JUEGO
(define-struct videojuego (escena menu-y menu-v jug-x jug-y)) 

; 2. ESTRUCTURA PARA LOS MUROS
(define-struct muro (x y w h))

; --- CONSTANTES GLOBALES ---
(define ANCHO 1200)
(define ALTO 700)     
(define VELOCIDAD-MENU 5)
(define VELOCIDAD-JUGADOR 10) 

; --- DIMENSIONES Y COLORES ---
(define ANCHO-MURO 30)
(define COLOR-LABERINTO (make-color 255 119 0))
(define COLOR-META "green")
(define COLOR-JUGADOR "black")
(define TAMANO-JUGADOR 20)

; --- POSICIONES INICIALES DE CADA NIVEL ---
(define INICIO-L1-X 150)
(define INICIO-L1-Y 300)
(define INICIO-L2-X 150) 
(define INICIO-L2-Y 400)
(define INICIO-L3-X 750)
(define INICIO-L3-Y 350)

; ESTADO INICIAL: ARRANCA DESDE EL MENU
(define ESTADO-INICIAL (make-videojuego "menu" 100 VELOCIDAD-MENU INICIO-L1-X INICIO-L1-Y)) 

;PRIMERA META
(define META-1 (make-muro 700 50 30 30)) 


;MENU Y ESCENARIO
(define cambio-escena(rectangle 1200 700 "solid" "white"))
(define color-crema(make-color 245 232 211))
(define boton-para-play(rectangle 300 100 "solid" "gray"))
(define cuadrado-rellenar1(rectangle 500 1400 "solid" color-crema))
(define cuadrado-rellenar2(rectangle 500 1400 "solid" color-crema))
(define fondo-laberinto(bitmap "laberinto.png"))
(define titulo (bitmap "Titulo-juego.png"))
(define boton-play (bitmap "boton-play-sin.png"))
(define Meta(bitmap "Meta.png"))
(define maze(bitmap "mazegame.png"))
(define teclas(bitmap "flechas.png"))
(define fondo-escena (empty-scene ANCHO ALTO))
(define nueva-escena (empty-scene ANCHO ALTO))
(define ganador-imagen(bitmap "ganador.png"))
(define LVL1(bitmap "NIVEL1.png"))
(define LVL2(bitmap "NIVEL2.png"))
(define LVL3(bitmap "NIVEL3.png"))

(define color-naranja(make-color 255 119 0))
(define color-naranja-claro(make-color 255 169 89))
(define rojo-claro(make-color 255 153 153))
(define azul-claro(make-color 135 206 250))
(define escenario-1-pared-1(rectangle 150 1400 "solid" color-naranja))
(define escenario-1-pared-2(rectangle 90 1400 "solid" color-naranja))
(define escenario-1-pared-3(rectangle 150 1400 "solid" color-naranja))
(define escenario-1-pared-1b(rectangle 150 1400 "solid" "red"))
(define escenario-1-pared-2b(rectangle 90 1400 "solid" "red"))
(define escenario-1-pared-3b(rectangle 150 1400 "solid" "red"))
(define escenario-1-pared-1c(rectangle 150 1400 "solid" "blue"))
(define escenario-1-pared-2c(rectangle 90 1400 "solid" "blue"))
(define escenario-1-pared-3c(rectangle 150 1400 "solid" "blue"))
(define circulo-decorativo-pequeño(ellipse 10 10 "solid" color-naranja-claro))
(define circulo-decorativo-grande(ellipse 30 30 "solid" color-naranja-claro))
(define circulo-decorativo-pequeñoB(ellipse 10 10 "solid" rojo-claro))
(define circulo-decorativo-grandeB(ellipse 30 30 "solid" rojo-claro))
(define circulo-decorativo-pequeñoC(ellipse 10 10 "solid" azul-claro))
(define circulo-decorativo-grandeC(ellipse 30 30 "solid" azul-claro))

;JUGADOR
(define jugador-cubo(rectangle 20 20 "solid" "black"))

; ==============================================================================
;                                 NIVEL 1
; ==============================================================================
(define (dibujar-nivel-juego-1 estado)
  (define escena-nivel(place-image cambio-escena 600 350 nueva-escena))
  (define decoracion-escen-1(place-image escenario-1-pared-1 0 0 escena-nivel))
  (define decoracion-escen-2(place-image escenario-1-pared-2 800 0 decoracion-escen-1))
  (define decoracion-escen-3(place-image escenario-1-pared-3 1200 0 decoracion-escen-2))
  
  (define circulo-1(place-image circulo-decorativo-pequeño 810 10 decoracion-escen-3))
  (define circulo-2(place-image circulo-decorativo-pequeño 800 430 circulo-1))
  (define circulo-3(place-image circulo-decorativo-pequeño 810 350 circulo-2))
  (define circulo-4(place-image circulo-decorativo-pequeño 820 100 circulo-3))
  (define circulo-5(place-image circulo-decorativo-pequeño 810 280 circulo-4))
  (define circulo-6(place-image circulo-decorativo-pequeño 770 355 circulo-5))
  (define circulo-7(place-image circulo-decorativo-pequeño 812 522 circulo-6))
  (define circulo-8(place-image circulo-decorativo-pequeño 822 666 circulo-7))
  (define circulo-9(place-image circulo-decorativo-pequeño 810 639 circulo-8))
  (define circulo-10(place-image circulo-decorativo-grande 810 500 circulo-9))
  (define circulo-11(place-image circulo-decorativo-grande 780 560 circulo-10))
  (define circulo-12(place-image circulo-decorativo-grande 810 400 circulo-11))
  (define circulo-13(place-image circulo-decorativo-grande 810 223 circulo-12))
  (define circulo-14(place-image circulo-decorativo-grande 800 60 circulo-13))

  (define circulo-15(place-image circulo-decorativo-pequeño 10 10 circulo-14))
  (define circulo-16(place-image circulo-decorativo-pequeño 20 430 circulo-15))
  (define circulo-17(place-image circulo-decorativo-pequeño 10 350 circulo-16))
  (define circulo-18(place-image circulo-decorativo-pequeño 0 100 circulo-17))
  (define circulo-19(place-image circulo-decorativo-pequeño 10 280 circulo-18))
  (define circulo-20(place-image circulo-decorativo-pequeño 70 355 circulo-19))
  (define circulo-21(place-image circulo-decorativo-pequeño 12 522 circulo-20))
  (define circulo-22(place-image circulo-decorativo-pequeño 22 666 circulo-21))
  (define circulo-23(place-image circulo-decorativo-pequeño 10 639 circulo-22))
  (define circulo-24(place-image circulo-decorativo-grande 3 500 circulo-23))
  (define circulo-25(place-image circulo-decorativo-grande 60 560 circulo-24))
  (define circulo-26(place-image circulo-decorativo-grande 10 400 circulo-25))
  (define circulo-27(place-image circulo-decorativo-grande 5 223 circulo-26))
  (define circulo-28(place-image circulo-decorativo-grande 20 60 circulo-27))

  (define circulo-29(place-image circulo-decorativo-pequeño 1150 10 circulo-28))
  (define circulo-30(place-image circulo-decorativo-pequeño 1170 430 circulo-29))
  (define circulo-31(place-image circulo-decorativo-pequeño 1180 350 circulo-30))
  (define circulo-32(place-image circulo-decorativo-pequeño 1150 100 circulo-31))
  (define circulo-33(place-image circulo-decorativo-pequeño 1160 280 circulo-32))
  (define circulo-34(place-image circulo-decorativo-pequeño 1170 355 circulo-33))
  (define circulo-35(place-image circulo-decorativo-pequeño 1162 522 circulo-34))
  (define circulo-36(place-image circulo-decorativo-pequeño 1172 666 circulo-35))
  (define circulo-37(place-image circulo-decorativo-pequeño 1160 639 circulo-36))
  (define circulo-38(place-image circulo-decorativo-grande 1153 500 circulo-37))
  (define circulo-39(place-image circulo-decorativo-grande 1160 560 circulo-38))
  (define circulo-40(place-image circulo-decorativo-grande 1160 400 circulo-39))
  (define circulo-41(place-image circulo-decorativo-grande 1155 223 circulo-40))
  (define circulo-42(place-image circulo-decorativo-grande 1170 60 circulo-41))

  ;MUROS LABERINTO
  (define muro-1(place-image pared-1 200 100 circulo-42))
  (define muro-2(place-image pared-2 200 550 muro-1))
  (define muro-3(place-image pared-3 460 10 muro-2))
  (define muro-4(place-image pared-4 460 690 muro-3))
  (define muro-5(place-image pared-5 700 670 muro-4))
  
  (define muro-6(place-image pared-6 290 340 muro-5))
  (define muro-7(place-image pared-7 379 375 muro-6))
  (define muro-8(place-image pared-8 344 439 muro-7))
  (define muro-9(place-image pared-9 310 550 muro-8))
  (define muro-10(place-image pared-10 435 614 muro-9))
  (define muro-11(place-image pared-11 630 380 muro-10))
  (define muro-12(place-image pared-12 495 85 muro-11))
  (define muro-13(place-image pared-13 440 145 muro-12))
  (define muro-14(place-image pared-14 236 85 muro-13))
  (define muro-15(place-image pared-15 245 129 muro-14))
  (define muro-16(place-image pared-16 560 500 muro-15))
  (define muro-17(place-image pared-17 415 260 muro-16))
  (define muro-18(place-image pared-18 560 300 muro-17))
  (define muro-19(place-image pared-19 600 614 muro-18))
  (define muro-20(place-image pared-20 270 200 muro-19))
  (define muro-21(place-image pared-21 470 450 muro-20))
  (define meta(place-image Meta 700 50 muro-21))
  (define decoracion(place-image maze 980 100 meta))
  (define decoracion-2(place-image teclas 980 550 decoracion))
  (define contador(place-image LVL1 990 330 decoracion-2))
  (place-image jugador-cubo (videojuego-jug-x estado) (videojuego-jug-y estado) contador))

;PAREDES LABERINTO (NIVEL 1)
(define pared-1(rectangle 30 350 "solid" color-naranja))
(define pared-2(rectangle 30 450 "solid" color-naranja))
(define pared-3(rectangle 500 30 "solid" color-naranja))
(define pared-4(rectangle 500 30 "solid" color-naranja))
(define pared-5(rectangle 30 1200 "solid" color-naranja))

(define pared-6(rectangle 150 30 "solid" color-naranja))
(define pared-7(rectangle 30 100 "solid" color-naranja))
(define pared-8(rectangle 100 30 "solid" color-naranja))
(define pared-9(rectangle 30 100 "solid" color-naranja))
(define pared-10(rectangle 280 30 "solid" color-naranja))
(define pared-11(rectangle 30 500 "solid" color-naranja))
(define pared-12(rectangle 385 30 "solid" color-naranja))
(define pared-13(rectangle 361 30 "solid" color-naranja))
(define pared-14(rectangle 47 30 "solid" color-naranja))
(define pared-15(rectangle 30 61 "solid" color-naranja))
(define pared-16(rectangle 30 200 "solid" color-naranja))
(define pared-17(rectangle 320 30 "solid" color-naranja))
(define pared-18(rectangle 30 100 "solid" color-naranja))
(define pared-19(rectangle 60 30 "solid" color-naranja))
(define pared-20(rectangle 30 100 "solid" color-naranja))
(define pared-21(rectangle 30 200 "solid" color-naranja))

(define lista-muros-nivel-1
  (list
   (make-muro 200 100 30 350)
   (make-muro 200 550 30 450)
   (make-muro 460 10 500 30)
   (make-muro 460 690 500 30)
   (make-muro 700 670 30 1200)
   (make-muro 290 340 150 30)
   (make-muro 379 375 30 100)
   (make-muro 344 439 100 0)
   (make-muro 310 550 30 100)
   (make-muro 435 614 280 30)
   (make-muro 630 380 30 500)
   (make-muro 495 85 385 30)
   (make-muro 440 145 361 30)
   (make-muro 236 85 47 30)
   (make-muro 245 129 30 61)
   (make-muro 560 500 30 100)
   (make-muro 415 260 320 30)
   (make-muro 560 300 30 80)
   (make-muro 600 614 60 50)
   (make-muro 270 200 30 100)
   (make-muro 470 450 30 200)

   (make-muro 0 15 370 30)
   (make-muro 0 685 370 30)
   (make-muro 770 10 370 30)
   (make-muro 770 690 370 30)
   (make-muro 15 0 30 1400)
   (make-muro 770 0 30 1400)
   ));X Y W H LISTA DE MUROS

; ==============================================================================
;                                 NIVEL 2
; ==============================================================================

(define (dibujar-nivel-juego-2 estado)
  (define escena-nivel(place-image cambio-escena 600 350 nueva-escena))
  (define decoracion-escen-1(place-image escenario-1-pared-1b 0 0 escena-nivel))
  (define decoracion-escen-2(place-image escenario-1-pared-2b 800 0 decoracion-escen-1))
  (define decoracion-escen-3(place-image escenario-1-pared-3b 1200 0 decoracion-escen-2))
  
  (define circulo-1(place-image circulo-decorativo-pequeñoB 810 10 decoracion-escen-3))
  (define circulo-2(place-image circulo-decorativo-pequeñoB 800 430 circulo-1))
  (define circulo-3(place-image circulo-decorativo-pequeñoB 810 350 circulo-2))
  (define circulo-4(place-image circulo-decorativo-pequeñoB 820 100 circulo-3))
  (define circulo-5(place-image circulo-decorativo-pequeñoB 810 280 circulo-4))
  (define circulo-6(place-image circulo-decorativo-pequeñoB 770 355 circulo-5))
  (define circulo-7(place-image circulo-decorativo-pequeñoB 812 522 circulo-6))
  (define circulo-8(place-image circulo-decorativo-pequeñoB 822 666 circulo-7))
  (define circulo-9(place-image circulo-decorativo-pequeñoB 810 639 circulo-8))
  (define circulo-10(place-image circulo-decorativo-grandeB 810 500 circulo-9))
  (define circulo-11(place-image circulo-decorativo-grandeB 780 560 circulo-10))
  (define circulo-12(place-image circulo-decorativo-grandeB 810 400 circulo-11))
  (define circulo-13(place-image circulo-decorativo-grandeB 810 223 circulo-12))
  (define circulo-14(place-image circulo-decorativo-grandeB 800 60 circulo-13))

  (define circulo-15(place-image circulo-decorativo-pequeñoB 10 10 circulo-14))
  (define circulo-16(place-image circulo-decorativo-pequeñoB 20 430 circulo-15))
  (define circulo-17(place-image circulo-decorativo-pequeñoB 10 350 circulo-16))
  (define circulo-18(place-image circulo-decorativo-pequeñoB 0 100 circulo-17))
  (define circulo-19(place-image circulo-decorativo-pequeñoB 10 280 circulo-18))
  (define circulo-20(place-image circulo-decorativo-pequeñoB 70 355 circulo-19))
  (define circulo-21(place-image circulo-decorativo-pequeñoB 12 522 circulo-20))
  (define circulo-22(place-image circulo-decorativo-pequeñoB 22 666 circulo-21))
  (define circulo-23(place-image circulo-decorativo-pequeñoB 10 639 circulo-22))
  (define circulo-24(place-image circulo-decorativo-grandeB 3 500 circulo-23))
  (define circulo-25(place-image circulo-decorativo-grandeB 60 560 circulo-24))
  (define circulo-26(place-image circulo-decorativo-grandeB 10 400 circulo-25))
  (define circulo-27(place-image circulo-decorativo-grandeB 5 223 circulo-26))
  (define circulo-28(place-image circulo-decorativo-grandeB 20 60 circulo-27))

  (define circulo-29(place-image circulo-decorativo-pequeñoB 1150 10 circulo-28))
  (define circulo-30(place-image circulo-decorativo-pequeñoB 1170 430 circulo-29))
  (define circulo-31(place-image circulo-decorativo-pequeñoB 1180 350 circulo-30))
  (define circulo-32(place-image circulo-decorativo-pequeñoB 1150 100 circulo-31))
  (define circulo-33(place-image circulo-decorativo-pequeñoB 1160 280 circulo-32))
  (define circulo-34(place-image circulo-decorativo-pequeñoB 1170 355 circulo-33))
  (define circulo-35(place-image circulo-decorativo-pequeñoB 1162 522 circulo-34))
  (define circulo-36(place-image circulo-decorativo-pequeñoB 1172 666 circulo-35))
  (define circulo-37(place-image circulo-decorativo-pequeñoB 1160 639 circulo-36))
  (define circulo-38(place-image circulo-decorativo-grandeB 1153 500 circulo-37))
  (define circulo-39(place-image circulo-decorativo-grandeB 1160 560 circulo-38))
  (define circulo-40(place-image circulo-decorativo-grandeB 1160 400 circulo-39))
  (define circulo-41(place-image circulo-decorativo-grandeB 1155 223 circulo-40))
  (define circulo-42(place-image circulo-decorativo-grandeB 1170 60 circulo-41))

  ;MUROS LABERINTO
(define muro-1  (place-image pared-1b 200 600 circulo-42))
  (define muro-2  (place-image pared-2b 200 150 muro-1))
  (define muro-3  (place-image pared-3b 460 690 muro-2))
  (define muro-4  (place-image pared-4b 460 10  muro-3))
  (define muro-5  (place-image pared-5b 700 30  muro-4))
  (define muro-6  (place-image pared-6b 290 360 muro-5))
  (define muro-7  (place-image pared-7b 379 325 muro-6))
  (define muro-8  (place-image pared-8b 344 261 muro-7))
  (define muro-9  (place-image pared-9b 310 150 muro-8))
  (define muro-10 (place-image pared-10b 435 86  muro-9))
  (define muro-11 (place-image pared-11b 630 320 muro-10))
  (define muro-12 (place-image pared-12b 495 615 muro-11))
  (define muro-13 (place-image pared-13b 440 555 muro-12))
  (define muro-14 (place-image pared-14b 236 615 muro-13))
  (define muro-15 (place-image pared-15b 245 571 muro-14))
  (define muro-16 (place-image pared-16b 560 200 muro-15))
  (define muro-17 (place-image pared-17b 415 440 muro-16))
  (define muro-18 (place-image pared-18b 560 400 muro-17))
  (define muro-19 (place-image pared-19b 600 86  muro-18))
  (define muro-20 (place-image pared-20b 270 500 muro-19))
  (define muro-21 (place-image pared-21b 470 250 muro-20))
  (define meta(place-image Meta 700 650 muro-21))
  (define decoracion(place-image maze 980 100 meta))
  (define decoracion-2(place-image teclas 980 550 decoracion))
  (define contador(place-image LVL2 990 330 decoracion-2))
  (place-image jugador-cubo (videojuego-jug-x estado) (videojuego-jug-y estado) contador))

;PAREDES LABERINTO (NIVEL 2)
(define pared-1b(rectangle 30 350 "solid" "red"))
(define pared-2b(rectangle 30 450 "solid" "red"))
(define pared-3b(rectangle 500 30 "solid" "red"))
(define pared-4b(rectangle 500 30 "solid" "red"))
(define pared-5b(rectangle 30 1200 "solid" "red"))

(define pared-6b(rectangle 150 30 "solid" "red"))
(define pared-7b(rectangle 30 100 "solid" "red"))
(define pared-8b(rectangle 100 30 "solid" "red"))
(define pared-9b(rectangle 30 100 "solid" "red"))
(define pared-10b(rectangle 280 30 "solid" "red"))
(define pared-11b(rectangle 30 500 "solid" "red"))
(define pared-12b(rectangle 385 30 "solid" "red"))
(define pared-13b(rectangle 361 30 "solid" "red"))
(define pared-14b(rectangle 47 30 "solid" "red"))
(define pared-15b(rectangle 30 61 "solid" "red"))
(define pared-16b(rectangle 30 200 "solid" "red"))
(define pared-17b(rectangle 320 30 "solid" "red"))
(define pared-18b(rectangle 30 100 "solid" "red"))
(define pared-19b(rectangle 60 30 "solid" "red"))
(define pared-20b(rectangle 30 100 "solid" "red"))
(define pared-21b(rectangle 30 200 "solid" "red"))


(define lista-muros-nivel-2
  (list
   (make-muro 200 600 30 350)
   (make-muro 200 150 30 450)
   (make-muro 460 690 500 30)
   (make-muro 460 10 500 30)
   (make-muro 700 30 30 1200)
   (make-muro 290 360 150 30)
   (make-muro 379 325 30 100)
   (make-muro 344 261 100 0)
   (make-muro 310 150 30 100)
   (make-muro 435 86 280 30)
   (make-muro 630 320 30 500)
   (make-muro 440 555 361 30)
   (make-muro 495 615 385 30)
   (make-muro 236 615 47 30)
   (make-muro 245 571 30 61)
   (make-muro 560 200 30 200)
   (make-muro 415 440 320 30)
   (make-muro 560 386 30 80)
   (make-muro 600 86 60 50)
   (make-muro 270 500 30 100)
   
   (make-muro 470 250 30 200)

   (make-muro 0 15 370 30)
   (make-muro 0 685 370 30)
   (make-muro 770 10 370 30)
   (make-muro 770 690 370 30)
   (make-muro 15 0 30 1400)
   (make-muro 770 0 30 1400)
   ));X Y W H LISTA DE MUROS


;SEGUNDA META
(define META-2 (make-muro 700 650 30 30))

; ==============================================================================
;                                 NIVEL 3
; ==============================================================================

(define (dibujar-nivel-juego-3 estado)
  (define escena-nivel(place-image cambio-escena 600 350 nueva-escena))
  (define decoracion-escen-1(place-image escenario-1-pared-1c 0 0 escena-nivel))
  (define decoracion-escen-2(place-image escenario-1-pared-2c 800 0 decoracion-escen-1))
  (define decoracion-escen-3(place-image escenario-1-pared-3c 1200 0 decoracion-escen-2))

  (define circulo-1(place-image circulo-decorativo-pequeñoC 810 10 decoracion-escen-3))
  (define circulo-2(place-image circulo-decorativo-pequeñoC 800 430 circulo-1))
  (define circulo-3(place-image circulo-decorativo-pequeñoC 810 350 circulo-2))
  (define circulo-4(place-image circulo-decorativo-pequeñoC 820 100 circulo-3))
  (define circulo-5(place-image circulo-decorativo-pequeñoC 810 280 circulo-4))
  (define circulo-6(place-image circulo-decorativo-pequeñoC 770 355 circulo-5))
  (define circulo-7(place-image circulo-decorativo-pequeñoC 812 522 circulo-6))
  (define circulo-8(place-image circulo-decorativo-pequeñoC 822 666 circulo-7))
  (define circulo-9(place-image circulo-decorativo-pequeñoC 810 639 circulo-8))
  (define circulo-10(place-image circulo-decorativo-grandeC 810 500 circulo-9))
  (define circulo-11(place-image circulo-decorativo-grandeC 780 560 circulo-10))
  (define circulo-12(place-image circulo-decorativo-grandeC 810 400 circulo-11))
  (define circulo-13(place-image circulo-decorativo-grandeC 810 223 circulo-12))
  (define circulo-14(place-image circulo-decorativo-grandeC 800 60 circulo-13))

  (define circulo-15(place-image circulo-decorativo-pequeñoC 10 10 circulo-14))
  (define circulo-16(place-image circulo-decorativo-pequeñoC 20 430 circulo-15))
  (define circulo-17(place-image circulo-decorativo-pequeñoC 10 350 circulo-16))
  (define circulo-18(place-image circulo-decorativo-pequeñoC 0 100 circulo-17))
  (define circulo-19(place-image circulo-decorativo-pequeñoC 10 280 circulo-18))
  (define circulo-20(place-image circulo-decorativo-pequeñoC 70 355 circulo-19))
  (define circulo-21(place-image circulo-decorativo-pequeñoC 12 522 circulo-20))
  (define circulo-22(place-image circulo-decorativo-pequeñoC 22 666 circulo-21))
  (define circulo-23(place-image circulo-decorativo-pequeñoC 10 639 circulo-22))
  (define circulo-24(place-image circulo-decorativo-grandeC  3 500 circulo-23))
  (define circulo-25(place-image circulo-decorativo-grandeC 60 560 circulo-24))
  (define circulo-26(place-image circulo-decorativo-grandeC 10 400 circulo-25))
  (define circulo-27(place-image circulo-decorativo-grandeC 5 223 circulo-26))
  (define circulo-28(place-image circulo-decorativo-grandeC 20 60 circulo-27))

  (define circulo-29(place-image circulo-decorativo-pequeñoC 1150 10 circulo-28))
  (define circulo-30(place-image circulo-decorativo-pequeñoC 1170 430 circulo-29))
  (define circulo-31(place-image circulo-decorativo-pequeñoC 1180 350 circulo-30))
  (define circulo-32(place-image circulo-decorativo-pequeñoC 1150 100 circulo-31))
  (define circulo-33(place-image circulo-decorativo-pequeñoC 1160 280 circulo-32))
  (define circulo-34(place-image circulo-decorativo-pequeñoC 1170 355 circulo-33))
  (define circulo-35(place-image circulo-decorativo-pequeñoC 1162 522 circulo-34))
  (define circulo-36(place-image circulo-decorativo-pequeñoC 1172 666 circulo-35))
  (define circulo-37(place-image circulo-decorativo-pequeñoC 1160 639 circulo-36))
  (define circulo-38(place-image circulo-decorativo-grandeC 1153 500 circulo-37))
  (define circulo-39(place-image circulo-decorativo-grandeC 1160 560 circulo-38))
  (define circulo-40(place-image circulo-decorativo-grandeC 1160 400 circulo-39))
  (define circulo-41(place-image circulo-decorativo-grandeC 1155 223 circulo-40))
  (define circulo-42(place-image circulo-decorativo-grandeC 1170 60 circulo-41))

  ;MUROS LABERINTO
(define muro-1  (place-image pared-1C  700 600 circulo-42))
(define muro-2  (place-image pared-2C  700 150 muro-1))
(define muro-3  (place-image pared-3C   440 690 muro-2))
(define muro-4  (place-image pared-4C   440 10  muro-3))
(define muro-5  (place-image pared-5C   200 30  muro-4))
(define muro-6  (place-image pared-6C   610 360 muro-5))
(define muro-7  (place-image pared-7C   521 325 muro-6))
(define muro-8  (place-image pared-8C   556 261 muro-7))
(define muro-9  (place-image pared-9C   590 150 muro-8))
(define muro-10 (place-image pared-10C  465 86  muro-9))
(define muro-11 (place-image pared-11C  270 320 muro-10))
(define muro-12 (place-image pared-12C  405 615 muro-11))
(define muro-13 (place-image pared-13C  460 555 muro-12))
(define muro-14 (place-image pared-14C  664 615 muro-13))
(define muro-15 (place-image pared-15C  655 571 muro-14))
(define muro-16 (place-image pared-16C  340 200 muro-15))
(define muro-17 (place-image pared-17C  485 440 muro-16))
(define muro-18 (place-image pared-18C  340 400 muro-17))
(define muro-19 (place-image pared-19C  300 86  muro-18))
(define muro-20 (place-image pared-20C  630 500 muro-19))
(define muro-21 (place-image pared-21C  450 250 muro-20))
  (define meta(place-image Meta 200 650 muro-21))
  (define decoracion(place-image maze 980 100 meta))
  (define decoracion-2(place-image teclas 980 550 decoracion))
  (define contador(place-image LVL3 990 330 decoracion-2))
  (place-image jugador-cubo (videojuego-jug-x estado) (videojuego-jug-y estado) contador))

;PAREDES LABERINTO (NIVEL 3)
(define pared-1C(rectangle 30 350 "solid" "blue"))
(define pared-2C(rectangle 30 450 "solid" "blue"))
(define pared-3C(rectangle 500 30 "solid" "blue"))
(define pared-4C(rectangle 500 30 "solid" "blue"))
(define pared-5C(rectangle 30 1200 "solid" "blue"))

(define pared-6C(rectangle 150 30 "solid" "blue"))
(define pared-7C(rectangle 30 100 "solid" "blue"))
(define pared-8C(rectangle 100 30 "solid" "blue"))
(define pared-9C(rectangle 30 100 "solid" "blue"))
(define pared-10C(rectangle 280 30 "solid" "blue"))
(define pared-11C(rectangle 30 500 "solid" "blue"))
(define pared-12C(rectangle 385 30 "solid" "blue"))
(define pared-13C(rectangle 361 30 "solid" "blue"))
(define pared-14C(rectangle 47 30 "solid" "blue"))
(define pared-15C(rectangle 30 61 "solid" "blue"))
(define pared-16C(rectangle 30 200 "solid" "blue"))
(define pared-17C(rectangle 320 30 "solid" "blue"))
(define pared-18C(rectangle 30 100 "solid" "blue"))
(define pared-19C(rectangle 60 30 "solid" "blue"))
(define pared-20C(rectangle 30 100 "solid" "blue"))
(define pared-21C(rectangle 30 200 "solid" "blue"))

(define lista-muros-nivel-3
  (list
   (make-muro 700 600 30 350)
   (make-muro 700 150 30 450)
   (make-muro 440 690 500 30)
   (make-muro 440 10 500 30)
   (make-muro 200 30 30 1200)
   (make-muro 610 360 150 30)
   (make-muro 521 325 30 100)
   (make-muro 556 261 100 0)
   (make-muro 590 150 30 100)
   (make-muro 465 86 280 30)
   (make-muro 270 320 30 500)
   (make-muro 460 615 300 30)
   (make-muro 460 555 321 30) 
   (make-muro 260 615 280 30)
   (make-muro 664 615 47 30)
   (make-muro 655 571 30 61)
   (make-muro 340 200 30 200)
   (make-muro 485 440 320 30)
   (make-muro 340 386 30 80)
   (make-muro 300 86 60 50)
   (make-muro 630 500 30 100)
   (make-muro 450 250 30 200)

   (make-muro 0 15 370 30)
   (make-muro 0 685 370 30)
   (make-muro 770 10 370 30)
   (make-muro 770 690 370 30)
   (make-muro 15 0 30 1400)
   (make-muro 770 0 30 1400)
   ));X Y W H LISTA DE MUROS


;TERCERA META
(define META-3 (make-muro 200 650 30 30))

; ==============================================================================
;                                LÓGICA GENERAL
; ==============================================================================

; Función que selecciona los muros y la posicion inicial cuando se pasa de nivel
(define (obtener-geometria escena)
  (cond
    [(string=? escena "jugando")  (list lista-muros-nivel-1 META-1 INICIO-L2-X INICIO-L2-Y "jugando2")]
    [(string=? escena "jugando2") (list lista-muros-nivel-2 META-2 INICIO-L3-X INICIO-L3-Y "jugando3")]
    [(string=? escena "jugando3") (list lista-muros-nivel-3 META-3 INICIO-L3-X INICIO-L3-Y "ganador")]
    [else (list empty empty 0 0 "error")])) ; Caso por defecto

; --- DETECTOR DE COLISIONES ---
; Comprueba si el jugador choca con cualquier muro en la lista (lista-muros)
(define (hay-colision? x y lista-muros)
  (ormap (lambda (m)
           (and 
            ; Colisión Horizontal
            (< (abs (- x (muro-x m))) (+ (/ TAMANO-JUGADOR 2) (/ (muro-w m) 2)))
            ; Colisión Vertical
            (< (abs (- y (muro-y m))) (+ (/ TAMANO-JUGADOR 2) (/ (muro-h m) 2)))))
         lista-muros))

; --- FUNCIÓN: DETECTOR DE VICTORIA ---
; Comprueba si el jugador (x, y) ha tocado el área de la meta (meta-linea)
(define (verificar-victoria x y meta-linea)
  (and
   ; Colisión Horizontal con META
   (< (abs (- x (muro-x meta-linea))) (+ (/ TAMANO-JUGADOR 2) (/ (muro-w meta-linea) 2)))
   ; Colisión Vertical con META
   (< (abs (- y (muro-y meta-linea))) (+ (/ TAMANO-JUGADOR 2) (/ (muro-h meta-linea) 2)))))

;función para dibujar el estado "ganador final" cuando se complete el nivel 3
(define (dibujar-ganador estado)
  (place-image ganador-imagen 600 350(empty-scene ANCHO ALTO "black")))

; ==============================================================================
;                                 MENU
; ==============================================================================
(define (dibujar-menu estado)
  (define escena-fondo-relleno1 (place-image cuadrado-rellenar1 0 0 fondo-escena))
  (define escena-fondo-relleno2 (place-image cuadrado-rellenar2 1200 0 escena-fondo-relleno1))
  (define escena-fondo (place-image fondo-laberinto 600 350 escena-fondo-relleno2))
  (define boton-cuadrado (place-image boton-para-play 590 400 escena-fondo))
  (define escena-boton (place-image boton-play 600 400 boton-cuadrado))
  (place-image titulo 600 (videojuego-menu-y estado) escena-boton))

;CAMBIO DE ESCENA CUANDO SE COMPLETE UN NIVEL
(define (controlador-dibujo estado)
  (cond
    [(string=? (videojuego-escena estado) "menu")      (dibujar-menu estado)]
    [(string=? (videojuego-escena estado) "jugando")   (dibujar-nivel-juego-1 estado)] ; Nivel 1
    [(string=? (videojuego-escena estado) "jugando2")  (dibujar-nivel-juego-2 estado)] ; Nivel 2
    [(string=? (videojuego-escena estado) "jugando3")   (dibujar-nivel-juego-3 estado)]; Nivel 3
    [(string=? (videojuego-escena estado) "ganador")   (dibujar-ganador estado)]
    [else (empty-scene ANCHO ALTO "white")]))

; --- FUNCION DE MOVIMIENTO ---

(define (mover-menu estado)
  (cond
    [(string=? (videojuego-escena estado) "menu")
     (let* ([y-actual (videojuego-menu-y estado)]
            [v-actual (videojuego-menu-v estado)]
            [jx (videojuego-jug-x estado)]
            [jy (videojuego-jug-y estado)]
            [y-siguiente (+ y-actual v-actual)]) 
       (cond 
         [(< y-siguiente 50)  (make-videojuego "menu" 50 (- v-actual) jx jy)]
         [(> y-siguiente 250) (make-videojuego "menu" 250 (- v-actual) jx jy)]
         [else (make-videojuego "menu" y-siguiente v-actual jx jy)]))]
    [else estado]))

; --- CONTROLADOR DE TECLAS ---

(define (controlador-teclas estado tecla)
  (if (or (string=? (videojuego-escena estado) "jugando")
          (string=? (videojuego-escena estado) "jugando2")
          (string=? (videojuego-escena estado) "jugando3"))
      (let* ([my (videojuego-menu-y estado)]
             [mv (videojuego-menu-v estado)]
             [x  (videojuego-jug-x estado)]
             [y  (videojuego-jug-y estado)]
             [geometria (obtener-geometria (videojuego-escena estado))]
             [muros (first geometria)]
             [meta-linea (second geometria)]
             [inicio-x (third geometria)]
             [inicio-y (fourth geometria)]
             [next-escena (fifth geometria)]
             [next-x (cond [(key=? tecla "left")  (- x VELOCIDAD-JUGADOR)]
                           [(key=? tecla "right") (+ x VELOCIDAD-JUGADOR)]
                           [else x])]
             [next-y (cond [(key=? tecla "up")    (- y VELOCIDAD-JUGADOR)]
                           [(key=? tecla "down")  (+ y VELOCIDAD-JUGADOR)]
                           [else y])])
        
        ;SI HAY COLISION NO SE MUEVE
        (if (hay-colision? next-x next-y muros)
            estado
            
            ; 2. SI NO CHOCA, VERIFICAMOS VICTORIA CON LA META DEL NIVEL ACTUAL
            (if (verificar-victoria next-x next-y meta-linea)
                ; Si toca la meta, pasamos a la siguiente escena con nuevas coordenadas
                (make-videojuego next-escena my mv inicio-x inicio-y)
                ; Si no hay colisión ni victoria, simplemente nos movemos
                (make-videojuego (videojuego-escena estado) my mv next-x next-y))))
      
      ; Si no estamos jugando, no cambia nada
      estado))

; --- FUNCION DE MOUSE ---

(define (controlador-mouse estado x y evento)
  (cond
    ; Click en el botón de Play del menú
    [(and (string=? (videojuego-escena estado) "menu")
          (string=? evento "button-down")
          (> x 450) (< x 750) 
          (> y 350) (< y 450)) 
     (make-videojuego "jugando" 0 0 INICIO-L1-X INICIO-L1-Y)]
    
    ; Click en la pantalla de "ganador" (Final) para reiniciar
    [(and (string=? (videojuego-escena estado) "ganador")
          (string=? evento "button-down"))
     ESTADO-INICIAL]
    
    [else estado]))

; --- FUNCION PARA ARRANCAR EL JUEGO ---

(big-bang ESTADO-INICIAL
  (to-draw controlador-dibujo)
  (on-tick mover-menu 0.03)
  (on-mouse controlador-mouse)
  (on-key controlador-teclas)
  (name "MAZE"))