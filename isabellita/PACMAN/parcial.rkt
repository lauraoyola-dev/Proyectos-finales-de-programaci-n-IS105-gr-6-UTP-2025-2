(define (sumar-vector vec pos suma)
  (if (< pos (vector-length vec))
      (sumar-vector vec (+ pos 1) (+ suma (vector-ref vec pos)))
      suma))

(define (promedio vec)
  (/ (sumar-vector vec 0 0) (vector-length vec)))

(define (llenar-resultado vec-original vec-resultado pos prom)
  (if (< pos (vector-length vec-original))
      (begin
        (if (> (vector-ref vec-original pos) prom)
            (vector-set! vec-resultado pos 1)
            (vector-set! vec-resultado pos 0))
        (llenar-resultado vec-original vec-resultado (+ pos 1) prom))
      vec-resultado))

(define (convertir-vector vec)
  (llenar-resultado vec (make-vector (vector-length vec)) 0 (promedio vec)))

; Función para contar 1s en el vector resultado
(define (contar-unos vec pos cont)
  (if (< pos (vector-length vec))
      (if (= (vector-ref vec pos) 1)
          (contar-unos vec (+ pos 1) (+ cont 1))
          (contar-unos vec (+ pos 1) cont))
      cont))

(define vector #(5 8 3 9 2 7 4 6 10 1))
(define prom (promedio vector))
(define resultado (convertir-vector vector))

(displayln "Vector original:")
(displayln vector)
(displayln "Vector convertido:")
(displayln resultado)
(displayln (string-append "Promedio: " (number->string prom)))
(displayln (string-append "Valores por encima del promedio: " (number->string (contar-unos resultado 0 0))))

;cadenas

(define (es-vocal? c)
  (or (char=? c #\a) (char=? c #\e) (char=? c #\i) (char=? c #\o) (char=? c #\u)
      (char=? c #\A) (char=? c #\E) (char=? c #\I) (char=? c #\O) (char=? c #\U)))

(define (quitar-espacios-vocales texto pos resultado)
  (if (< pos (string-length texto))
      (if (or (char=? (string-ref texto pos) #\space)
              (es-vocal? (string-ref texto pos)))
          (quitar-espacios-vocales texto (+ pos 1) resultado)
          (quitar-espacios-vocales texto (+ pos 1) (string-append resultado (string (string-ref texto pos)))))
      resultado))

(define (invertir-cadena texto pos resultado)
  (if (< pos 0)
      resultado
      (invertir-cadena texto (- pos 1) (string-append resultado (string (string-ref texto pos))))))

(define (procesar-cadena texto)
  (invertir-cadena (quitar-espacios-vocales texto 0 "") 
                   (- (string-length (quitar-espacios-vocales texto 0 "")) 1) 
                   ""))

(define texto "Programar es bonito")
(displayln (string-append "Texto original: " texto))
(displayln (string-append "Texto procesado: " (procesar-cadena texto)))


;estructura

(define-struct estudiante (codigo nombre promedio semestre))

(define (encontrar-por-codigo estudiante n-codigo i)
  (if (< i (vector-length estudiante))
      (if (string-ci=? (estudiante-codigo (vector-ref estudiante i)) n-codigo)
          (begin
            (display (estudiante-codigo (vector-ref estudiante i)))
            (newline)
            (display (estudiante-nombre (vector-ref estudiante i)))
            (newline)
            (display (estudiante-promedio (vector-ref estudiante i)))
            (newline)
            (display (estudiante-semestre (vector-ref estudiante i)))
            (newline))
          (encontrar-por-codigo estudiante n-codigo (+ i 1)))
      (display "ESTUDIANTE NO ENONTRADO")))
          
      
(define (cambiar-semestre estudiante i n-codigo)
  (if (< i (vector-length estudiante))
      (let ((est (vector-ref estudiante i)))
        (if (string-ci=? (estudiante-codigo est) n-codigo)
            (begin
              (vector-set! estudiante i
                           (make-estudiante
                            (estudiante-codigo est)
                            (estudiante-nombre est)
                            (estudiante-promedio est)
                            (+ (estudiante-semestre est) 1)))
              (display "Semestre actualizado para: ")
              (display (estudiante-codigo est))
              (newline)
              (display (estudiante-nombre est))
              (newline)
              (display (estudiante-promedio est))
              (newline)
              (display (+(estudiante-semestre est)1)))
            (cambiar-semestre estudiante (+ i 1) n-codigo)))
      (display "ESTUDIANTE NO ENCONTRADO")))

(define (promedio  



(define (principal)
  (define estudiante (vector
                      (make-estudiante "101010" "Laura" 4 1)
                      (make-estudiante "121212" "Sofia" 4 2)
                      (make-estudiante "131313" "Juan" 2 1)
                      (make-estudiante "000000" "Federico" 2 2)))
  (display "Ingrese el estudiantes que desea buscar (por codigo): ")
  (define n-codigo (read))
  (encontrar-por-codigo estudiante n-codigo 0)
  (newline)
  (display "EL NUEVO VECTOR ES ")
  (newline)
  (cambiar-semestre estudiante 0 n-codigo)
  (newline)
  (newline)
  (display "PROMEDIO NOTAS ")
  (promedio-notas estudiante 0)
  (newline)
  (display "ESTUDIANTES CON PROMEDIO MAYOR AL GENERAL ")
  (promedio-mayor estudiante 0 promedio-notas))


(principal)