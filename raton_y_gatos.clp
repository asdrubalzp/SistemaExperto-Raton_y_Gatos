
;==========================================
; Título: Juego del Ratón y los Gatos
; Autor : Geovanny Zambrano
; Fecha  : 4 Sep 2020
;==========================================

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; INSTRUCCIONES
; Para comenzar la ejecución por primera vez escriba el comando (intrucciones)
; Si desea omitir las instucciones ejecute el comando (jugar)
;
; NOTACIONES Y REFERENCIAS
; Casilla negra : 0
; Casilla blanca : 1
; Casilla con gato : 5 X
; Casilla con ratón : 4
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


(deffunction jugar ()
  "Función para comenzar un nuevo juego"
  (clear-window)
  (reset)
  (run)
)

(deffunction instrucciones ()
  "Función para mostrar las instruccion del juego"


  (clear-window)
  (printout t crlf   "    ::::::::::::::::::::::::::::JUEGO DEL RATON Y LOS GATOS::::::::::::::::::::::::"crlf)
  (printout t crlf   "    INSTRUCCIONES"crlf)

  (printout t crlf   "    Se necesitan dos jugadores y un tablero de ajedrez."crlf
                     "    Se utilizan cuatro fichas del mismo color, que simbolizan a los gatos,"crlf
                     "    y otra ficha de distinto color, que simboliza al ratón."crlf
                     "    Los cuatro gatos se sitúan en un extremo del tablero, ocupando todas las"crlf
                     "    casillas negras de la primera fila. El ratón se sitúa en el otro extremo"crlf
                     "    del tablero, en cualquiera de las casillas negras de la primera fila. "crlf
  )
  (printout t crlf   "    Escribe una letra para continuar...")
  (bind ?input (read))
  (clear-window)


  (printout t crlf   "    ::::::::::::::::::::::::::::JUEGO DEL RATON Y LOS GATOS::::::::::::::::::::::::"crlf)
  (printout t crlf   "    INSTRUCCIONES"crlf)

  (printout t crlf   "1-  El ratón es el primero en moverse. En cualquier momento el ratón puede moverse"crlf
                     "    a cualquiera de las casillas negras próximas a él, siempre que la casilla no "crlf
                     "    esté ya ocupada por un gato."crlf crlf

                     "2-  Después de moverse el ratón, le toca el turno a uno de los gatos. Los gatos "crlf
                     "    pueden moverse sólo hacia delante, siempre qu e la casilla no esté ocupada por "crlf
                     "    el ratón o por alguno de los otros gatos."crlf crlf

                     "3-  Cuando se haya movido uno de los gatos, le tocará el turno al ratón, y así "crlf
                     "    sucesivamente."crlf crlf

                     "4-  Gana el ratón si consigue llegar al lado contrario, es decir, a cualquiera "crlf
                     "    de las casillas de las que salieron los gatos."crlf crlf

                     "5-  Ganan los gatos si consiguen atrapar al ratón, de manera que el ratón no pueda"crlf
                     "    moverse más."crlf crlf
  )
  (printout t crlf   "    Escribe una letra para comenzar...")
  (bind ?input (read))
  (jugar)
)

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; TEMPLATES

(deftemplate casilla
  "Define la estructura de las casillas de un tablero de ajedrez"

  ;El campo fila contendrá el número de fila de la casilla
  (slot fila (type NUMBER))

  ;El campo columna contendrá el número de fila de la casilla
  (slot columna (type NUMBER))

  ;El campo valor contendrá el identificador de una casilla
  (multislot valor (type NUMBER))
)

(deftemplate pieza
  "Estructura de las piezas a imprimir por pantalla.
   Cada casilla internanmente consta de un espacio de 3x3 por ello se usan 3 partes"

  ;Campo para enlazar el valor de casilla con la pieza a imprimir por pantalla
  (slot valor)

  ;Contiene la primer parte a imprimir de una pieza
  (slot parte1)

  ;Contiene la segunda parte a imprimir de una pieza
  (slot parte2)

  ;Contiene la tercera parte a imprimir de una pieza
  (slot parte3)
)


; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; HECHOS INICIALES

(deffacts hechos-iniciales
  "Declaracion de hechos iniciales"

  ;Hecho de control para ingresar el tablero sin piezas.
  (ingresar-tablero)

  ;Hecho para controlar la impresion del tablero.
  (imprime 1 1)

  ;Hecho para definir que parte de la regla se debe imprimir.
  (parteImprimir 1)

  ;Hecho que funciona como contador de coincidencias de una regla.

  ;Hecho de control para actualizar la impresion del tablero.
  (actualizar-tablero)

  ;Define la figura que se mostrará al imprimir una casilla blanca
  (pieza
    (valor 0)
    (parte1 "       ")
    (parte2 "       ")
    (parte3 "       ")
  )

  ;Define la figura que se mostrará al imprimir una casilla negra
  (pieza
    (valor 1)
    (parte1 " . . . ")
    (parte2 " . . . ")
    (parte3 " . . . ")
  )

  ;Define la figura que se mostrará al imprimir una casilla con un raton
  (pieza
    (valor 4)
    (parte1 "(_)_(_)")
    (parte2 " (o o) ")
    (parte3 "==\\o/==")
  )

  ;Define la figura que se mostrará al imprimir una casilla con un gato
  (pieza
    (valor 5)
    (parte1 " /\\_/\\ ")
    (parte2 "( o.o )")
    (parte3 " > ^ < ")
  )
)


; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; REGLAS

(defrule ingresar-tablero-en-blanco

  "
    Regla que determina el valor de una casilla basandose en un tablero de ajedrez
    8x8.

    Variables:
    ?h -> Hecho de control para ingresar el tablero en blanco.
    ?i -> Iterador que controla el número de filas a imprimir.
    ?j -> Iterador que controla el número de columnas a imprimir.

    Assersiones:
    (casilla (fila ?i)(columna ?j)(valor 0)): Template del tipo casilla que contendrá
    un número de fila, numero de columna y un valor o identificador de la casilla.

    (ingresar-pos-iniciales): Hecho de control para insertar la posicion inicial de
    las piezas.
  "

 ; Hecho de control para la activacion de la presente regla.
  ?h <- (ingresar-tablero)
  =>

 ; Se elimina el hecho de control que activa a la regla
  (retract ?h)

 ; Se inserta el hecho de control para ingresar posiciones iniciales
  (assert (ingresar-pos-iniciales))

 ; Bucle for para ingresar las 64 casillas con su respectivo valor de manera automática
  (loop-for-count (?i 1 8)do
    (loop-for-count (?j 1 8)do

 ; Condición para determinar si la casilla es negra o blanca, es decir si contendrá
 ; un valor 1 u 0.
      (if (eq(mod (+ ?i ?j) 2) 0)
        then
          (assert (casilla (fila ?i)(columna ?j)(valor 0)))
        else
          (assert (casilla (fila ?i)(columna ?j)(valor 1)))
      )
    )
  )
)

(defrule posiciones-piezas-iniciales

  "
    Regla para modificar el tablero en blanco y añadirle la posicion de las piezas
    iniciales.

    Variables:
    ?h     -> Hecho de control para ingresar las posiciones iniciales.
    ?raton -> Número de hecho de una casilla determinada.

    ?gato2 -> Número de hecho de una casilla determinada.
    ?gato3 -> Número de hecho de una casilla determinada.
    ?gato4 -> Número de hecho de una casilla determinada.
    ?gato5 -> Número de hecho de una casilla determinada.


    Assersiones (Modificaciones):
    Se modifica el valor de las casilla seleccionadas.
  "

 ; Hecho de control para ejecutar regla de modificar piezas.
  ?h <- (ingresar-pos-iniciales)

  ;Se obtiene el número de hecho de la casilla donde se colocará la pieza del raton.

  ?raton <- (casilla (fila 1)(columna 4)(valor ?))

  ?gato1 <- (casilla (fila 8)(columna 1)(valor ?))
  ?gato2 <- (casilla (fila 8)(columna 3)(valor ?))
  ?gato3 <- (casilla (fila 8)(columna 5)(valor ?))
  ?gato4 <- (casilla (fila 8)(columna 7)(valor ?))

  =>
  ;Se modifica el valor de casilla con número de hecho ?raton
  (modify ?raton (valor 4))

  ;Se modifica el valor de casilla con número de hecho ?gato1
  (modify ?gato1 (valor 5 1))

  ;Se modifica el valor de casilla con número de hecho ?gato2
  (modify ?gato2 (valor 5 2))

  ;Se modifica el valor de casilla con número de hecho ?gato3
  (modify ?gato3 (valor 5 3))

  ;Se modifica el valor de casilla con número de hecho ?gato4
  (modify ?gato4 (valor 5 4))

  ;se elimina el hecho de control que activa la presente regla.
  (retract ?h)
)


(defrule actualizar-tablero

  "
    Regla para imprimir el tablero dependiendo de el valor que tenga una casilla.

    Variables:
    ?h     -> Hecho de control para iterar el número de casillas del tablero.
    ?i     -> Iterador del número de filas
    ?j     -> Iterador del número de columnas.
    ?value -> Valor de pieza que contedrá la casilla que se itera.
    ?p1    -> Parte uno de la figura de la pieza.
    ?p2    -> Parte dos de la figura de la pieza.
    ?p3    -> Parte tres de la figura de la pieza.
    ?f     -> Hecho de control para determinar que parte de la pieza se imprime
    ?parteImprimir    -> Contador que determina que parte de la pieza se imprime.
    ?g     -> Hecho de control para actualizar el tablero


    Assersiones :
    (imprime x x) :Hecho de control para validar que casilla se tiene que imprimir.
    (parteImprimir X) :Hecho de control que itera las casillas internamente.
    (pedir-movimiento-raton) :Hecho de control para activar la regla de pedir el movimiento
    del raton al jugador.
  "

  ;valida que solo se ejecutara la regla en caso que la casilla
  ;a imprimir este en el rango de 8x8 del tablero
  ?h <- (imprime ?i ?j&:(<= (* ?i ?j) 64))

  ;Se capturan los datos de una determinada pieza para su posterior impresion
  (pieza  (valor ?value)(parte1 ?p1)(parte2 ?p2)(parte3 ?p3))

  ;Hecho de control que captura la parte de la pieza a imprimir
  ?f <- (parteImprimir ?parteImprimir)

  ;Hecho de control con la que se activa la presente regla.
  ?g <-(actualizar-tablero)

  ;Condicion para capturar todas las casillas del tablero
  (casilla(fila ?i)(columna ?j)(valor ?value $?))
   =>

   ;para simular un ciclo, se elimina el hecho que contiene las filas y columnas para su posterior
   ;insercion con una posicion iterada.
   (retract ?h)

   ;condicion para imprimir la primer linea del tablero
    (if (and (= ?i 1) (= ?j 1) (= ?parteImprimir 1)) then
     (printout t crlf crlf" ------- ------- ------- ------- ------- ------- ------- -------"crlf)
    )

    ;condicion para imprimir la parte 1 de las piezas
    (if (and (and (<= ?i 8) (< ?j 8)) (= ?parteImprimir 1))then
     (printout t "|" ?p1)
      (assert (imprime ?i (+ ?j 1)))
    )

    ;condicion para imprimir el borde derecho del tablero
    (if (and (and (<= ?i 8) (= ?j 8)) (= ?parteImprimir 1)) then
          (printout t  "|"?p1"|" crlf  )
          (assert (imprime ?i 1))
          (assert (parteImprimir 2))
          (retract ?f)
    )

    ;condicion para la segunda fila de una pieza.
    (if (and (and (<= ?i 8) (< ?j 8)) (= ?parteImprimir 2))then
      (printout t "|" ?p2)
       (assert (imprime ?i (+ ?j 1)))
    )

    ;condicion para imprimir el borde derecho de la segunda fila de las piezas.
    (if (and (and (<= ?i 8) (= ?j 8)) (= ?parteImprimir 2))then
           (printout t "|"?p2"| " ?i  crlf )
           (assert (imprime ?i 1))
           (retract ?f)
           (assert (parteImprimir 3))
    )

    ;condicion para imprimir la tercer fila de las piezas
    (if (and (and (<= ?i 8) (< ?j 8)) (= ?parteImprimir 3))then
        (printout t "|"?p3)
        (assert (imprime ?i (+ ?j 1)))
    )

    ;condicion para imprimir el borde derecho de la tercer fila de las casillas
    (if (and (and (<= ?i 8) (= ?j 8)) (= ?parteImprimir 3))then
          (printout t "|" ?p3 "|" crlf
            " ------- ------- ------- ------- ------- ------- ------- -------"crlf)
          (assert (imprime (+ ?i 1) 1))
          (assert (parteImprimir 1))
          (retract ?f)
    )

    ;condicion para imprimir los indices de las casillas en la parte inferior
    (if (and (= ?i 8) (= ?j 8) (= ?parteImprimir 3)) then
         (printout t"    1       2       3       4       5       6       7       8 " crlf crlf)
         (retract ?g)
         (assert (imprime 1 1))
         (assert (pedir-movimiento-raton))
    )
)



(defrule pedir-movimiento-raton
  "
    Regla para solicitar la posicion hacia donde se movera el raton

    Variables:
    ?h                  -> Hecho de control para activar la regla
    ?filaActual         -> Fila donde se encuentra el raton al momento de solicitar la nueva posicion.
    ?columnaActual      -> Columna donde se encuentra el raton al momento de solicitar la nueva posicion.
    ?repetir            -> Hecho de control para la validacion de una posicion valida.
    ?fila               -> Variable donde se almacenara el input de fila del usuario.
    ?columna            -> Variable donde se almacenara el input de columna del usuario.
    ?diferenciaFilas    -> Variable que almacena la resta entre la fila actual del raton y la fila del input.
    ?diferenciaColumnas -> Variable que almacena la resta entre la columna actual del raton y la columna del input.

    Assersiones :
    (fila-columna-a-mover x x) : Hecho para almacenar la fila y la columna digitadas por el usuario.
    (comprobar-casilla-ocupada): Hecho de control para verificar que el input del usuario no este ocupado por otra pieza.
  "

  ; condicion para determinar la ejecucion de la presente regla.
  ?h <- (pedir-movimiento-raton)

  ;condicion para caputurar la posicion del raton.
  (casilla(fila ?filaActual) (columna ?columnaActual)(valor 4))

  =>

  ;Elimina el hecho de control para activar la presente regla.
  (retract ?h)

  ;Creacion de la variable de control repetir, que determinara si se le debe pedir nuevamente el input al usuario.
  (bind ?repetir TRUE)

  ;estructura de control para mostrar mensajes de error mientras el input no sea valido.
    (while (eq ?repetir TRUE)do

        (printout t crlf "Ingresa la fila y la columna donde se moverá el ratón:" crlf)
        (printout t "Fila    [1-8] :")

        ;Se hace uso de la funcion (read) para capturar el input por teclado del usuario en la variable ?fila
        (bind ?fila (read))
        (printout t "Columna [1-8] :")

        ;Se captura el input en la variable ?columna
        (bind ?columna (read))

        ;condidicion para determinar si el input dado por el usuario usuario se encuentra dentro del rango del tablero 8x8
        (if (or (> ?fila 8) (> ?columna 8) (< ?fila 1) (< ?columna 1) )
          then
            (printout t crlf "Posición fuera del rango del tablero..."crlf"INGRESE NUEVAMENTE."crlf crlf)
          else

            ;Se almacena la diferecia de filas en la variblale del mismo nombre
            (bind ?diferenciaFilas (abs (- ?filaActual ?fila)))

            ;Se almacena la diferencia de columnas en la variable del mismo nombre.
            (bind ?diferenciaColumnas (abs (- ?columnaActual ?columna)))

            ;Condicion para determinar si el input dado por el usuario se encuentra dentro de una de las 4 esquinas del raton,
            ;en caso que esta condicion se cumpla se procede a almacenar el input, caso contrario se muestra un mensaje de error.
            (if (and(= (+ ?diferenciaFilas ?diferenciaColumnas) 2 ) (<> ?filaActual ?fila) (<> ?columnaActual ?columna))
            then
              (assert (fila-columna-a-mover ?fila ?columna))
              (assert (comprobar-casilla-ocupada))
              (bind ?repetir FALSE)
              (clear-window)
            else
              (printout t crlf "Posición fuera de alcance del ratón..."crlf"INGRESE NUEVAMENTE."crlf crlf)
            )
        )
    )

)

(defrule comprobar-casilla-ocupada
  "
  Regla para verificar que la posicion a mover del raton ademas de estar dentro de una de las 4 esquinas, no se
  encuentra ocupada por otra pieza.

  Variables:
  ?h                  -> Hecho de control para activar la regla
  ?f                  -> Hecho de control para capturar la fila y columna a mover del raton
  ?f1                 -> Fila donde se desea mover el raton.
  ?c1                 -> Columna donde se desea mover el raton.
  ?value              -> Valor que tendra la casilla hacia donde se desea mover el raton.

  Assersiones :
  (ejecutar-movimiento-jugador-raton): Hecho de control para ejecutar el movimiento del raton
  (pedir-movimiento-raton) : Hecho de control para ejecutar nuevamente la regla que obtiene el input del usuario.
  (actualizar-tablero) : Hecho de control para la ejecucion de la regla que imprime el tablero.
  "
  ;Condicion para determinar que el hecho de control para que se ejecute esta regla existe.
  ?h <- (comprobar-casilla-ocupada)

  ;Condicion para capturar el input dado por el usuario.
  ?f<-(fila-columna-a-mover ?f1 ?c1)

  ;Condicion para capturar el valor de la casilla hacia donde se desea mover el usuario.
  (casilla (fila ?f1)(columna ?c1)(valor ?value $?))
  =>

  ;Se elimina el hecho de control que permite ejecutar la presente regla.
  (retract ?h)

  ;Condicion para determinar que la casilla a mover tiene un valor de 1 es decir si la casilla esta disponible.
  ;En caso que la casilla este desocupada se inserta el hecho control que activara la modificacion
  ;y la actualizacion del tablero. En Caso que la casilla ya este ocupada se muestra un mensaje de error y se
  ;inserta el hecho de control para pedir el input de movimiento nuevamente.
  (if (= ?value 1)
  then
    (assert (ejecutar-movimiento-jugador-raton))
  else

    (assert (pedir-movimiento-raton))
    (assert (actualizar-tablero))
    (printout t crlf "la posición ya está ocupada..."crlf"INGRESE NUEVAMENTE."crlf crlf)
    (retract ?f)
  )
)

(defrule ejecutar-movimiento-jugador-raton

  "
    Regla para ejecutar el movimiento del jugador raton,se modifica el valor
    de la pieza donde se encuentra el raton y el valor de la pieza donde se movera el raton.

    Variables:
    ?h                  -> Hecho de control para activar esta regla
    ?f                  -> Hecho de control para capturar la fila y columna a mover del raton
    ?f1                 -> Fila donde se desea mover el raton.
    ?c1                 -> Columna donde se desea mover el raton.
    ?f2                 -> Fila donde se encuentra el raton.
    ?c2                 -> Columna donde se encuentra el raton.
    ?casilla-a-mover    -> Indice de hecho que contine la casilla a la que se movera el raton
    ?casilla-actual     -> Indice de hecho que contine la casilla donde se encuentra el raton

    Assersiones y Modificaciones :
    (calcular-esquinas-raton) : Hecho de control para ejecutar la regla que obtiene las 4 esquinas de la posicion del raton.
    Se modifican las casillas donde se encuentra el raton y donde se movera, de esta manera
    cuando se imprima nuevamente el tablero cambiara la posicion en la que se imprimen las piezas.
    "

  ;Hecho de control para activar la presente la regla.
  ?h <-(ejecutar-movimiento-jugador-raton)

  ;Hecho de control para capturar la fila y la columna hacia donde se movera el raton
  ?f <-(fila-columna-a-mover ?f1 ?c1)

  ;Hecho para obtener el indice de hecho de la casilla donde se desea mover al raton,
  ;con este indice se hace modificacion del valor de la casilla.
  ?casilla-a-mover <- (casilla (fila ?f1)(columna ?c1)(valor ?))

  ;Hecho para obtener el indice de hecho de la casilla donde se encuentra el raton,
  ;con este indice se hace modificacion del valor de la casilla.
  ?casilla-actual  <- (casilla (fila ?f2)(columna ?c2)(valor 4))
  =>

  ;Se elimina el hecho de control que activa la presente regla
  (retract ?h)

  ;Se elimina el hecho que contiene la fila y la columna capturada.
  (retract ?f)

  ;Modificacion del valor de la casilla a mover, se modifica a valor 4 ya que es la representacion del
  ;raton en el tablero
  (modify ?casilla-a-mover (valor 4))

  ;Modificacion del valor de la casilla actual, se modifica a valor 1 ya que es la representacion del
  ;casilla negra en el tablero
  (modify ?casilla-actual (valor 1))

  ;Hecho de control para encontrar las esquinas del raton.
  (assert (calcular-esquinas-raton))
)

(defrule buscar-diagonales-gatos

  "
    Regla para añadir un hecho que contiene las esquinas de cada gato. El hecho contendrá la fila superior
    y las dos columnas adyacentes, es decir la esquina izquierda y la esquina derecha de cada gato.

    Variables:
    ?idgato             -> Indice de hecho donde se encuentra la casilla que contiene un gato.
    ?filaGatos          -> Variable para capturar la fila donde se encuentre un determinado gato.
    ?columnaGatos       -> Variable para capturar la columna donde se encuentre un determinado gato.
    ?fila-a-buscar      -> Variable que contiene una fila hacia arriba de la fila actual de un gato.

    ?columna-der-a-buscar -> Variable que contiene la columna derecha con respecto a la posicion de un gato.
    ?columna-izq-a-buscar -> Variable que contiene la columna izquierda con respecto a la posicion de un gato.

    Asersiones :
    (esquina-gato ?idGato ?fila-a-buscar ?columna-izq-a-buscar ?columna-der-a-buscar)
    -> El hecho se compone por un idGato que luego servira para obtener el valor y la posición del gato en cuestión.
    además se inserta la fila superior y la columna izquierda y derecha de el gato.
    "

  ; Hecho de control para activar la presente regla.
  (buscar-diagonales-gatos)

  ; Condicion para capturar el indice de hecho y la posición de un determinado gatos.
  ?idGato <-(casilla (fila ?filaGatos) (columna ?columnaGatos) (valor 5 ?))

  =>

  ;Creacion de variable para almacenar la fila superior del gato.
  (bind ?fila-a-buscar(- ?filaGatos 1))

  ;Creacion de variable para almacenar la columna derecha del gato
  (bind ?columna-der-a-buscar(+ ?columnaGatos 1))

  ;Creacion de variable para almacenar la columna izquierda del gato.
  (bind ?columna-izq-a-buscar(- ?columnaGatos 1))

  ;Condicion para determinar si el gato no se encuentra en la primer fila. Solo se podra buscar esquinas de gatos
  ;que se encuentren por debajo de la fila 1.
  (if (> ?fila-a-buscar 1)
    then
    ;Condicion para deteminar si el gato se encuentra en la columna uno, en caso que lo esté
    ;debido no existe ni una casilla con columna 0 se procede a determinar que la columna izquierda del
    ;gato se encontrara en la misma posicion, es decir en la columna 0. Esto se lo hace con el fin de tener
    ;un hecho compacto es decir que siempre contedrá 4 parámetros.
    ;Con este parseo de inserta el hecho con la esquina del gato.
      (if (= ?columna-izq-a-buscar 0)
        then
        (assert(esquina-gato ?idGato ?fila-a-buscar 1 ?columna-der-a-buscar))
      )
      ;Basandose en la misma presima de la condicion anterior si la columna derecha del gato corresponde a la
      ;columna 9 se procede a insertar la misma columna donde se encuentra el gato.
      ;
      (if (= ?columna-der-a-buscar 9)
        then
        (assert(esquina-gato ?idGato ?fila-a-buscar ?columna-izq-a-buscar 8))
      )

      ;En caso que el gato se encuntre dentro del rango 6x6, es decir tiene esquinas en ambos lados, se procede a
      ;insertar el hecho con los datos pertinentes.
      (if (and(> ?columna-izq-a-buscar 0)(< ?columna-der-a-buscar 9))
        then
        (assert(esquina-gato ?idGato ?fila-a-buscar ?columna-izq-a-buscar ?columna-der-a-buscar))
      )
    else
    (assert(esquina-gato ?idGato ?filaGatos ?columna-izq-a-buscar ?columna-der-a-buscar))
  )
)

(defrule ejecutar-movimiento-completar-fila-gatos


  "
    Regla con contiene la logica para hacer que los gatos suban en bloque, se aplica de manera
    que un gato comprueba su dos esquinas superiores de esta manera se evalua que aquel gato que solo
    tenga una esquina disponible es decir solo una esquina con valor 1(casilla negra), se movera a esa
    esquina con valor uno.

    La primer descripcion muestra la posicion inicial de las piezas, se puede apreciar que el unico gato que tiene solo
    una esquina disponible es el primero por end es que se movera de primero.
        _______________
    8 |_|#|_|4|_|#|_|#|
    7 |#|_|#|_|#|_|#|_|
    6 |_|#|_|#|_|#|_|#|
    5 |#|_|#|_|#|_|#|_|
    4 |_|#|_|#|_|#|_|#|
    3 |#|_|#|_|#|_|#|_|
    2 |_|1|_|1|_|1|_|1|
    1 |5|_|5|_|5|_|5|_|
       1 2 3 4 5 6 7 8

   La segunda representacion hace referecia a lo que sucede cuando se ejecuta el primer movimiento, nuevamente se
   calculan las esquinas y se mueve el gato que solo tiene una esquina en uno disponible, en este caso se movera, el
   segundo gato.
       _______________
   8 |_|#|_|4|_|#|_|#|
   7 |#|_|#|_|#|_|#|_|
   6 |_|#|_|#|_|#|_|#|
   5 |#|_|#|_|#|_|#|_|
   4 |_|#|_|#|_|#|_|#|
   3 |1|_|1|_|#|_|#|_|
   2 |_|5|_|1|_|1|_|1|
   1 |#|_|5|_|5|_|5|_|
      1 2 3 4 5 6 7 8

    La tercera representacion hace referecia a lo que sucede cuando se presentan dos o mas coincidencias con la premisa
    que describe que se movera la que tenga solo una esquina en 1 disponible, en estos casos se efectua la operación de determinar
    cual de los gatos que cumplen la premisa se encuentra más abajo del tablero, de esta manera el gato que este más abajo del
    tablero será el que se moverá. En caso de que se encuentren dos o más coincidencias con la misma profundidad se mueve el
    primer gato que empareje con la premisa de izquierda a derecha.
        _______________
    8 |_|#|_|4|_|#|_|#|
    7 |#|_|#|_|#|_|#|_|
    6 |_|#|_|#|_|#|_|#|
    5 |#|_|#|_|#|_|#|_|
    4 |_|1|_|1|_|#|_|#|
    3 |1|_|5|_|1|_|#|_|
    2 |_|5|_|5|_|1|_|#|
    1 |#|_|#|_|5|_|#|_|
       1 2 3 4 5 6 7 8

    Variables:

    ?filaRaton          -> Variable que contendrá la fila actual del raton
    ?colRaton           -> Variable que contendrá la columna actual del raton

    La x en el nombre de las variables a continuacion hace referecia a que las variables son para cada gato es decir 1,2,3,4

    ?esquinaGatox       -> Variable para capturar el indice de hecho de las esquinas de un determinado gato.
    ?idGatox            -> Variable que captura del hecho de las esquinas de los gatos, es indice de hecho del gato que
                           tiene esas esquinas.
    ?fila-esquina-gato-x -> Variable que contiene la fila superior de un determinado gato.
    ?columnna-esquina-izq-gatox -> Variable que contiene la esquina izquierda de un determinado gato.
    ?columnna-esquina-der-gatox -> Variable que contiene la esquina derecha de un determinado gato.
    ?valor1x            -> Variable que contiene el valor de la casilla de la esquina izquierda de un determinado gato.
    ?valor2x            -> Variable que contiene el valor de la casilla de la esquina derecha de un determinado gato.

    Assersiones :
    -> (assert(fila-columna-mover-gatos ?fila-esquina-gato-x ?columnna-esquina-izq-gatox ?idGatox))
    El hecho se compone por la fila y la columna a la cual se movera el gato, ademas se agrega el indice de hecho del gato que va a
    ejecutar este moviemiento.

    -> (assert(ejecutar-movimiento-maquina-gato))
    Hecho de control para ejecutar la modificacion del tablero con los nuevos valores de los gatos.

    ->(assert(buscar-diagonales-gatos))
    Luego de ejecutar todas la condiciones, se insertan los hechos de control para buscar nuevamente las esquinas
    de los gatos.

    ->(assert(semi-encerrar-raton))
    se inserta el hecho de control de la regla que busca semiencerrar al raton. Esta regla tendra una jerarquia menor
    a la presente regla.

    ->(assert(gato-mas-alejado))
    se inserta el hecho de control de la regla que busca mover al gato más alejado del raton. Esta regla tendra una jerarquia menor
    a la presente regla.
    "

  ;Condicion para obtener la fila donde se encuentra el raton
  ?raton <- (casilla (fila ?filaRaton) (columna ?colRaton)(valor 4))

  ;Condicion para obtener el indice de hecho de las esquinas del gato uno además se optiene el indice de hecho del gato 1
  ;asimismo su fila superior, esquina izquierda, esquina derecha.
  ?esquinaGato1 <- (esquina-gato ?idGato1 ?fila-esquina-gato-1 ?columnna-esquina-izq-gato1 ?columnna-esquina-der-gato1)

  ;Condicion para capturar dado una fila y una columna el valor que tendra la esquina izquierda del gato 1.
  (casilla (fila ?fila-esquina-gato-1) (columna ?columnna-esquina-izq-gato1)(valor ?valor11 $?))

  ;Condicion para capturar dado una fila y una columna el valor que tendra la esquina derecha del gato 1.
  (casilla (fila ?fila-esquina-gato-1) (columna ?columnna-esquina-der-gato1)(valor ?valor21 $?))

  ;Las demás condiciones practicamente cumplen la misma función que las tres anteriores, cambiando unicamente el número de gato.
  ?esquinaGato2 <- (esquina-gato ?idGato2&~?idGato1 ?fila-esquina-gato-2 ?columnna-esquina-izq-gato2 ?columnna-esquina-der-gato2)
  (casilla (fila ?fila-esquina-gato-2) (columna ?columnna-esquina-izq-gato2)(valor ?valor12 $?))
  (casilla (fila ?fila-esquina-gato-2) (columna ?columnna-esquina-der-gato2)(valor ?valor22 $?))

  ?esquinaGato3 <- (esquina-gato ?idGato3&~?idGato2&~?idGato1 ?fila-esquina-gato-3 ?columnna-esquina-izq-gato3 ?columnna-esquina-der-gato3)
  (casilla (fila ?fila-esquina-gato-3) (columna ?columnna-esquina-izq-gato3)(valor ?valor13 $?))
  (casilla (fila ?fila-esquina-gato-3) (columna ?columnna-esquina-der-gato3)(valor ?valor23 $?))

  ?esquinaGato4 <- (esquina-gato ?idGato4&~?idGato3&~?idGato2&~?idGato1 ?fila-esquina-gato-4 ?columnna-esquina-izq-gato4 ?columnna-esquina-der-gato4)
  (casilla (fila ?fila-esquina-gato-4) (columna ?columnna-esquina-izq-gato4)(valor ?valor14 $?))
  (casilla (fila ?fila-esquina-gato-4) (columna ?columnna-esquina-der-gato4)(valor ?valor24 $?))

 =>

 ; Creacion de variables auxiliares que contendran que gato fue que conincidio con la premisa de subir en bloque.
 ; Ademas serviran para saber si existen dos o más coincidencias de las premisas.
  (bind ?contador1(+ 0 0))
  (bind ?contador2(+ 0 0))
  (bind ?contador3(+ 0 0))
  (bind ?contador4(+ 0 0))

  ;Condicion para determinar si el gato 1 tiene solo una esquina con valor uno disponible, en caso que esto se cumpla
  ;se elimina el hecho de las esquinas del gato uno y además se suma en 1 el contador del primer gato.
  ;En caso que la condicion no se cumpla se elimina las esquinas del gato 1.
    (if (or (eq ?valor11 ?valor21) (eq ?valor11 4)(eq  ?valor21 4) )
       then
         (retract ?esquinaGato1)
       else
       (retract ?esquinaGato1)

         (bind ?contador1(+ 0 1))
     )

     ;Condicion para determinar si el gato 2 tiene solo una esquina con valor uno disponible, en caso que esto se cumpla
     ;se elimina el hecho de las esquinas del gato 2 y además se suma en 1 el contador del segundo gato.
     ;En caso que la condicion no se cumpla se elimina las esquinas del gato 2.
    (if (or (eq ?valor12 ?valor22) (eq ?valor12 4)(eq  ?valor22 4) )
       then
         (retract ?esquinaGato2)
       else
       (retract ?esquinaGato2)

       (bind ?contador2(+ 0 1))

     )

     ;Condicion para determinar si el gato 3 tiene solo una esquina con valor uno disponible, en caso que esto se cumpla
     ;se elimina el hecho de las esquinas del gato 3 y además se suma en 1 el contador del tercer gato.
     ;En caso que la condicion no se cumpla se elimina las esquinas del gato 3.
    (if (or (eq ?valor13 ?valor23) (eq ?valor13 4)(eq  ?valor23 4) )
       then
         (retract ?esquinaGato3)
       else
       (retract ?esquinaGato3)

       (bind ?contador3(+ 0 1))
     )


     ;Condicion para determinar si el gato 4 tiene solo una esquina con valor uno disponible, en caso que esto se cumpla
     ;se elimina el hecho de las esquinas del gato 4 y además se suma en 1 el contador del cuarto gato.
     ;En caso que la condicion no se cumpla se elimina las esquinas del gato 4.
    (if (or (eq ?valor14 ?valor24) (eq ?valor14 4)(eq  ?valor24 4) )
      then
        (retract ?esquinaGato4)
      else
      (retract ?esquinaGato4)

      (bind ?contador4(+ 0 1))
    )

    ;Una vez que se realicen todas las comprobaciones se procede a sumar los contadores y determinar si existe o no
    ;mas de una coincidencia.
    ;En caso que hayan dos o más coincidencias se procede sacar las diferecias con respecto a la fila del raton.
    (if (> (+ ?contador1 ?contador2 ?contador3 ?contador4) 1 )
      then
      ;Variables auxiliares que contiene la diferencia de todos los gatos con respecto a la fila del raton.
      (bind ?diferencia1 (- ?fila-esquina-gato-1 ?filaRaton))
      (bind ?diferencia2 (- ?fila-esquina-gato-2 ?filaRaton))
      (bind ?diferencia3 (- ?fila-esquina-gato-3 ?filaRaton))
      (bind ?diferencia4 (- ?fila-esquina-gato-4 ?filaRaton))
      ;se escoje la coincidencia de mayor profundidad.

      ;Cuando se obtienen todas las diferencias se comprueba si la el gato 1 es el que se encuentra más abajo con respecto al raton.
      (if(and (>= ?diferencia1 ?diferencia2)(>= ?diferencia1 ?diferencia3)(>= ?diferencia1 ?diferencia4) (eq ?contador1 1))
        then
        ;En caso de que el gato 1 sea el que se encuentre más abajo se procede a realizar la comprobacion de hacia donde mover
        ;el gato, si la esquina izquierda tiene el valor de 1 el gato uno se moverá hacia la esquina izquierda caso contrario
        ;se moverá a la esquina derecha.

          (if (eq ?valor11 1)
                  then
                  (assert(fila-columna-mover-gatos ?fila-esquina-gato-1 ?columnna-esquina-izq-gato1 ?idGato1))

                  else
                  (assert(fila-columna-mover-gatos ?fila-esquina-gato-1 ?columnna-esquina-der-gato1 ?idGato1))

          )
          (assert(ejecutar-movimiento-maquina-gato))
      )

      ;Cuando se obtienen todas las diferencias se comprueba si la el gato 2 es el que se encuentra más abajo con respecto al raton.
      (if(and (>= ?diferencia2 ?diferencia1)(>= ?diferencia2 ?diferencia3)(>= ?diferencia2 ?diferencia4) (eq ?contador2 1))
        then
        ;En caso de que el gato 1 sea el que se encuentre más abajo se procede a realizar la comprobacion de hacia donde mover
        ;el gato, si la esquina izquierda tiene el valor de 1 el gato 2 se moverá hacia la esquina izquierda caso contrario
        ;se moverá a la esquina derecha.
          (if (eq ?valor12 1)
                  then
                  (assert(fila-columna-mover-gatos ?fila-esquina-gato-2 ?columnna-esquina-izq-gato2 ?idGato2))


                  else
                  (assert(fila-columna-mover-gatos ?fila-esquina-gato-2 ?columnna-esquina-der-gato2 ?idGato2))

          )
          (assert(ejecutar-movimiento-maquina-gato))


      )

      ;Cuando se obtienen todas las diferencias se comprueba si la el gato 3 es el que se encuentra más abajo con respecto al raton.
      (if(and (>= ?diferencia3 ?diferencia1)(>= ?diferencia3 ?diferencia2)(>= ?diferencia3 ?diferencia4) (eq ?contador3 1))
        then

        ;En caso de que el gato 3 sea el que se encuentre más abajo se procede a realizar la comprobacion de hacia donde mover
        ;el gato, si la esquina izquierda tiene el valor de 1 el gato 3 se moverá hacia la esquina izquierda caso contrario
        ;se moverá a la esquina derecha.
          (if (eq ?valor13 1)
                  then
                  (assert(fila-columna-mover-gatos ?fila-esquina-gato-3 ?columnna-esquina-izq-gato3 ?idGato3))

                  else
                  (assert(fila-columna-mover-gatos ?fila-esquina-gato-3 ?columnna-esquina-der-gato3 ?idGato3))

          )
          (assert(ejecutar-movimiento-maquina-gato))
      )

      ;Cuando se obtienen todas las diferencias se comprueba si la el gato 4 es el que se encuentra más abajo con respecto al raton.
      (if(and (>= ?diferencia4 ?diferencia1)(>= ?diferencia4 ?diferencia2)(>= ?diferencia4 ?diferencia3) (eq ?contador4 1))
        then
        ;En caso de que el gato 4 sea el que se encuentre más abajo se procede a realizar la comprobacion de hacia donde mover
        ;el gato, si la esquina izquierda tiene el valor de 1 el gato 4 se moverá hacia la esquina izquierda caso contrario
        ;se moverá a la esquina derecha.
          (if (eq ?valor14 1)
                  then
                  (assert(fila-columna-mover-gatos ?fila-esquina-gato-4 ?columnna-esquina-izq-gato4 ?idGato4))

                  else
                  (assert(fila-columna-mover-gatos ?fila-esquina-gato-4 ?columnna-esquina-der-gato4 ?idGato4))

          )
          (assert(ejecutar-movimiento-maquina-gato))
      )

      else
      ;Caso contrario de la comprobación de que existan dos o más coincidencias de la premisa del movimiento en bloque.
      ;Es decir el siguiente bloque se ejcutara cuando solo exista una sola coincidencia.

        ;Para determinar cual fue el gato que emparejo con la presima se hace uso del contador creado para cada gato,
        ;si el contador1 tiene un valor de 1 quiere decir que el gato numero 1 coincidio con la regla.
        ;Esta comprobración se la hará para cada gato.
        (if(eq ?contador1 1)then; para saber que gato fue que coincidio.
        ;si la esquina izquierda tiene el valor de 1 el gato 1 se moverá hacia la esquina izquierda caso contrario
        ;se moverá a la esquina derecha.
          (if (eq ?valor11 1)
                  then
                  (assert(fila-columna-mover-gatos ?fila-esquina-gato-1 ?columnna-esquina-izq-gato1 ?idGato1))

                  else
                  (assert(fila-columna-mover-gatos ?fila-esquina-gato-1 ?columnna-esquina-der-gato1 ?idGato1))

            )
            (assert(ejecutar-movimiento-maquina-gato))
          )

          (if(eq ?contador2 1)then; para saber que gato fue que coincidio.
            (if (eq ?valor12 1)
                    then
                    (assert(fila-columna-mover-gatos ?fila-esquina-gato-2 ?columnna-esquina-izq-gato2 ?idGato2))
                    else
                    (assert(fila-columna-mover-gatos ?fila-esquina-gato-2 ?columnna-esquina-der-gato2 ?idGato2))

              )
              (assert(ejecutar-movimiento-maquina-gato))
          )

          (if(eq ?contador3 1)
            then; para saber que gato fue que coincidio.
            (if (eq ?valor13 1)
                    then
                    (assert(fila-columna-mover-gatos ?fila-esquina-gato-3 ?columnna-esquina-izq-gato3 ?idGato3))
                    else
                    (assert(fila-columna-mover-gatos ?fila-esquina-gato-3 ?columnna-esquina-der-gato3 ?idGato3))

              )
              (assert(ejecutar-movimiento-maquina-gato))
          )

          (if(eq ?contador4 1)then; para saber que gato fue que coincidio.
            (if (eq ?valor14 1)
                    then
                    (assert(fila-columna-mover-gatos ?fila-esquina-gato-4 ?columnna-esquina-izq-gato4 ?idGato4))
                    else
                    (assert(fila-columna-mover-gatos ?fila-esquina-gato-4 ?columnna-esquina-der-gato4 ?idGato4))

              )
              (assert(ejecutar-movimiento-maquina-gato))
          )
    )

    ; Luego de ejecutar todas la condiciones, se insertan los hechos de control para buscar nuevamente las esquinas
    ;de los gatos.
    (assert(buscar-diagonales-gatos))

    ;se inserta el hecho de control de la regla que busca semiencerrar al raton. Esta regla tendra una jerarquia menor
    ;a la presente regla.
    (assert(semi-encerrar-raton))

    ;se inserta el hecho de control de la regla que busca mover al gato más alejado del raton. Esta regla tendra una jerarquia menor
    ;a la presente regla.
    (assert(gato-mas-alejado))
)


(defrule mover-gato-mas-alejado-raton
  "
    Regla que contien la lógica para determinar cuando se debe mover el gato más
    alejado del raton.
    En caso que existan más de dos columnas de diferecia entre algún raton y un gato
    el gato que cumpla la condicion se acercara hacia el raton.
    En caso de que existan dos o más gatos que tengan dos columnas de diferecia con el raton se moverá
    el que este más lejos, y en caso que esten a la misma distancia se moverá el gato más a la izquierda del tablero.
        _______________
    8 |_|#|_|#|_|#|_|#|
    7 |#|_|#|_|#|_|#|_|
    6 |_|#|_|#|_|#|_|#|
    5 |#|_|#|_|#|_|#|_|
    4 |_|#|_|#|_|#|_|#|
    3 |#|_|#|_|#|_|4|_|
    2 |_|5|_|5|_|5|_|5|
    1 |#|_|#|_|#|_|#|_|
       1 2 3 4 5 6 7 8

         _______________
     8 |_|#|_|#|_|#|_|#|
     7 |#|_|#|_|#|_|#|_|
     6 |_|#|_|#|_|#|_|#|
     5 |#|_|#|_|#|_|#|_|
     4 |_|#|_|#|_|#|_|#|
     3 |#|_|5|_|#|_|4|_|
     2 |_|#|_|5|_|5|_|5|
     1 |#|_|#|_|#|_|#|_|
        1 2 3 4 5 6 7 8

    Variables:

    ?hechoMoverGatoMasAlejado  -> Indice de hecho que contiene el hecho de activación de la presente regla.
    ?colRaton           -> Variable que contendrá la columna actual del raton
    ?gatoX              -> Variable que contendrá el indice de hecho de un determinado gato.
    ?filaGatoX          -> Variable que contendrá la fila de un determinado gato.
    ?colGatoX           -> Variable que contendrá la columna de un determinado gato.

    Assersiones :
    -> (assert(fila-columna-mover-gatos ?filaNueva ?colNueva ?gatoX))
    El hecho se compone por la fila y la columna a la cual se movera el gato, ademas se agrega el indice de hecho del gato que va a
    ejecutar este moviemiento.

    -> (assert(ejecutar-movimiento-maquina-gato))
    Hecho de control para ejecutar la modificacion del tablero con los nuevos valores de los gatos.

   "

   ;Declaracion de salience -10 ya que se pretende ejecutar esta regla cuando no empareje ninguna regla más.
   (declare (salience -10))

   ;condicion para deteminar si existe el hecho de control para ejecutar la presente regla.
   ?hechoMoverGatoMasAlejado <- (gato-mas-alejado)

   ;Condición para obtener la columna en la que se encuentra el raton.
   (casilla (fila ?)(columna ?colRaton)(valor 4))

   ;Condicion para obtener la posicon del gato 1.
   ?gato1 <- (casilla (fila ?filaGato1)(columna ?colGato1)(valor 5 1))

   ;Condicion para obtener la posicon del gato 2.
   ?gato2 <- (casilla (fila ?filaGato2)(columna ?colGato2)(valor 5 2))

   ;Condicion para obtener la posicon del gato 3.
   ?gato3 <- (casilla (fila ?filaGato3)(columna ?colGato3)(valor 5 3))

   ;Condicion para obtener la posicon del gato 4.
   ?gato4 <- (casilla (fila ?filaGato4)(columna ?colGato4)(valor 5 4))

   =>
   ;acción de eliminar el hecho de control que activa la presente regla.
   (retract ?hechoMoverGatoMasAlejado)

   ;Creación de variables auxiliares que contendrán la diferecia de todos los gatos con respecto
   ;a la columna del raton.
   (bind ?diferencia1 (abs (- ?colRaton ?colGato1)))
   (bind ?diferencia2 (abs (- ?colRaton ?colGato2)))
   (bind ?diferencia3 (abs (- ?colRaton ?colGato3)))
   (bind ?diferencia4 (abs (- ?colRaton ?colGato4)))

   ;Condicion para determinar si existe algun gato que este a más de dos columnas del raton.
   (if(or (> ?diferencia1 2)(> ?diferencia2 2)(> ?diferencia3 2)(> ?diferencia4 2))
      then
      ;En caso de que exista algun gato con una diferencia mayor a dos se procede a evaluar cual es
      ;el gato más alejado con respecto a la columna del gato.

      ;Evalua si el gato más alejado fue el gato 1.
      (if(and (> ?diferencia1 ?diferencia2)(> ?diferencia1 ?diferencia3)(> ?diferencia1 ?diferencia4))
      then
      ;en caso que el gato más alejado sea el gato 1 se calcula la fila hacia arriba del gato 1.
      (bind ?filaNueva (- ?filaGato1 1))
      ;Evalua en que parte del tablero se encuentra el raton en caso que se encuentre a partir de la
      ;columna 5 el gato se moverá hacia la derecha, en caso se encuentre en una columna menor a 5
      ;el gato se moverá hacia la izquierda.
        (if ( > ?colRaton 4)
          then
          (bind ?colNueva (+ ?colGato1 1))
          else
          (bind ?colNueva (- ?colGato1 1))
        )
        ;luego de calcular hacia donde se moverá el gato se insertan los datos en el hecho para ejecutar el movimiento.
        (assert(fila-columna-mover-gatos ?filaNueva ?colNueva ?gato1))
        (assert(ejecutar-movimiento-maquina-gato))

      )

      ;Evalua si el gato más alejado fue el gato 2.
      (if(and (> ?diferencia2 ?diferencia1)(> ?diferencia2 ?diferencia3)(> ?diferencia2 ?diferencia4))
      then
      (bind ?filaNueva (- ?filaGato2 1))

        (if ( > ?colRaton 4)
          then
          (bind ?colNueva (+ ?colGato2 1))
          else
          (bind ?colNueva (- ?colGato2 1))
        )
        (assert(fila-columna-mover-gatos ?filaNueva ?colNueva ?gato2))
        (assert(ejecutar-movimiento-maquina-gato))

      )

      ;Evalua si el gato más alejado fue el gato 3.
      (if(and (> ?diferencia3 ?diferencia1)(> ?diferencia3 ?diferencia2)(> ?diferencia3 ?diferencia4))
      then
      (bind ?filaNueva (- ?filaGato3 1))

        (if ( > ?colRaton 4)
          then
          (bind ?colNueva (+ ?colGato3 1))
          else
          (bind ?colNueva (- ?colGato3 1))
        )
        (assert(fila-columna-mover-gatos ?filaNueva ?colNueva ?gato3))
        (assert(ejecutar-movimiento-maquina-gato))

      )

      ;Evalua si el gato más alejado fue el gato 4.
      (if(and (> ?diferencia4 ?diferencia1)(> ?diferencia4 ?diferencia2)(> ?diferencia4 ?diferencia3))
      then
      (bind ?filaNueva (- ?filaGato4 1))

        (if ( > ?colRaton 4)
          then
          (bind ?colNueva (+ ?colGato4 1))
          else
          (bind ?colNueva (- ?colGato4 1))
        )
        (assert(fila-columna-mover-gatos ?filaNueva ?colNueva ?gato4))
        (assert(ejecutar-movimiento-maquina-gato))

      )

      else
      ;En caso de que no exista ningun gato con una distacia mayor a dos con respecto a la columna del raton
      ; se moverá el gato 4 hacia la derecha.
        (if (= ?diferencia1 ?diferencia4)
          then
          (bind ?fila-esquina(- ?filaGato4 1))
          (bind ?col-esquina-derecha(+ ?colGato4 1))
          (assert(fila-columna-mover-gatos ?fila-esquina ?col-esquina-derecha ?gato4))
          (assert(ejecutar-movimiento-maquina-gato))
        )


   )
)

(defrule semi-encerrar-raton-que-esta-en-medio
  "
    comprueba si un gato tiene en sus esquinas a un gato y al raton
    si existen dos de estas coincidencias se movera hacia el lado del tablero
    en el cual se encuentre el raton.

        _______________
    8 |_|#|_|#|_|#|_|#|
    7 |#|_|#|_|#|_|#|_|
    6 |_|#|_|#|_|#|_|#|
    5 |#|_|#|_|#|_|#|_|
    4 |_|#|_|#|_|#|_|#|
    3 |#|_|5|_|4|_|5|_|
    2 |_|#|_|5|_|5|_|#|
    1 |#|_|#|_|#|_|#|_|
       1 2 3 4 5 6 7 8

         _______________
     8 |_|#|_|#|_|#|_|#|
     7 |#|_|#|_|#|_|#|_|
     6 |_|#|_|#|_|#|_|#|
     5 |#|_|#|_|#|_|#|_|
     4 |_|#|_|5|_|#|_|#|
     3 |#|_|#|_|4|_|5|_|
     2 |_|#|_|5|_|5|_|#|
     1 |#|_|#|_|#|_|#|_|
        1 2 3 4 5 6 7 8


    Variables:

    ?h                  -> Indice de hecho que contiene el hecho de activación de la presente regla.
    ?filaRaton          -> Variable que contendrá la fila actual del raton
    ?colRaton           -> Variable que contendrá la columna actual del raton
    ?gatoX              -> Variable que contendrá el indice de hecho de un determinado gato.
    ?filaGatoX          -> Variable que contendrá la fila de un determinado gato.
    ?colGatoX           -> Variable que contendrá la columna de un determinado gato.

    Assersiones :
    -> (assert(fila-columna-mover-gatos ?fila-a-mover ?esquina-a-mover ?gato1))
    El hecho se compone por la fila y la columna a la cual se movera el gato, ademas se agrega el indice de hecho del gato que va a
    ejecutar este moviemiento.

    -> (assert(ejecutar-movimiento-maquina-gato))
    Hecho de control para ejecutar la modificacion del tablero con los nuevos valores de los gatos.

    "

    ;condicion para capturar el indice de hecho que activa la presente regla.
    ?h <- (semi-encerrar-raton)

    ;Condición para obtener la fila y la columna en la que se encuentra el raton.
    (casilla (fila ?filaRaton)(columna ?colRaton)(valor 4))

    ;Condicion para obtener la posicion del gato 1.
    ?gato1 <- (casilla (fila ?filaGato1)(columna ?colGato1)(valor 5 1))

    ;Condicion para obtener la posicion del gato 2.
    ?gato2 <- (casilla (fila ?filaGato2)(columna ?colGato2)(valor 5 2))

    ;Condicion para obtener la posicion del gato 3.
    ?gato3 <- (casilla (fila ?filaGato3)(columna ?colGato3)(valor 5 3))

    ;Condicion para obtener la posicion del gato 4.
    ?gato4 <- (casilla (fila ?filaGato4)(columna ?colGato4)(valor 5 4))


    ;Validación para determinar las filas de los diferentes gatos cumplen el patron que se busca.
    (test (or (and (= ?filaRaton ?filaGato1 ?filaGato4) (= (+ ?filaRaton 1) ?filaGato2 ?filaGato3))
              (and (= ?filaRaton ?filaGato2 ?filaGato4) (= (+ ?filaRaton 1) ?filaGato1 ?filaGato3))
          )
    )

    ;Validación para determinar las columnas de los diferentes gatos cumplen el patron que se busca.
    (test (or (and (= (+ ?colRaton 1) ?colGato3) (= (+ ?colRaton 2) ?colGato4) (= (- ?colRaton 1) ?colGato2)(= (- ?colRaton 2) ?colGato1))
              (and (= (+ ?colRaton 1) ?colGato3) (= (+ ?colRaton 2) ?colGato4) (= (- ?colRaton 1) ?colGato1)(= (- ?colRaton 2) ?colGato2))
          )
    )


    =>
    ;acción de eliminar el hecho de control que activa la presente regla.
    (retract ?h)

    ;condicion para determinar hacia que lado del tablero se encuntra el raton
    (assert(regladesemiencerrar))

    ;condicion para determinar que gato se moverá, puede ser el 1 el 2 o el 4.
    ;Esto debido a que en pruebas a mano se determino que gato 3 nunca cumplirá con el patro requerido.
    (if(> ?colRaton 4)
      then
      ;En caso que el gato se encuentre hacia la izquierda del tablero y que el gato  1 cumpla el patron
      ;se movera el gato uno, caso contrario se preguntara si el gato 2 cumple el patron.

      (if (= (- ?colRaton 2) ?colGato1)
        then
        ;En caso que el gato se encuentre hacia la izquierda del tablero y que el gato  1 cumpla el patron
        ;se movera el gato uno, caso contrario se preguntara si el gato 2 cumple el patron.
        (bind ?fila-a-mover (- ?filaGato1 1))
        (bind ?esquina-a-mover (+ ?colGato1 1))
        (assert(fila-columna-mover-gatos ?fila-a-mover ?esquina-a-mover ?gato1))
        else
        (bind ?fila-a-mover (- ?filaGato2 1))
        (bind ?esquina-a-mover (+ ?colGato2 1))
        (assert(fila-columna-mover-gatos ?fila-a-mover ?esquina-a-mover ?gato2))
      )
      ;Si el gato se encuntra en la parte del tablero el gato que se movera sera el 4.
      else
      (bind ?esquina-a-mover (- ?colGato4 1))
      (assert(fila-columna-mover-gatos ?fila-a-mover ?esquina-a-mover ?gato4))

    )
    (assert(ejecutar-movimiento-maquina-gato))
)


(defrule calcular-esquinas-raton

    "
    Regla para añadir un hecho que contiene las cuatro esquinas del raton. El hecho contendrá la fila superior
    y las dos columnas adyacentes. Las fila inferior y sus dos columnas adyacentes.

    Variables:
    ?h                  -> Indice de hecho para la activacion de la presente regla.
    ?filaRaton          -> Variable para capturar la fila donde se encuentra el raton.
    ?colRaton           -> Variable para capturar la columna donde se encuentra el raton.

    Asersiones :
    (assert(esquinasRaton ?filaRaton ?fila-superior ?columnaIzq ?columnaDer))
    -> El hecho se compone por un idGato que luego servira para obtener el valor y la posición del gato en cuestión.
    además se inserta la fila superior y la columna izquierda y derecha de el gato.

    (assert (cubrirPosibleAvanceRaton))
    ->Hecho de control para activar la regla mencionada.

    (assert(encerrar-raton))
    ->Hecho de control para activar la regla mencionada.
    "
    ;Condicion para deteminar si existe el hecho de control que activa la presente regla.
    ?h <- (calcular-esquinas-raton)

    ;Condicion para obtener la posicion del ratón.
    (casilla (fila ?filaRaton)(columna ?colRaton)(valor 4))
  =>

  ;Creacion de variables auxiliares para calcular la posicion de las 4 esquinas del raton.
  (bind ?fila-inferior  (- ?filaRaton 1))
  (bind ?fila-superior (+ ?filaRaton 1))
  (bind ?columnaIzq (- ?colRaton 1))
  (bind ?columnaDer (+ ?colRaton 1))

  ;Condicion para determinar el el gato se encuentra en la esquina derecha del tablero
  ;En caso que lo este solo tendrá una casilla como esquina.
  (if (and (= ?filaRaton 1)(= ?colRaton 8) )
    then
      (assert(esquinasRaton ?filaRaton ?fila-superior ?columnaIzq ?colRaton))

    else
    ;Condicion para determinar si el raton se encuentra en la fila 1 del tablero
    ;En caso que lo este solo tendrá dos casillas, superior izquierda, superior derecha.
    (if (= ?filaRaton 1)
      then
        (assert(esquinasRaton ?filaRaton ?fila-superior ?columnaIzq ?columnaDer))
    )
    ;Condicion para determinar si el raton se encuentra en la columna 8 del tablero
    ;En caso que lo este solo tendrá dos casillas, inferior izquierda, superior derecha
    (if (= ?colRaton 8)
      then
        (assert(esquinasRaton ?fila-inferior ?fila-superior ?columnaIzq ?colRaton))
    )
    ;Condicion para determinar si el raton se encuentra en la columna 1 del tablero
    ;En caso que lo este solo tendrá dos casillas, inferior izquierda, superior izquierda
    (if (= ?colRaton 1)
      then
        (assert(esquinasRaton ?fila-inferior ?fila-superior ?colRaton ?columnaDer))
    )
    ;Condicion para determinar si el raton se encuentra en la parte central del tablero
    ;En este caso poseera las 4 esquinas.
    (if (and (> ?filaRaton 1) (> ?colRaton 1) (< ?colRaton 8))
      then
      (assert(esquinasRaton ?fila-inferior ?fila-superior ?columnaIzq ?columnaDer))

    )
  )

  ;una vez que se comprueben todas las condiciones se insertan los hechos de control de Las
  ;siguientes reglas en pila.
  (assert (cubrirPosibleAvanceRaton))
  (assert(encerrar-raton))

)

(defrule encerrar-raton
  "
    Comprueba si el raton solo tiene una casilla disponible para moverse, en caso de cumplirse
    esto, busca un gato que pueda llegar a esta posicion.

    Variables :
    ?fila-inferior       -> Variable que contiene la fila inferior del raton.
    ?fila-superior       -> Variable que contiene la fila superior del raton.
    ?columnaIzq          -> Variable que contiene la columna izquierda del raton.
    ?columnaDer          -> Variable que contiene la columna derecha del raton.

    Asersiones:
    ->(assert(fila-columna-mover-gatos ?fila-inferior ?esquinaDerGatoX ?gatoX))
    El hecho se compone por la fila y la columna a la cual se movera el gato, ademas se agrega el indice de hecho del gato que va a
    ejecutar este moviemiento.

    ->(assert(ejecutar-movimiento-maquina-gato))
    Hecho de control para ejecutar la modificacion del tablero con los nuevos valores de los gatos.

    ->(assert(finalizar-juego))
    Hecho de control para activar la regla correspondiente.
  "
  ;Declarion de el salience mas alto del sistema debido a que esta será la primer regla de moviemiento que se ejucutará
  (declare (salience 10))

  ;hecho de control para activar la presente regla.
  (encerrar-raton)

  ;Condicion para obtener el indice de hecho de la esquina del raton para su posterior eliminación,
  ;asimismo se captura la fila y las 4 esquinas del raton.
  ?h <- (esquinasRaton ?fila-inferior ?fila-superior ?columnaIzq ?columnaDer)

  ;Condicion para obtener el valor de la esquina inferior izquierda del raton.
  (casilla (fila ?fila-inferior)(columna ?columnaIzq)(valor ?valorInferiorIzq $?))

  ;Condicion para obtener el valor de la esquina inferior derecha del raton.
  (casilla (fila ?fila-inferior)(columna ?columnaDer)(valor ?valorInferiorDer $?))

  ;Condicion para obtener el valor de la esquina superior izquierda del raton.
  (casilla (fila ?fila-superior)(columna ?columnaIzq)(valor ?valorSuperiorIzq $?))

  ;Condicion para obtener el valor de la esquina superior derecha del raton.
  (casilla (fila ?fila-superior)(columna ?columnaDer)(valor ?valorSuperiorDer $?))


  ;condiciones para obtener las posiciones de todos los gatos.
  ?gato1 <- (casilla (fila ?filaGato1)(columna ?colGato1)(valor 5 1))
  ?gato2 <- (casilla (fila ?filaGato2)(columna ?colGato2)(valor 5 2))
  ?gato3 <- (casilla (fila ?filaGato3)(columna ?colGato3)(valor 5 3))
  ?gato4 <- (casilla (fila ?filaGato4)(columna ?colGato4)(valor 5 4))
    =>
    ;Se elimina el hecho que contiene la esquina del raton.
    (retract ?h)


    ;Variables auxiliares que contendran la posicion de las filas del gato 1
    (bind ?filaGato1 (- ?filaGato1 1))
    (bind ?esquinaIzqGato1 (- ?colGato1 1))
    (bind ?esquinaDerGato1 (+ ?colGato1 1))

    ;Variables auxiliares que contendran la posicion de las filas del gato 2
    (bind ?filaGato2 (- ?filaGato2 1))
    (bind ?esquinaIzqGato2 (- ?colGato2 1))
    (bind ?esquinaDerGato2 (+ ?colGato2 1))

    ;Variables auxiliares que contendran la posicion de las filas del gato 3
    (bind ?filaGato3 (- ?filaGato3 1))
    (bind ?esquinaIzqGato3 (- ?colGato3 1))
    (bind ?esquinaDerGato3 (+ ?colGato3 1))

    ;Variables auxiliares que contendran la posicion de las filas del gato 4
    (bind ?filaGato4 (- ?filaGato4 1))
    (bind ?esquinaIzqGato4 (- ?colGato4 1))
    (bind ?esquinaDerGato4 (+ ?colGato4 1))

    ;Condicion para validar que la unica esquina que el gato tiene disponible es la inferior izquierda.
    (if (and (= ?valorInferiorIzq 1) (<> ?valorInferiorDer 1)(<> ?valorSuperiorIzq 1)(<> ?valorSuperiorDer 1))
      then

          ;Condicion para comprobar que el gato 1 tiene una esquina en la esquina inferior izquierda del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
          (if (and (= ?fila-inferior ?filaGato1) (= ?columnaIzq ?esquinaDerGato1))
            then

            (assert(fila-columna-mover-gatos ?fila-inferior ?esquinaDerGato1 ?gato1))
            (assert(ejecutar-movimiento-maquina-gato))
            (assert(finalizar-juego))

          )

          ;Condicion para comprobar que el gato 2 tiene una esquina en la esquina inferior izquierda del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
          (if (and (= ?fila-inferior ?filaGato2) (= ?columnaIzq ?esquinaDerGato2))
            then

            (assert(fila-columna-mover-gatos ?fila-inferior ?esquinaDerGato2 ?gato2))
            (assert(ejecutar-movimiento-maquina-gato))

            (assert(finalizar-juego))
          )

          ;Condicion para comprobar que el gato 3 tiene una esquina en la esquina inferior izquierda del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
          (if (and (= ?fila-inferior ?filaGato3) (= ?columnaIzq ?esquinaDerGato3))
            then

            (assert(fila-columna-mover-gatos ?fila-inferior ?esquinaDerGato3 ?gato3))
            (assert(ejecutar-movimiento-maquina-gato))

            (assert(finalizar-juego))

          )

          ;Condicion para comprobar que el gato 4 tiene una esquina en la esquina inferior izquierda del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
          (if (and (= ?fila-inferior ?filaGato3) (= ?columnaIzq ?esquinaDerGato3))
            then
            (assert(fila-columna-mover-gatos ?fila-inferior ?esquinaDerGato3 ?gato3))
            (assert(ejecutar-movimiento-maquina-gato))

            (assert(finalizar-juego))

          )
    )


    ;Condicion para validar que la unica esquina que el gato tiene disponible es la inferior derecha.
    (if (and (= ?valorInferiorDer 1) (<> ?valorInferiorIzq 1)(<> ?valorSuperiorIzq 1)(<> ?valorSuperiorDer 1))
      then

          ;Condicion para comprobar que el gato 1 tiene una esquina en la esquina inferior derecha del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
          (if (and (= ?fila-inferior ?filaGato1) (= ?columnaDer ?esquinaIzqGato1))
            then

            (assert(fila-columna-mover-gatos ?fila-inferior ?esquinaIzqGato1 ?gato1))
            (assert(ejecutar-movimiento-maquina-gato))

            (assert(finalizar-juego))

          )

          ;Condicion para comprobar que el gato 2 tiene una esquina en la esquina inferior derecha del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
          (if (and (= ?fila-inferior ?filaGato2) (= ?columnaDer ?esquinaIzqGato2))
            then
            (assert (buscar-diagonales-gatos))

            (assert(fila-columna-mover-gatos ?fila-inferior ?esquinaIzqGato2 ?gato2))
            (assert(ejecutar-movimiento-maquina-gato))

            (assert(finalizar-juego))

          )

          ;Condicion para comprobar que el gato 3 tiene una esquina en la esquina inferior derecha del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
          (if (and (= ?fila-inferior ?filaGato3) (= ?columnaDer ?esquinaIzqGato3))
            then

            (assert(fila-columna-mover-gatos ?fila-inferior ?esquinaIzqGato3 ?gato3))
            (assert(ejecutar-movimiento-maquina-gato))

            (assert(finalizar-juego))

          )

          ;Condicion para comprobar que el gato 4 tiene una esquina en la esquina inferior derecha del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
          (if (and (= ?fila-inferior ?filaGato4) (= ?columnaDer ?esquinaIzqGato4))
            then

            (assert(fila-columna-mover-gatos ?fila-inferior ?esquinaIzqGato4 ?gato4))
            (assert(ejecutar-movimiento-maquina-gato))

            (assert(finalizar-juego))

          )
    )

    ;Condicion para validar que la unica esquina que el gato tiene disponible es la superior izquierda.
    (if (and (= ?valorSuperiorIzq 1) (<> ?valorInferiorDer 1)(<> ?valorInferiorIzq 1)(<> ?valorSuperiorDer 1))
      then

          ;Condicion para comprobar que el gato 1 tiene una esquina derecha en la esquina superior izquierda del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
          (if (and (= ?fila-superior ?filaGato1) (= ?columnaIzq ?esquinaDerGato1))
            then

            (assert(fila-columna-mover-gatos ?fila-superior ?esquinaDerGato1 ?gato1))
            (assert(ejecutar-movimiento-maquina-gato))
            (assert(finalizar-juego))

            else
            ;Condicion para comprobar que el gato 1 tiene una esquina izquierda en la esquina superior izquierda del raton.
            ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
            ;para mostrar el mensaje de juego finalizado.
                (if (and (= ?fila-superior ?filaGato1) (= ?columnaIzq ?esquinaIzqGato1))
                  then
                  (assert(fila-columna-mover-gatos ?fila-superior ?esquinaIzqGato1 ?gato1))
                  (assert(ejecutar-movimiento-maquina-gato))
                  (assert(finalizar-juego))
                )

          )

          (if (and (= ?fila-superior ?filaGato2) (= ?columnaIzq ?esquinaDerGato2))
            then
            ;Condicion para comprobar que el gato 2 tiene una esquina derecha en la esquina superior izquierda del raton.
            ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
            ;para mostrar el mensaje de juego finalizado.
            (assert(fila-columna-mover-gatos ?fila-superior ?esquinaDerGato2 ?gato2))
            (assert(ejecutar-movimiento-maquina-gato))
            (assert(finalizar-juego))

            else
            ;Condicion para comprobar que el gato 2 tiene una esquina izquierda en la esquina superior izquierda del raton.
            ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
            ;para mostrar el mensaje de juego finalizado.
                (if (and (= ?fila-superior ?filaGato2) (= ?columnaIzq ?esquinaIzqGato2))
                  then
                  (assert(fila-columna-mover-gatos ?fila-superior ?esquinaIzqGato2 ?gato2))
                  (assert(ejecutar-movimiento-maquina-gato))
                  (assert(finalizar-juego))
                )
          )

          (if (and (= ?fila-superior ?filaGato3) (= ?columnaIzq ?esquinaDerGato3))
          ;Condicion para comprobar que el gato 3 tiene una esquina derecha en la esquina superior izquierda del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
            then
            (assert(fila-columna-mover-gatos ?fila-superior ?esquinaDerGato3 ?gato3))
            (assert(ejecutar-movimiento-maquina-gato))
            (assert(finalizar-juego))

            else
            ;Condicion para comprobar que el gato 3 tiene una esquina izquierda en la esquina superior izquierda del raton.
            ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
            ;para mostrar el mensaje de juego finalizado.
                (if (and (= ?fila-superior ?filaGato3) (= ?columnaIzq ?esquinaIzqGato3))
                  then
                  (assert(fila-columna-mover-gatos ?fila-superior ?esquinaIzqGato3 ?gato3))
                  (assert(ejecutar-movimiento-maquina-gato))
                  (assert(finalizar-juego))
                )

          )

          ;Condicion para comprobar que el gato 4 tiene una esquina derecha en la esquina superior izquierda del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
          (if (and (= ?fila-superior ?filaGato4) (= ?columnaIzq ?esquinaDerGato4))
            then

            (assert(fila-columna-mover-gatos ?fila-superior ?esquinaDerGato3 ?gato4))
            (assert(ejecutar-movimiento-maquina-gato))
            (assert(finalizar-juego))
            else
            ;Condicion para comprobar que el gato 1 tiene una esquina izquierda en la esquina superior izquierda del raton.
            ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
            ;para mostrar el mensaje de juego finalizado.
                (if (and (= ?fila-superior ?filaGato4) (= ?columnaIzq ?esquinaIzqGato4))
                  then
                  (assert(fila-columna-mover-gatos ?fila-superior ?esquinaIzqGato4 ?gato4))
                  (assert(ejecutar-movimiento-maquina-gato))
                  (assert(finalizar-juego))
                )

          )
    )


    ;Condicion para validar que la unica esquina que el gato tiene disponible es la superior derecha.
    (if (and (= ?valorSuperiorDer 1) (<> ?valorInferiorDer 1)(<> ?valorInferiorIzq 1)(<> ?valorSuperiorIzq 1))
      then

          ;Condicion para comprobar que el gato 1 tiene una esquina izquierda en la esquina superior derecha del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
          (if (and (= ?fila-superior ?filaGato1) (= ?columnaDer ?esquinaDerGato1))
            then

            (assert(fila-columna-mover-gatos ?fila-superior ?esquinaDerGato1 ?gato1))
            (assert(ejecutar-movimiento-maquina-gato))
            (assert(finalizar-juego))

            else
            ;Condicion para comprobar que el gato 1 tiene una esquina izquierda en la esquina superior derecha del raton.
            ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
            ;para mostrar el mensaje de juego finalizado.
                (if (and (= ?fila-superior ?filaGato1) (= ?columnaDer ?esquinaIzqGato1))
                  then
                  (assert(fila-columna-mover-gatos ?fila-superior ?esquinaIzqGato1 ?gato1))
                  (assert(ejecutar-movimiento-maquina-gato))
                  (assert(finalizar-juego))
                )
          )

          ;Condicion para comprobar que el gato 2 tiene una esquina izquierda en la esquina superior derecha del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
          (if (and (= ?fila-superior ?filaGato2) (= ?columnaDer ?esquinaDerGato2))
            then

            (assert(fila-columna-mover-gatos ?fila-superior ?esquinaDerGato2 ?gato2))
            (assert(ejecutar-movimiento-maquina-gato))
            (assert(finalizar-juego))

            else
            ;Condicion para comprobar que el gato 2 tiene una esquina izquierda en la esquina superior derecha del raton.
            ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
            ;para mostrar el mensaje de juego finalizado.
                (if (and (= ?fila-superior ?filaGato2) (= ?columnaDer ?esquinaIzqGato2))
                  then
                  (assert(fila-columna-mover-gatos ?fila-superior ?esquinaIzqGato2 ?gato2))
                  (assert(ejecutar-movimiento-maquina-gato))
                  (assert(finalizar-juego))
                )
          )

          ;Condicion para comprobar que el gato 3 tiene una esquina izquierda en la esquina superior derecha del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
          (if (and (= ?fila-superior ?filaGato3) (= ?columnaDer ?esquinaDerGato3))
            then

            (assert(fila-columna-mover-gatos ?fila-superior ?esquinaDerGato3 ?gato3))
            (assert(ejecutar-movimiento-maquina-gato))
            (assert(finalizar-juego))

            else
            ;Condicion para comprobar que el gato 3 tiene una esquina izquierda en la esquina superior derecha del raton.
            ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
            ;para mostrar el mensaje de juego finalizado.
                (if (and (= ?fila-superior ?filaGato3) (= ?columnaDer ?esquinaIzqGato3))
                  then
                  (assert(fila-columna-mover-gatos ?fila-superior ?esquinaIzqGato3 ?gato3))
                  (assert(ejecutar-movimiento-maquina-gato))
                  (assert(finalizar-juego))
                )
          )

          ;Condicion para comprobar que el gato 4 tiene una esquina izquierda en la esquina superior derecha del raton.
          ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
          ;para mostrar el mensaje de juego finalizado.
          (if (and (= ?fila-superior ?filaGato4) (= ?columnaDer ?esquinaDerGato4))
            then

            (assert(fila-columna-mover-gatos ?fila-superior ?esquinaDerGato4 ?gato4))
            (assert(ejecutar-movimiento-maquina-gato))
            (assert(finalizar-juego))

            else
            ;Condicion para comprobar que el gato 4 tiene una esquina izquierda en la esquina superior derecha del raton.
            ;En caso de cumplirirse realiza las acerciones de movimiento del gato, y ademas inserta el hecho
            ;para mostrar el mensaje de juego finalizado.
                (if (and (= ?fila-superior ?filaGato4) (= ?columnaDer ?esquinaIzqGato4))
                  then
                  (assert(fila-columna-mover-gatos ?fila-superior ?esquinaIzqGato4 ?gato4))
                  (assert(ejecutar-movimiento-maquina-gato))
                  (assert(finalizar-juego))
                )
          )
    )


)

(defrule cubrir-posible-avance-raton
  "
    Regla que contiene la logica de moviemiento de diferentes patrones o movimientos especiales
    que deben ser ejecutados por determinados gatos.

    Variables :
    ?h                   -> Variable que contiene el hecho de control para activar la presente regla.
    ?filaRaton           -> Variable que contiene la fila actual del raton.
    ?colRaton            -> Variable que contiene la columna actual del raton.
    ?filaGatoX           -> Variable que contiene la fila actual de un determinado gato.
    ?colGatoX            -> Variable que contiene la columna actual un determinado gato


    Asersiones:

    ->(assert (buscar-diagonales-gatos))
    Hecho de control para actualizar las esquinas de los gatos.

    ->(assert(fila-columna-mover-gatos ?fila-esquina ?col-esquina-derecha ?gatoX))

    El hecho se compone por la fila y la columna a la cual se movera el gato, ademas se agrega el indice de hecho del gato que va a
    ejecutar este moviemiento.

    ->(assert(ejecutar-movimiento-maquina-gato))
    Hecho de control para ejecutar la modificacion del tablero con los nuevos valores de los gatos.

  "

   ;Indice de hecho para activar la presente regla.
    ?h <-(cubrirPosibleAvanceRaton)
    ;Condicion para capturar la posicion del raton
    ?raton <- (casilla (fila ?filaRaton)(columna ?colRaton)(valor 4))

    ;Condiciones para obtener la posicion de cada uno de los gatos
    ?gato1 <- (casilla (fila ?filaGato1)(columna ?colGato1)(valor 5 1))
    ?gato2 <- (casilla (fila ?filaGato2)(columna ?colGato2)(valor 5 2))
    ?gato3 <- (casilla (fila ?filaGato3)(columna ?colGato3)(valor 5 3))
    ?gato4 <- (casilla (fila ?filaGato4)(columna ?colGato4)(valor 5 4))


  =>
  (assert (buscar-diagonales-gatos))


  ;PATRON DE PLANO DE TERCERA DIMESION HACIA LA DERECHA.
      ;  _______________
    ; 8 |_|#|_|#|_|#|_|#|
    ; 7 |#|_|#|_|#|_|#|_|
    ; 6 |_|#|_|4|_|#|_|#|
    ; 5 |#|_|#|_|#|_|#|_|
    ; 4 |_|#|_|5|_|5|_|#|
    ; 3 |#|_|#|_|5|_|#|_|
    ; 2 |_|5|_|#|_|#|_|#|
    ; 1 |#|_|#|_|#|_|#|_|
    ;    1 2 3 4 5 6 7 8

    ;En caso que las condiciones favorezcan a este patron se comprueba que gato
    ;esta al frente del raton, solo puede ser el 1 o el 2, en cualquier caso el gato
    ;se moverá hacia la esquina izquierda.
    ;  _______________
    ; 8 |_|#|_|#|_|#|_|#|
    ; 7 |#|_|#|_|#|_|#|_|
    ; 6 |_|#|_|4|_|#|_|#|
    ; 5 |#|_|5|_|#|_|#|_|
    ; 4 |_|#|_|#|_|5|_|#|
    ; 3 |#|_|#|_|5|_|#|_|
    ; 2 |_|5|_|#|_|#|_|#|
    ; 1 |#|_|#|_|#|_|#|_|
    ;    1 2 3 4 5 6 7 8

    ;condiciones para comprobar si existe el patron antes mencionado, ademas se verifica
    ;si el gato 1 es es que se encuentra al frente del raton.
  (if (and (= (+ ?filaRaton 2) ?filaGato1)(= ?colGato1 ?colRaton) )
    then

    (if (or (= (+ ?colGato1 2) ?colGato2)(= (+ ?colGato1 2) ?colGato3)(= (+ ?colGato1 2) ?colGato4))
      then

      (if (or  (and (= (+ ?filaGato1 1) ?filaGato2) (= (+ ?colGato1 1) ?colGato2))
               (and (= (+ ?filaGato1 1) ?filaGato3) (= (+ ?colGato1 1) ?colGato3))
               (and (= (+ ?filaGato1 1) ?filaGato4) (= (+ ?colGato1 1) ?colGato4))
          )
        then
        (printout t crlf "lul")

          (bind ?fila-esquina(- ?filaGato1 1))
          (bind ?col-esquina-derecha(- ?colGato1 1))
          (assert(fila-columna-mover-gatos ?fila-esquina ?col-esquina-derecha ?gato1))
          (assert(ejecutar-movimiento-maquina-gato))

      )
    )

  )

  ;;condiciones para comprobar si existe el patron antes mencionado, ademas se verifica
  ;si el gato 2 es es que se encuentra al frente del raton.
  (if (and (= (+ ?filaRaton 2) ?filaGato2)(= ?colGato2 ?colRaton) )
    then

    (if (or (= (+ ?colGato2 2) ?colGato3)(= (+ ?colGato2 2) ?colGato1)(= (+ ?colGato2 2) ?colGato4))
      then

      (if (or  (and (= (+ ?filaGato2 1) ?filaGato1) (= (+ ?colGato2 1) ?colGato1))
               (and (= (+ ?filaGato2 1) ?filaGato3) (= (+ ?colGato2 1) ?colGato3))
               (and (= (+ ?filaGato2 1) ?filaGato4) (= (+ ?colGato2 1) ?colGato4))
          )
        then
          (bind ?fila-esquina(- ?filaGato2 1))
          (bind ?col-esquina-derecha(- ?colGato2 1))
          (assert(fila-columna-mover-gatos ?fila-esquina ?col-esquina-derecha ?gato2))
          (assert(ejecutar-movimiento-maquina-gato))

      )
    )

  )

  ;PATRON DE TRES EN FILA UNO ABAJO.
      ;  _______________
    ; 8 |_|#|_|#|_|#|_|#|
    ; 7 |#|_|#|_|#|_|#|_|
    ; 6 |_|#|_|4|_|#|_|#|
    ; 5 |#|_|#|_|#|_|#|_|
    ; 4 |_|5|_|5|_|5|_|#|
    ; 3 |#|_|#|_|5|_|#|_|
    ; 2 |_|#|_|#|_|#|_|#|
    ; 1 |#|_|#|_|#|_|#|_|
    ;    1 2 3 4 5 6 7 8

    ;En caso que las condiciones favorezcan a este patron se comprueba que gato
    ;esta al frente del raton para la posterior comprabación de cual es el gato de la derecha
    ;el cual solo puede ser el 3 o el 4.
    ;  _______________
    ; 8 |_|#|_|#|_|#|_|#|
    ; 7 |#|_|#|_|#|_|#|_|
    ; 6 |_|#|_|4|_|#|_|#|
    ; 5 |#|_|#|_|#|_|5|_|
    ; 4 |_|5|_|5|_|#|_|#|
    ; 3 |#|_|#|_|5|_|#|_|
    ; 2 |_|#|_|#|_|#|_|#|
    ; 1 |#|_|#|_|#|_|#|_|
    ;    1 2 3 4 5 6 7 8


    ;condiciones para comprobar si existe el patron antes mencionado
  (if (or (and (= (+ ?filaRaton 2) ?filaGato2)(= ?colGato2 ?colRaton) )
          (and (= (+ ?filaRaton 2) ?filaGato1)(= ?colGato1 ?colRaton) ))
    then

    (if (and(or (= (+ ?colRaton 2) ?colGato4) (= (+ ?colRaton 2) ?colGato3))
            (or (= (- ?colRaton 2) ?colGato1) (= (- ?colRaton 2) ?colGato2))
            (or (= (+ ?colRaton 1) ?colGato3) (= (+ ?colRaton 1) ?colGato4)))
      then
      ;Condicion para deteminar si el gato 4 es el gato más a la derecha.
      (if (and (= ?filaGato2 ?filaGato1) (= ?filaGato2 ?filaGato4)(= (+ ?filaGato2 1) ?filaGato3))
        then

          (bind ?fila-esquina(- ?filaGato4 1))
          (bind ?col-esquina-derecha(+ ?colGato4 1))
          (assert(fila-columna-mover-gatos ?fila-esquina ?col-esquina-derecha ?gato4))
          (assert(ejecutar-movimiento-maquina-gato))
        else
        ;Condicion para deteminar si el gato 3 es el gato más a la derecha.
        (if (and (= ?filaGato2 ?filaGato1) (= ?filaGato2 ?filaGato3)(= (+ ?filaGato2 1) ?filaGato4))
          then
          (printout t crlf "siu")

            (bind ?fila-esquina(- ?filaGato3 1))
            (bind ?col-esquina-derecha(+ ?colGato3 1))
            (assert(fila-columna-mover-gatos ?fila-esquina ?col-esquina-derecha ?gato3))
            (assert(ejecutar-movimiento-maquina-gato))

        )

      )
    )
  )

  ;PATRON DE PLANO DE TERCERA DIMESION HACIA LA IZQUIERDA.
     ;  _______________
   ; 8 |_|#|_|#|_|#|_|#|
   ; 7 |#|_|#|_|#|_|#|_|
   ; 6 |_|#|_|#|_|4|_|#|
   ; 5 |#|_|#|_|#|_|#|_|
   ; 4 |_|5|_|5|_|5|_|#|
   ; 3 |#|_|#|_|5|_|#|_|
   ; 2 |_|#|_|#|_|#|_|#|
   ; 1 |#|_|#|_|#|_|#|_|
   ;    1 2 3 4 5 6 7 8

   ;En caso que las condiciones favorezcan a este patron se comprueba que gato
   ;es el que se encuentra a la derecha del patron. Este gato se moverá hacia la esquina derecha.
     ;  _______________
   ; 8 |_|#|_|#|_|#|_|#|
   ; 7 |#|_|#|_|#|_|#|_|
   ; 6 |_|#|_|#|_|4|_|#|
   ; 5 |#|_|#|_|#|_|5|_|
   ; 4 |_|5|_|5|_|#|_|#|
   ; 3 |#|_|#|_|5|_|#|_|
   ; 2 |_|#|_|#|_|#|_|#|
   ; 1 |#|_|#|_|#|_|#|_|
   ;    1 2 3 4 5 6 7 8

   ;condiciones para comprobar si existe el patron antes mencionado, ademas se verifica
   ;si el gato 4 es es que se encuentra a la derecha.

  (if (and (= (+ ?filaRaton 2) ?filaGato4)(= ?colGato4 ?colRaton) )
    then
    (if (or (= (- ?colGato4 2) ?colGato2)(= (- ?colGato4 2) ?colGato3)(= (- ?colGato4 2) ?colGato1))
      then

      (if (or  (and (= (+ ?filaGato4 1) ?filaGato2) (= (- ?colGato4 1) ?colGato2))
               (and (= (+ ?filaGato4 1) ?filaGato3) (= (- ?colGato4 1) ?colGato3))
               (and (= (+ ?filaGato4 1) ?filaGato1) (= (- ?colGato4 1) ?colGato1))
          )
        then
          (bind ?fila-esquina(- ?filaGato4 1))
          (bind ?col-esquina-derecha(+ ?colGato4 1))
          (printout t crlf "bro")

          (assert(fila-columna-mover-gatos ?fila-esquina ?col-esquina-derecha ?gato4))
          (assert(ejecutar-movimiento-maquina-gato))

      )
    )

  )

  ;condiciones para comprobar si existe el patron antes mencionado, ademas se verifica
  ;si el gato 3 es es que se encuentra a la derecha.
  (if (and (= (+ ?filaRaton 2) ?filaGato3)(= ?colGato3 ?colRaton) )
    then
    (if (or (= (- ?colGato3 2) ?colGato2)(= (- ?colGato3 2) ?colGato4)(= (- ?colGato3 2) ?colGato1))
      then

      (if (or  (and (= (+ ?filaGato3 1) ?filaGato2) (= (- ?colGato3 1) ?colGato2))
               (and (= (+ ?filaGato3 1) ?filaGato4) (= (- ?colGato3 1) ?colGato4))
               (and (= (+ ?filaGato3 1) ?filaGato1) (= (- ?colGato3 1) ?colGato1))
          )
        then
          (bind ?fila-esquina(- ?filaGato3 1))
          (bind ?col-esquina-derecha(+ ?colGato3 1))

          (assert(fila-columna-mover-gatos ?fila-esquina ?col-esquina-derecha ?gato3))
          (assert(ejecutar-movimiento-maquina-gato))

      )
    )

  )

  ;PATRON SERPIENTE
      ;  _______________
    ; 1 |_|#|_|#|_|#|_|#|
    ; 2 |#|_|#|_|#|_|#|_|
    ; 3 |_|#|_|#|_|#|_|#|
    ; 4 |#|_|#|_|4|_|#|_|
    ; 5 |_|#|_|#|_|5|_|#|
    ; 6 |#|_|5|_|5|_|#|_|
    ; 7 |_|5|_|#|_|#|_|#|
    ; 8 |#|_|#|_|#|_|#|_|
    ;    1 2 3 4 5 6 7 8

    ;En caso que las condiciones favorezcan a este patron se comprueba que gato
    ;esta al lado izquierdo del gato que esta al frente, el gato que se moverá solo
    ;podra ser el 1 o el 2.
      ;  _______________
    ; 1 |_|#|_|#|_|#|_|#|
    ; 2 |#|_|#|_|#|_|#|_|
    ; 3 |_|#|_|#|_|#|_|#|
    ; 4 |#|_|#|_|4|_|#|_|
    ; 5 |_|#|_|5|_|5|_|#|
    ; 6 |#|_|#|_|5|_|#|_|
    ; 7 |_|5|_|#|_|#|_|#|
    ; 8 |#|_|#|_|#|_|#|_|
    ;    1 2 3 4 5 6 7 8

  (bind ?filaGatoFrente (+ 0  0))
  (bind ?colGatoFrente (+ 0 0))
  ;Condicion para determinar cual es el gato que se encuentra el frente del raton
  ;solo puede ser el 3 o el 4.
  (if (and (= (+ ?filaRaton 2) ?filaGato3)(= ?colGato3 ?colRaton) )
    then
        (bind ?filaGatoFrente (+ ?filaGato3 0))
        (bind ?colGatoFrente (+ ?colGato3 0))
      else
      (if (and (= (+ ?filaRaton 2) ?filaGato4)(= ?colGato4 ?colRaton) )
        then

        (bind ?filaGatoFrente (+ ?filaGato4 0))
        (bind ?colGatoFrente (+ ?colGato4 0))
      )
    )


    ;Condicion para determinar si se cumple el patron de la serpiente, se verifica cada
    ;una de  las posiciones del patron
    (if (or (= (- ?colGatoFrente 2) ?colGato2)(= (- ?colGatoFrente 2) ?colGato1))
      then
        (if (or (and (= (- ?filaGatoFrente 1) ?filaGato4) (= (+ ?colGatoFrente 1) ?colGato4))
            (and (= (- ?filaGatoFrente 1) ?filaGato3) (= (+ ?colGatoFrente 1) ?colGato3)))
          then
          ;Se comprueba cual es el gato que esta más a la izquierda del patron, en caso que sea
          ;el gato 1 el gato que se moverá sera el gato 2 hacia la esquina derecha
          (if (and (= ?filaGatoFrente ?filaGato2) (= (- ?colGatoFrente 2) ?colGato2) (=(+ ?filaGatoFrente 1) ?filaGato1))
            then
            (bind ?fila-esquina(- ?filaGato2 1))
            (bind ?col-esquina-derecha(+ ?colGato2 1))

            (assert(fila-columna-mover-gatos ?fila-esquina ?col-esquina-derecha ?gato2))

            (assert(ejecutar-movimiento-maquina-gato))

          else
          ;Se comprueba cual es el gato que esta más a la izquierda del patron, en caso que sea
          ;el gato 2 el gato que se moverá sera el gato 1 hacia la esquina derecha
            (if (and (= ?filaGatoFrente ?filaGato1) (= (- ?colGatoFrente 2) ?colGato1)(=(+ ?filaGatoFrente 1) ?filaGato2))
              then
              (bind ?fila-esquina(- ?filaGato1 1))
              (bind ?col-esquina-derecha(+ ?colGato1 1))

              (assert(fila-columna-mover-gatos ?fila-esquina ?col-esquina-derecha ?gato1))

              (assert(ejecutar-movimiento-maquina-gato))
            )
          )
      )
    )



    ;;Condiciones para determinar si el gato 1 o el gato 2 se quedaron atrasados del grupo de gatos,
    ;En caso que las condiciones se cumplan el gato atrasado se moverá hacia la derecha.
  (if (and (=(+ ?filaRaton 2) ?filaGato2) (=(+ ?filaGato2 2) ?filaGato1) (< (- ?colRaton ?colGato1) 4)
            (> ?filaGato1 ?filaGato3)   (> ?filaGato1 ?filaGato4))
    then
      (bind ?fila-esquina(- ?filaGato1 1))
      (bind ?col-esquina-derecha(+ ?colGato1 1))

      (assert(fila-columna-mover-gatos ?fila-esquina ?col-esquina-derecha ?gato1))
      (assert(ejecutar-movimiento-maquina-gato))

    else
    (if (and (=(+ ?filaRaton 2) ?filaGato1) (=(+ ?filaGato1 2) ?filaGato2) (< (- ?colRaton ?colGato2) 4)
              (> ?filaGato2 ?filaGato3)   (> ?filaGato2 ?filaGato4))
      then
        (bind ?fila-esquina(- ?filaGato2 1))
        (bind ?col-esquina-derecha(+ ?colGato2 1))

        (assert(fila-columna-mover-gatos ?fila-esquina ?col-esquina-derecha ?gato2))
        (assert(ejecutar-movimiento-maquina-gato))
      )
  )
)

(defrule finalizar-juego

  "Regla para finalizar la ejecución del juego"
  (finalizar-juego)
  =>

  (printout t crlf"____ ____ _  _ ____    ____ _  _ ____ ____" crlf)
  (printout t "| __ |__| |\\/| |___    |  | |  | |___ |__/" crlf)
  (printout t "|__] |  | |  | |___    |__|  \\/  |___ |  \\ ..." crlf crlf )
  (printout t "____ ____ _  _ ____ _  _    _    ____ ____    ____ ____ ___ ____ ____" crlf)
  (printout t "| __ |__| |\\ | |__| |\\ |    |    |  | [__     | __ |__|  |  |  | [__" crlf)
  (printout t "|__] |  | | \\| |  | | \\|    |___ |__| ___]    |__] |  |  |  |__| ___]" crlf)

  (printout t crlf "Escribe el comando (jugar) para empezar una nueva partida..." crlf)


  (halt)
)


(defrule ejecutar-movimiento-maquina-gato

  "Regla para ejecutar el movimiento del gato modificando el valor de los hechos de las casillas

  Variables :

  ?j           -> Indice del hecho de buscar las esquinas de los gatos.
  ?h           -> Indice del hecho de ejecutar los movimientos de la maquina.
  ?pos-a-mover -> Indice del hecho que contiene la fila, la columna hacia donde ira el gato y
                  además el indice de hecho del gato a mover.

  ?casilla-a-mover -> Obtiene el indice de hecho donde se encuentra la posicion a la que moverá el gato.
  ?nuevaFila   -> Número de fila a la que se moverá el gato
  ?nuevaColumna-> Número de fila a la que se moverá el gato
  ?idGato      -> Indice de hecho del gato que se moverá.

  Asersiones :

  (actualizar-tablero)

  "
  ;Se comprueba que exista el hecho que activa la busqueda de esquina de los gatos, para su posterior eliminación.
  ?j <-(buscar-diagonales-gatos)
  ;Se comprueba que exista el hecho que activa la presente regla.
  ?h <- (ejecutar-movimiento-maquina-gato)

  ;Se comprueba que exista el hecho que indique la posicion a la que se moverá el gato
  ?pos-a-mover <-(fila-columna-mover-gatos ?nuevaFila ?nuevaColumna ?idGato)

  ;Se captura la casilla a la que se moverá el gato.
  ?casilla-a-mover <- (casilla (fila ?nuevaFila) (columna ?nuevaColumna) (valor ?))

  =>
  ;Se eliminan los hecho de comprobación.
  (retract ?h)
  (retract ?pos-a-mover)
  (retract ?j)

  ;Mediante la función fact slot value se obtiene el valor del gato que se moverá esto
  ;con el fin de modificar la casilla con su respectivo valor.
  (bind ?value-gato-que-se-movio (fact-slot-value ?idGato valor))

  ;Se modifica la casilla a la que se moverá el gato con el valor que se obtuvo del fact slot value
  (modify ?casilla-a-mover (valor ?value-gato-que-se-movio))

  ;se modifica la casilla del gato que se moverá colocandole como valor 1 es decir dejando la
  ;casilla en negro.
  (modify ?idGato (valor 1))

  ;se actuliza el tablero para observar los cambios.
  (assert (actualizar-tablero))
  (printout t crlf "El movimiento del computador fue el gato con id# :" ?value-gato-que-se-movio "hacia" crlf
                   "[Fila]    :" ?nuevaFila crlf
                   "[Columna] :"  ?nuevaColumna )
)
