# Juego del ratón y los gatos en [CLIPS](http://www.clipsrules.net/)
<p align="center">
  <img src="https://raw.github.com/geovannyzp/SistemaExperto-Raton_y_Gatos/master/ReadmeImgs/primerMovimiento.png">
</p>

El sistema presenta la inteligencia artificial de las piezas de los gatos, de manera que un 100% de las partidas son ganadas por los gatos.
Para ejecutar el programa se necesita la version [6.4](http://www.clipsrules.net/CLIPS64.html) de  clips.
Para iniciar el juego por primera vez escriba carge el archivo .clp en la consola de clips luego escriba el comando *(jugar)*.
<p align="center">
  <img width="560" height="205" src="https://raw.github.com/geovannyzp/SistemaExperto-Raton_y_Gatos/master/ReadmeImgs/loadgame.png">
</p>

En caso que desee ver las instrucciones del juego escriba el comando *(instrucciones)*
<p align="center">
  <img width="560" height="205" src="https://raw.github.com/geovannyzp/SistemaExperto-Raton_y_Gatos/master/ReadmeImgs/instrucciones.png">
</p>

# Descripción del juego
Este juego se caracteriza por ser uno de los entretenidos jugados en un tablero de ajedrez.

El objetivo del ratón es alcanzar la fila de salida de los gatos, es decir tiene que esquivar la marca de los gatos para llegar hasta el otro extremo.

El objetivo de los gatos es intentar encerrar al ratón para que este no llegue al extremo del tablero. 

## ¿Cómo se juega?

 1. El ratón es el primero en moverse. En cualquier momento el ratón puede moverse a cualquiera de las casillas negras próximas a él, siempre que la casilla no esté ya ocupada por un gato.
 2. Después de moverse el ratón, le toca el turno a uno de los gatos. Los gatos pueden moverse sólo hacia delante, siempre que la casilla no esté ocupada por el ratón o por alguno de los otros gatos.
 3.  Cuando se haya movido uno de los gatos, le tocará el turno al ratón, y así sucesivamente.
 4. Gana el ratón si consigue llegar al lado contrario, es decir, a cualquiera de las casillas de las que salieron los gatos.
 5. Ganan los gatos si consiguen atrapar al ratón, de manera que el ratón no pueda moverse más.
 <p align="center">
  <img src="https://raw.github.com/geovannyzp/SistemaExperto-Raton_y_Gatos/master/ReadmeImgs/paso1.png">
</p>

## ¿Cómo se gana?

El ratón gana cuando ha logrado llegar a la fila de salida de los gatos.

Los gatos ganan en caso que encierren al ratón, es decir cuando el ratón no tiene casillas disponibles para moverse.

<p align="center">
  <img src="https://raw.github.com/geovannyzp/SistemaExperto-Raton_y_Gatos/master/ReadmeImgs/final.png">
</p>

<p align="center">
  <img width="560" height="205" src="https://raw.github.com/geovannyzp/SistemaExperto-Raton_y_Gatos/master/ReadmeImgs/gameoverr.png">
</p>

 
