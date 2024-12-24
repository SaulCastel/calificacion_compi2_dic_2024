lista
    = movimiento ';' (salto '-'+ salto movimiento ';')* !.

movimiento
    = (blancas/negras) _ pieza casilla _ pieza casilla

blancas
    = 'Piezas' ' blancas'i (":")?

negras
    = 'Piezas' ' negras'i ":"?

pieza
    = [RDT] / [ACP]

casilla
    = [a-h][12345678][+#]?

_ "espacio"
    = [ \t]+

salto
    = "\n" / "\r"
