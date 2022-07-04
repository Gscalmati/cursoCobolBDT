      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
   ******************************************************************
      *************************LINEAS DE IMPRESION**********************
      ******************************************************************
       01  lin-titulo.
           03 filler pic x(30) value spaces.
           03 filler pic x(19) values "BANCO EL CORRALITO".
           03 filler pic x(30) value spaces.
       01  lin-titulo-planilla.
           03 filler pic x(10) value spaces.
           03 filler pic x(50) values
           "Totales de Movimientos de Caja de Ahorro del dia: ".
      *    COMO CONFIGURAR FECHA
           03 l-fecha.
               05 l-dia pic 99.
               05 filler pic x value "/".
               05 l-mes pic 99.
               05 filler pic xxx value "/20".
               05 l-anio pic 99.
           03 filler pic x(10) value spaces.
       01  lin-detalle-datos.
           03 filler pic x(5) value spaces.
           03 l-socio pic z.zzz.zzz.zz9.
           03 filler pic x(3) value spaces.
           03 l-nombre pic x(20).
           03 filler pic x(3) value spaces.
           03 l-dni pic zz.zzz.zz9.
           03 filler pic x(3) value spaces.
           03 l-saldo pic zzz.zzz.zz9,99.
           03 filler pic x(5) value spaces.
       01  lin-divisoria.
      *    Aca es solo poner ALL al lado de value para que llene la linea
           03 filler pic x(80) value all "-".
       01  lin-total-op.
           03 filler pic x(22) value "TOTAL DE OPERACIONES:".
           03 l-cant-op pic zz.zz9.
           03 filler pic x(53) value spaces.
       01  lin-monto-total-op.
           03 filler pic x(27) value "MONTO TOTAL DE OPERACIONES:".
           03 l-acum-op pic zzz.zzz.zzz.zz9,99.
           03 filler pic x(38) value spaces.
       01  lin-detalle-op pic x(27) value "Detalle de las operaciones:".
       01  lin-tipos-operaciones.
           03 l-tipo-op pic x(2).
           03 filler pic x(3) value " - ".
           03 l-detalle-op pic x(24).
           03 l-total-op pic zzz.zzz.zz9,99.
      ******************************************************************
      *************************VARIABLES DE ENTRADA*********************
      ******************************************************************
       01  var-ingresos.
           03 num-soc-aux pic 9(10).
       01  socio.
           03 ing-num-socio pic 9(6).
           03 ing-cuenta pic x(2).
           03 ing-op pic x(2).
           03 ing-importe pic s9(7)v99.
      ******************************************************************
      *************************VARIABLES DE TRABAJO*********************
      ******************************************************************
       01  ok pic 9.
       01  acum-cant-op pic 9(5).
       01  acum-mont-op pic 9(12)v99.
       01  aux-monto pic 9(12)v99.
       01  i pic 9(4).

       01  tabla-operaciones.
           03 longitud pic 9.
           03  vec-operaciones occurs 1 to 15 depending longitud.
               05 vec-tipo-operacion pic x(2).
               05 vec-detalle-operacion pic x(24).
               05 vec-total-operacion pic 9(9)v99.
     ******************************************************************
      *************************PROGRAMA PPAL****************************
      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-inicio-general.
           PERFORM 200-ingresoSocio.
           PERFORM UNTIL (num-soc-aux = 0)
               PERFORM 300-ingreso-restoDatos
               PERFORM 400-procesoDatos
               PERFORM 200-ingresoSocio
           END-PERFORM.
           PERFORM 120-ENCABEZADO.
           PERFORM 500-fin.
            STOP RUN.

      *
      ******************************************************************
      *************************FUNCIONES********************************
      ******************************************************************

           100-inicio-general.
      *    ASIGNO A LONGITUD LA CANTIDAD DE OPERACIONES
               MOVE 7 TO longitud.
               MOVE zeros TO acum-cant-op.
               MOVE zeros TO acum-mont-op.
      *    PONGO EL VECTOR EN CONDICIONES DE TRABAJAR
               PERFORM 110-armar-vector.

           110-armar-vector.
               PERFORM 112-agregar-operaciones.
               PERFORM 114-agregar-detalles.
               PERFORM 116-incializar-vector.


           112-agregar-operaciones.
               MOVE "DE" TO vec-tipo-operacion(1).
               MOVE "RE" TO vec-tipo-operacion(2).
               MOVE "TE" TO vec-tipo-operacion(3).
               MOVE "TR" TO vec-tipo-operacion(4).
               MOVE "DT" TO vec-tipo-operacion(5).
               MOVE "DC" TO vec-tipo-operacion(6).
               MOVE "CC" TO vec-tipo-operacion(7).

           114-agregar-detalles.
             MOVE "DEPOSTIO EN EFECTIVO:" TO vec-detalle-operacion(1).
             MOVE "RETIRO EN EFECTIVO:" TO vec-detalle-operacion(2).
             MOVE "TRANSFERENCIA EMITIDA:" TO vec-detalle-operacion(3).
             MOVE "TRANSFERENCIA RECIBIDA:" TO vec-detalle-operacion(4).
             MOVE "DEBITO CON TARJETA:" TO vec-detalle-operacion(5).
             MOVE "DEBITO EN CUENTA:" TO vec-detalle-operacion(6).
             MOVE "CREDITO EN CUENTA:" TO vec-detalle-operacion(7).

           116-incializar-vector.
      *         ACA ESTABA ANTES I < LONGITUD
               PERFORM VARYING i FROM 1 by 1 UNTIL (i > longitud)
                   MOVE zeros TO vec-total-operacion(i)
               END-PERFORM.

           120-encabezado.
               PERFORM 128-IMPRIMO-ENCABEZADO.


           128-IMPRIMO-ENCABEZADO.
               DISPLAY lin-titulo.
               ACCEPT l-fecha FROM DATE.
               DISPLAY lin-titulo-planilla.
               DISPLAY lin-divisoria.



           200-ingresoSocio.
               DISPLAY "Ingrese num. de socio "
               ACCEPT num-soc-aux.
               perform until (num-soc-aux >= 0)
                   DISPLAY
                   'ERROR, Ingrese un num. de socio (0 para FIN)'
                   ACCEPT num-soc-aux
           END-PERFORM.


           300-ingreso-restoDatos.
               PERFORM 325-ingresoCuenta.
               PERFORM 335-ingresoOp-dinamico.
               PERFORM 345-ingresoMonto.

           325-ingresoCuenta.
               DISPLAY "Ingrese tipo de cuenta:"
      -         " CC-CUENTA CORRIENTE / CA - CAJA DE AHORRO ".
               ACCEPT ing-cuenta.
               PERFORM UNTIL (ing-cuenta = "CC" OR ing-cuenta = "CA")
                   DISPLAY 'ERROR, Ingrese tipo valido: CC / CA'
                   ACCEPT ing-cuenta
               END-PERFORM.

      *    INGRESO UTILIZANDO LOS VECTORES, INDEPENDIENTE DE CANTIDAD DE DATOS EN VECTOR
       335-ingresoOp-dinamico.
               DISPLAY "Ingrese tipo de operacion:"
               PERFORM 337-displayOperaciones.
      *         DISPLAY "DE / RE / TE / TR / DT / DC / CC".
               ACCEPT ing-op.
      *        VALIDADOR DINAMICO
               MOVE 0 TO ok.
               PERFORM UNTIL (ok = 1)
                   PERFORM VARYING i FROM 1 BY 1 UNTIL (ok = 1)
                   OR (i>longitud)
                       IF (vec-tipo-operacion(i) = ing-op)
                           MOVE 1 To ok
                       END-IF
                   END-PERFORM
                   IF (ok = 0)
                       DISPLAY
                   'ERROR, Ingrese un tipo de Op. valida'
                       PERFORM 337-displayOperaciones
      *                DISPLAY "DE / RE / TE / TR / DT / DC / CC".
                       ACCEPT ing-op
                       END-IF
               END-PERFORM.

       337-displayOperaciones.
           PERFORM VARYING i FROM 1 BY 1 UNTIL (i>longitud)
                   DISPLAY vec-tipo-operacion(i)
               END-PERFORM.

               345-ingresoMonto.
                   DISPLAY "Ingrese importe de la operacion".
                   ACCEPT ing-importe.
                   PERFORM UNTIL (ing-importe > 0)
                   DISPLAY "Ingrese importe de la operacion valido"
                   ACCEPT ing-importe
               END-PERFORM.

               400-procesoDatos.
                    if(ing-cuenta = "CA")
                       COMPUTE acum-cant-op = acum-cant-op + 1
                       PERFORM 410-agregarAlVector
                    END-IF.
      *    ESTO SE PUEDE HACER PONIENDO LA CONDICION DEL FOR DEL 225
      *         Y DEL IF 226 JUNTAS EN UN SOLO FOR, Y NO ES NECESARIO
      *              PREGUNTAR CADA VEZ QUE ITERA, SINO QUE SE DETIENE EN EL LUGAR CORRECTO, ya que I es GLOBAL
               410-agregarAlVector.
                   PERFORM VARYING i FROM 1 by 1 UNTIL (i > longitud)
                       if (vec-tipo-operacion(i) = ing-op)
              COMPUTE
           vec-total-operacion(i) = ing-importe + vec-total-operacion(i)
               COMPUTE acum-mont-op = acum-mont-op + ing-importe
              END-IF
                   END-PERFORM.

               420-imprimirVector.
                   PERFORM VARYING i FROM 1 by 1 UNTIL (i > longitud)
                   PERFORM 425-asignarVariablesVector

                   DISPLAY lin-tipos-operaciones
                   END-PERFORM.

               425-asignarVariablesVector.
                   MOVE vec-tipo-operacion(i) TO l-tipo-op.
                   MOVE vec-detalle-operacion(i) TO l-detalle-op.
                   MOVE vec-total-operacion(i) TO l-total-op.


               500-fin.
                   PERFORM 525-asignarVariablesTotal.
                   PERFORM 535-imprimirFinal.
                   PERFORM 420-imprimirVector.


               525-asignarVariablesTotal.
                   MOVE acum-cant-op TO l-cant-op.
                   MOVE acum-mont-op TO l-acum-op.

               535-imprimirFinal.
                   DISPLAY lin-total-op.
                   DISPLAY lin-monto-total-op.
                   DISPLAY lin-detalle-op.

       END PROGRAM YOUR-PROGRAM-NAME.
