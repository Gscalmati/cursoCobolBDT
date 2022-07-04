      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

      *****************************************************************
      ********************** LINEAS DE IMPRESION **********************
      *****************************************************************
       01  lin-titulo.
           03 filler pic x(30) value spaces.
           03 filler pic x(19) value "Banco El Corralito".
           03 filler pic x(31) value spaces.

       01  lin-movCaFecha.
           03 filler pic x(10) value spaces.
           03 filler pic x(50) value "Totales de Movimientos de Caja de
      -    "Ahorro del dia: ".
           03 lin-fecha.
                   05 lin-dia   pic 99.
                   05 filler pic x value "/".
                   05 lin-mes   pic 99.
                   05 filler pic x(3) value "/20".
                   05 lin-anio  pic 99.
           03 filler pic x(10) value spaces.

       01  lin-guion.
           03 filler pic x(80) value all "-".

       01  lin-operaciones.
           03 filler pic x(22) value "TOTAL DE OPERACIONES:".
           03 totOperacion pic zz.zz9.
       01  lin-montOperacion.
           03 filler pic x(28) value "MONTO TOTAL DE OPERACIONES:".
           03 totMontoOp pic zzz.zzz.zzz.zz9,99.
       01  lin-detalle pic x(27) value "Detalle de las Operaciones:".
       01  lin-detalleOp.
           03 detalleOp pic xx.
           03 filler pic xxx value " - ".
           03 nomOp pic x(23).
           03 impOp pic zzz.zzz.zz9,99.

      *****************************************************************
      ********************** VARIABLES DE ENTRADA**********************
      *****************************************************************
       01  variables-ingreso.
           03 ing-socio   pic s9(6).
           03 ing-cuenta  pic x(2).
           03 ing-operac  pic x(2).
           03 ing-monto   pic s9(7)v99.

      *****************************************************************
      ********************* CONSTANTES QUE ME DAN *********************
      *****************************************************************
       01  tabla-detalle.
           03  cod-operaciones.
               05 filler pic xx value "DE".
               05 filler pic xx value "RE".
               05 filler pic xx value "TE".
               05 filler pic xx value "TR".
               05 filler pic xx value "DT".
               05 filler pic xx value "DC".
               05 filler pic xx value "CC".
           03 vecOper redefines cod-operaciones occurs 7 TIMES.
               05 Oper    pic x(2).

           03  nom-operaciones.
               05 filler pic x(22) value "DEPOSITO EN EFECTIVO".
               05 filler pic x(22) value "RETIRO EN EFECTIVO".
               05 filler pic x(22) value "TRANSFERENCIA EMITIDA".
               05 filler pic x(22) value "TRANSFERENCIA RECIBIDA".
               05 filler pic x(22) value "DEBITO CON TARJETA".
               05 filler pic x(22) value "DEBITO EN CUENTA".
               05 filler pic x(22) value "CREDITO EN CUENTA".
           03 vecNomOper redefines nom-operaciones occurs 7 TIMES.
               05 NomOper pic x(22).

           03 largo PIC 99.
           03 vecImporte OCCURS 1 to 15 DEPENDING largo.
               05 Importe pic s9(7)v99.
.     *****************************************************************
      ********************** VARIABLES DE TRABAJO *********************
      *****************************************************************
       01  contOp       pic 9(9).
       01  acumMonto    pic s9(10)v99.
       01  i            pic 9(2).
       01  fecha.
           03 anio pic 99.
           03 mes  pic 99.
           03 dia  pic 99.
       01  Aux-ing-cuenta  pic x(2).
       01  Aux-ing-operac  pic x(2).
       01  cont pic 99 value 1.
       01  concat-cod-oper pic x(25) values spaces.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      ******************************************************************
      ********************* PROGRAMA PRINCIPAL *************************
      ******************************************************************
           PERFORM 100-INICIO-GENERAL.
           PERFORM 200-INGRESO-SOCIO.
           PERFORM UNTIL ing-socio = ZERO
              PERFORM 300-INGRESO-RESTO
              PERFORM 400-PROCESO-DATOS
              PERFORM 200-INGRESO-SOCIO
           END-PERFORM.
           PERFORM 500-FIN-GENERAL.
           STOP RUN.
      ******************************************************************
      ***************************** RUTINAS ****************************
      ******************************************************************
       100-INICIO-GENERAL.
           PERFORM 120-INICIO-EN-CERO.

       120-INICIO-EN-CERO.
           PERFORM 125-INICIALIZO-IMPORTE-EN-CERO.
           move ZERO to contOp.
           move ZERO to acumMonto.
           move 7    to largo.

       125-INICIALIZO-IMPORTE-EN-CERO.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > largo
              MOVE ZERO TO Importe(i)
           END-PERFORM.

       200-INGRESO-SOCIO.
           DISPLAY "Ingrese el numero de socio, cero para salir"
           PERFORM 250-VALIDAR-POSITIVO.

       250-VALIDAR-POSITIVO.
           ACCEPT ing-socio.
           PERFORM UNTIL ing-socio >=0
           DISPLAY "Error, ingrese un numero positivo o cero para salir"
           ACCEPT ing-socio
           END-PERFORM.

       300-INGRESO-RESTO.
           PERFORM 320-INGRESO-CUENTA.
           PERFORM 340-INGRESO-OPERACION.
           PERFORM 360-INGRESO-MONTO.

       320-INGRESO-CUENTA.
           DISPLAY "Ingrese el tipo de cuenta, CC-CA".
           PERFORM 325-VERIFICAR-CHAR.

       325-VERIFICAR-CHAR.
           ACCEPT Aux-ing-cuenta.
           move FUNCTION UPPER-CASE(Aux-ing-cuenta) to ing-cuenta.
           PERFORM UNTIL ing-cuenta ="CC" OR ing-cuenta ="CA"
               DISPLAY "Error, debe ingresar: CA o CC"
                ACCEPT Aux-ing-cuenta
                move FUNCTION UPPER-CASE(Aux-ing-cuenta) to ing-cuenta
           END-PERFORM.

       340-INGRESO-OPERACION.
           PERFORM 347-TRAIGO-OPERACION.
           move FUNCTION UPPER-CASE(Aux-ing-operac) to ing-operac.
           PERFORM 342-VERIFICAR-TIPO-OPERACION.

       341-MOSTRAR-LAS-OPERACIONES.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > largo
               STRING Oper(i) DELIMITED BY SIZE
                      "-" DELIMITED BY SIZE
                      INTO concat-cod-oper
                      WITH POINTER cont
               END-STRING
           END-PERFORM.
           DISPLAY concat-cod-oper.

       342-VERIFICAR-TIPO-OPERACION.
           PERFORM 345-BUSCAR-TIPO-OPERACION
           PERFORM until i <= largo
               DISPLAY "Error"
               PERFORM 347-TRAIGO-OPERACION
               move FUNCTION UPPER-CASE(Aux-ing-operac) to ing-operac
               PERFORM 345-BUSCAR-TIPO-OPERACION
           END-PERFORM.

       345-BUSCAR-TIPO-OPERACION.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i>largo OR
           ing-operac = Oper(i)
           END-PERFORM.

       347-TRAIGO-OPERACION.
           DISPLAY "Ingrese el tipo de operacion:".
           PERFORM 341-MOSTRAR-LAS-OPERACIONES.
           ACCEPT Aux-ing-operac.

       360-INGRESO-MONTO.
           PERFORM 385-TRAIGO-IMPORTE.
           PERFORM 365-VERIFICAR-NO-NEGATIVO.

       365-VERIFICAR-NO-NEGATIVO.
           PERFORM UNTIL ing-monto >=0
               DISPLAY "Error,monto negativo."
               PERFORM 385-TRAIGO-IMPORTE
           END-PERFORM.

       385-TRAIGO-IMPORTE.
           DISPLAY "Ingrese monto mayor a 0".
           ACCEPT ing-monto.

       400-PROCESO-DATOS.
           IF ing-cuenta = "CA"
               ADD ing-monto TO acumMonto
               ADD 1 TO contOp
               PERFORM 345-BUSCAR-TIPO-OPERACION
               ADD ing-monto to Importe(i)
           END-IF.


       500-FIN-GENERAL.
           PERFORM 510-MUESTRO-ENCABEZADO.
           PERFORM 520-MUESTRO-RESULTADO-OPERACION.

       510-MUESTRO-ENCABEZADO.
           PERFORM 550-OBTENGO-FECHA.
           PERFORM 525-ARMO-LIN-MOVCAFECHA.
           PERFORM 515-ENCABEZADO.

       515-ENCABEZADO.
           DISPLAY lin-titulo.
           DISPLAY lin-movCaFecha.
           DISPLAY lin-guion.

       520-MUESTRO-RESULTADO-OPERACION.
           PERFORM 527-ARMO-LIN-OPERACIONES.
           PERFORM 528-ARMO-LIN-MONTOPERACION.
           DISPLAY lin-operaciones.
           display lin-montOperacion.
           DISPLAY lin-detalle.
           PERFORM 522-MUESTRO-DATOS-VECOPERACION.

       522-MUESTRO-DATOS-VECOPERACION.
           PERFORM VARYING i from 1 by 1 until i>largo
               PERFORM 526-ARMO-LIN-DETALLEOP
               DISPLAY lin-detalleOp
           END-PERFORM.

       525-ARMO-LIN-MOVCAFECHA.
           MOVE dia TO lin-dia.
           MOVE mes TO lin-mes.
           MOVE anio TO lin-anio.

       526-ARMO-LIN-DETALLEOP.
           MOVE Oper(i) TO detalleOp.
           MOVE NomOper(i) TO nomOp.
           MOVE Importe(i) TO impOp.

       527-ARMO-LIN-OPERACIONES.
           MOVE contOp TO totOperacion.

       528-ARMO-LIN-MONTOPERACION.
           MOVE acumMonto TO totMontoOp.

       550-OBTENGO-FECHA.
           ACCEPT FECHA FROM DATE.

       END PROGRAM YOUR-PROGRAM-NAME.
