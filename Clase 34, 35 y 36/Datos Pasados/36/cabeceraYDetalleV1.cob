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
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACCIONES
               ASSIGN TO "../transacciones.txt"
               ORGANIZATION LINE SEQUENTIAL.
           SELECT TRANSACCIONES-ACT
               ASSIGN TO "../transaccionesAct.txt"
               ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACCIONES.
       01  tr-cab-reg.
           03 tr-cab-tipo      PIC X.
           03 tr-cab-fecha     PIC 9(8).
       01  tr-det-reg.
           03 tr-det-tipo      PIC X.
           03 tr-det-socio     PIC 9999.
           03 tr-det-importe   PIC S9(7)V99.
       FD  TRANSACCIONES-ACT.
       01  tra-reg.
           03 tra-socio        PIC 9(4).
           03 tra-importe      PIC S9(8)V99.

       WORKING-STORAGE SECTION.

      *************************  PANTALLA LISTADO **********************
       01  lin-titulo.
           03 filler pic x(24) value spaces.
           03 filler pic x(23) value "TRANSACCIONES DE FECHA ".
           03 l-dia  pic 99.
           03 filler pic x value "/".
           03 l-mes  pic 99.
           03 filler pic x value "/".
           03 l-anio pic 9999.
           03 filler pic x(23) value spaces.

       01  lin-titulos-socios.
           03 filler pic x(28) value spaces.
           03 filler pic x(10) value "Nro. Socio".
           03 filler pic x(7) value spaces.
           03 filler pic x(7) value "Importe".
           03 filler pic x(28) value spaces.

       01  lin-detalle-datos.
           03 filler       pic X(29) value spaces.
           03 l-tra-socio  pic zzz9.
           03 filler       pic X(8) value spaces.
           03 l-tra-importe pic zz.zzz.zz9,99.
           03 filler       pic X(29) value spaces.

       01  lin-espacio-blanco.
           03 FILLER PIC X(80) VALUE ALL SPACES.

       01  fecha.
           03 anio             PIC 9(4).
           03 mes              PIC 99.
           03 dia              PIC 99.
       77  flag                PIC 9.
       77  flag-fecha          PIC 9.
       77  flag-trans-act      PIC 9.
       77  acum-importe        PIC S9(8)V99.
       77  guarda-enter        PIC X.
       01  tr-det-socio-ant    PIC 9(4).


       PROCEDURE DIVISION.
      ******************************************************************
      **************************PROGRAMA PRINCIPAL**********************
      ******************************************************************
       MAIN-PROCEDURE.

           PERFORM 100-INICIO-GENERAL.
           PERFORM 200-INGRESO-GENERAL.
           PERFORM UNTIL fecha = 0
               PERFORM 320-INICIO-CAB-C
               PERFORM 340-LEER-REGISTRO
               PERFORM UNTIL flag = 1
                   PERFORM UNTIL flag = 1 OR fecha <> tr-cab-fecha
                       OR fecha < tr-cab-fecha
                       PERFORM 330-INICIO-CAB-D
                       PERFORM 340-LEER-REGISTRO
                       PERFORM UNTIL flag = 1
                           OR tr-det-tipo <> "D"
                               PERFORM 360-INICIO-SOCIO
                               PERFORM UNTIL flag = 1
                                   OR tr-det-tipo <> "D"
                                   OR tr-det-socio <> tr-det-socio-ant
                                       PERFORM 365-ACUMULO-SOCIO
                                       PERFORM 340-LEER-REGISTRO
                               END-PERFORM
                               PERFORM 380-FIN-SOCIO
                       END-PERFORM
                       PERFORM 393-FIN-CAB-D
                   END-PERFORM
                   PERFORM 396-FIN-CAB-C
               END-PERFORM
               PERFORM 398-FIN-TRANSACCIONES
               PERFORM 200-INGRESO-GENERAL
           END-PERFORM.
           PERFORM 500-FIN-GENERAL.
           STOP RUN.

      ******************************************************************
      ******************************* RUTINAS **************************
      ******************************************************************

       050-PAUSA-PANTALLA.
           PERFORM 052-TRAIGO-ENTER.
           PERFORM UNTIL guarda-enter = SPACES
               display "Error, tenes que ingresar ENTER"
               PERFORM 052-TRAIGO-ENTER
           END-PERFORM.

       052-TRAIGO-ENTER.
           DISPLAY lin-espacio-blanco.
           DISPLAY "Presione la tecla ENTER para continuar...".
           ACCEPT guarda-enter.

       100-INICIO-GENERAL.

       200-INGRESO-GENERAL.
           PERFORM 220-TRAIGO-FECHA.
           PERFORM UNTIL fecha >= 0
              display "Error, te dije un numero positivo, x fin 0"
              PERFORM 220-TRAIGO-FECHA
           END-PERFORM.

       220-TRAIGO-FECHA.
           DISPLAY lin-espacio-blanco.
           display "Ingrese una fecha 'AAAAMMDD' (> 0), por fin ingrese
      -     "0".
           ACCEPT fecha.

       320-INICIO-CAB-C.
           MOVE 0 TO flag.
           MOVE 0 TO flag-fecha.
           PERFORM 325-ABRO-TRANSACCIONES.
           PERFORM 330-ABRO-TRANSACCIONES-ACT.

       330-INICIO-CAB-D.
           MOVE 1 TO flag-fecha.

       325-ABRO-TRANSACCIONES.
           OPEN INPUT TRANSACCIONES.

       330-ABRO-TRANSACCIONES-ACT.
           OPEN OUTPUT TRANSACCIONES-ACT.

       340-LEER-REGISTRO.
           READ TRANSACCIONES AT END MOVE 1 TO flag.

       345-MUEVO-ANTERIOR.
           MOVE tr-det-socio TO tr-det-socio-ant.

       360-INICIO-SOCIO.
           MOVE 0 TO acum-importe.
           PERFORM 345-MUEVO-ANTERIOR.

       365-ACUMULO-SOCIO.
           ADD tr-det-importe TO acum-importe.

       380-FIN-SOCIO.
           PERFORM 385-MUEVO-REG-ACT.
           PERFORM 390-GRABO-TRANS-ACT.

       385-MUEVO-REG-ACT.
           MOVE tr-det-socio-ant TO tra-socio.
           MOVE acum-importe TO tra-importe.

       390-GRABO-TRANS-ACT.
           WRITE tra-reg.

       393-FIN-CAB-D.
           MOVE 1 TO flag.

       396-FIN-CAB-C.
           IF flag <> 1
               PERFORM 340-LEER-REGISTRO
               PERFORM UNTIL flag = 1 OR tr-det-tipo <> "D"
                   PERFORM 340-LEER-REGISTRO
           END-PERFORM.

       398-FIN-TRANSACCIONES.
           PERFORM 400-CIERRO-TRANSACCIONES.
           PERFORM 405-CIERRO-TRANSACCIONES-ACT.
           IF flag-fecha = 0
               DISPLAY "No hubo transacciones en la fecha ", dia, "/",
                mes, "/", anio
           ELSE
           PERFORM 410-MUESTRO-TRANS-ACT.

       400-CIERRO-TRANSACCIONES.
           CLOSE TRANSACCIONES.

       405-CIERRO-TRANSACCIONES-ACT.
           CLOSE TRANSACCIONES-ACT.

       410-MUESTRO-TRANS-ACT.
           PERFORM 412-ABRO-TRANSACCIONES-ACT.
           PERFORM 415-MUESTRO-ENCABEZADO.
           PERFORM 420-LEO-TRANS-ACT.
           PERFORM UNTIL flag-trans-act = 1
               PERFORM 430-MUESTRO-DETALLE
               PERFORM 420-LEO-TRANS-ACT
           END-PERFORM.
           PERFORM 405-CIERRO-TRANSACCIONES-ACT.

       412-ABRO-TRANSACCIONES-ACT.
           OPEN INPUT TRANSACCIONES-ACT.
           MOVE 0 TO flag-trans-act.

       415-MUESTRO-ENCABEZADO.
           MOVE dia TO l-dia.
           MOVE mes TO l-mes.
           MOVE anio TO l-anio.
           DISPLAY lin-titulo.
           DISPLAY lin-titulos-socios.

       420-LEO-TRANS-ACT.
           READ TRANSACCIONES-ACT AT END MOVE 1 TO flag-trans-act.

       425-MUEVO-LIN-DETALLE.
           MOVE tra-socio TO l-tra-socio.
           MOVE tra-importe TO l-tra-importe.

       430-MUESTRO-DETALLE.
           PERFORM 425-MUEVO-LIN-DETALLE.
           DISPLAY lin-detalle-datos.

       500-FIN-GENERAL.

       END PROGRAM YOUR-PROGRAM-NAME.
