      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCH-TRANS ASSIGN TO "..\transacciones.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD ARCH-TRANS.
       01  tr-cab-reg.
           03 tr-cab-tipo pic x.
           03 tr-cab-fecha pic 9(8).
       01  tr-det-reg.
           03 tr-det-tipo pic x.
           03 tr-det-socio pic 9999.
           03 tr-det-importe pic s9(7)v99.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 200-INGRESO-FECHA.
            PERFORM UNTIL (tr-cab-fecha = 0)
               PERFORM 600-GRABO-FECHA
               PERFORM 300-INICIO-SOCIO
               PERFORM 400-INGRESO-SOCIO
               PERFORM UNTIL (tr-det-socio = 0)
                   PERFORM 500-INGRESO-RESTO
                   PERFORM 700-GRABO-TRANS
                   PERFORM 400-INGRESO-SOCIO
               END-PERFORM
               PERFORM 200-INGRESO-FECHA
            END-PERFORM.
            PERFORM 800-FIN-GENERAL.
       STOP RUN.

       100-INICIO-GENERAL.
           OPEN OUTPUT ARCH-TRANS.
           DISPLAY "Creador de Transacciones 3000".

       200-INGRESO-FECHA.
           DISPLAY "Ingrese fecha bajo este formato 'AAAAMMDD'".
           ACCEPT tr-cab-fecha.

       300-INICIO-SOCIO.
           MOVE "D" TO tr-det-tipo.

       400-INGRESO-SOCIO.
           DISPLAY "Ingrese Numero de socio, debe ser > a 0".
           ACCEPT tr-det-socio.

       500-INGRESO-RESTO.
           DISPLAY "Ingrese Importe de la transaccion".
           ACCEPT tr-det-importe.

       600-GRABO-FECHA.
           MOVE "C" TO tr-cab-tipo.
           WRITE tr-cab-reg.

       700-GRABO-TRANS.
           WRITE tr-det-reg.

       800-FIN-GENERAL.
           CLOSE ARCH-TRANS.

       END PROGRAM YOUR-PROGRAM-NAME.
