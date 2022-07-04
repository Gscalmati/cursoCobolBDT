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

           SELECT ARCH-TRANS-ACTUAL ASSIGN TO nombreFecha
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  ARCH-TRANS.
       01  tr-cab-reg.
           03 tr-cab-tipo pic x.
           03 tr-cab-fecha pic 9(8).
       01  tr-det-reg.
           03 tr-det-tipo pic x.
           03 tr-det-socio pic 9999.
           03 tr-det-importe pic s9(7)v99.

       FD  ARCH-TRANS-ACTUAL.
       01  tra-reg.
           03 tra-socio pic 9999.
           03 tra-importe pic s9(8)v99.

       WORKING-STORAGE SECTION.
           01 nombreFecha pic x(31).
           01 flag-transac pic 9 value 1.
           01 ing-fecha pic 9(8).
           01 socioAnterior pic 9999.
           01 totalSocio pic s9(8)v99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 150-INGRESO-FECHA.
            PERFORM 200-LEER-TRANSAC.
            PERFORM UNTIL (flag-transac = 0)
               PERFORM 250-BUSCO-FECHA
               PERFORM UNTIL
               (flag-transac = 0) or (tr-cab-tipo = "C")
                   PERFORM 300-INICIO-SOCIO
                   PERFORM UNTIL
               (flag-transac = 0) or (tr-cab-tipo = "C") or
               (tr-det-socio <> socioAnterior)
                       PERFORM 400-PROCESO-SOCIO
                       PERFORM 200-LEER-TRANSAC
                   END-PERFORM
                   PERFORM 500-FIN-SOCIO
               END-PERFORM
               PERFORM 600-FIN-FECHA
            END-PERFORM
            PERFORM 900-FIN-GENERAL.

       STOP RUN.

       100-INICIO-GENERAL.
           OPEN INPUT ARCH-TRANS.

       150-INGRESO-FECHA.
           DISPLAY "Ingrese fecha bajo este formato 'AAAAMMDD'".
           ACCEPT ing-fecha.
           PERFORM 180-CREAR-ARCH.

           180-CREAR-ARCH.
               STRING "..\transaccionesDia" DELIMITED BY SIZE
               ing-fecha DELIMITED BY SIZE
               ".txt" DELIMITED BY SIZE
               INTO nombreFecha
               OPEN OUTPUT ARCH-TRANS-ACTUAL.

       200-LEER-TRANSAC.
           DISPLAY "LEO TRANS".
           READ ARCH-TRANS AT END MOVE 0 TO flag-transac.
           DISPLAY tr-cab-reg.

       250-BUSCO-FECHA.
           PERFORM UNTIL (flag-transac = 0) and (tr-cab-tipo = "C")
               PERFORM 200-LEER-TRANSAC
           END-PERFORM.
           IF (tr-cab-fecha = ing-fecha)
              PERFORM 200-LEER-TRANSAC
           ELSE
               PERFORM 250-BUSCO-FECHA
           END-IF.
           IF (flag-transac = 0)
               DISPLAY "No se encontro registro".

       300-INICIO-SOCIO.
           MOVE 0 TO totalSocio.
           MOVE tr-det-socio TO socioAnterior.

       400-PROCESO-SOCIO.
           ADD tr-det-importe TO totalSocio.

       500-FIN-SOCIO.
           MOVE socioAnterior TO tra-socio.
           MOVE totalSocio TO tra-importe.
           WRITE tra-reg.

       600-FIN-FECHA.

       900-FIN-GENERAL.
       END PROGRAM YOUR-PROGRAM-NAME.
