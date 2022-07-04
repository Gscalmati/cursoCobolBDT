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
           SELECT ERROR-LOG ASSIGN TO
           "..\errorLog.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  ERROR-LOG.
       01  log-reg.
           03 log-socio pic 99999.
           03 log-mov pic x.
           03 log-importe pic s9(7)v99.
           03 log-error pic x(40).

       WORKING-STORAGE SECTION.
       01  cont pic 999.
       01  flag pic 9 VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-LEER-REG.
           PERFORM UNTIL (flag = 0)
               PERFORM 300-PROCESO
               PERFORM 200-LEER-REG
            END-PERFORM.
            PERFORM 400-FIN.
            STOP RUN.

       100-INICIO.
           MOVE ZEROES TO cont.
           DISPLAY "Lector de Archivos Secuenciales de Salida 3000"
           DISPLAY "ERROR LOG"
           OPEN INPUT ERROR-LOG.

       200-LEER-REG.
           READ ERROR-LOG AT END MOVE 0 TO flag.

       300-PROCESO.
           DISPLAY log-socio, " ", log-mov, " ", log-error " ".

       400-FIN.
           CLOSE ERROR-LOG.
       END PROGRAM YOUR-PROGRAM-NAME.
