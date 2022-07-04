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
           SELECT SOCIOS ASSIGN TO "..\sociosIndex.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS soc-cod.
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 reg-cod-ind.
               05 soc-fil pic 9.
               05 soc-cod pic 999.
           03 soc-nom pic x(15).
           03 soc-saldo pic s9(6)v99.
       WORKING-STORAGE SECTION.

       01  flag-soc pic 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-TRAIGO.
            PERFORM UNTIL flag-soc = 0
               PERFORM 400-PROCESO
               PERFORM 200-TRAIGO
            END-PERFORM.
            PERFORM 500-FIN.
            STOP RUN.

           100-INICIO.
               OPEN INPUT SOCIOS.
               MOVE 1 TO flag-soc.

           200-TRAIGO.
               READ SOCIOS AT END MOVE 0 TO flag-soc  .

           400-PROCESO.
               DISPLAY soc-fil, "-",
               soc-cod, " - ", soc-nom, " - ", soc-saldo.

           500-FIN.
               CLOSE SOCIOS.
       END PROGRAM YOUR-PROGRAM-NAME.
