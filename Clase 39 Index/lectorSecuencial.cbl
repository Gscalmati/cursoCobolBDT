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
           SELECT socios ASSIGN TO "..\socios.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS soc-cod.
       DATA DIVISION.
       FILE SECTION.
       FD  socios.
       01  soc-reg.
           03 soc-cod pic 999.
           03 soc-nom pic x(10).
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

           300-INGRESO-RESTO.
               DISPLAY "Ingrese nombre del socio."
               ACCEPT soc-nom.

           400-PROCESO.
               DISPLAY soc-cod, " - ", soc-nom.

           500-FIN.
               CLOSE SOCIOS.
       END PROGRAM YOUR-PROGRAM-NAME.
