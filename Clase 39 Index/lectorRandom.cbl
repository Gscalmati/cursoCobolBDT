****************************************************************
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
               ACCESS MODE IS RANDOM
               RECORD KEY IS soc-cod.
       DATA DIVISION.
       FILE SECTION.
       FD  socios.
       01  soc-reg.
           03 soc-cod pic 999.
           03 soc-nom pic x(10).
       WORKING-STORAGE SECTION.

       01  cod-aux pic 999.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-INGRESE-NUM.
            PERFORM UNTIL cod-aux = 0
               PERFORM 400-PROCESO
               PERFORM 200-INGRESE-NUM
            END-PERFORM.
            PERFORM 500-FIN.
            STOP RUN.

           100-INICIO.
               OPEN INPUT SOCIOS.


           200-INGRESE-NUM.
               DISPLAY "Ingrese numero del socio a leer. 0 para FIN"
               ACCEPT cod-aux.
               MOVE cod-aux TO soc-cod.

           400-PROCESO.
               READ SOCIOS INVALID KEY
                               DISPLAY "Soc Inexistente"
                           NOT INVALID KEY
                               DISPLAY soc-cod, " - ", soc-nom.

           500-FIN.
               CLOSE SOCIOS.
       END PROGRAM YOUR-PROGRAM-NAME.
