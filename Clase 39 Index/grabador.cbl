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
            PERFORM 200-INGRESO.
            PERFORM UNTIL cod-aux = 0
               PERFORM 300-INGRESO-RESTO
               PERFORM 400-PROCESO
               PERFORM 200-INGRESO
            END-PERFORM.
            PERFORM 500-FIN.
            STOP RUN.

           100-INICIO.
               OPEN OUTPUT SOCIOS.

           200-INGRESO.
               DISPLAY "Ingrese codigo del socio. 0 para fin."
               ACCEPT cod-aux.
               MOVE cod-aux TO soc-cod.

           300-INGRESO-RESTO.
               DISPLAY "Ingrese nombre del socio."
               ACCEPT soc-nom.

           400-PROCESO.
               WRITE soc-reg INVALID KEY
                   DISPLAY "ERROR Grabacion impedida".

           500-FIN.
               CLOSE SOCIOS.
       END PROGRAM YOUR-PROGRAM-NAME.
