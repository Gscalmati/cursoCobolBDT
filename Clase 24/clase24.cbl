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
      *    Asi seleccionamos un archivo SECUENCIAL
           SELECT SOCIOS ASSIGN TO
           "C:\Users\Usuario\Desktop\Curso COBOL\Clase 24\arSocios.dat".
      *    Si uno no configura la ORGANIZACION, se toma como SECUENCIAL
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01 soc-reg.
           03 soc-id pic 9(4).
           03 soc-nombre pic x(15).
           03 soc-saldo pic s9(8)v99.
       WORKING-STORAGE SECTION.
       01  cont pic 999.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-ING-SOCIO.
            PERFORM UNTIL (soc-id = 0)
               PERFORM 300-ING-RESTO
               PERFORM 400-PROCESO
               PERFORM 200-ING-SOCIO
            END-PERFORM.
            PERFORM 500-FIN.
            STOP RUN.

       100-INICIO.
           MOVE ZEROES TO cont.
           DISPLAY "Grabador de Archivos Secuenciales de Salida 3000"
           OPEN OUTPUT SOCIOS.

       200-ING-SOCIO.
           DISPLAY "Ingrese num. de socio".
           ACCEPT soc-id.

       300-ING-RESTO.
           PERFORM 320-ING-NOMBRE.
           PERFORM 340-ING-SALDO.

       320-ING-NOMBRE.
           DISPLAY "Ingrese nombre de socio".
           ACCEPT soc-nombre.

       340-ING-SALDO.
           DISPLAY "Ingrese saldo de socio".
           ACCEPT soc-saldo.

       400-PROCESO.
           ADD 1 TO cont.
           WRITE soc-reg.


       500-FIN.
           CLOSE SOCIOS.
           DISPLAY "La cantidad de socios es ", cont.


       END PROGRAM YOUR-PROGRAM-NAME.
