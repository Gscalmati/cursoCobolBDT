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
           SELECT FILIALES ASSIGN TO
           "..\filiales.dat".
      *    Si uno no configura la ORGANIZACION, se toma como SECUENCIAL
       DATA DIVISION.
       FILE SECTION.
       FD  FILIALES.
       01  fil-reg.
           03 fil-codigo pic 99.
           03 fil-nombre pic x(15).
       WORKING-STORAGE SECTION.
       01  cont pic 999.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-ING-FILIAL.
            PERFORM UNTIL (fil-codigo = 0)
               PERFORM 300-ING-RESTO
               PERFORM 400-PROCESO
               PERFORM 200-ING-FILIAL
            END-PERFORM.
            PERFORM 500-FIN.
            STOP RUN.

       100-INICIO.
           MOVE ZEROES TO cont.
           DISPLAY "Grabador de Archivos Secuenciales de Salida 3000"
           OPEN OUTPUT FILIALES.

       200-ING-FILIAL.
           DISPLAY "Ingrese Numero de Filial".
           ACCEPT fil-codigo.

       300-ING-RESTO.
           PERFORM 320-ING-NOMBRE.


       320-ING-NOMBRE.
           DISPLAY "Ingrese nombre de filial".
           ACCEPT fil-nombre.


       400-PROCESO.
           ADD 1 TO cont.
           WRITE fil-reg.


       500-FIN.
           CLOSE FILIALES.
           DISPLAY "La cantidad de filiales es ", cont.


       END PROGRAM YOUR-PROGRAM-NAME.
