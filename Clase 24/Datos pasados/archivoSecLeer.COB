      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *INPUT-OUTPUT SECTION.: seccion de entrada/salida -> control de archivos
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIOS
           ASSIGN TO
           "C:\Users\nlembo\OneDrive - Edenor\Escritorio\COBOL\soc.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01 soc-reg.
           03 soc-id     PIC 9(4).
           03 soc-nombre PIC x(15).
           03 soc-saldo  PIC s9(8).
       WORKING-STORAGE SECTION.
       01  cont pic 999 VALUE ZEROS.
       01  flag PIC x VALUE "n".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
     **************************PROGRAMA PRINCIPAL**********************
           PERFORM 100-INICIO.
           PERFORM 200-LEER-REGISTRO.
           PERFORM UNTIL flag = "s"
               PERFORM 400-PROCESO
               PERFORM 200-LEER-REGISTRO
           END-PERFORM.
               PERFORM 500-FIN.
      *****************************RUTINAS*****************************
           100-INICIO.
               DISPLAY "Programa para leer un archivo de entrada".
               OPEN INPUT SOCIOS.

           200-LEER-REGISTRO.
              READ SOCIOS AT END MOVE "s" TO flag.

           400-PROCESO.
               DISPLAY soc-id "," soc-nombre, ",", soc-saldo.
               ADD 1 TO cont.
           500-FIN.
              CLOSE SOCIOS.
              DISPLAY "Se procesaron ", cont, " socios".
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
