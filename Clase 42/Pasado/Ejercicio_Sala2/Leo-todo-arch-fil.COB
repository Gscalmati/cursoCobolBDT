      ******************************************************************
      * Author: Ezequiel Galluzo, Matías Medela, Cecilia Olmos,
      *         Martin Perez
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Ejercicio_Sala2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILIALES
           ASSIGN TO '..\Archivos\filiales.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILIALES.
       01  fil-reg.
           03 fil-cod pic 9999.
               88 fin-prog value 0.
           03 fil-nom pic x(20).

       WORKING-STORAGE SECTION.
      *********variables de trabajo *******************************************
       77  w-eof-filial pic 9 value 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 200-LEO-FILIAL.
            PERFORM UNTIL w-eof-filial IS EQUALS 1
               PERFORM 300-MUESTRO-FILIAL
               PERFORM 200-LEO-FILIAL
            END-PERFORM.
            PERFORM 400-FIN-GENERAL.
            STOP RUN.


      *********INICIO RUTINAS *******************************************
       100-INICIO-GENERAL.
           PERFORM 120-ABRIR-ARCHIVOS.

       120-ABRIR-ARCHIVOS.
           OPEN INPUT FILIALES.

       200-LEO-FILIAL.
           READ FILIALES AT END MOVE 1 TO w-eof-filial.

       300-MUESTRO-FILIAL.
           DISPLAY fil-cod, ", ", fil-nom.

       400-FIN-GENERAL.
           PERFORM 420-CIERRO-ARCHIVOS.

       420-CIERRO-ARCHIVOS.
           CLOSE FILIALES.
       END PROGRAM Ejercicio_Sala2.
