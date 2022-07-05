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
           SELECT FILIALES-SEQ
           ASSIGN TO '..\Archivos\filiales.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT FILIALES-INDEX
           ASSIGN TO '..\Archivos\filiales.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS fil-cod.

       DATA DIVISION.
       FILE SECTION.
       FD  FILIALES-INDEX.
       01  fil-reg.
           03 fil-cod pic 9999.
           03 fil-nom pic x(20).

       FD  FILIALES-SEQ.
       01  fil-seq-reg.
           03 fil-seq-cod pic 9999.
           03 fil-seq-nom pic x(20).
       WORKING-STORAGE SECTION.
      *********variables de trabajo *******************************************
       77  w-eof-filial pic 9 value 0.
       PROCEDURE DIVISION.
           PERFORM 100-INICIO-GENERAL.
           PERFORM 200-LEO-FILIAL-SEQ.
           PERFORM UNTIL w-eof-filial IS EQUALS 1
               PERFORM 300-ARMO-FIL-REG
               PERFORM 350-ESCRIBO-FIL-INDEX
               PERFORM 200-LEO-FILIAL-SEQ
           END-PERFORM.
           PERFORM 400-FIN-GENERAL.
       MAIN-PROCEDURE.
            STOP RUN.


      *********INICIO RUTINAS *******************************************
       100-INICIO-GENERAL.
           PERFORM 120-ABRIR-ARCHIVOS.

       120-ABRIR-ARCHIVOS.
           OPEN INPUT FILIALES-SEQ.
           OPEN OUTPUT FILIALES-INDEX.

       200-LEO-FILIAL-SEQ.
           READ FILIALES-SEQ AT END MOVE 1 TO w-eof-filial.

       300-ARMO-FIL-REG.
           MOVE fil-seq-reg TO fil-reg.

       350-ESCRIBO-FIL-INDEX.
           WRITE fil-reg INVALID KEY
                         DISPLAY "ERROR AL ESCRIBIR EL REGISTRO".
       400-FIN-GENERAL.
           PERFORM 420-CERRAR-ARCHIVOS.

       420-CERRAR-ARCHIVOS.
           CLOSE FILIALES-SEQ.
           CLOSE FILIALES-INDEX.

       END PROGRAM Ejercicio_Sala2.
