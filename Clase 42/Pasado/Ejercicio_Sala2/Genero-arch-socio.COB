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
           SELECT SOCIOS-SEQ
           ASSIGN TO '..\Archivos\socios.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SOCIOS-INDEX
           ASSIGN TO '..\Archivos\socios.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS soc-llave.

       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS-INDEX.
       01  soc-reg.
           03 soc-llave.
               05 soc-filial pic 9999.
               05 soc-cod pic 9999.
           03 soc-nom pic x(20).
           03 soc-saldo pic s9(7)v99.

       FD  SOCIOS-SEQ.
       01  soc-seq-reg.
           03 soc-seq-llave.
               05 soc-seq-filial pic 9999.
               05 soc-seq-cod pic 9999.
           03 soc-seq-nom pic x(20).
           03 soc-seq-saldo pic s9(7)v99.

       WORKING-STORAGE SECTION.
      *********variables de trabajo *******************************************
       77  w-eof-socio pic 9 value 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 200-LEO-SOCIO-SEQ.
            PERFORM UNTIL w-eof-socio IS EQUALS 1
               PERFORM 300-ARMO-SOC-SEQ-REG
               PERFORM 350-ESCRIBO-SOC-INDEX
               PERFORM 200-LEO-SOCIO-SEQ
            END-PERFORM.
            PERFORM 400-FIN-GENERAL.
            STOP RUN.


      *********INICIO RUTINAS *******************************************
       100-INICIO-GENERAL.
           PERFORM 120-ABRIR-ARCHIVOS.

       120-ABRIR-ARCHIVOS.
           OPEN INPUT SOCIOS-SEQ.
           OPEN OUTPUT SOCIOS-INDEX.

       200-LEO-SOCIO-SEQ.
           READ SOCIOS-SEQ AT END MOVE 1 TO w-eof-socio.

       300-ARMO-SOC-SEQ-REG.
           MOVE soc-seq-reg TO soc-reg.

       350-ESCRIBO-SOC-INDEX.
           WRITE soc-reg INVALID KEY
                         DISPLAY "ERROR AL ESCRIBIR EL REGISTRO".
       400-FIN-GENERAL.
           PERFORM 420-CERRAR-ARCHIVOS.

       420-CERRAR-ARCHIVOS.
           CLOSE SOCIOS-SEQ.
           CLOSE SOCIOS-INDEX.

       END PROGRAM Ejercicio_Sala2.
