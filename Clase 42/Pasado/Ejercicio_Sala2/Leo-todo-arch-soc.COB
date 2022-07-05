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
           SELECT SOCIOS
           ASSIGN TO '..\Archivos\socios.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-llave.
               05 soc-filial pic 9999.
               05 soc-cod pic 9999.
           03 soc-nom pic x(20).
           03 soc-saldo pic s9(7)v99.

       WORKING-STORAGE SECTION.
      *********variables de trabajo *******************************************
       77  w-eof-socio pic 9 value 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 200-LEO-SOCIO-SEQ.
            PERFORM UNTIL w-eof-socio IS EQUALS 1
               PERFORM 300-MUESTRO-SOCIO
               PERFORM 200-LEO-SOCIO-SEQ
            END-PERFORM.
            PERFORM 400-FIN-GENERAL.
            STOP RUN.


      *********INICIO RUTINAS *******************************************
       100-INICIO-GENERAL.
           PERFORM 120-ABRIR-ARCHIVOS.

       120-ABRIR-ARCHIVOS.
           OPEN INPUT SOCIOS.

       200-LEO-SOCIO-SEQ.
           READ SOCIOS AT END MOVE 1 TO w-eof-socio.

       300-MUESTRO-SOCIO.
           DISPLAY soc-filial,", ",soc-cod,", ",soc-nom,", ",soc-saldo.

       400-FIN-GENERAL.
           PERFORM 420-CERRAR-ARCHIVOS.

       420-CERRAR-ARCHIVOS.
           CLOSE SOCIOS.

       END PROGRAM Ejercicio_Sala2.
