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
           SELECT SOCIOS ASSIGN TO "..\sociosIndex.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS reg-codigos.

           SELECT FILIALES ASSIGN TO "..\filialesIndex.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS fil-cod.
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 reg-codigos.
               05 soc-fil pic 9.
               05 soc-cod pic 999.
           03 soc-nom pic x(15).
           03 soc-saldo pic s9(6)v99.

       FD  FILIALES.
       01  fil-reg.
           03 fil-cod pic 99.
           03 fil-nom pic x(15).

       WORKING-STORAGE SECTION.
       01  ing-fil pic 99.
       01  flag pic 9.
       01  filActual pic 99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-PEDIR-FILIAL.
            PERFORM UNTIL ing-fil = 0
               PERFORM 300-MOSTRAR-FILIAL
               PERFORM 200-PEDIR-FILIAL
            END-PERFORM.
            PERFORM 400-FINAL.
           STOP RUN.

           100-INICIO.
               OPEN INPUT FILIALES.
               OPEN INPUT SOCIOS.

           200-PEDIR-FILIAL.
               DISPLAY "Inserte Numero de Filial. Para fin ingrese 0"
               ACCEPT ing-fil.
               MOVE 0 TO flag.
               PERFORM UNTIL flag = 1 OR ing-fil = 0
                   PERFORM 210-VALIDAR-FILIAL
               END-PERFORM.

           210-VALIDAR-FILIAL.
               MOVE ing-fil TO fil-cod.
               READ FILIALES
                   INVALID KEY
                      DISPLAY "Filial no Encontrada"
                      DISPLAY "Inserte una Filial Existente. 0 para fin"
                      ACCEPT ing-fil
                   NOT INVALID KEY
                      MOVE 1 TO flag.




           300-MOSTRAR-FILIAL.
               PERFORM 310-INICIO-SOCIOS.
               DISPLAY reg-codigos.
               START SOCIOS KEY IS = reg-codigos
                   INVALID KEY
                       DISPLAY "No hay Socios para esta Filial"
                       EXIT
               END-START.
               DISPLAY "FILIAL N ", fil-cod, " - ", fil-nom.
               PERFORM 340-ACT-FIL.
               PERFORM 360-LEER-SOC.
               PERFORM UNTIL filActual <> soc-fil OR flag = 1
                   PERFORM 380-IMPRIMIR-SOCIO
                   PERFORM 360-LEER-SOC
               END-PERFORM.



           310-INICIO-SOCIOS.
               MOVE fil-cod TO soc-fil.
               MOVE ZEROES TO soc-cod.
               MOVE ZEROES TO flag.

           340-ACT-FIL.
               MOVE fil-cod TO filActual.

           360-LEER-SOC.
               READ SOCIOS NEXT AT END MOVE 1 TO flag.

           380-IMPRIMIR-SOCIO.
               DISPLAY soc-cod, " - ",
               soc-fil, " - ",
               soc-nom, " - $", soc-saldo.

           400-FINAL.
               CLOSE SOCIOS.
               CLOSE FILIALES.

       END PROGRAM YOUR-PROGRAM-NAME.
