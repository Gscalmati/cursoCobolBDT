      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  acu-monto pic 9(6).
       01  plata pic s9(5).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            perform inicio.
            perform ingreso.
            perform desarrollo until plata = 0.
            perform fin.
            STOP RUN.

       inicio.
           display "Programa para juntar plata para Gise"
           move zeros to acu-monto.

       ingreso.
           DISPLAY "ingrese cuanto quiere aportar para Gise"
           perform ingreso-controlado.

       fin.
           display "Lo que se ha recolectado es: ", acu-monto, "pesos".

       ingreso-controlado.
           accept plata.
           perform ingcont until plata >= 0.

       desarrollo.
           perform proceso.
           perform ingreso.

       proceso.
           add plata to acu-monto.

       ingcont.
           display "Ehh calmate, le estas choreando a Gise"
           accept plata.


       END PROGRAM YOUR-PROGRAM-NAME.
