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
       01  campos.
           03 campo1 pic x(10) value "casa nueva".
           03 campo2 pic x(4) value "rojo".
       01  res redefines campos pic x(14).
       01  salida-campo pic x(15).
       01  cont pic 99 value 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           display "res vale " res
           string campo1 DELIMITED by spaces
                   "-" DELIMITED by size
                  campo2 DELIMITED BY SIZE
                  into salida-campo
                  with POINTER cont
                  on OVERFLOW display "sonamos"
                  not on OVERFLOW display "estamos salvados"
           END-STRING
           display "la salida es " salida-campo.
           display "cont vale " cont
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
