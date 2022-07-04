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
       01  tabla.
           03 filler pic x(9) value "Lunes".
           03 filler pic x(9) value "Martes".
           03 filler pic x(9) value "Miércoles".
           03 filler pic x(9) value "Jueves".
           03 filler pic x(9) value "Viernes".
           03 filler pic x(9) value "Sábado".
           03 filler pic x(9) value "Domingo".
       01  re-tabla redefines tabla.
           03 vec pic x(9) occurs 7 TIMES indexed by n.
       77  i pic 9.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            SEARCH vec VARYING i
               at END
                  display "No ta"
               when "Martes" = vec(n)
                   display "Si!!!", vec(n)
           END-SEARCH.
               display n
               display i
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
