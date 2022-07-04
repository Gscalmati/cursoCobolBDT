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
       01  pru pic x(19).
       01  tabla.
           03 campos.
               05 filler pic x(4) value "casa".
               05 filler pic x(4) value "pepe".
               05 filler pic x(4) value "rojo".
               05 filler pic x(4) value "lila".
           03  re-tabla redefines campos.
               05 vec pic x(4) occurs 4.
           03  cont pic 99 value 1.
       01  i pic 9.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           INITIALIZE pru
            perform varying i from 1 by 1 until i > 4
               STRING vec(i) delimited by SIZE
                       " " delimited by size
                   into pru
                   with pointer cont
                   on overflow display "sonamos"
                   not on overflow display "salvados"
                   DISPLAy "pru vale " pru
               END-STRING
            END-PERFORM.
            display "cont vale " cont
            display "ta tan ta tan: " pru "*"
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
