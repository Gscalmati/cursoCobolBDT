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
           01  nombre PIC x(15).
           PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               DISPLAY "Hola, como te llamas?".
               ACCEPT nombre.
               DISPLAY "Un placer ", nombre, " que lindo nombre".
               STOP RUN.
           END PROGRAM YOUR-PROGRAM-NAME.
