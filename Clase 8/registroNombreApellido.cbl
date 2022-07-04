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
           01  nombre.
           03  nom PIC x(10).
           03 ape PIC x(10) .
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
               DISPLAY "Inserte nombre"
               ACCEPT nom
               DISPLAY "Inserte apellido"
               ACCEPT ape
               DISPLAY "Un gusto enorme, ", nombre
               STOP RUN.
           END PROGRAM YOUR-PROGRAM-NAME.
