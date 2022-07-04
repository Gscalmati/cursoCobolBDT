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
           SELECT ALUMNOS ASSIGN TO "..\alumnos.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT CURSOS ASSIGN TO "..\cursos.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT MATERIAS ASSIGN TO "..\materias.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORT-CURSOS ASSIGN TO "SORTWORK".

           SELECT LISTADO ASSIGN TO
           PRINTER, "..\impresion.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ALUMNOS.
       01  alu-reg-cab.
           03 alu-cab-tiporeg pic x(1).
           03 alu-cab-cod pic 9(6).
           03 alu-cab-nombre pic x(20).
       01  alu-reg-det.
           03 alu-det-tiporeg pic x(1).
           03 alu-det-materia pic 99.
           03 alu-det-nota pic 99.

       FD  CURSOS.
       01  cur-reg.
           03 cur-curso pic xx.
           03 cur-alumnos pic 9(6).


       FD  MATERIAS.
       01  mat-reg.
           03 mat-cod pic 99.
           03 mat-nombre pic x(20).

       FD  SORT-CURSOS.
       01  srt-reg.
           03 srt-cod pic xx.
           03 srt-alumno pic 9(6).

       FD  LISTADO
           LINAGE IS 60
           WITH FOOTING AT 58
           LINES AT TOP 3
           LINES AT BOTTOM 3.
       01  lis-reg pic x(80).

       WORKING-STORAGE SECTION.


       01  tabla-cursos.
           03 vec-cursos OCCURS 99 TIMES.
               05 vec-cant-alum pic 9999 value 0.
               05 vec-acum-notas pic 999999 value 0.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.




           STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
