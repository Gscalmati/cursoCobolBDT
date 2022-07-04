      ******************************************************************
      * Author:EDUARDO Y MARCELO
      * Date:09/05/2022
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIOS
               ASSIGN TO "..\socios.dat".
           SELECT FILIALES
               ASSIGN TO "..\filial1.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  FILIALES.
       01  fil-reg.
           03 fil-id     pic 9.
           03 fil-nombre pic X(15).
       FD  SOCIOS.
       01  soc-reg.
           03 soc-filial  pic 9.
           03 soc-socio pic 9(4).
           03 soc-importe  pic S9(8)V99.
           03 soc-modalidad pic X.
       WORKING-STORAGE SECTION.

      *************************  PANTALLA LISTADO **********************
       01  lin-titulo.
           03 filler pic x(30) value spaces.
           03 filler pic x(19) value "BANCO EL CORRALITO".
           03 filler pic x(31) value spaces.

       01  lin-subtitulo.
           03 filler pic x(17) value spaces.
           03 filler pic x(45) value "LISTADO DE TRANSFERENCIAS BANCARIA
      -    "S DE SOCIOS".
           03 filler pic x(18) value spaces.

       01  lin-titulo-cabecera-filial.
           03 FILLER           PIC X(26) VALUE SPACE.
           03 FILLER           PIC X(8) VALUE "FILIAL: ".
           03 lin-fil-codigo   PIC 9.
           03 FILLER           PIC XXX VALUE " - ".
           03 lin-fil-nombre   PIC X(15).
           03 FILLER           PIC X(27) VALUE SPACE.

       01  lin-titulo-cabecera-socios.
           03 FILLER PIC x(30) VALUE SPACES.
           03 FILLER PIC x(5)  VALUE "SOCIO".
           03 FILLER PIC x(8)  VALUE SPACES.
           03 FILLER PIC x(7)  VALUE "IMPORTE".
           03 FILLER PIC x(30) VALUE SPACES.

       01  lin-datos-soc.
           03 FILLER           PIC X(31) VALUE SPACES.
           03 lin-soc-socio    PIC 9(4).
           03 FILLER           PIC X(2) VALUE SPACES.
           03 lin-soc-tot-imp  PIC Z.ZZZ.ZZZ.ZZ9,99.
           03 FILLER           PIC X(27).

       01  lin-guiones.
           03 FILLER PIC X(28) VALUE SPACES.
           03 FILLER PIC X(8)  VALUE ALL "-".
           03 FILLER PIC X(5)  VALUE SPACES.
           03 FILLER PIC X(12) VALUE ALL "-".
           03 FILLER PIC X(27) VALUE SPACES.

       01  lin-datos-fil.
           03 FILLER           PIC X(20) VALUE SPACES.
           03 FILLER           PIC X(6) VALUE "Total:".
           03 FILLER           PIC X(5) VALUE SPACES.
           03 lin-acum-soc     PIC zzz9.
           03 FILLER           PIC XX VALUE SPACES.
           03 lin-fil-tot-imp  PIC z.zzz.zzz.zz9,99.
           03 FILLER           PIC X(21) VALUE SPACES.

       01  lin-espacion-blanco.
           03 FILLER PIC X(80) VALUE ALL SPACES.

       01  lin-tit-pie.
           03 FILLER PIC X(30) VALUE SPACES.
           03 FILLER PIC X(20) VALUE "ESTADISTICA GENERAL:".
           03 FILLER PIC X(30) VALUE SPACES.

       01  lin-tot-soc-gen.
           03 FILLER PIC X(5) VALUE SPACES.
           03 FILLER PIC X(25) VALUE "En total se procesaron a ".
           03 lin-tot-acum-soc PIC z.zz9.
           03 FILLER PIC X(45) VALUE SPACES.

       01  lin-tot-imp-gen.
           03 FILLER PIC X(5) VALUE SPACES.
           03 FILLER PIC X(38) VALUE "El importe general transferido fue
      -        " de ".
           03 lin-tot-imp-gene PIC z.zzz.zzz.zz9,99.
           03 FILLER PIC X(21) VALUE SPACES.

       01  lin-may-fil.
           03 FILLER PIC X(5) VALUE SPACES.
           03 FILLER PIC X(56) VALUE "La Filial que mas socios operaron
      -        "transferencias fue la ".
           03 lin-may-fil-id PIC 9.
           03 FILLER PIC X(3) VALUE " - ".
           03 lin-may-fil-nom PIC X(15).
           03 FILLER PIC X(5) VALUE " con ".
           03 lin-may-acum-soc PIC z.zz9.
           03 FILLER PIC X(8) VALUE " socios.".

      ************************ VARIABLES GENERALES *********************

       01  tabla-filiales.
           03 filial-id-nombre OCCURS 9 TIMES.
               05 filial-id pic 9.
               05 filial-nombre pic X(15).

       01  cont            PIC 9(4).
       01  opcion          PIC 9   VALUE 9.
       77  guarda-enter    PIC X.
       01  fecha.
           03 anio     PIC 99.
           03 mes      PIC 99.
           03 dia      PIC 99.
       01  flag            PIC X.
       77  acu-socios-gral PIC 9(4).
       77  imp-gral-transf PIC 9(10)V99.
       01  reg-filial-may.
           03 fil-id-may PIC 9.
           03 fil-nom-may PIC X(15).
           03 fil-cant-may pic 9(4).

      ************************* VARIABLES FILIALES *********************
       01  soc-filial-ant PIC 9.
       01  acu-socios-fil PIC 9(4).
       01  imp-transf-fil PIC 9(10)V99.


      ************************* VARIABLES SOCIOS ***********************
       01  soc-socio-ant  PIC 9(4).
       01  acu-socios-soc PIC 9(4).
       01  imp-transf-soc PIC 9(10)V99.


       SCREEN SECTION.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-GENERAL.
           PERFORM 200-LEER-GENERAL.
           PERFORM UNTIL flag = "s"
               PERFORM 300-PROCESO-GENERAL
           END-PERFORM.
           PERFORM 400-FIN-GENERAL.
           STOP RUN.

      ********************** RUTINAS MODULO GENERAL ********************
       100-INICIO-GENERAL.
           PERFORM 120-INICIO-VEC-FIL.
           MOVE ZEROES TO acu-socios-gral.
           MOVE ZEROES TO  imp-gral-transf.
           MOVE ZEROES TO reg-filial-may.
           MOVE "n" TO flag.
           OPEN INPUT FILIALES.
           OPEN INPUT SOCIOS.
           PERFORM 140-IMPRIMO-ENCABEZADO-GENERAL.

       120-INICIO-VEC-FIL.
           PERFORM 123-INICIO-FILIALES.
           PERFORM 126-LEER-REGISTRO-FILIALES.
           PERFORM UNTIL flag = "s"
               PERFORM 129-PROCESO-FILIALES
               PERFORM 126-LEER-REGISTRO-FILIALES
           END-PERFORM.
           PERFORM 132-FIN-FILIALES.

       123-INICIO-FILIALES.
           MOVE ZERO TO cont.
           MOVE "n" TO flag.
           OPEN INPUT FILIALES.

       126-LEER-REGISTRO-FILIALES.
           READ FILIALES AT END MOVE "s" TO flag.

       129-PROCESO-FILIALES.
           MOVE fil-id to filial-id (fil-id).
           MOVE fil-nombre to filial-nombre(fil-id).

       132-FIN-FILIALES.
           CLOSE FILIALES.

       140-IMPRIMO-ENCABEZADO-GENERAL.
           DISPLAY lin-titulo.
           DISPLAY lin-subtitulo.

       200-LEER-GENERAL.
           PERFORM 380-LEER-REGISTRO-SOCIOS.

       300-PROCESO-GENERAL.
           PERFORM 320-INICIO-FILIAL.
           PERFORM UNTIL NOT flag <> "s"
               OR NOT soc-filial = soc-filial-ant
               PERFORM 360-PROCESO-FILIAL
           END-PERFORM.
           PERFORM 390-FIN-FILIAL.

       400-FIN-GENERAL.
           MOVE acu-socios-gral TO lin-tot-acum-soc.
           MOVE imp-gral-transf TO lin-tot-imp-gene.
           MOVE fil-id-may TO lin-may-fil-id.
           MOVE fil-nom-may TO lin-may-fil-nom.
           MOVE fil-cant-may TO lin-may-acum-soc.
           DISPLAY lin-espacion-blanco.
           DISPLAY lin-tit-pie.
           DISPLAY lin-tot-soc-gen.
           DISPLAY lin-tot-imp-gen.
           DISPLAY lin-may-fil.
           CLOSE FILIALES.
           CLOSE SOCIOS.

      ******************** R U T I N A S MOD. FILIAL *******************

       320-INICIO-FILIAL.
           MOVE 0 TO acu-socios-fil.
           MOVE 0 TO imp-transf-fil.
           display lin-espacion-blanco.
           PERFORM 322-ENCABEZADO-FILIAL.
           MOVE soc-filial TO soc-filial-ant.

       322-ENCABEZADO-FILIAL.
           MOVE filial-id(soc-filial) TO lin-fil-codigo.
           MOVE filial-nombre(soc-filial) TO lin-fil-nombre.
           DISPLAY lin-titulo-cabecera-filial.
           DISPLAY lin-titulo-cabecera-socios.

       360-PROCESO-FILIAL.
           PERFORM 362-INICIO-SOCIO.
           PERFORM UNTIL NOT flag <> "s"
               OR NOT soc-socio = soc-socio-ant
               OR NOT soc-filial = soc-filial-ant
               PERFORM 365-PROCESO-SOCIO
               PERFORM 380-LEER-REGISTRO-SOCIOS
           END-PERFORM.
           PERFORM 375-FIN-SOCIO.

       380-LEER-REGISTRO-SOCIOS.
           READ SOCIOS AT END MOVE "s" TO flag.

       390-FIN-FILIAL.
           MOVE acu-socios-fil TO lin-acum-soc.
           MOVE imp-transf-fil TO lin-fil-tot-imp.
           DISPLAY lin-guiones.
           DISPLAY lin-datos-fil.
           ADD acu-socios-fil TO acu-socios-gral.
           ADD imp-transf-fil TO imp-gral-transf.
           IF acu-socios-fil > fil-cant-may THEN
               MOVE soc-filial-ant TO fil-id-may
               MOVE filial-nombre(soc-filial-ant)TO fil-nom-may
               MOVE acu-socios-fil to fil-cant-may
           END-IF.

      ******************** R U T I N A S MOD. SOCIOS *******************

       362-INICIO-SOCIO.
           MOVE 0 TO imp-transf-soc.
           MOVE soc-socio TO soc-socio-ant.

       365-PROCESO-SOCIO.
           IF soc-modalidad = "T" THEN
               ADD soc-importe to imp-transf-soc
           END-IF.

       375-FIN-SOCIO.
           IF imp-transf-soc > 0 THEN
               MOVE soc-socio-ant TO lin-soc-socio
               MOVE imp-transf-soc TO lin-soc-tot-imp
               DISPLAY lin-datos-soc
               ADD 1 TO acu-socios-fil
               ADD imp-transf-soc TO imp-transf-fil
           END-IF.

       END PROGRAM YOUR-PROGRAM-NAME.
