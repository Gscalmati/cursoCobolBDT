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

      ********************** PANTALLA MENU PRINCIPAL *******************
       01  linea1.
           03 filler  pic x(60) value spaces.
           03 filler  pic x(7)  value "FECHA: ".
           03 l-dia   pic 99.
           03 filler  pic x     value "/".
           03 l-mes   pic 99.
           03 filler  pic xxx   value "/20".
           03 l-anio  pic 99.
           03 filler  pic xxx   value spaces.
       01  linea2.
           03 filler pic x(32)  value spaces.
           03 filler pic x(16)  value "Menu de Opciones".
           03 filler pic x(32)  value spaces.
       01  linea3.
           03 filler pic x(80)  value all "-".
       01  linea4.
           03 filler pic x(30)  value spaces.
           03 filler pic x(17)  value "1 - Leer Filiales".
           03 filler pic x(33)  value spaces.
       01  linea5.
           03 filler pic x(30)  value spaces.
           03 filler pic x(15)  value "2 - Leer Socios".
           03 filler pic x(35)  value spaces.
       01  linea6.
           03 filler pic x(30)  value spaces.
           03 filler pic x(24)  value "3 - Leer Vector Filiales".
           03 filler pic x(26)  value spaces.
       01  linea7.
           03 filler pic x(30)  value spaces.
           03 filler pic x(21)  value "4 - Mostrar Listado 1".
           03 filler pic x(29)  value spaces.
       01  linea8.
           03 filler pic x(30)  value spaces.
           03 filler pic x(21)  value "5 - Mostrar Listado 2".
           03 filler pic x(29)  value spaces.
       01  linea9.
           03 filler pic x(30)  value spaces.
           03 filler pic x(9)   value "0 - Salir".
           03 filler pic x(41)  value spaces.
       01  linea10.
           03 filler pic x(80)  value spaces.
       01  linea11.
           03 filler pic x(80)  value all "-".
       01  linea12.
           03 filler pic x(30)  value spaces.
           03 filler pic x(23)  value "Introduzca una opcion: ".
           03 filler pic x(27)  value spaces.

       77  l-cont    pic zzz9.

      ********************* PANTALLA LECTURA FILIALES ******************
       01  lin-titulos-filiales.
           03 filler pic x(6) value spaces.
           03 filler pic x(11) value "Cod. Filial".
           03 filler pic x(3) value spaces.
           03 filler pic x(15) value "Nombre Filial".
           03 filler pic x(45) value spaces.
       01  lin-detalle-filiales.
           03 filler       pic x(16) value spaces.
           03 l-fil-codigo pic 9.
           03 filler       pic x(3) value spaces.
           03 l-fil-nombre pic x(15).
           03 filler       pic x(45) value spaces.

      *********************  PANTALLA LECTURA SOCIOS *******************
       01  lin-titulos-socios.
           03 filler pic x(6) value spaces.
           03 filler pic x(11) value "Cod. Filial".
           03 filler pic x(3) value spaces.
           03 filler pic x(10) value "Nro. Socio".
           03 filler pic x(7) value spaces.
           03 filler pic x(7) value "Importe".
           03 filler pic x(7) value spaces.
           03 filler pic x(9) value "Modalidad".
           03 filler pic x(30) value spaces.
       01  lin-detalle-datos.
           03 filler       pic x(16) value spaces.
           03 l-filial     pic 9.
           03 filler       pic x(9) value spaces.
           03 l-socio      pic 9(4).
           03 filler       pic x(3) value spaces.
           03 l-importe    pic zz.zzz.zz9,99.
           03 filler       pic x(9) value spaces.
           03 l-modalidad  pic x.
           03 filler       pic x(53) value spaces.

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
           03 FILLER           PIC X(6) VALUE SPACES.
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
           03 FILLER           PIC X(3) VALUE SPACES.
           03 lin-acum-soc     PIC 9(4).
           03 FILLER           PIC X(8) VALUE SPACES.
           03 lin-fil-tot-imp  PIC z.zzz.zzz.zz9,99.
           03 FILLER           PIC X(23) VALUE SPACES.

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

      ************************* VARIABLES VECTOR ***********************
       77  i PIC 99.

       SCREEN SECTION.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-GENERAL.
           PERFORM 200-INGRESO-GENERAL.
           PERFORM UNTIL opcion = 0
               PERFORM 300-PROCESO-GENERAL
               PERFORM 200-INGRESO-GENERAL
           END-PERFORM.
           PERFORM 800-FIN-GENERAL.
           STOP RUN.

      ****************** R U T I N A S G E N E R A L E S ***************

       100-INICIO-GENERAL.
           ACCEPT fecha FROM DATE.
           PERFORM 120-INICIO-VEC-FIL.
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

       200-INGRESO-GENERAL.
           PERFORM 220-TRAIGO-OPC.
           PERFORM UNTIL opcion >= 0 AND <= 5
              display "Error, te dije un número entre 0 y 5"
              PERFORM 700-PAUSA-PANTALLA
              PERFORM 220-TRAIGO-OPC
           END-PERFORM.

       220-TRAIGO-OPC.
           PERFORM 230-MUESTRO-MENU.
           ACCEPT opcion.

       230-MUESTRO-MENU.
      *     233-TRAIGO-FECHA.
      *     DISPLAY linea1.
           DISPLAY linea2.
           DISPLAY linea3.
           DISPLAY linea4.
           DISPLAY linea5.
           DISPLAY linea6.
           DISPLAY linea7.
           DISPLAY linea8.
           DISPLAY linea9.
           DISPLAY linea10.
           DISPLAY linea11.
           DISPLAY linea12.

       233-TRAIGO-FECHA.
           MOVE dia TO l-dia.
           MOVE mes TO l-mes.
           MOVE anio TO l-anio.

       300-PROCESO-GENERAL.
           EVALUATE opcion
               WHEN 1 PERFORM 400-LEER-FILIALES
               WHEN 2 PERFORM 500-LEER-SOCIOS
               WHEN 3 PERFORM 550-LEER-VECTOR
               WHEN 4 PERFORM 600-EMITIR-LISTADO1
               WHEN 5 PERFORM 650-EMITIR-LISTADO2
           END-EVALUATE.

       800-FIN-GENERAL.

       700-PAUSA-PANTALLA.
           PERFORM 710-TRAIGO-ENTER.
           PERFORM UNTIL guarda-enter = SPACES
               display "Error, tenes que ingresar ENTER"
               PERFORM 710-TRAIGO-ENTER
           END-PERFORM.

       710-TRAIGO-ENTER.
           DISPLAY "Presione la tecla ENTER para continuar...".
           ACCEPT guarda-enter.

      ************************ R U T I N A S OPC-1 *********************
       400-LEER-FILIALES.
           PERFORM 420-INICIO-FILIALES.
           PERFORM 440-LEER-REGISTRO-FILIALES.
           PERFORM UNTIL flag = "s"
               PERFORM 460-PROCESO-FILIALES
               PERFORM 440-LEER-REGISTRO-FILIALES
           END-PERFORM.
           PERFORM 480-FIN-FILIALES.

       420-INICIO-FILIALES.
           MOVE ZERO TO cont.
           MOVE "n" TO flag.
           DISPLAY "Programa para leer filiales".
           OPEN INPUT FILIALES.
           PERFORM 425-IMPRIMO-ENCABEZADO-FILIALES.

       425-IMPRIMO-ENCABEZADO-FILIALES.
           DISPLAY lin-titulos-filiales.

       440-LEER-REGISTRO-FILIALES.
           READ FILIALES AT END MOVE "s" TO flag.

       460-PROCESO-FILIALES.
           MOVE fil-id to l-fil-codigo.
           MOVE fil-nombre to l-fil-nombre.
           display lin-detalle-filiales.
           ADD 1 TO cont.

       480-FIN-FILIALES.
           CLOSE FILIALES.
           MOVE cont TO l-cont.
           DISPLAY "Se procesaron ", l-cont, " filiales".
           DISPLAY lin-espacion-blanco.
           PERFORM 700-PAUSA-PANTALLA.


      ************************ R U T I N A S OPC-2 *********************
       500-LEER-SOCIOS.
           PERFORM 520-INICIO-SOCIOS.
           PERFORM 540-LEER-REGISTRO-SOCIOS.
           PERFORM UNTIL flag = "s"
               PERFORM 560-PROCESO-SOCIOS
               PERFORM 540-LEER-REGISTRO-SOCIOS
           END-PERFORM.
           PERFORM 580-FIN-SOCIOS.

       520-INICIO-SOCIOS.
           MOVE ZERO TO cont.
           MOVE "n" TO flag.
           DISPLAY "Programa para leer Socios".
           OPEN INPUT SOCIOS.
           PERFORM 525-IMPRIMO-ENCABEZADO-SOCIOS.

       525-IMPRIMO-ENCABEZADO-SOCIOS.
           DISPLAY lin-titulos-socios.

       540-LEER-REGISTRO-SOCIOS.
           READ SOCIOS AT END MOVE "s" TO flag.

       560-PROCESO-SOCIOS.
           MOVE soc-filial to l-filial.
           MOVE soc-socio to l-socio.
           MOVE soc-importe to l-importe.
           MOVE soc-modalidad to l-modalidad.
           display lin-detalle-datos.
           ADD 1 TO cont.

       580-FIN-SOCIOS.
           CLOSE SOCIOS.
           MOVE cont TO l-cont.
           DISPLAY "Se procesaron ", l-cont, " socios".
           DISPLAY lin-espacion-blanco.
           PERFORM 700-PAUSA-PANTALLA.

      ************************ R U T I N A S OPC-3 *********************
       550-LEER-VECTOR.
           PERFORM 300-INICIO-VEC.
           PERFORM VARYING i FROM 1 BY 1 UNTIL NOT i <= 9
               PERFORM 400-PROCESO-VEC
           END-PERFORM.
           PERFORM 700-PAUSA-PANTALLA.

       300-INICIO-VEC.
           PERFORM 425-IMPRIMO-ENCABEZADO-FILIALES.
           MOVE 1 TO i.

       400-PROCESO-VEC.
           MOVE filial-id (i) to l-fil-codigo.
           MOVE filial-nombre (i) to l-fil-nombre.
           display lin-detalle-filiales.

      ************************ R U T I N A S OPC-4 *********************

       600-EMITIR-LISTADO1.
           PERFORM 100-INICIO-GENERAL-OPC4.
           PERFORM 200-LEER-GENERAL.
           PERFORM UNTIL flag = "s"
               PERFORM 300-PROCESO-GENERAL-OPC4
           END-PERFORM.
           PERFORM 400-FIN-GENERAL.

      ********************** RUTINAS MODULO GENERAL ********************
       100-INICIO-GENERAL-OPC4.
           MOVE ZEROES TO acu-socios-gral.
           MOVE ZEROES TO  imp-gral-transf.
           MOVE ZEROES TO reg-filial-may.
           MOVE "n" TO flag.
           OPEN INPUT SOCIOS.
           PERFORM 140-IMPRIMO-ENCABEZADO-GENERAL.

       300-PROCESO-GENERAL-OPC4.
           PERFORM 320-INICIO-FILIAL-OPC4.
           PERFORM UNTIL NOT flag <> "s"
               OR NOT soc-filial = soc-filial-ant
               PERFORM 360-PROCESO-FILIAL
           END-PERFORM.
           PERFORM 390-FIN-FILIAL-OPC4.

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
      *     CLOSE FILIALES.
           CLOSE SOCIOS.
           PERFORM 700-PAUSA-PANTALLA.

      ******************** R U T I N A S MOD. FILIAL *******************

       320-INICIO-FILIAL-OPC4.
           MOVE 0 TO acu-socios-fil.
           MOVE 0 TO imp-transf-fil.
           display lin-espacion-blanco.
           PERFORM 322-ENCABEZADO-FILIAL-OPC4.
           MOVE soc-filial TO soc-filial-ant.

       322-ENCABEZADO-FILIAL-OPC4.
           PERFORM 324-TRAIGO-NOM-FILIAL.
           DISPLAY lin-titulo-cabecera-filial.
           DISPLAY lin-espacion-blanco.
           DISPLAY lin-titulo-cabecera-socios.

       324-TRAIGO-NOM-FILIAL.
           PERFORM 420-INICIO-FILIALES-OPC4.
           PERFORM 440-LEER-REGISTRO-FILIALES.
           PERFORM UNTIL NOT flag <> "s" OR NOT fil-id <> soc-filial
               PERFORM 440-LEER-REGISTRO-FILIALES
           END-PERFORM.
           PERFORM 480-FIN-FILIALES-OPC1.

       420-INICIO-FILIALES-OPC4.
           MOVE "n" TO flag.
           OPEN INPUT FILIALES.

       480-FIN-FILIALES-OPC1.
           MOVE fil-id TO lin-fil-codigo.
           MOVE fil-nombre TO lin-fil-nombre.
      *     DISPLAY lin-titulo-cabecera-filial.
      *     DISPLAY lin-espacion-blanco.
           CLOSE FILIALES.

       390-FIN-FILIAL-OPC4.
           MOVE acu-socios-fil TO lin-acum-soc.
           MOVE imp-transf-fil TO lin-fil-tot-imp.
           DISPLAY lin-guiones.
           DISPLAY lin-datos-fil.
           ADD acu-socios-fil TO acu-socios-gral.
           ADD imp-transf-fil TO imp-gral-transf.
           IF acu-socios-fil > fil-cant-may THEN
               MOVE soc-filial-ant TO fil-id-may
               MOVE lin-fil-nombre TO fil-nom-may
               MOVE acu-socios-fil to fil-cant-may
           END-IF.


      ******************** R U T I N A S MOD. SOCIOS *******************


      ************************ R U T I N A S OPC-5 *********************

      ******************** R U T I N A S MOD. GENERAL ******************

       650-EMITIR-LISTADO2.
           PERFORM 100-INICIO-GENERAL-LIST.
           PERFORM 200-LEER-GENERAL.
           PERFORM UNTIL flag = "s"
               PERFORM 300-PROCESO-GENERAL-LIST
           END-PERFORM.
           PERFORM 400-FIN-GENERAL-LIST.

      ********************** RUTINAS MODULO GENERAL ********************

       100-INICIO-GENERAL-LIST.
           MOVE ZEROES TO acu-socios-gral.
           MOVE ZEROES TO  imp-gral-transf.
           MOVE ZEROES TO reg-filial-may.
           MOVE "n" TO flag.
           OPEN INPUT FILIALES.
           OPEN INPUT SOCIOS.
           PERFORM 140-IMPRIMO-ENCABEZADO-GENERAL.

       140-IMPRIMO-ENCABEZADO-GENERAL.
           DISPLAY lin-titulo.
           DISPLAY lin-subtitulo.

       200-LEER-GENERAL.
           PERFORM 380-LEER-REGISTRO-SOCIOS.

       300-PROCESO-GENERAL-LIST.
           PERFORM 320-INICIO-FILIAL.
           PERFORM UNTIL NOT flag <> "s"
               OR NOT soc-filial = soc-filial-ant
               PERFORM 360-PROCESO-FILIAL
           END-PERFORM.
           PERFORM 390-FIN-FILIAL.

       400-FIN-GENERAL-LIST.
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
           DISPLAY lin-espacion-blanco.
           PERFORM 700-PAUSA-PANTALLA.
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
