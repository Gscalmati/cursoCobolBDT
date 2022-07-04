      ******************************************************************
      * Author:SALA 5
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
               ASSIGN TO "D:\Datos\Cobol\2022-05-09\socios5.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-id     PIC 9(4).
           03 soc-nombre PIC X(15).
           03 soc-saldo  PIC S9(8)V99.
       WORKING-STORAGE SECTION.
       01  linea1.
           03 filler  pic x(60)    value spaces.
           03 filler  pic x(7)     value "FECHA: ".
           03 l-dia   pic 99.
           03 filler  pic x        value "/".
           03 l-mes   pic 99.
           03 filler  pic xxx      value "/20".
           03 l-anio  pic 99.
           03 filler  pic xxx      value spaces.
       01  linea2.
           03 filler pic x(32)     value spaces.
           03 filler pic x(16)     value "Menu de Opciones".
           03 filler pic x(32)     value spaces.
       01  linea3.
           03 filler pic x(80)     value all "-".
       01  linea4.
           03 filler pic x(30)     value spaces.
           03 filler pic x(17)     value "1 - Cargar Socios".
           03 filler pic x(33)     value spaces.
       01  linea5.
           03 filler pic x(30)     value spaces.
           03 filler pic x(22)     value "2 - Agregar Socios".
           03 filler pic x(28)     value spaces.
       01  linea6.
           03 filler pic x(30)     value spaces.
           03 filler pic x(15)     value "3 - Leer Socios".
           03 filler pic x(35)     value spaces.
       01  linea7.
           03 filler pic x(30)     value spaces.
           03 filler pic x(9)      value "0 - Salir".
           03 filler pic x(41)     value spaces.
       01  linea8.
           03 filler pic x(80)     value spaces.
       01  linea9.
           03 filler pic x(80)     value all "-".
       01  linea10.
           03 filler pic x(30)     value spaces.
           03 filler pic x(23)     value "Introduzca una opcion: ".
           03 filler pic x(27)     value spaces.

       01  lin-titulos-socios.
           03 filler pic x(6) value spaces.
           03 filler pic x(10) value "Nro. Socio".
           03 filler pic x(3) value spaces.
           03 filler pic x(17) value "Nombre y Apellido".
           03 filler pic x(8) value spaces.
           03 filler pic x(6) value "Saldo".
           03 filler pic x(30) value spaces.
       01  lin-detalle-datos.
           03 filler   pic x(12) value spaces.
           03 l-socio  pic 9(4).
           03 filler   pic x(5) value spaces.
           03 l-nombre pic x(15).
           03 filler   pic x(3) value spaces.
           03 l-saldo  pic zz.zzz.zz9,99.
           03 filler   pic x(31) value spaces.

       01  cont            PIC 999.
       01  flag            PIC X.
       01  opcion          PIC 9   VALUE 9.
       77  guarda-enter    PIC X.
       01  fecha.
           03 anio     PIC 99.
           03 mes      PIC 99.
           03 dia      PIC 99.

       SCREEN SECTION.
      * 01  PANTALLA-MENU.
      *     03 BLANK SCREEN.
      *     03 LINE 1  COLUMN 60 VALUE "FECHA ".
      *     03 LINE 1  COLUMN 67 PIC 99 FROM dia.
      *     03 LINE 1  COLUMN 69 VALUE "/".
      *     03 LINE 1  COLUMN 70 PIC 99 FROM mes.
      *     03 LINE 1  COLUMN 72 VALUE "/".
      *     03 LINE 1  COLUMN 73 PIC 99 FROM anio.
      *     03 LINE 4  COLUMN 28 REVERSE VALUE "MENU DE OPCIONES".
      *     03 LINE 6  COLUMN 1  PIC X(80) FROM subrayado.
      *     03 LINE 8  COLUMN 28 REVERSE VALUE " 1 ".
      *     03 LINE 8  COLUMN 33 VALUE "CARGAR ARCHIVO".
      *     03 LINE 10 COLUMN 28 REVERSE VALUE " 2 ".
      *     03 LINE 10 COLUMN 33 VALUE " AGREGAR DATOS".
      *     03 LINE 12 COLUMN 28 REVERSE VALUE " 3 ".
      *     03 LINE 12 COLUMN 33 VALUE "LEER ARCHIVO".
      *     03 LINE 14 COLUMN 1  PIC X(80) FROM subrayado.
      *     03 LINE 16 COLUMN 30 BLINK VALUE "INTRODUZCA OPCION?".
      *     03 LINE 16 COLUMN 50 PIC 9 TO opcion.
      * 01  select-opcion line 17 column 30 pic 9 USING opcion.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-GENERAL.
           PERFORM 200-INGRESO-GENERAL.
           PERFORM UNTIL opcion = 0
               PERFORM 300-PROCESO-GENERAL
               PERFORM 200-INGRESO-GENERAL
           END-PERFORM.
           PERFORM 800-FIN-GENERAL.

      ****************** R U T I N A S G E N E R A L E S ***************

       100-INICIO-GENERAL.
           ACCEPT fecha FROM DATE.

       200-INGRESO-GENERAL.
           PERFORM 220-TRAIGO-OPC.
           PERFORM UNTIL opcion >= 0 AND <= 3
              display "Error, te dije un número entre 0 y 3"
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

       233-TRAIGO-FECHA.
           MOVE dia TO l-dia.
           MOVE mes TO l-mes.
           MOVE anio TO l-anio.

       300-PROCESO-GENERAL.
           IF opcion = 1
               PERFORM 400-CARGAR-SOCIOS
           ELSE
               IF opcion = 2
                   PERFORM 500-AGREGAR-SOCIOS
               ELSE
                   IF opcion = 3
                       PERFORM 600-LEER-SOCIOS
               END-IF
           END-IF.

       800-FIN-GENERAL.
           STOP RUN.

       440-ING-SOCIO.
           PERFORM 445-TRAIGO-SOCIO.
           PERFORM UNTIL soc-id >= 0
              display "Error, te dije un numero positivo, x fin 0"
              PERFORM 445-TRAIGO-SOCIO
           END-PERFORM.

       445-TRAIGO-SOCIO.
           display "Ingrese el nro. de socio (> 0), por fin ingrese 0".
           accept soc-id.

       460-INGRESO-RESTO.
           PERFORM 465-ING-NOMBRE.
           PERFORM 470-ING-SALDO.

       465-ING-NOMBRE.
           PERFORM 466-TRAIGO-NOMBRE.
           PERFORM UNTIL soc-nombre not = SPACES
              display "Error, tenés que ingresar algo"
              PERFORM 466-TRAIGO-NOMBRE
           END-PERFORM.

       466-TRAIGO-NOMBRE.
           display "Ingrese el nombre del socio ", soc-id
           accept soc-nombre.

       470-ING-SALDO.
           DISPLAY "Ingrese el saldo"
           ACCEPT soc-saldo.

       480-PROCESO.
           ADD 1 TO cont.
           WRITE soc-reg.

       499-FIN.
           CLOSE SOCIOS.
           DISPLAY "se procesaron ", cont, " socios".
           PERFORM 700-PAUSA-PANTALLA.

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

       400-CARGAR-SOCIOS.
           PERFORM 420-INICIO.
           PERFORM 440-ING-SOCIO.
           PERFORM UNTIL soc-id = 0
             PERFORM 460-INGRESO-RESTO
             PERFORM 480-PROCESO
             PERFORM 440-ING-SOCIO
           END-PERFORM.
           PERFORM 499-FIN.

       420-INICIO.
           MOVE ZERO TO cont.
           DISPLAY "Programa para grabar un archivo de salida".
           OPEN OUTPUT SOCIOS.

      ************************ R U T I N A S OPC-2 *********************

       500-AGREGAR-SOCIOS.
           PERFORM 520-INICIO.
           PERFORM 440-ING-SOCIO
           PERFORM UNTIL soc-id = 0
             PERFORM 460-INGRESO-RESTO
             PERFORM 480-PROCESO
             PERFORM 440-ING-SOCIO
           END-PERFORM.
           PERFORM 499-FIN.

       520-INICIO.
           MOVE ZERO TO cont.
           DISPLAY "Programa para grabar un archivo de salida".
           OPEN EXTEND SOCIOS.

      ************************ R U T I N A S OPC-3 *********************

       600-LEER-SOCIOS.
           PERFORM 620-INICIO.
           PERFORM 640-LEER-REGISTRO.
           PERFORM UNTIL flag = "s"
               PERFORM 660-PROCESO
               PERFORM 640-LEER-REGISTRO
           END-PERFORM.
           PERFORM 499-FIN.

       620-INICIO.
           MOVE ZERO TO cont.
           MOVE "n" TO flag.
           DISPLAY "Programa para leer un archivo de entrada".
           OPEN INPUT SOCIOS.
           PERFORM 625-IMPRIMO-ENCABEZADO.

       625-IMPRIMO-ENCABEZADO.
           DISPLAY lin-titulos-socios.

       640-LEER-REGISTRO.
           READ SOCIOS AT END MOVE "s" TO flag.

       660-PROCESO.
           MOVE soc-id to l-socio.
           MOVE soc-nombre to l-nombre.
           MOVE soc-saldo to l-saldo.
           display lin-detalle-datos.
           ADD 1 TO cont.

       END PROGRAM YOUR-PROGRAM-NAME.
