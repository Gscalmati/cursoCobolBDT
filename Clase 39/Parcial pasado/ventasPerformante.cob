      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXAMEN-NEGOCIO-VENTAS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VENTAS ASSIGN TO "..\ventas.txt"
               ORGANIZATION is line SEQUENTIAL.
           SELECT FACTURAS ASSIGN TO "..\facturas.txt"
               ORGANIZATION is line SEQUENTIAL.
           SELECT ARTICULOS ASSIGN TO "..\articulos.txt"
               ORGANIZATION is line SEQUENTIAL.
           SELECT VENTAS-SORT ASSIGN TO "SORTWORK".
           SELECT LISTADO ASSIGN TO
               PRINTER, "..\impre.txt"
               ORGANIZATION is line SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  VENTAS.
       01  ven-reg.
           03 ven-fecha            pic 9(8).
           03 ven-factura          pic 9(8).
       FD  FACTURAS.
       01  fac_reg_cab.
           03 fac_cab_tiporeg      pic X.
           03 fac_cab_factura      pic 9(8).
       01  fac_reg_det.
           03 fac_det_tiporeg      pic X.
           03 fac_det_art          pic 99.
           03 fac_det_cant         pic 999.
       FD  ARTICULOS.
       01  art_reg.
           03 art_cod              pic 99.
           03 art_nombre           pic X(20).
       SD  VENTAS-SORT.
       01  srt-ven-reg.
           03 srt-ven-fecha        pic 9(8).
           03 srt-ven-factura      pic 9(8).
       FD  LISTADO
           LINAGE is 61 LINES
               with FOOTING at 59
               lines at top 3
               lines at bottom 3.
       01  lis-reg pic x(80).
      *
       WORKING-STORAGE SECTION.
       77  flagVentasSort          pic X.
       77  flagArticulos           pic X.
       77  flagFacturas            pic X.
       77  cod                     pic 999.
       01  fac_cab_factura_ant     pic 9(8).
       01  srt-fac_det_art_ant     pic 99.
       01  w-cont-paginas          pic 99.
       01  vec-cant-art.
           03 acu-cant-art         pic 9999 OCCURS 99 TIMES.
      *
      ************************* LINEAS DE IMPRESION ********************
      *
       01  cabecera1.
           03 filler       pic x(2).
           03 filler       pic x(55) value "LISTADO DE ARTICULOS DE LAS
      -         "FACTURAS EMITIDAS EN VENTAS".
           03 filler       pic x(5)  value spaces.
           03 filler       pic x(13) value "NRO. DE PAG. ".
           03 l-nro-pag    pic 99.
           03 filler       pic x(3).
       01  cabecera2.
           03 filler       pic x(80) value all "-".
       01  cabecera3.
           03 filler       pic x(18) value spaces.
           03 filler       pic x(6)  value "CODIGO".
           03 filler       pic x(12)  value spaces.
           03 filler       pic x(6)  value "NOMBRE".
           03 filler       pic x(12)  value spaces.
           03 filler       pic x(8)  value "CANTIDAD".
           03 filler       pic x(18) value spaces.
       01  cabecera4.
           03 filler       pic x(18) value spaces.
           03 filler       pic x(6)  value all "-".
           03 filler       pic x(12)  value spaces.
           03 filler       pic x(6)  value all "-".
           03 filler       pic x(12)  value spaces.
           03 filler       pic x(8)  value all "-".
           03 filler       pic x(18) value spaces.
       01  detalle.
           03 filler       pic x(20) value spaces.
           03 l-cod        pic x(2)  value spaces.
           03 filler       pic x(7)  value spaces.
           03 l-nombre     pic x(20).
           03 filler       pic x(5)  value spaces.
           03 l-cantidad   pic z.zz9.
           03 filler       pic x(22) value spaces.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           SORT VENTAS-SORT
               ASCENDING KEY srt-ven-factura
               USING VENTAS
               OUTPUT PROCEDURE IS 0100-OUTPUT-PROCEDURE.
      *
            STOP RUN.
      *
      ********************** RUTINAS OUTPUT PRECEDURE ******************
      *
       0100-OUTPUT-PROCEDURE.
           PERFORM 0110-INICIO-GENERAL.
           PERFORM 0120-LEER-REGISTRO-VENTAS-SORT.
           PERFORM 0130-LEER-REGISTRO-FACTURAS.
           PERFORM UNTIL flagVentasSort = "s"
               PERFORM 0140-INICIO-CAB-C
               PERFORM UNTIL flagVentasSort = "s" OR flagFacturas = "s"
                   OR srt-ven-factura <> fac_cab_factura
                       PERFORM 0150-INICIO-CAB-D
                       PERFORM 0130-LEER-REGISTRO-FACTURAS
                       PERFORM UNTIL flagFacturas = "s"
                           OR fac_det_tiporeg <> "D"
                           PERFORM 0160-ACUMULO-VECTOR
                           PERFORM 0130-LEER-REGISTRO-FACTURAS
                       END-PERFORM
                       PERFORM 0170-FIN-CAB-D
                       PERFORM 0120-LEER-REGISTRO-VENTAS-SORT
               END-PERFORM
               PERFORM 0180-FIN-CAB-C
           END-PERFORM
           PERFORM 0190-FIN-GENERAL.
      *
       0110-INICIO-GENERAL.
           PERFORM 0113-INICIO-VAR-GRAL.
           PERFORM 0116-ABRO-FACTURAS.
      *
       0113-INICIO-VAR-GRAL.
           MOVE "n" TO flagVentasSort.
           MOVE "n" TO flagFacturas.
           MOVE ZEROES TO vec-cant-art.
      *
       0116-ABRO-FACTURAS.
           OPEN INPUT FACTURAS.
      *
       0120-LEER-REGISTRO-VENTAS-SORT.
           RETURN VENTAS-SORT AT END MOVE "s" TO flagVentasSort.
      *
       0130-LEER-REGISTRO-FACTURAS.
           READ FACTURAS AT END MOVE "s" TO flagFacturas.
      *
       0140-INICIO-CAB-C.
      *
       0150-INICIO-CAB-D.
      *
       0160-ACUMULO-VECTOR.
           ADD fac_det_cant TO acu-cant-art(fac_det_art).
      *
       0170-FIN-CAB-D.
      *
       0180-FIN-CAB-C.
           IF flagFacturas <> "s"
                   PERFORM 0130-LEER-REGISTRO-FACTURAS
                   PERFORM UNTIL flagFacturas = "s"
                       OR fac_det_tiporeg <> "D"
                       PERFORM 0130-LEER-REGISTRO-FACTURAS
                   END-PERFORM.
      *
       0190-FIN-GENERAL.
           PERFORM 0193-CIERRO-FACTURAS.
           PERFORM 200-IMPRIMO-LISTADO.
      *
       0193-CIERRO-FACTURAS.
           CLOSE FACTURAS.
      *
      ************************** RUTINAS LISTADO ***********************
      *
       200-IMPRIMO-LISTADO.
           PERFORM 0210-INICIO-LISTADO.
           PERFORM VARYING cod FROM 1 BY 1 UNTIL cod > 99
               PERFORM 0220-ARMO-LINEA-DET
           END-PERFORM.
           PERFORM 0240-FIN-LISTADO.
*
       0210-INICIO-LISTADO.
           PERFORM 0213-INICIO-VAR-GRAL-LIST.
           PERFORM 0216-ABRO-ARCHIVOS.
           PERFORM 0219-LISTAR-ENCABEZADO.
      *
       0213-INICIO-VAR-GRAL-LIST.
           MOVE "n" TO flagArticulos.
           MOVE ZEROES TO w-cont-paginas.
           MOVE ZEROES TO cod.
      *
       0216-ABRO-ARCHIVOS.
           OPEN INPUT ARTICULOS.
           OPEN OUTPUT LISTADO.
      *
       0219-LISTAR-ENCABEZADO.
           ADD 1 TO w-cont-paginas.
           MOVE w-cont-paginas to l-nro-pag.
           IF w-cont-paginas = 1
               WRITE lis-reg FROM cabecera1
               DISPLAY lis-reg
           ELSE
               WRITE lis-reg FROM cabecera1 AFTER ADVANCING PAGE
           END-IF.
           WRITE lis-reg FROM cabecera2 AFTER 2.
           DISPLAY lis-reg
           WRITE lis-reg FROM cabecera3 AFTER 2.
           DISPLAY lis-reg.
           WRITE lis-reg FROM cabecera4.
           DISPLAY lis-reg.
      *
       0220-ARMO-LINEA-DET.
           IF acu-cant-art(cod) > 0
               PERFORM 0222-IMPRIMO-DET.

       0222-IMPRIMO-DET.
           IF LINAGE-COUNTER = 56
               PERFORM 0219-LISTAR-ENCABEZADO.
           PERFORM 0224-GENERAR-LINEA.
           PERFORM 0230-LISTAR-DETALLE.
      *
       0224-GENERAR-LINEA.
           PERFORM 0226-TRAIGO-NOMBRE.
           MOVE art_cod                TO l-cod.
           MOVE art_nombre             TO l-nombre.
           MOVE acu-cant-art(cod)      TO l-cantidad.
      *
       0226-TRAIGO-NOMBRE.
           PERFORM 0228-LEER-REGISTRO-ARTICULOS.
           PERFORM UNTIL flagArticulos = "s"
               OR art_cod = cod
               PERFORM 0228-LEER-REGISTRO-ARTICULOS
           END-PERFORM.
      *
       0228-LEER-REGISTRO-ARTICULOS.
           READ ARTICULOS AT END MOVE "s" TO flagArticulos.
      *
       0230-LISTAR-DETALLE.
           WRITE lis-reg FROM detalle AFTER ADVANCING 1 LINE
               AT END-OF-PAGE PERFORM 0219-LISTAR-ENCABEZADO.
           DISPLAY lis-reg.
      *
       0240-FIN-LISTADO.
           PERFORM 0243-CIERRO-ARCHIVOS.
      *
       0243-CIERRO-ARCHIVOS.
           CLOSE ARTICULOS.
           CLOSE LISTADO.
      *
       END PROGRAM EXAMEN-NEGOCIO-VENTAS.
