      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO-SALA2-ORDEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIOS
           ASSIGN TO
           "D:\linux cecilia\COBOL\archivo\OrdenIO\archSoc.dat".

           SELECT FILIALES
           ASSIGN TO
           "D:\linux cecilia\COBOL\archivo\OrdenIO\archFil.dat".
           SELECT SOCIOS-SORT
           ASSIGN TO "sortwork".
           SELECT LISTADO
           ASSIGN TO PRINTER,
           "D:\linux cecilia\COBOL\archivo\OrdenIO\impOrden.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-filial pic 9.
           03 soc-socio pic 9(4).
           03 soc-importe pic 9(8)v99.
           03 soc-modalidad pic x.

       FD  FILIALES.
       01  fil-reg.
           03 fil-codigo pic 9.
           03 fil-nombre pic x(15).
       SD  SOCIOS-SORT.
       01  srt-reg.
           03 srt-socio pic 9(4).
           03 srt-filial pic 9.
           03 srt-importe pic s9(8)v99.
           03 srt-modalidad pic x.
           03 srt-nombre-filial pic x(15).

       FD  LISTADO
           LINAGE IS 60 LINES
           with FOOTING AT 50
           lines at top 1
           lines at BOTTOM 1.
       01  lis-reg pic x(80).
       WORKING-STORAGE SECTION.

       77  w-i pic 99.
       01  w-fecha.
           03 w-anio pic 99.
           03 w-mes pic 99.
           03 w-dia pic 99.
       01  w-cont-paginas pic 99.
      ****** flags fin archivo ************************************************
       77  w-flagSocio pic 9.
       77  w-flagFilial pic 9.
       77  w-flagSocSort pic 9.
      ****** modelo temático general ******************************************
       01  tab-filiales.
           03 largo pic 9.
           03 vec-filial OCCURS 1 TO 9 DEPENDING ON largo VALUE ZEROS.
               05 vec-fil-cod pic 9.
               05 vec-fil-nom pic x(15).
               05 vec-fil-cant pic 999.
       77  w-gen-cant-trans pic 999.
       77  w-gen-acum-imp pic 9(10)v99.
      ****** modelo temático filial *******************************************
       77  w-fil-cod-ant pic 9.
       77  w-fil-nom-ant pic x(15).
       77  w-fil-cant-trans pic 9(4).
       77  w-fil-acum-imp pic 9(10)v99.
      ****** modelo temático socio ********************************************
       77  w-soc-cod-ant pic 9(4).
       77  w-soc-acum-imp pic 9(10)v99.

      ****** lineas de impresion **********************************************

       01  cabecera1.
           03  lin-titulo.
               05 filler pic x(30) value spaces.
               05 filler pic x(19) value "BANCO: EL CORRALITO".
               05 filler pic x(5) value spaces.
               05 filler pic x(11) value "Pagina nro:".
               05 l-nro-pag pic 9(2).
               05 filler pic x(13) value spaces.
       01  cabecera2.
           03  lin-sub-titulo.
               05 filler pic x(17) value spaces.
               05 filler pic x(50) value "LISTADO DE TRANSFERENCIAS "-
           "BANCARIAS DE SOCIOS AL:".
               05 l-dia pic z9.
               05 filler pic x value "/".
               05 l-mes pic 99.
               05 filler pic x(3) value "/20".
               05 l-anio pic 99.
       01  cabecera3.
           03  lin-guarda.
               05 filler pic x(80) value all "*".
       01  cabecera4.
           03  lin-titulo-filial.
               05 filler pic x(26) value spaces.
               05 filler pic x(8) value "FILIAL: ".
               05 l-fil-dat-cod pic x.
               05 filler pic x(3) value " - ".
               05 l-fil-dat-nom pic x(15).
       01  cabecera5.
           03  lin-titulo-soc.
               05 filler pic x(30) value spaces.
               05 soc-dat-cod pic x(5) value "SOCIO".
               05 filler  pic x(8) value space.
               05 soc-dat-imp pic x(7) value "IMPORTE".
       01  detalle1.
           03  lin-dat-soc.
               05 filler pic x(31) value spaces.
               05 l-soc-cod pic 9(4).
               05 filler pic x(3) value spaces.
               05 l-soc-imp pic z.zzz.zzz.zz9,99.
       01  cabecera6.
           03  lin-guion-fin-soc.
               05 filler pic x(27) value spaces.
               05 filler pic x(12) value all "-".
               05 filler pic x(3) value spaces.
               05 filler pic x(12) value all "-".
       01  detalle2.
           03  lin-tot-fil.
               05 filler pic x(20) value spaces.
               05 filler pic x(7) value "Total:".
               05 filler pic x(4) value spaces.
               05 l-fil-cant-trans pic zz9.
               05 filler pic x(4) value spaces.
               05 l-fil-cant-imp pic z.zzz.zzz.zz9,99.
       01  cabecera7.
           03  lin-titulo-gen.
               05 filler pic x(30) value space.
               05 filler pic x(20) value "ESTADISTICA GENERAL:".
       01  cabecera7-1.
           03  lin-subrayado.
               05 filler pic x(30) value space.
               05 filler pic x(20) value all "-".
               05 filler pic x(30) value space.
       01  cabecera8.
           03  lin-tot-gen.
               05 filler pic x(25) value "En total se procesaron a".
               05 l-tot-trans pic zz9.
               05 filler pic x(7) value " socios".
       01  cabecera9.
           03  lin-tot-imp-gen.
               05 filler pic x(36) value "El importe general"-
           "transferido fue de".
               05 l-tot-imp pic zzz.zzz.zzz.zz9,99.
       01  cabecera10.
           03  lin-max-fil-gen.
               05 filler pic x(59) value "La filial/es que mas socios "-
           "operaron transferencias fueron:".
       01  cabecera11.
           03  lin-max-filiales.
               05 l-max-cod-fil pic 9.
               05 filler pic x(3) value " - ".
               05 l-max-nom-fil pic x(15).
               05 filler pic x(5) value " con".
               05 l-max-cont-soc pic zz9.
               05 filler pic x(7) value " socios".



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           SORT SOCIOS-SORT ASCENDING srt-nombre-filial,
                            DESCENDING srt-importe
           INPUT PROCEDURE IS input-procedure
           OUTPUT PROCEDURE IS output-procedure.

           STOP RUN.

       input-procedure.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 230-LEER-FILIAL.
            PERFORM 330-LEER-SOCIO.
            PERFORM UNTIL w-flagSocio IS EQUALS 1
               PERFORM 200-INICIO-FILIAL
               PERFORM UNTIL w-flagSocio IS EQUALS 1 OR
                             fil-codigo IS NOT EQUALS soc-filial
                   PERFORM 300-INICIO-SOCIO
                   PERFORM UNTIL w-flagSocio IS EQUALS 1 OR
                                 soc-socio IS NOT EQUALS w-soc-cod-ant
                       PERFORM 350-PROCESO-SOCIO
                       PERFORM 330-LEER-SOCIO
                   END-PERFORM
                   PERFORM 400-FIN-SOCIO
               END-PERFORM
               PERFORM 230-LEER-FILIAL
            END-PERFORM.
            PERFORM 600-FIN-GENERAL.

       output-procedure.
           PERFORM 700-INICIO-OUTPUT-GEN.
           PERFORM 750-LEER-SOC-SORT.
           PERFORM UNTIL w-flagSocSort IS EQUALS 1
               PERFORM 800-INICIO-OUTPUT-FIL
               PERFORM UNTIL w-flagSocSort IS EQUALS 1 OR
                             w-fil-cod-ant IS NOT EQUALS srt-filial
                   PERFORM 850-PROCESO-OUTPUT-FIL
                   PERFORM 750-LEER-SOC-SORT
               END-PERFORM
               PERFORM 900-FIN-OUTPUT-FIL
           END-PERFORM.
           PERFORM 1000-FIN-OUTPUT-GEN.


      ******* RUTINAS INPUT-PROCEDURE *****************************************
       100-INICIO-GENERAL.
           PERFORM 120-INICIALIZO-VARIABLES-GEN.
           PERFORM 125-ABRO-ARCHIVOS.

       120-INICIALIZO-VARIABLES-GEN.
           MOVE ZERO TO w-flagFilial.
           MOVE ZERO TO w-flagSocio.

       125-ABRO-ARCHIVOS.
           OPEN INPUT FILIALES.
           OPEN INPUT SOCIOS.

       200-INICIO-FILIAL.
           PERFORM 210-BUSCO-FILIALES.

       210-BUSCO-FILIALES.
           PERFORM 230-LEER-FILIAL UNTIL w-flagFilial IS EQUALS 1 OR
                                        fil-codigo IS EQUALS soc-filial.

       230-LEER-FILIAL.
           READ FILIALES AT END MOVE 1 TO w-flagFilial.

       300-INICIO-SOCIO.
           MOVE soc-socio TO w-soc-cod-ant.
           MOVE ZERO TO w-soc-acum-imp.

       330-LEER-SOCIO.
           READ SOCIOS AT END MOVE 1 TO w-flagSocio.

       350-PROCESO-SOCIO.
           IF soc-modalidad IS EQUALS "T" THEN
               ADD soc-importe TO w-soc-acum-imp
           END-IF.

       400-FIN-SOCIO.
           IF w-soc-acum-imp IS NOT ZERO
               PERFORM 410-ARMO-SORT-REG
               RELEASE srt-reg
           END-IF.

       410-ARMO-SORT-REG.
           MOVE fil-codigo TO srt-filial.
           MOVE fil-nombre TO srt-nombre-filial.
           MOVE w-soc-acum-imp TO srt-importe.
           MOVE w-soc-cod-ant TO srt-socio.

       600-FIN-GENERAL.
           PERFORM 625-CIERRO-ARCHIVOS.

       625-CIERRO-ARCHIVOS.
           CLOSE FILIALES.
           CLOSE SOCIOS.

      ******* RUTINAS OUTPUT-PROCEDURE ****************************************
       700-INICIO-OUTPUT-GEN.
           PERFORM 710-INICIALIZO-VARAIBLES-GEN.
           PERFORM 720-ABRO-ARCHIVO-IMP.
           PERFORM 980-OBTENER-FECHA.
           PERFORM 730-IMPRIMO-ENCABEZADO.


       710-INICIALIZO-VARAIBLES-GEN.
           MOVE ZERO TO w-flagSocSort.
           MOVE 1 TO largo.
           MOVE ZERO TO w-gen-cant-trans.
           MOVE ZERO TO w-gen-acum-imp.
           MOVE ZERO TO w-cont-paginas.

       720-ABRO-ARCHIVO-IMP.
           OPEN OUTPUT LISTADO.

       730-IMPRIMO-ENCABEZADO.
            ADD 1 TO w-cont-paginas.
            MOVE w-cont-paginas to l-nro-pag.
            WRITE lis-reg FROM cabecera3 AFTER 1.
            WRITE lis-reg FROM cabecera1 AFTER 1.
            WRITE lis-reg FROM cabecera2 AFTER 1.
            WRITE lis-reg FROM cabecera3 AFTER 1.

       750-LEER-SOC-SORT.
           RETURN SOCIOS-SORT AT END MOVE 1 TO w-flagSocSort.

       800-INICIO-OUTPUT-FIL.
           MOVE srt-filial TO w-fil-cod-ant.
           MOVE srt-nombre-filial TO w-fil-nom-ant.
           PERFORM 810-INICIALIZO-VARIABLES-FIL.
           PERFORM 820-IMPRIMO-TITULO-FIL.

       810-INICIALIZO-VARIABLES-FIL.
           MOVE ZERO TO w-fil-cant-trans.
           MOVE ZERO TO w-fil-acum-imp.


       820-IMPRIMO-TITULO-FIL.
           PERFORM 812-ARMO-LIN-TIT-FIL.
            WRITE lis-reg FROM cabecera4 AFTER 2.
            WRITE lis-reg FROM cabecera5 AFTER 2.

       812-ARMO-LIN-TIT-FIL.
           MOVE srt-filial TO l-fil-dat-cod.
           MOVE srt-nombre-filial TO l-fil-dat-nom.

       850-PROCESO-OUTPUT-FIL.
           PERFORM 860-ARMO-LIN-SOC.
           WRITE lis-reg FROM detalle1 AFTER 1.
           ADD 1 TO w-fil-cant-trans.
           ADD srt-importe TO w-fil-acum-imp.


       860-ARMO-LIN-SOC.
           MOVE srt-socio TO l-soc-cod.
           MOVE srt-importe TO l-soc-imp.

       900-FIN-OUTPUT-FIL.
           PERFORM 950-PROCESO-GEN.
           PERFORM 910-ARMO-LIN-FIL.
           PERFORM 912-MUESTRO-DATOS-FIL.

       910-ARMO-LIN-FIL.
           MOVE w-fil-cant-trans TO l-fil-cant-trans.
           MOVE w-fil-acum-imp TO l-fil-cant-imp.


       912-MUESTRO-DATOS-FIL.
            WRITE lis-reg FROM cabecera6 AFTER 1.
            WRITE lis-reg FROM detalle2 AFTER 1.

       950-PROCESO-GEN.
           ADD w-fil-cant-trans TO w-gen-cant-trans.
           ADD w-fil-acum-imp TO w-gen-acum-imp.
           PERFORM 960-BUSCO-MAXIMO-FIL.


       960-BUSCO-MAXIMO-FIL.
           IF w-fil-cant-trans IS EQUALS vec-fil-cant(largo) THEN
               PERFORM 967-MUEVO-DATOS-A-VEC
           ELSE IF w-fil-cant-trans > vec-fil-cant(largo) THEN
               PERFORM 965-INICIALIZO-EN-CERO-VEC
               PERFORM 967-MUEVO-DATOS-A-VEC
           END-IF.

       965-INICIALIZO-EN-CERO-VEC.
           PERFORM VARYING w-i FROM 1 BY 1 UNTIL w-i > largo
               MOVE ZERO TO vec-fil-cod(w-i)
               MOVE " " TO vec-fil-nom(w-i)
               MOVE ZERO TO vec-fil-cant(w-i)
           END-PERFORM.
           MOVE 1 TO largo.

       967-MUEVO-DATOS-A-VEC.
           ADD 1 TO largo.
           MOVE w-fil-cant-trans TO vec-fil-cant(largo).
           MOVE w-fil-cod-ant TO vec-fil-cod(largo).
           MOVE w-fil-nom-ant TO vec-fil-nom(largo).

       980-OBTENER-FECHA.
           accept w-fecha from DATE.
           PERFORM 985-ARMAR-FECHA.

           985-ARMAR-FECHA.
           MOVE w-dia TO l-dia.
           MOVE w-mes to l-mes.
           MOVE w-anio to l-anio.

       1000-FIN-OUTPUT-GEN.
           PERFORM 1010-MUESTRO-ENCABEZADO-GEN.
           PERFORM 1015-ARMO-DATOS-GEN.
           PERFORM 1020-MUESTRO-DATOS-GEN.
           PERFORM 1030-CIERRO-ARCH-IMP.

       1010-MUESTRO-ENCABEZADO-GEN.
            WRITE lis-reg FROM cabecera3 AFTER 1.
            WRITE lis-reg FROM cabecera7 AFTER 1.
            WRITE lis-reg FROM cabecera7-1 AFTER 1.

       1015-ARMO-DATOS-GEN.
           MOVE w-gen-cant-trans TO l-tot-trans.
           MOVE w-gen-acum-imp TO l-tot-imp.

       1020-MUESTRO-DATOS-GEN.
           WRITE lis-reg FROM cabecera8 after 1.
           write lis-reg from cabecera9 after 1.
           WRITE lis-reg from cabecera10 after 1.
           PERFORM 1025-MUESTRO-DATOS-VEC.

       1025-MUESTRO-DATOS-VEC.
           PERFORM VARYING w-i FROM 2 BY 1 UNTIL w-i > largo
               PERFORM 1026-ARMO-LINEA-FILIALES
               WRITE lis-reg FROM cabecera11 AFTER 1

           END-PERFORM.

       1026-ARMO-LINEA-FILIALES.
           MOVE vec-fil-nom(w-i) TO l-max-nom-fil.
           MOVE vec-fil-cant(w-i) TO l-max-cont-soc.
           MOVE vec-fil-cod(w-i) TO l-max-cod-fil.

       1030-CIERRO-ARCH-IMP.
           CLOSE LISTADO.

       END PROGRAM EJERCICIO-SALA2-ORDEN.
