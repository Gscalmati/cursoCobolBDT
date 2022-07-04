      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Asi seleccionamos un archivo SECUENCIAL
           SELECT SOCIOS ASSIGN TO
           "..\socios.dat".
      *    Si uno no configura la ORGANIZACION, se toma como SECUENCIAL
           SELECT FILIALES ASSIGN TO
           "..\filiales.dat".

           SELECT LISTADO ASSIGN TO
           PRINTER "..\impresora.txt".

           SELECT SORT-ARCH
           ASSIGN TO "SORTWORK".
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01 soc-reg.
           03 soc-filial pic 99.
           03 soc-socio pic 99999.
           03 soc-importe pic s9(8)v99.
           03 soc-modalidad pic x(1).
       FD  FILIALES.
       01  fil-reg.
           03 fil-codigo pic 99.
           03 fil-nombre pic x(15).

       FD  LISTADO
           LINAGE IS 60 LINES
           WITH FOOTING AT 50.
       01  lis-reg pic x(80).

       SD  SORT-ARCH.
       01  sort-reg.
           03 sort-filial pic 99.
           03 sort-socio pic 99999.
           03 sort-importe pic s9(8)v99.
      *     03 sort-modalidad pic x(1).
           03 sort-nombre-fil pic x(15).
       WORKING-STORAGE SECTION.
      ******************************************************************
      ********************** LINEAS DE IMPRESION **********************
      *****************************************************************
       01  lin-titulo.
           03 filler pic x(30) value spaces.
           03 filler pic x(19) value "BANCO EL CORRALITO".
           03 filler pic x(31) value spaces.

       01  lin-subtitulo.
           03 filler pic x(17) value spaces.
           03 filler pic x(46) value
           "LISTADO DE TRANSFERENCIAS BANCARIAS DE SOCIOS".
           03 filler pic x(17) value spaces.

       01  lin-filial.
           03 filler pic x(26) value spaces.
           03 filler pic x(8) value "FILIAL: ".
           03 l-num-filial pic 99.
           03 filler pic xxx value " - ".
           03 l-nom-filial pic x(16).
           03 filler pic x(26) value spaces.

       01  lin-columnas.
           03 filler pic x(22) value spaces.
           03 filler pic x(6) value "SOCIO".
           03 filler pic x(22) value spaces.
           03 filler pic x(10) value "IMPORTE".
           03 filler pic x(20) value spaces.

       01  lin-datos.
           03 filler pic x(22) value spaces.
           03 l-socio pic x(6).
           03 filler pic x(22) value spaces.
           03 l-importe pic zz.zzz.zz9,99.
           03 filler pic x(22) value spaces.

       01  lin-espacios.
           03 filler pic x(22) value spaces.
           03 filler pic x(6) value "------".
           03 filler pic x(22) value spaces.
           03 filler pic x(10) value "----------".
           03 filler pic x(20) value spaces.

       01  lin-datos-total.
           03 filler pic x(15) value spaces.
           03 filler pic x(7) value "Total: ".
           03 l-cant-filial pic x(6).
           03 filler pic x(22) value spaces.
           03 l-importe-filial pic zz.zzz.zz9,99.
           03 filler pic x(22) value spaces.

       01  lin-titulo-estadistica.
           03 filler pic x(10) value spaces.
           03 filler pic x(20) value "ESTADISTICA GENERAL:".

       01  lin-estad-importe.
           03 filler pic x(38) value
           "El importe general transferido fue de ".
           03 l-estad-importe pic zz.zzz.zz9,99.

       01  lin-estad-cant.
           03 filler pic x(25) value "En total se procesaron a ".
           03 l-cant-socios pic 99.
           03 filler pic x(6) value "socios".

       01  lin-estad-filial-mas-socios.
           03 filler pic x(56) value
           "La filial que mas socios operaron transferencias fue la ".
           03 l-num-filial-mas-socios pic 9.
           03 filler pic xxx value " - ".
           03 l-nom-filial-mas-socios pic x(15).
           03 filler pic x(4) value "con ".
           03 l-cant-mas-socios pic 99.
           03 filler pic x(6) value "socios".
      *****************************************************************
      ********************** VARIABLES DE TRABAJO *********************
      *****************************************************************
       01  flag-filial pic 9 value 1.
       01  flag-socio pic 9 value 1.
       01  flag-sort pic 9 value 1.

       01  filialActual pic 99.
       01  nomFilialActual pic x(15).
       01  socAnterior pic 9999.

       01  i pic 99.
      ********************** ACUMULADORES ******************************
       01  cantGeneral pic 999.
       01  totalGeneral pic s9(12)v99.
       01  importeSocio pic s9(12)v99.

       01  cantTransfSocio pic 99.
       01  cantTransfFilial pic 999.
       01  totalFilial pic 9(12)v99.
      ********************** MAXIMOS ***********************************
       01  tablaMax.
           03 longitud pic 9.
           03 vecMax OCCURS 1 TO 9 DEPENDING ON longitud VALUE ZEROS.
               05 numFilialMax pic 9.
               05 nomFilialMax pic x(15).
               05 cantTransfMax pic 999.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           SORT SORT-ARCH ASCENDING sort-nombre-fil,
           DESCENDING sort-importe
           INPUT PROCEDURE IS 20-PROCEDURE-INTRO
           OUTPUT PROCEDURE IS 40-PROCEDURE-OUTPUT

           STOP RUN.


           20-PROCEDURE-INTRO.
               PERFORM 100-INICIO-GENERAL.
      *    ES NECESARIO LEER AMBOS REGISTROS ANTES DEL CICLO PARA NOPERDER 1 REGISTRO MAS ADELANTE CUANDO OCURRA EL CAMBIO DE FILIAL

               PERFORM 300-LEER-REG-SOCIOS.
               PERFORM 200-LEER-REG-FILIALES.
               PERFORM UNTIL (flag-socio = 0)
      *         IF (soc-filial = fil-codigo)
      *             PERFORM 600-INICIO-FILIAL
                   PERFORM UNTIL
                   (flag-socio = 0) or (fil-codigo <> soc-filial)
                           PERFORM 400-INICIO-CIRCUITO-SOCIO
                           PERFORM UNTIL (socAnterior <> soc-socio)
                       or (fil-codigo <> soc-filial) or (flag-socio = 0)
                                   PERFORM 450-PROCESO-CIRCUITO-SOCIO
                                   PERFORM 300-LEER-REG-SOCIOS
                           END-PERFORM
                           PERFORM 475-FIN-CIRCUITO-SOCIO
                       END-PERFORM
      *             PERFORM 820-FIN-FILIAL
      *         END-IF
                   PERFORM 200-LEER-REG-FILIALES
               END-PERFORM
               PERFORM 998-CERRAR-ARCHIVOS.

       40-PROCEDURE-OUTPUT.
           PERFORM 500-INICIO-OUTPUT-PROCEDURE.
           PERFORM 520-LEER-SORT-ARCH.
           PERFORM UNTIL (flag-sort = 0)
               PERFORM 600-INICIO-FILIAL
               PERFORM UNTIL
               (flag-sort = 0) or (filialActual <> sort-filial)
                   PERFORM 650-PROCESO-FILIAL-OUTPUT
                   PERFORM 520-LEER-SORT-ARCH
               END-PERFORM
               PERFORM 820-FIN-FILIAL-OUTPUT
           END-PERFORM.
           PERFORM 800-FIN-GENERAL-OUTPUT.

       100-INICIO-GENERAL.
           OPEN INPUT SOCIOS.
           OPEN INPUT FILIALES.


       200-LEER-REG-FILIALES.
           READ FILIALES AT END MOVE 0 TO flag-filial.


       300-LEER-REG-SOCIOS.
           READ SOCIOS AT END MOVE 0 TO flag-socio.


       400-INICIO-CIRCUITO-SOCIO.
           MOVE soc-socio TO socAnterior.
           MOVE ZEROES to importeSocio.

       450-PROCESO-CIRCUITO-SOCIO.
           if (soc-modalidad = "T")
               ADD soc-importe TO importeSocio.

       475-FIN-CIRCUITO-SOCIO.
           PERFORM 480-GRABACIONES-SOCIO.


       480-GRABACIONES-SOCIO.
           if (importeSocio > 0)
               PERFORM 490-ARMAR-REG-SORT
               PERFORM 495-GRABAR-REG-SORT
           END-IF.

       490-ARMAR-REG-SORT.
           MOVE fil-codigo TO sort-filial.
           MOVE socAnterior TO sort-socio.
           MOVE importeSocio TO sort-importe.
           MOVE fil-nombre TO sort-nombre-fil.

       495-GRABAR-REG-SORT.

           RELEASE sort-reg.
      *-----------------------------------------------------------------*
       500-INICIO-OUTPUT-PROCEDURE.
           DISPLAY lin-titulo.
           DISPLAY lin-subtitulo.
           MOVE ZEROES TO totalGeneral.
           MOVE ZEROES TO cantGeneral.

       520-LEER-SORT-ARCH.
           RETURN SORT-ARCH AT END MOVE 0 TO flag-sort.

       600-INICIO-FILIAL.
           MOVE ZEROES TO cantTransfFilial.
           MOVE ZEROES TO totalFilial.

           MOVE sort-filial TO l-num-filial.
           MOVE sort-nombre-fil TO l-nom-filial.
           DISPLAY lin-filial.
           DISPLAY lin-columnas.

           MOVE sort-filial TO filialActual.
           MOVE sort-nombre-fil TO nomFilialActual.

       650-PROCESO-FILIAL-OUTPUT.
           ADD 1 TO cantTransfFilial.
           ADD sort-importe TO totalFilial.
           PERFORM 670-IMPRIMIR-SOCIO-OUTPUT.

       670-IMPRIMIR-SOCIO-OUTPUT.
           MOVE sort-socio TO l-socio.
           MOVE sort-importe TO l-importe.
           DISPLAY lin-datos.


       800-FIN-GENERAL-OUTPUT.
           DISPLAY lin-titulo-estadistica.
           DISPLAY "En total se procesaron ", cantGeneral, " socios".
           DISPLAY
           "El importe general transferido fue de ", totalGeneral.
           PERFORM 860-IMPRIMIR-VECTOR-MAX.


       820-FIN-FILIAL-OUTPUT.
           ADD cantTransfFilial TO cantGeneral.
           ADD totalFilial TO totalGeneral.
           PERFORM 830-CONSULTAR-MAX.

           MOVE cantTransfFilial TO l-cant-filial.
           MOVE totalFilial TO l-importe-filial.
           DISPLAY lin-espacios.
           DISPLAY lin-datos-total.

       830-CONSULTAR-MAX.
           IF (cantTransfFilial > cantTransfMax(longitud))
                PERFORM 840-INICIALIZAR-VECTOR
                PERFORM 850-AGREGAR-AL-VECTOR
           ELSE IF (cantTransfFilial = cantTransfMax(longitud))
               PERFORM 850-AGREGAR-AL-VECTOR
           END-IF.

       840-INICIALIZAR-VECTOR.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > longitud
               MOVE " " TO nomFilialMax(i)
               MOVE 0 TO numFilialMax(i)
               MOVE 0 TO cantTransfMax(i)
           END-PERFORM.
           MOVE 0 TO longitud.

       850-AGREGAR-AL-VECTOR.
           ADD 1 TO longitud.
           MOVE nomFilialActual TO nomFilialMax(longitud).
           MOVE filialActual TO numFilialMax(longitud).
           MOVE cantTransfFilial TO cantTransfMax(longitud).


       860-IMPRIMIR-VECTOR-MAX.
           DISPLAY
           "La/las filial/es que mas ",
           "socios operaron transferencias fueron: "
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > longitud
               DISPLAY numFilialMax(i), " - ", nomFilialMax(i), " ",
               cantTransfMax(i)
           END-PERFORM.

       998-CERRAR-ARCHIVOS.
               CLOSE SOCIOS.
               CLOSE FILIALES.

       999-CERRAR-SORT.
               CLOSE LISTADO.

       END PROGRAM YOUR-PROGRAM-NAME.
