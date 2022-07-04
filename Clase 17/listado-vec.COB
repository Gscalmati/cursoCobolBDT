      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       environment division.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT is comma.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *****************************************************************
      ********************** LINEAS DE IMPRESION **********************
      *****************************************************************
       01  lin-titulo.
           03 filler pic x(30) value spaces.
           03 filler pic x(19) value "Banco El Corralito".
           03 filler pic x(31) value spaces.
       01  lin-suc.
           03 filler pic x(33) values spaces.
           03 filler pic x(10) value "Sucursal:".
           03 l-suc  pic z99.
           03 filler pic x(34) value spaces.
       01  lin-titulo-planilla.
           03 filler pic x(22) value spaces.
           03 filler pic x(36) value "Listado de socios con saldo negati
      -     "vo".
           03 filler pic x(22) value spaces.
       01  lin-titulos-socios.
           03 filler pic x(6) value spaces.
           03 filler pic x(10) value "Nro. Socio".
           03 filler pic x(6) value spaces.
           03 filler pic x(17) value "Nombre y Apellido".
           03 filler pic x(8) value spaces.
           03 filler pic x(3) value "Dni".
           03 filler pic x(10) value spaces.
           03 filler pic x(12) value "Saldo Deudor".
           03 filler pic x(8) value spaces.
       01  lin-detalle-datos.
           03 filler   pic x(5) value spaces.
           03 l-socio  pic z.zzz.zzz.zz9.
           03 filler   pic x(3) value spaces.
           03 l-nombre pic x(20).
           03 filler   pic x(3) value spaces.
           03 l-dni    pic zz.zzz.zz9.
           03 filler   pic x(3) value spaces.
           03 l-saldo  pic zzz.zzz.zz9,99.
           03 filler   pic x(5) value spaces.
       01  lin-total.
           03 filler pic x(37) values spaces.
           03 filler pic x(16) value "Total de Deuda:".
           03 l-acum-saldo pic zzz.zzz.zzz.zz9,99.
           03 filler pic x(5) value spaces.
      *****************************************************************
      ********************** VARIABLES DE ENTRADA**********************
      *****************************************************************
       01  var-ingresos.
           03 ing-suc    pic 999.
           03 aux-socio  pic 9(10).
           03 tab-socios.
              05 largo pic 9.
              05 socios occurs 1 to 100 depending on largo.
                 07 ing-socio  pic 9(10).
                 07 ing-nombre pic x(20).
                 07 ing-dni    pic 9(8).
                 07 ing-saldo  pic s9(9)v99.
      *****************************************************************
      ********************** VARIABLES DE TRABAJO *********************
      *****************************************************************
       01  acum pic s9(12)v99.
       01  i pic 999.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *****************************************************************
      ********************** PROGRAMA PRINCIPAL ***********************
      *****************************************************************
           PERFORM 100-INICIO-GENERAL.
           PERFORM 200-INGRESO-DATOS.
           PERFORM 350-INICIO-LISTADO.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > largo
                 PERFORM 400-PROCESO
           END-PERFORM.
           PERFORM 500-FIN.
           STOP RUN.
      *****************************************************************
      *******************    R U T I N A S      ***********************
      *****************************************************************
       100-INICIO-GENERAL.
           PERFORM 125-INGRESO-SUC.
           move 0 to largo.
       350-INICIO-LISTADO.
           PERFORM 120-ENCABEZADO.
           move zeros to acum.
       120-ENCABEZADO.
           PERFORM 128-IMPRIMO-ENCABEZADO.
       125-INGRESO-SUC.
           PERFORM 126-TRAIGO-SUC.
           PERFORM UNTIL ing-suc >= 1 AND ing-suc <= 100
              display "Error, te dije entre 1 y 100"
              PERFORM 126-TRAIGO-SUC
           END-PERFORM.
       126-TRAIGO-SUC.
           display "Ingrese el nro. de sucursal (1-100)"
           accept ing-suc.
       128-IMPRIMO-ENCABEZADO.
           move ing-suc to l-suc.
           display lin-titulo.
           display lin-suc.
           display lin-titulo-planilla.
           display lin-titulos-socios.
       200-INGRESO-DATOS.
           PERFORM 250-INGRESO-SOCIO.
           PERFORM UNTIL aux-socio = ZERO
              PERFORM 300-CARGO-SOCIO
              PERFORM 250-INGRESO-SOCIO
           END-PERFORM.

       250-INGRESO-SOCIO.
           PERFORM 226-TRAIGO-SOCIO.
           PERFORM UNTIL aux-socio >= 0
              display "Error, te dije un número positivo, x fin 0"
              PERFORM 226-TRAIGO-SOCIO
           END-PERFORM.
       226-TRAIGO-SOCIO.
           display "Ingrese el nro. de socio (> 0) y por fin ing. 0".
           accept aux-socio.
       300-CARGO-SOCIO.
           add 1 to largo
           move aux-socio to ing-socio(largo)
           PERFORM 325-INGRESO-NOMBRE.
           PERFORM 335-INGRESO-DNI.
           PERFORM 345-INGRESO-SALDO.
       325-INGRESO-NOMBRE.
           PERFORM 326-TRAIGO-NOMBRE.
           PERFORM UNTIL ing-nombre(largo) not = SPACES
              display "Error, tenés que ingresar algo"
              PERFORM 326-TRAIGO-NOMBRE
           END-PERFORM.
       326-TRAIGO-NOMBRE.
           display "Ingrese el nombre del socio ", ing-socio(largo)
           accept ing-nombre(largo).
       335-INGRESO-DNI.
           PERFORM 336-TRAIGO-DNI.
           PERFORM UNTIL ing-dni(largo) >= 10000
              display "Error, DNI no válido"
              PERFORM 336-TRAIGO-DNI
           END-PERFORM.
       336-TRAIGO-DNI.
           display "Ingrese el nro. DNI".
           accept ing-dni(largo).
       345-INGRESO-SALDO.
           display "Ingrese el saldo del socio"
           accept ing-saldo(largo).
       400-PROCESO.
           if ing-saldo(i) < 0
               compute acum = acum + ing-saldo(i)
               PERFORM 420-ARMO-LINEA
               display lin-detalle-datos.
       420-ARMO-LINEA.
           move ing-socio(i) to l-socio
           move ing-nombre(i) to l-nombre
           move ing-dni(i) to l-dni
           move ing-saldo(i) to l-saldo.
       500-FIN.
           move acum to l-acum-saldo.
           display lin-total.
       END PROGRAM YOUR-PROGRAM-NAME.
