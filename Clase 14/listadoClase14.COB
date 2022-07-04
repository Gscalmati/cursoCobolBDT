      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *************************LINEAS DE IMPRESION**********************
      ******************************************************************
       01  lin-titulo.
           03 filler pic x(30) value spaces.
           03 filler pic x(19) values "Banco el Corralito".
           03 filler pic x(30) value spaces.
       01  lin-suc.
           03 filler pic x(33) value spaces.
           03 filler pic x(10) value "Sucursal: ".
           03 l-suc pic z99.
           03 filler pic x(34) value spaces.
       01  lin-titulo-planilla.
           03 filler pic x(22) value spaces.
           03 filler pic x(36) values
           "Listado de Socios con Saldo Negativo".
           03 filler pic x(22) value spaces.
       01  lin-titulos-socios.
           03 filler pic x(6) value spaces.
           03 filler pic x(10) values "Num. Socio".
           03 filler pic x(6) value spaces.
           03 filler pic x(17) values "Nombre y Apellido".
           03 filler pic x(8) value spaces.
           03 filler pic x(3) values "DNI".
           03 filler pic x(10) value spaces.
           03 filler pic x(12) values "Saldo Deudor".
           03 filler pic x(8) value spaces.
       01  lin-detalle-datos.
           03 filler pic x(5) value spaces.
           03 l-socio pic z.zzz.zzz.zz9.
           03 filler pic x(3) value spaces.
           03 l-nombre pic x(20).
           03 filler pic x(3) value spaces.
           03 l-dni pic zz.zzz.zz9.
           03 filler pic x(3) value spaces.
           03 l-saldo pic zzz.zzz.zz9,99.
           03 filler pic x(5) value spaces.
       01  lin-total.
           03 filler pic x(41) value spaces.
           03 filler pic x(16) value "Total de deuda:".
           03 l-acum-saldo pic zzz.zzz.zzz.zz9,99.
           03 filler pic x(5) value spaces.
      ******************************************************************
      *************************VARIABLES DE ENTRADA*********************
      ******************************************************************
       01  var-ingresos.
           03 ing-suc pic s999.
           03 ing-socio pic 9(10).
           03 ing-nombre pic x(20).
           03 ing-dni pic x(8).
           03 ing-saldo pic s9(9)v99.
      ******************************************************************
      *************************VARIABLES DE TRABAJO*********************
      ******************************************************************
       01  acumulador pic s9(12)v99.
      ******************************************************************
      *************************PROGRAMA PPAL****************************
      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-inicio.
           PERFORM 200-ingresoSocio.

           PERFORM UNTIL ing-socio = 0
               PERFORM 300-ingreso-restoDatos
               PERFORM 400-procesoDatos
               PERFORM 200-ingresoSocio

           END-PERFORM.

           PERFORM 500-fin.
            STOP RUN.


      ******************************************************************
      *************************FUNCIONES********************************
      ******************************************************************

           100-inicio.
               PERFORM 120-ENCABEZADO.
               move zeros to acumulador.

           120-encabezado.
               PERFORM 125-INGRESO-SUC.
               PERFORM 128-IMPRIMO-ENCABEZADO.

           125-INGRESO-SUC.
           DISPLAY "Ingrese num. de sucursal de 1 a 100"
           ACCEPT ing-suc.
           perform until (ing-suc >= 1) and (ing-suc <= 100)
               DISPLAY
               'ERROR, Ingrese un valor de sucursal entre 1 y 100'
               ACCEPT ing-suc
           END-PERFORM.

           128-IMPRIMO-ENCABEZADO.
               MOVE ing-suc TO l-suc.

               DISPLAY lin-titulo.
               DISPLAY lin-suc.
               DISPLAY lin-titulo-planilla.

           200-ingresoSocio.
               DISPLAY "Ingrese num. de socio "
               ACCEPT ing-socio.
               perform until (ing-suc >= 0)
                   DISPLAY
                   'ERROR, Ingrese un num. de socio (0 para FIN)'
                   ACCEPT ing-socio
           END-PERFORM.


           300-ingreso-restoDatos.
               PERFORM 325-ingresoNombre.
               PERFORM 335-ingresoDNI.
               PERFORM 345-ingresoSaldo.

           325-ingresoNombre.
               DISPLAY "Ingrese nombre del socio "
               ACCEPT ing-nombre
               PERFORM UNTIL (ing-nombre not = spaces)
                   DISPLAY 'ERROR, El nombre no puede ser vacío'
                   ACCEPT ing-nombre
           END-PERFORM.

           335-ingresoDNI.
               DISPLAY "Ingrese num. de DNI"
               ACCEPT ing-dni.
               perform until (ing-dni > 0) AND (ing-dni <= 99999999)
                   DISPLAY
                   'ERROR, Ingrese un num. de DNI válido,',
                   'mayor a 0 y menor a 99.999.999'
                   ACCEPT ing-dni
           END-PERFORM.

               345-ingresoSaldo.
                   DISPLAY "Ingrese saldo del socio".
                   ACCEPT ing-saldo.

               400-procesoDatos.
                   if (ing-saldo < 0)
                       compute acumulador = acumulador + ing-saldo.
                       PERFORM 420-imprimirSocio.


               420-imprimirSocio.
                   PERFORM 425-asignarVariablesSocio.
                   DISPLAY lin-detalle-datos.

               425-asignarVariablesSocio.
                   MOVE ing-socio TO l-socio.
                   MOVE ing-nombre TO l-nombre.
                   MOVE ing-dni TO l-dni.
                   MOVE ing-saldo TO l-saldo.

               500-fin.
                   PERFORM 525-asignarVariablesTotal.
                   DISPLAY lin-total.

               525-asignarVariablesTotal.
                   MOVE acumulador TO l-acum-saldo.

       END PROGRAM YOUR-PROGRAM-NAME.
