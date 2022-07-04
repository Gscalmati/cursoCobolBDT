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
      *    Asi seleccionamos un archivo SECUENCIAL
           SELECT NOVEDADES ASSIGN TO
           "..\novedades.dat".
      *    Si uno no configura la ORGANIZACION, se toma como SECUENCIAL
       DATA DIVISION.
       FILE SECTION.
       FD  NOVEDADES.
       01  nov-reg.
           03 nov-socio pic 9999.
           03 nov-mov pic x.
           03 nov-nombre pic x(20).
           03 nov-importe pic s9(7)v99.

       WORKING-STORAGE SECTION.
       01  cont pic 999.
       01  opcion pic 9.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-ING-SOCIO.
            PERFORM UNTIL (nov-socio = 0)
               PERFORM 300-ING-RESTO
               PERFORM 400-PROCESO
               PERFORM 200-ING-SOCIO
            END-PERFORM.
            PERFORM 500-FIN.
            STOP RUN.

       100-INICIO.
           MOVE ZEROES TO cont.
           DISPLAY "Grabador de Archivos Secuenciales de Salida 3001"
           DISPLAY "Modelo: N0V3DAD35"
           OPEN OUTPUT NOVEDADES.

       200-ING-SOCIO.
           DISPLAY "Ingrese Numero de Socio".
           ACCEPT nov-socio.
           PERFORM 205-VALIDAR-FILIAL.

       205-VALIDAR-FILIAL.
           PERFORM UNTIL (nov-socio >= 0 and nov-socio <= 9999)
               DISPLAY "Error. Inserte un Codigo de Socio Valido"
               ACCEPT nov-socio
           END-PERFORM.

       300-ING-RESTO.
           PERFORM 310-ING-MOV.

           EVALUATE nov-mov
           WHEN "A"
                PERFORM 320-ING-NOMBRE
                PERFORM 330-ING-IMPORTE
           WHEN "M"
               PERFORM 318-OPCIONES
           WHEN "B"
               MOVE 0 TO nov-importe
               MOVE SPACE TO nov-nombre
           END-EVALUATE.



       310-ING-MOV.
           DISPLAY "Ingrese tipo de movimiento: A / B / M".
           ACCEPT nov-mov.
           PERFORM 315-VALIDAR-TIPO-MOV.

       316-ING-OPCIONES.
           DISPLAY
               "Desea modificar el Nombre, una operacion, o ambos?"
               DISPLAY "1 - Nombre, 2 - Operacion, 3 - Ambos"
           ACCEPT opcion
           PERFORM 319-VALIDAR-OPC.

       318-OPCIONES.
               PERFORM 316-ING-OPCIONES.
               EVALUATE  opcion
	          WHEN 1
			            PERFORM 320-ING-NOMBRE
                MOVE 0 TO nov-importe
	          WHEN 2
			            PERFORM 330-ING-IMPORTE
               MOVE SPACE TO nov-nombre
	          WHEN 3
			            PERFORM 320-ING-NOMBRE
                PERFORM 330-ING-IMPORTE
               END-EVALUATE.


       319-VALIDAR-OPC.
           PERFORM UNTIL (opcion = 1 or opcion = 2 or opcion = 3)
               DISPLAY "Error. Inserte un tipo de opcion valida"
               ACCEPT opcion
           END-PERFORM.

       315-VALIDAR-TIPO-MOV.
           PERFORM UNTIL (nov-mov ="A" or nov-mov ="B" or nov-mov = "M")
               DISPLAY "Error. Inserte un tipo de operacion valida"
               ACCEPT nov-mov
           END-PERFORM.

       320-ING-NOMBRE.
           DISPLAY "Ingrese nombre de socio".
           ACCEPT nov-nombre.
           PERFORM 325-VALIDAR-NOM-SOC.

       325-VALIDAR-NOM-SOC.
           PERFORM UNTIL (nov-nombre <> " ")
               DISPLAY "Error. Inserte un Nombre de socio valido"
               ACCEPT nov-nombre
           END-PERFORM.

       330-ING-IMPORTE.
           DISPLAY "Ingrese saldo del socio".
           ACCEPT nov-importe.
           PERFORM 335-VALIDAR-IMP-SOC.

       335-VALIDAR-IMP-SOC.
           PERFORM UNTIL (nov-importe < 9999999)
               DISPLAY "Error. Inserte un Importe de valido"
               ACCEPT nov-importe
           END-PERFORM.

       400-PROCESO.
           ADD 1 TO cont.
           WRITE nov-reg.

       500-FIN.
           CLOSE NOVEDADES.


       END PROGRAM YOUR-PROGRAM-NAME.
