      ******************************************************************
      * Author: CESAR SANGABRIEL MARTINEZ
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Agenda.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *NADA
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BASE ASSIGN
           "C:/cobol/Programas/base.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD BASE.
           01 REG-CONTACTO        PIC X(93).


       WORKING-STORAGE SECTION.

       77  WS-SW                  PIC X VALUE SPACES.
       77  WS-CONTADOR            PIC 99 VALUE 11.

       77  WS-PAUSAR-SC           PIC X VALUE SPACES.

       77  WS-OPCION              PIC X VALUE SPACES.



       01  WS-AREAS-A-USAR.
           05 WS-CONTACTO.

              10 WS-NOMBRE        PIC A(10).
              10 WS-APE-PATERNO   PIC A(12).
              10 WS-APE-MATERNO   PIC A(12).
              10 WS-NUM-CEL       PIC 9(10).
              10 FILLER           PIC X(03).
              10 WS-CORREO        PIC A(40).


       SCREEN SECTION.


       01  SS-PRINCIPAL
           FOREGROUND-COLOR IS 02 HIGHLIGHT.
           05 LINE 03 COL 10 VALUE
           "==========================================================".
           05 LINE 04 COL 30 VALUE "AGENDA TELEFONICA".
           05 LINE 05 COL 10 VALUE
           "==========================================================".
           05 LINE 06 COL 10 VALUE "1 NUEVO CONTACTO ".
           05 LINE 06 COL 35 VALUE "2 EDITAR ".
           05 LINE 06 COL 56 VALUE "3 SALIR ".
           05 LINE 07 COL 10 VALUE
           "__________________________________________________________".
           05 LINE 23 COL 10 VALUE "OPCION ".
           05 LINE 25 COL 10 VALUE
           "__________________________________________________________".
           05 PIC X USING WS-OPCION LINE 23 COL 17.


       01  SS-NUEVO-CONTACTO
           FOREGROUND-COLOR IS 02 HIGHLIGHT.
           05 LINE 14 COL 30 VALUE
           "_________________________________________________".
           05 LINE 15 COL 30 VALUE
           "|NOMBRE:                                        |".
           05 LINE 16 COL 30 VALUE
           "|APELLIDO PATERNO:                              |".
           05 LINE 17 COL 30 VALUE
           "|APELLIDO MATERNO:                              |".
           05 LINE 18 COL 30 VALUE
           "|TELEFONO:                                      |".
           05 LINE 19 COL 30 VALUE
           "|CORREO:                                        |".
           05 LINE 20 COL 30 VALUE
           "-------------------------------------------------".
           05 PIC X USING WS-PAUSAR-SC LINE 15 COL 50.

       01  SS-PREG-CONTACTO
           FOREGROUND-COLOR IS 02 HIGHLIGHT.
           05 LINE 21 COL 30 VALUE "¿DESEA AGREGAR OTRO CONTACTO? S/N".
           05 PIC X USING WS-OPCION LINE 21 COL 64.


       01  SS-IMPRIME-ARCHIVO
           FOREGROUND-COLOR IS 02 HIGHLIGHT.
           05 LINE 15 COL 35 VALUE "HOLA ARCHIVO ".
           05 PIC X USING WS-OPCION LINE 23 COL 17.

       01  SS-CONTACTOS
           FOREGROUND-COLOR IS 02 HIGHLIGHT.
           05 LINE 10 COL 03 VALUE "NOMBRE ".
           05 LINE 10 COL 13 VALUE "A.PATERNO ".
           05 LINE 10 COL 25 VALUE "A.MATERNO ".
           05 LINE 10 COL 37 VALUE "TELEFONO ".
           05 LINE 10 COL 50 VALUE "CORREO ".
           05 PIC X USING WS-PAUSAR-SC LINE 17 COL 26.

       01  SS-OPCION-INVALIDA
           FOREGROUND-COLOR IS 02 HIGHLIGHT.
           05 LINE 23 COL 10 VALUE "Ingrese una opcion valida".
           05 PIC X USING WS-OPCION LINE 23 COL 48.

       01  SS-LIMPIAR-PANTALLA.
           05 BLANK SCREEN.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

       100-MUESTRA-PANTALLA-INICIAL.
           DISPLAY SS-LIMPIAR-PANTALLA

           DISPLAY SS-CONTACTOS.
           110-ABRIR-ARCHIVO.
               OPEN INPUT BASE.
           110-FIN. EXIT.

           120-LEER-ARCHIVO.

               PERFORM UNTIL WS-SW = 'Y'
                   ADD 1 TO WS-CONTADOR
                   READ BASE INTO WS-CONTACTO
                   AT END MOVE 'Y' TO WS-SW
                   NOT AT END DISPLAY WS-CONTACTO LINE WS-CONTADOR COL 3
                   END-READ
               END-PERFORM.
               MOVE ' ' TO WS-SW.
               MOVE 11 TO WS-CONTADOR.

           120-FIN. EXIT.
           130-CERRAR-ARCHIVO.
               CLOSE BASE.
           130-FIN. EXIT.


           DISPLAY SS-PRINCIPAL.
           ACCEPT  SS-PRINCIPAL.


           IF      WS-OPCION = 1
              PERFORM 200-NUEVO-CONTACTO THRU 200-FIN
           ELSE IF WS-OPCION = 2
               PERFORM 300-IMPRIME-ARCHIVO THRU 300-FIN
           ELSE IF WS-OPCION = 3
               PERFORM 400-SALIR
           ELSE IF WS-OPCION NOT EQUAL TO '1' OR '2' OR '3'
               DISPLAY SS-OPCION-INVALIDA
               ACCEPT SS-OPCION-INVALIDA
               PERFORM 100-MUESTRA-PANTALLA-INICIAL THRU 100-FIN

           END-IF.

       100-FIN. EXIT.

       GOBACK.

       200-NUEVO-CONTACTO.
           DISPLAY SS-NUEVO-CONTACTO


           ACCEPT WS-NOMBRE         LINE 15 COL 50.
           ACCEPT WS-APE-PATERNO    LINE 16 COL 50.
           ACCEPT WS-APE-MATERNO    LINE 17 COL 50.
           ACCEPT WS-NUM-CEL        LINE 18 COL 50.
           ACCEPT WS-CORREO         LINE 19 COL 50.


       210-ABRIR-ARCHIVO.
           OPEN EXTEND BASE.
       210-FIN. EXIT.
       220-ESCRIBIR-ARCHIVO.
           WRITE REG-CONTACTO FROM WS-CONTACTO.
       220-FIN. EXIT.
       230-CERRA-ARCHIVO.
               CLOSE BASE.
       230-FIN. EXIT.


           DISPLAY SS-PREG-CONTACTO
           ACCEPT  SS-PREG-CONTACTO
           IF      WS-OPCION = "s" OR "S"
               PERFORM 200-NUEVO-CONTACTO THRU 200-FIN
           ELSE IF WS-OPCION = "n" OR "N"
               PERFORM 100-MUESTRA-PANTALLA-INICIAL THRU 100-FIN
           END-IF.

           EXIT.


       200-FIN. EXIT.

       300-IMPRIME-ARCHIVO.
           DISPLAY SS-IMPRIME-ARCHIVO
           ACCEPT SS-IMPRIME-ARCHIVO.

       300-FIN. EXIT.

       400-SALIR.
           DISPLAY " ".
       400-FIN. EXIT.

       iyljlljljk
