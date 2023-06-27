      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID.                PGM_MAX_MIN.
      * AUTHOR.                   AYELEN RIVERO.

      * DATE-WRITTEN.              26/06/23.
      * DATE-COMPILED.

      * ------------------------------------------------------
      * PROGRAMA QUE EMITE UN REPORTE DE UN ARCHIVO CLASIFICADO
      * SE UTILIZA LA INSTRUCCION SORT CON LAS OPCIONES DE
      * INPUT PROCEDURE Y OUTPUT PROCEDURE
      * ------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLEADOS    ASSIGN TO
           "C:\Users\ayele\OneDrive\Escritorio\SortMaxMin\EMPLEA2.txt"
                               ORGANIZATION IS SEQUENTIAL
                               ACCESS MODE  IS SEQUENTIAL
                               FILE STATUS  IS FS-EMPLEADOS.
           SELECT REPORTE      ASSIGN TO
           "C:\Users\ayele\OneDrive\Escritorio\SortMaxMin\sortmaxmi.TXT"
                               ORGANIZATION IS SEQUENTIAL
                               ACCESS MODE  IS SEQUENTIAL
                               FILE STATUS  IS FS-REPORTE.

           SELECT ARCHIVO-SORT ASSIGN TO
           "C:\Users\ayele\OneDrive\Escritorio\SortMaxMin\ARCHSORT.TXT".

      *


       DATA DIVISION.
       FILE SECTION.
       FD  EMPLEADOS
           RECORDING MODE IS F.
       01  REG-EMPLEADOS          PIC X(73).

       FD  REPORTE
           RECORD CONTAINS 100 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  REG-REPORTE            PIC X(100).

       SD  ARCHIVO-SORT
           RECORD CONTAINS 73 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.

       01  REG-SORT.
           05 SORT-LEGAJO-EMP     PIC 9(03).
           05 SORT-NOMBRE-EMP     PIC X(15).
           05 SORT-APELLIDO-EMP   PIC X(15).
           05 SORT-STATUS-EMP     PIC 9(01).
           05 SORT-DEPTO-EMP      PIC X(15).
           05 SORT-PUESTO-EMP     PIC X(15).
           05 SORT-SALARIO-EMP    PIC 9(07)V99.

       WORKING-STORAGE SECTION.

       77  FS-EMPLEADOS                 PIC  X(02) VALUE ' '.
           88 88-FS-EMPLEADOS-YES                  VALUE '00'.
           88 88-FS-EMPLEADOS-EOF                  VALUE '10'.

       77  FS-REPORTE                    PIC  X(02) VALUE ' '.
           88 88-FS-REPORTE-YES                     VALUE '00'.
           88 88-FS-REPORTE-EOF                     VALUE '10'.

       77  WS-OPEN-EMPLEADOS             PIC  X(02) VALUE 'YO'.
           88 88-OPEN-EMPLEADOS-YES                 VALUE 'YS'.
           88 88-OPEN-EMPLEADOS-NO                  VALUE 'N'.

       77  WS-OPEN-REPORTE               PIC  X(02) VALUE 'YO'.
           88 88-OPEN-REPORTE-YES                   VALUE 'YS'.
           88 88-OPEN-REPORTE-NO                    VALUE 'N'.

       77  FS-ARCH-SORT                   PIC  X(02) VALUE ' '.
           88 88-FS-ARCH-YES                     VALUE '00'.
           88 88-FS-ARCH-EOF                     VALUE '10'.

       77  WS-OPEN-ARCH-SORT              PIC  X(02) VALUE 'YO'.
           88 88-OPEN-SORT-YES                   VALUE 'YS'.
           88 88-OPEN-SORT-NO                    VALUE 'N'.



      * COPY EMPLEADOS.

       01  WS-AREAS-A-USAR.
           05 WS-REG-EMPLEADOS.
              10 WS-LEGAJO-EMP    PIC X(03).
              10 WS-NOMBRE-EMP    PIC X(15).
              10 WS-APELLIDO-EMP  PIC X(15).
              10 WS-STATUS-EMP    PIC 9(01).
              10 WS-DEPTO-EMP     PIC X(15).
              10 WS-PUESTO-EMP    PIC X(15).
              10 WS-SALARIO-EMP   PIC 9(07)V99.
           05 WS-LEIDOS-EMP       PIC 9(05)    VALUE 0.
           05 WS-IMPRESOS         PIC 9(05)    VALUE 0.
           05 WS-TOT-SALARIOS     PIC 9(09)V99 VALUE 0.
           05 SW-FIN              PIC X(03)    VALUE ' '.
           05 WS-SELECCIONADOS    PIC 9(09)    VALUE 0.
      *


       01  WS-CURRENT-DATE.
           03 WS-ACTUAL-DATE.
              05 WS-DATE-AAAA     PIC 9(04).
              05 WS-DATE-MM       PIC 9(02).
              05 WS-DATE-DD       PIC 9(02).

      * TITULOS.

       01  WS-TITULO-1.
           03 FILLER              PIC X(38)    VALUE ' '.
           03 WS-TIT-1            PIC X(23)
                                  VALUE "EMPLEADOS DE LA EMPRESA".
           03 FILLER              PIC X(39)    VALUE ' '.

       01  WS-TITULO-2.
           03 FILLER              PIC X(08)    VALUE " FECHA: ".
           03 WS-TIT2-FECHA.
               05 TIT-2-DD         PIC 9(02).
               05 FILLER           PIC X(01)    VALUE "/".
               05 TIT-2-MM         PIC 9(02).
               05 FILLER           PIC X(01)    VALUE "/".
               05 TIT-2-AAAA       PIC 9(04).

           03 WS-TIT-2.
               05 FILLER           PIC X(70)    VALUE ' '.
               05 FILLER           PIC X(08)    VALUE "PAGINA: ".
               05 TIT-2-PAGINA     PIC ZZ9.
               05 FILLER           PIC X(01)    VALUE ' '.

       01  WS-GUIONES.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(98)    VALUE ALL "-".
           05 FILLER              PIC X(01)    VALUE ' '.

       01  WS-SUB-TITULO-1.
           05 FILLER              PIC X(01)    VALUE ' '.
           05 FILLER              PIC X(06)    VALUE "LEGAJO".
           05 FILLER              PIC X(02)    VALUE ' '.
           05 FILLER              PIC X(06)    VALUE "NOMBRE".
           05 FILLER              PIC X(10)    VALUE ' '.
           05 FILLER              PIC X(08)    VALUE "APELLIDO".
           05 FILLER              PIC X(07)    VALUE ' '.
           05 FILLER              PIC X(06)    VALUE "STATUS".
           05 FILLER              PIC X(02)    VALUE ' '.
           05 FILLER              PIC X(05)    VALUE "DEPTO".
           05 FILLER              PIC X(11)    VALUE ' '.
           05 FILLER              PIC X(06)    VALUE "PUESTO".
           05 FILLER              PIC X(13)    VALUE ' '.
           05 FILLER              PIC X(07)    VALUE "SALARIO".
           05 FILLER              PIC X(09)    VALUE ' '.

       01  WS-DETALLE.
           05 FILLER              PIC X(02)    VALUE ' '.
           05 WS-DET-LEGAJO       PIC ZZ9.
           05 FILLER              PIC X(04)    VALUE ' '.
           05 WS-DET-NOMBRE       PIC X(15).
           05 FILLER              PIC X(01)    VALUE ' '.
           05 WS-DET-APE          PIC X(15).
           05 FILLER              PIC X(01)    VALUE ' '.
           05 WS-DET-STATUS       PIC 9(01).
           05 FILLER              PIC X(06)    VALUE ' '.
           05 WS-DET-DEPTO        PIC X(15).
           05 FILLER              PIC X(01)    VALUE ' '.
           05 WS-DET-PUESTO       PIC X(15).
           05 FILLER              PIC X(01)    VALUE ' '.
           05 WS-DET-SALARIO      PIC Z,ZZZ,ZZ9.99.
           05 FILLER              PIC X(10)     VALUE ' '.

       01  WS-DETALLE-LEIDOS.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(29)
                                  VALUE "TOTAL DE EMPLEADOS LEIDOS  : ".
           05 WS-TOT-LEIDOS       PIC ZZ,ZZ9.
           05 FILLER              PIC X(44)    VALUE ' '.

       01  WS-DETALLE-IMPRESOS.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(29)
                                  VALUE "TOTAL DE EMPLEADOS IMPRESOS: ".
           05 WS-TOT-IMPRESOS     PIC ZZ,ZZ9.
           05 FILLER              PIC X(44)    VALUE ' '.

       01  WS-DETALLE-SALARIOS.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(29)
                                  VALUE "SUMA TOTAL DE SALARIOS    : ".
           05 WS-DET-SALARIO2     PIC $$$,$$$,$$9.99.
           05 FILLER              PIC X(36)    VALUE ' '.

       01  I                      PIC 9(2).
       01  CONTADOR OCCURS 7 TIMES PIC 9.
      *     05 SALARIO             PIC 9(6).

       01  WS-MAX-SALARIO      PIC 9(07)V99 VALUE 99999.
       01  WS-MIN-SALARIO      PIC 9(07)V99 VALUE 31000.


       01  WS-DET-MAX-SAL.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(16) VALUE "SALARIO MAXIMO:".
           05 WS-SAL-MAX          PIC Z,ZZZ,ZZ9.99.
           05 FILLER              PIC X(73) VALUE ' '.

       01  WS-DET-MIN-SAL.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(16) VALUE "SALARIO MINIMO:".
           05 WS-SAL-MIN          PIC Z,ZZZ,ZZ9.99.
           05 FILLER              PIC X(73) VALUE ' '.



      * DEFINICION DE FECHA ACTUAL

       01  CURRENT-DATE.
           05 DATE-DD             PIC 9(02).
           05 FILLER              PIC X     VALUE '/'.
           05 ATE-MM              PIC 9(02).
           05 FILLER              PIC X     VALUE '/'.
           05 DATE-AAA            PIC 9(04).


       PROCEDURE DIVISION.

           PERFORM 010-INICIO.


       010-INICIO.

           SORT ARCHIVO-SORT ON ASCENDING KEY
                SORT-SALARIO-EMP
                SORT-LEGAJO-EMP
           INPUT  PROCEDURE 020-ORDENA  THRU 020-FIN
           OUTPUT PROCEDURE 100-IMPRIME THRU 100-FIN


           PERFORM 055-CALC-MIN-MAX THRU 055-FIN.

           GOBACK.

       020-ORDENA.
           PERFORM 030-ABRE-ARCHIVOS THRU 030-FIN
           PERFORM 040-LEE-EMPLEADOS THRU 040-FIN
           PERFORM 040-SELECCIONA    THRU 040-FIN
                   UNTIL SW-FIN EQUAL "FIN".



       020-FIN.  EXIT.

       030-ABRE-ARCHIVOS.
           OPEN INPUT  EMPLEADOS
                OUTPUT REPORTE.


       030-FIN.  EXIT.

       040-SELECCIONA.
           IF WS-SALARIO-EMP >= 31000
              THEN
                 RELEASE REG-SORT FROM WS-REG-EMPLEADOS
                 ADD 1 TO WS-SELECCIONADOS
              END-IF
           ADD 1 TO WS-LEIDOS-EMP.




       040-LEE-EMPLEADOS.
           READ EMPLEADOS INTO WS-REG-EMPLEADOS AT END
                MOVE "FIN" TO SW-FIN.



       040-FIN.  EXIT.


       055-CALC-MIN-MAX.
           READ EMPLEADOS


               IF WS-SALARIO-EMP <= 31000
                   MOVE WS-SALARIO-EMP     TO WS-MIN-SALARIO
      *

               END-IF


               IF WS-SALARIO-EMP > 31000
               AND WS-SALARIO-EMP < 99999
                   MOVE WS-SALARIO-EMP     TO WS-MAX-SALARIO


               END-IF


           PERFORM 200-FINAL THRU 200-FIN.

       055-FIN.

       100-IMPRIME.
           WRITE REG-REPORTE FROM WS-TITULO-1

           MOVE FUNCTION CURRENT-DATE    TO WS-CURRENT-DATE

           MOVE WS-DATE-DD                    TO TIT-2-DD
           MOVE WS-DATE-MM                    TO TIT-2-MM
           MOVE WS-DATE-AAAA                  TO TIT-2-AAAA
           MOVE 1                             TO TIT-2-PAGINA
           WRITE REG-REPORTE FROM WS-TITULO-2 AFTER ADVANCING 1
           WRITE REG-REPORTE FROM WS-GUIONES  AFTER ADVANCING 1
           WRITE REG-REPORTE FROM WS-SUB-TITULO-1 AFTER ADVANCING 1
           WRITE REG-REPORTE FROM WS-GUIONES AFTER ADVANCING 1


           MOVE SPACES TO SW-FIN
           PERFORM 120-LEE-SORT THRU 120-FIN
           PERFORM 120-REPORTE  THRU 120-FIN
                   UNTIL SW-FIN EQUAL "FIN".


       100-FIN.  EXIT.



       110-FIN.  EXIT.

       120-REPORTE.
           ADD WS-SALARIO-EMP     TO WS-TOT-SALARIOS
           MOVE WS-LEGAJO-EMP     TO WS-DET-LEGAJO
           MOVE WS-NOMBRE-EMP     TO WS-DET-NOMBRE
           MOVE WS-APELLIDO-EMP   TO WS-DET-APE
           MOVE WS-STATUS-EMP     TO WS-DET-STATUS
           MOVE WS-DEPTO-EMP      TO WS-DET-DEPTO
           MOVE WS-PUESTO-EMP     TO WS-DET-PUESTO
           MOVE WS-SALARIO-EMP    TO WS-DET-SALARIO
           WRITE REG-REPORTE FROM WS-DETALLE AFTER ADVANCING 1

           ADD 1 TO WS-IMPRESOS.



       120-LEE-SORT.
           RETURN ARCHIVO-SORT INTO WS-REG-EMPLEADOS AT END
                MOVE "FIN" TO SW-FIN.

       120-FIN.  EXIT.

       200-FINAL.
           MOVE WS-LEIDOS-EMP     TO WS-TOT-LEIDOS
           WRITE REG-REPORTE FROM WS-DETALLE-LEIDOS AFTER ADVANCING 2
           MOVE WS-IMPRESOS       TO WS-TOT-IMPRESOS
           WRITE REG-REPORTE FROM WS-DETALLE-IMPRESOS AFTER ADVANCING 1
           MOVE WS-TOT-SALARIOS   TO WS-DET-SALARIO2
           WRITE REG-REPORTE FROM WS-DETALLE-SALARIOS AFTER ADVANCING 1
           MOVE WS-MAX-SALARIO    TO WS-SAL-MAX
           WRITE REG-REPORTE FROM WS-DET-MAX-SAL    AFTER ADVANCING 1
           MOVE WS-MIN-SALARIO    TO WS-SAL-MIN
           WRITE REG-REPORTE FROM WS-DET-MIN-SAL    AFTER ADVANCING 1

           CLOSE EMPLEADOS REPORTE.


       200-FIN.  EXIT.
