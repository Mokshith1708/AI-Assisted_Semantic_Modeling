       IDENTIFICATION DIVISION.
       PROGRAM-ID. ComplexTest.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 X           PIC 9(03) DISPLAY.

       01 Y           PIC S9(03) COMP.

       01 Z           PIC S9(03)V9(02) COMP-3.

       01 RESULT1     PIC S9(03) SIGN TRAILING.
       01 RESULT2     PIC S9(03)V9(02) COMP-3.
       01 MSG         PIC X(15).

       PROCEDURE DIVISION.

           COMPUTE RESULT1 = X + Y

           IF RESULT1 > 200
               COMPUTE RESULT2 = (Z + 10.75) / (Y + 1)
               IF RESULT2 < 10.00
                   MOVE "SMALL DIV" TO MSG
               ELSE
                   MOVE "LARGE DIV" TO MSG
               END-IF
           ELSE
               IF X = Y
                   MOVE "X=Y CASE" TO MSG
               ELSE
                   COMPUTE RESULT2 = Z + 5.25
                   MOVE "OTHER CASE" TO MSG
               END-IF
           END-IF.

           STOP RUN.
