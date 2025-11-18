       IDENTIFICATION DIVISION.
       PROGRAM-ID. LoginValidator.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 USER-ID          PIC X(08) DISPLAY.
       01 USER-TYPE        PIC X(01) DISPLAY.
       01 LOGIN-ATTEMPTS   PIC 9(02) COMP.
       01 LOGIN-HOUR       PIC 99 COMP.
       01 ACCESS-LEVEL     PIC 9(01) COMP.
       01 LOCKED           PIC X(01) VALUE "N".
       01 MESSAGES          PIC X(20).

       PROCEDURE DIVISION.

           IF LOGIN-ATTEMPTS > 3
               MOVE "Y" TO LOCKED
               MOVE "TOO MANY ATTEMPTS" TO MESSAGES
           ELSE
               IF LOGIN-HOUR < 8 OR LOGIN-HOUR > 18
                   MOVE "Y" TO LOCKED
                   MOVE "OUTSIDE ACCESS TIME" TO MESSAGES
               ELSE
                   IF USER-TYPE = "A"
                       COMPUTE ACCESS-LEVEL = 5
                       MOVE "ADMIN ACCESS" TO MESSAGES
                   ELSE
                       IF USER-TYPE = "U"
                           COMPUTE ACCESS-LEVEL = 3
                           MOVE "USER ACCESS" TO MESSAGES
                       ELSE
                           COMPUTE ACCESS-LEVEL = 1
                           MOVE "GUEST ACCESS" TO MESSAGES
                       END-IF
                   END-IF
               END-IF
           END-IF.

           STOP RUN.
