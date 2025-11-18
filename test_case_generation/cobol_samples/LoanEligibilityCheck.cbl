       IDENTIFICATION DIVISION.
       PROGRAM-ID. LoanEligibilityCheck.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  CUST-ID           PIC X(10).
       01  CUST-NAME         PIC X(20).
       01  AGE               PIC 9(02) COMP.
       01  INCOME            PIC S9(06)V99 COMP-3.
       01  CREDIT-SCORE      PIC 9(03) COMP.
       01  LOAN-AMT          PIC S9(05)V99 COMP-3.
       01  INTEREST-RATE     PIC S9(02)V9(2) COMP-3.
       01  LSTATUS            PIC X(15).
       01  REMARK           PIC X(40).

       PROCEDURE DIVISION.

           IF AGE < 21
               MOVE "NOT ELIGIBLE" TO LSTATUS
               MOVE "AGE BELOW THRESHOLD" TO REMARK
           ELSE
               IF CREDIT-SCORE >= 750
                   IF INCOME >= 50000
                       COMPUTE LOAN-AMT = INCOME * 0.60
                       COMPUTE INTEREST-RATE = 5.25
                       MOVE "APPROVED" TO LSTATUS
                       MOVE "PRIME CUSTOMER" TO REMARK
                   ELSE
                       MOVE "REJECTED" TO LSTATUS
                       MOVE "INSUFFICIENT INCOME" TO REMARK
                   END-IF
               ELSE
                   IF CREDIT-SCORE >= 600
                       IF INCOME >= 30000
                           COMPUTE LOAN-AMT = INCOME * 0.40
                           COMPUTE INTEREST-RATE = 7.75
                           MOVE "APPROVED" TO LSTATUS
                           MOVE "STANDARD CUSTOMER" TO REMARK
                       ELSE
                           MOVE "REJECTED" TO LSTATUS
                           MOVE "LOW INCOME" TO REMARK
                       END-IF
                   ELSE
                       MOVE "REJECTED" TO LSTATUS
                       MOVE "POOR CREDIT SCORE" TO REMARK
                   END-IF
               END-IF
           END-IF.

           STOP RUN.
