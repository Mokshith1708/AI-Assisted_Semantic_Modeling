       IDENTIFICATION DIVISION.
       PROGRAM-ID. ExtendedLoanRiskAssessment.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  CUST-ID           PIC X(10).
       01  CUST-NAME         PIC A(40).
       01  AGE               PIC 9(03) COMP-4.
       01  INCOME            PIC S9(09)V99 COMP-5.
       01  LOAN-AMOUNT       PIC S9(09)V99 COMP-3.
       01  LOAN-TENURE       PIC 9(02) COMP-4.
       01  EMPLOYMENT-YEARS  PIC 9(02) COMP.
       01  CREDIT-SCORE      PIC 9(03) COMP.
       01  RISK-FACTOR       PIC S9(03)V9(02) COMP-3.
       01  LOAN-STAT         PIC X(20).
       01  RISK-REMARK       PIC X(40).

       PROCEDURE DIVISION.

           IF AGE < 21
               MOVE "REJECTED            " TO LOAN-STAT
               MOVE "AGE TOO LOW" TO RISK-REMARK
           ELSE
               IF CREDIT-SCORE < 500
                   MOVE "REJECTED            " TO LOAN-STAT
                   MOVE "CREDIT TOO LOW" TO RISK-REMARK
               ELSE
                   IF INCOME < LOAN-AMOUNT
                       MOVE "REJECTED            " TO LOAN-STAT
                       MOVE "LOW INCOME" TO RISK-REMARK
                   ELSE
                       IF EMPLOYMENT-YEARS < 1
                           MOVE "REJECTED            " TO LOAN-STAT
                           MOVE "UNSTABLE JOB" TO RISK-REMARK
                       ELSE
                           IF LOAN-TENURE > 30
                               MOVE "REJECTED            " TO LOAN-STAT
                               MOVE "TENURE TOO LONG" TO RISK-REMARK
                           ELSE
                               IF CREDIT-SCORE > 750 AND
                                  INCOME > 800000
                                   MOVE "APPROVED PRIME      "
                                        TO LOAN-STAT
                                   MOVE "HIGH PROFILE CLIENT"
                                        TO RISK-REMARK
                               ELSE
                                   IF INCOME > 400000
                                       MOVE "APPROVED STANDARD   "
                                            TO LOAN-STAT
                                       MOVE "MODERATE RISK"
                                            TO RISK-REMARK
                                   ELSE
                                       MOVE "REVIEW              "
                                            TO LOAN-STAT
                                       MOVE "NEEDS MANUAL REVIEW"
                                            TO RISK-REMARK
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

           STOP RUN.
