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
           SELECT MASTER-FILE ASSIGN TO "indexedMaster.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS M-ACC-NUMBER
               ALTERNATE KEY IS M-ACC-NAME WITH DUPLICATES
               FILE STATUS IS WS-MS-STATUS.

       DATA DIVISION.

       FILE SECTION.
       FD MASTER-FILE.
       01 MASTER-REC.
           05 M-ACC-NUMBER PIC X(10).
           05 M-ACC-NAME   PIC X(20).
           05 M-BALANCE    PIC S9(10).
           05 M-ACC-TYPE   PIC X.
           05 M-ACC-STATUS PIC X.

       WORKING-STORAGE SECTION.
       01 USER-INPUT PIC 9.
       01 DELETE-INPUT PIC 9.
       01 ADD-ACCOUNT-NAME PIC X(20).
       01 ADD-ACCOUNT-TYPE PIC X.
       01 WS-MS-STATUS PIC XX.
       01 WS-ACC-NUM PIC 9(10).
       01 VIEW-INPUT PIC 9.
       01 UPDATE-INPUT PIC 9.
       01 ACC-NUM-SEARCH PIC 9(10).
       01 ACC-NAME-SEARCH PIC X(20).
       01 ACC-NUM-DELETE PIC X(10).
       01 ACC-NAME-UPDATE PIC X(20).
       01 ACC-TYPE-UPDATE PIC X.
       01 WS-FOUND PIC X VALUE 'N'.
       01 WS-EOF PIC X VALUE 'N'.
       01 INPUT-FILTER PIC 9.
       01 FILTER-LOW-BOUND PIC S9(10).
       01 FILTER-HIGH-BOUND PIC S9(10).
       01 FILTER-TYPE PIC X.

       PROCEDURE DIVISION.
           PERFORM MAIN-PROCEDURE
           STOP RUN.

       ADD-ACCOUNT.
           PERFORM GENERATE-ACCOUNT-NUMBER

           DISPLAY "Account Name: "
           ACCEPT ADD-ACCOUNT-NAME
           MOVE ADD-ACCOUNT-NAME TO M-ACC-NAME

           DISPLAY "Account Type: "
           ACCEPT ADD-ACCOUNT-TYPE
           MOVE ADD-ACCOUNT-TYPE TO M-ACC-TYPE

           MOVE 0   TO M-BALANCE
           MOVE 'A' TO M-ACC-STATUS

           WRITE MASTER-REC
               INVALID KEY DISPLAY "Duplicate key"
           END-WRITE.

       SEARCH-BY-NUM.
           MOVE 'N' TO WS-FOUND
           DISPLAY "Account Number: ".
           ACCEPT ACC-NUM-SEARCH
           MOVE ACC-NUM-SEARCH TO M-ACC-NUMBER
           READ MASTER-FILE KEY IS M-ACC-NUMBER
               INVALID KEY
                   DISPLAY "Not Found"
                   MOVE 'N' TO WS-FOUND
               NOT INVALID KEY
                   MOVE 'Y' TO WS-FOUND
                   DISPLAY "Account Number: " M-ACC-NUMBER
                   DISPLAY "Account Name: " M-ACC-NAME
                   DISPLAY "Account Balance: " M-BALANCE
                   DISPLAY "Account Type: " M-ACC-TYPE
                   DISPLAY "Account Status: " M-ACC-STATUS
           END-READ.

       SEARCH-BY-NAME.
           DISPLAY "Account Name: "
           ACCEPT ACC-NAME-SEARCH

           MOVE ACC-NAME-SEARCH TO M-ACC-NAME
           MOVE 'N' TO WS-FOUND

           START MASTER-FILE KEY >= M-ACC-NAME
               INVALID KEY
                   DISPLAY "Not Found"
                   EXIT PARAGRAPH
           END-START

           PERFORM UNTIL WS-EOF = 'Y'
               READ MASTER-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF M-ACC-NAME = ACC-NAME-SEARCH
                           MOVE 'Y' TO WS-FOUND
                           DISPLAY "Account Number: " M-ACC-NUMBER
                           DISPLAY "Account Name: " M-ACC-NAME
                           DISPLAY "Account Balance: " M-BALANCE
                           DISPLAY "Account Type: " M-ACC-TYPE
                           DISPLAY "Account Status: " M-ACC-STATUS
                       ELSE
                           MOVE 'Y' TO WS-EOF
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND = 'N'
               DISPLAY "Not Found"
           END-IF

           MOVE 'N' TO WS-EOF.

       MAIN-PROCEDURE.
           OPEN I-O MASTER-FILE
           PERFORM UNTIL USER-INPUT = 7
               PERFORM UNTIL USER-INPUT >= 1 AND USER-INPUT <= 7
                   DISPLAY "Select An Option:"
                   DISPLAY "1: Add Account"
                   DISPLAY "2: View Account"
                   DISPLAY "3: Update Account"
                   DISPLAY "4: Delete Account"
                   DISPLAY "5: List All Accounts"
                   DISPLAY "6: Filter Accounts"
                   DISPLAY "7: Exit"

                   ACCEPT USER-INPUT

                   IF USER-INPUT < 1 OR USER-INPUT > 7
                       DISPLAY "Invalid, Enter a number from 1 to 7"
                   END-IF
               END-PERFORM

               EVALUATE USER-INPUT
                   WHEN 1 PERFORM ADD-ACCOUNT
                   WHEN 2 PERFORM VIEW-ACCOUNT
                   WHEN 3 PERFORM UPDATE-ACCOUNT
                   WHEN 4 PERFORM DELETE-ACCOUNT
                   WHEN 5 PERFORM LIST-ALL-ACCOUNTS
                   WHEN 6 PERFORM FILTER-ACCOUNTS
               END-EVALUATE
               MOVE 0 TO USER-INPUT
           END-PERFORM
           CLOSE MASTER-FILE.

       VIEW-ACCOUNT.
           PERFORM UNTIL VIEW-INPUT = 1 OR VIEW-INPUT = 2
               DISPLAY "1. Search By Account Number"
               DISPLAY "2. Search By Account Name"
               ACCEPT VIEW-INPUT
               IF VIEW-INPUT NOT = 1 AND VIEW-INPUT NOT = 2
                   DISPLAY "Invalid, Select 1 Or 2"
               END-IF
           END-PERFORM

           EVALUATE VIEW-INPUT
               WHEN 1 PERFORM SEARCH-BY-NUM
               WHEN 2 PERFORM SEARCH-BY-NAME
           END-EVALUATE.

       UPDATE-ACCOUNT.
           PERFORM UNTIL UPDATE-INPUT = 1 OR UPDATE-INPUT = 2
               DISPLAY "1. Search By Account Number"
               DISPLAY "2. Search By Account Name"
               ACCEPT UPDATE-INPUT
               IF UPDATE-INPUT NOT = 1 AND UPDATE-INPUT NOT = 2
                   DISPLAY "Invalid, Select 1 or 2"
               END-IF
           END-PERFORM

           EVALUATE UPDATE-INPUT
               WHEN 1 PERFORM SEARCH-BY-NUM
               WHEN 2 PERFORM SEARCH-BY-NAME
           END-EVALUATE.

           DISPLAY "New Name: "
           ACCEPT ACC-NAME-UPDATE
           DISPLAY "New Type: "
           ACCEPT ACC-TYPE-UPDATE
           IF WS-FOUND = 'Y'
               MOVE ACC-TYPE-UPDATE TO M-ACC-TYPE
               MOVE ACC-NAME-UPDATE TO M-ACC-NAME
               REWRITE MASTER-REC
           ELSE
               DISPLAY "Update Failed"
           END-IF.

       SOFT-DELETE.
           READ MASTER-FILE KEY IS M-ACC-NUMBER
               INVALID KEY
                   DISPLAY "Not Found"
               NOT INVALID KEY
                   MOVE "D" TO M-ACC-STATUS
                   REWRITE MASTER-REC
                       INVALID KEY DISPLAY "Delete Failed"
                   END-REWRITE
                   DISPLAY "Account Number: " M-ACC-NUMBER
                   DISPLAY "Account Name: " M-ACC-NAME
                   DISPLAY "Account Balance: " M-BALANCE
                   DISPLAY "Account Type: " M-ACC-TYPE
                   DISPLAY "Account Status: " M-ACC-STATUS
                   DISPLAY "DELETED"
           END-READ.

       HARD-DELETE.
           READ MASTER-FILE KEY IS M-ACC-NUMBER
               INVALID KEY
                   DISPLAY "Not Found"
               NOT INVALID KEY
                   DELETE MASTER-FILE RECORD
                       INVALID KEY
                           DISPLAY "Record Not Found"
                       NOT INVALID KEY
                           DISPLAY "Account " M-ACC-NUMBER " Deleted"
                   END-DELETE
           END-READ.

       DELETE-ACCOUNT.
           DISPLAY "Account Number: "
           ACCEPT ACC-NUM-DELETE
           MOVE ACC-NUM-DELETE TO M-ACC-NUMBER
           PERFORM UNTIL DELETE-INPUT = 1 OR DELETE-INPUT = 2
               DISPLAY "1: Hard Delete"
               DISPLAY "2: Soft Delete"
               ACCEPT DELETE-INPUT

               IF DELETE-INPUT NOT = 1 AND DELETE-INPUT NOT = 2
                   DISPLAY "Invalid, enter 1 or 2"
               END-IF
           END-PERFORM
           EVALUATE DELETE-INPUT
               WHEN 1 PERFORM HARD-DELETE
               WHEN 2 PERFORM SOFT-DELETE
           END-EVALUATE.

       LIST-ALL-ACCOUNTS.
           MOVE LOW-VALUES TO M-ACC-NUMBER
           START MASTER-FILE KEY >= M-ACC-NUMBER
               INVALID KEY
                   DISPLAY "Could Not Find"
           END-START

           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ MASTER-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF M-ACC-STATUS NOT = 'D'
                           DISPLAY MASTER-REC
                       END-IF
               END-READ
           END-PERFORM.

       GENERATE-ACCOUNT-NUMBER.
           MOVE HIGH-VALUES TO M-ACC-NUMBER
           START MASTER-FILE KEY <= M-ACC-NUMBER
               INVALID KEY
                   MOVE "0000000001" TO M-ACC-NUMBER
               NOT INVALID KEY
                   READ MASTER-FILE PREVIOUS RECORD
                       AT END
                           MOVE 1 TO WS-ACC-NUM
                       NOT AT END
                           MOVE M-ACC-NUMBER TO WS-ACC-NUM
                           ADD 1 TO WS-ACC-NUM
                   END-READ
                   MOVE WS-ACC-NUM TO M-ACC-NUMBER
           END-START.

       FILTER-BY-TYPE.
           PERFORM UNTIL FILTER-TYPE = 'C' OR FILTER-TYPE = 'S'
               DISPLAY "Account Type: "
               ACCEPT FILTER-TYPE
               IF FILTER-TYPE NOT = 'C' AND FILTER-TYPE NOT = 'S'
                   DISPLAY "Enter 'C' for checking or 'S' for savings"
               END-IF
           END-PERFORM

           MOVE LOW-VALUES TO M-ACC-NUMBER
           START MASTER-FILE KEY >= M-ACC-NUMBER
               INVALID KEY
                   DISPLAY "File empty or error"
                   EXIT PARAGRAPH
           END-START

           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ MASTER-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF M-ACC-STATUS NOT = 'D' AND
                           M-ACC-TYPE = FILTER-TYPE
                           DISPLAY MASTER-REC
                       END-IF
               END-READ
           END-PERFORM.

       FILTER-BY-BALANCE.

           PERFORM UNTIL FILTER-LOW-BOUND >= 0 AND
                   FILTER-HIGH-BOUND <= 9999999999 AND
                   FILTER-LOW-BOUND < FILTER-HIGH-BOUND

               DISPLAY "Low Bound: "
               ACCEPT FILTER-LOW-BOUND

               DISPLAY "High Bound: "
               ACCEPT FILTER-HIGH-BOUND

               IF FILTER-LOW-BOUND < 0 OR
                   FILTER-HIGH-BOUND > 9999999999 OR
                   FILTER-LOW-BOUND >= FILTER-HIGH-BOUND
                   DISPLAY "Invalid range. Try again."
               END-IF

           END-PERFORM

           MOVE LOW-VALUES TO M-ACC-NUMBER
           START MASTER-FILE KEY >= M-ACC-NUMBER
               INVALID KEY
                   DISPLAY "File empty or error"
                   EXIT PARAGRAPH
           END-START

           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ MASTER-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF M-ACC-STATUS NOT = 'D' AND
                           M-BALANCE >= FILTER-LOW-BOUND AND
                           M-BALANCE <= FILTER-HIGH-BOUND
                           DISPLAY MASTER-REC
                       END-IF
               END-READ
           END-PERFORM.

       FILTER-ACCOUNTS.
           PERFORM UNTIL INPUT-FILTER = 1 OR INPUT-FILTER = 2
               DISPLAY "1: Filter By Balance"
               DISPLAY "2: Filter By Type"
               ACCEPT INPUT-FILTER
               IF INPUT-FILTER NOT = 1 AND INPUT-FILTER NOT = 2
                   DISPLAY "Invalid, enter 1 or 2"
               END-IF
           END-PERFORM

           EVALUATE INPUT-FILTER
               WHEN 1 PERFORM FILTER-BY-BALANCE
               WHEN 2 PERFORM FILTER-BY-TYPE
           END-EVALUATE.
