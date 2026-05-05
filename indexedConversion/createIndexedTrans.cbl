      ******************************************************************
      * Converts a line sequential transaction file to a native indexed
      * file.
      * Usage:
      *   createIndexedTrans [input-transactions] [output-indexed-trans]
      * Defaults:
      *   input-transactions   = transactions.dat
      *   output-indexed-trans = indexedTransactions
      *
      * Primary key  : account-number + transaction-id
      * Alternate key: account-number WITH DUPLICATES
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-INDEXED-TRANS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-TRANS-FILE ASSIGN TO DYNAMIC WS-INPUT-FILE
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.

           SELECT OUTPUT-TRANS-FILE ASSIGN TO DYNAMIC WS-OUTPUT-FILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS O-TRANS-KEY
               ALTERNATE RECORD KEY IS O-ACC-NUMBER
                   WITH DUPLICATES
               FILE STATUS IS WS-OUT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  INPUT-TRANS-FILE.
       01  INPUT-TRANS-REC.
           05 I-ACC-NUMBER    PIC X(10).
           05 I-ID            PIC 9(8).
           05 I-DATE          PIC X(8).
           05 I-TIME          PIC X(6).
           05 I-TRX-TYPE      PIC X.
           05 I-AMOUNT        PIC 9(10).

       FD  OUTPUT-TRANS-FILE.
       01  OUTPUT-TRANS-REC.
           05 O-TRANS-KEY.
               10 O-ACC-NUMBER PIC X(10).
               10 O-ID         PIC 9(8).
           05 O-DATE          PIC X(8).
           05 O-TIME          PIC X(6).
           05 O-TRX-TYPE      PIC X.
           05 O-AMOUNT        PIC 9(10).

       WORKING-STORAGE SECTION.
       01  WS-CMD-LINE                PIC X(200).
       01  WS-ARG-INPUT               PIC X(200).
       01  WS-ARG-OUTPUT              PIC X(200).
       01  WS-INPUT-FILE              PIC X(200)
           VALUE "transactions.dat".
       01  WS-OUTPUT-FILE             PIC X(200)
           VALUE "indexedTransactions".
       01  WS-IN-STATUS               PIC XX.
       01  WS-OUT-STATUS              PIC XX.
       01  WS-EOF                     PIC X VALUE "N".
           88 INPUT-EOF VALUE "Y".
       01  WS-RECORDS-READ            PIC 9(9) VALUE 0.
       01  WS-RECORDS-WRITTEN         PIC 9(9) VALUE 0.
       01  WS-DUPLICATES-SKIPPED      PIC 9(9) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM LOAD-ARGUMENTS
           PERFORM OPEN-FILES
           PERFORM READ-FIRST-RECORD

           PERFORM UNTIL INPUT-EOF
               ADD 1 TO WS-RECORDS-READ
               MOVE I-ACC-NUMBER TO O-ACC-NUMBER
               MOVE I-ID         TO O-ID
               MOVE I-DATE       TO O-DATE
               MOVE I-TIME       TO O-TIME
               MOVE I-TRX-TYPE   TO O-TRX-TYPE
               MOVE I-AMOUNT     TO O-AMOUNT

               WRITE OUTPUT-TRANS-REC
                   INVALID KEY
                       ADD 1 TO WS-DUPLICATES-SKIPPED
                       DISPLAY "Duplicate transaction skipped: "
                           O-ACC-NUMBER " / " O-ID
                   NOT INVALID KEY
                       ADD 1 TO WS-RECORDS-WRITTEN
               END-WRITE

               READ INPUT-TRANS-FILE
                   AT END
                       SET INPUT-EOF TO TRUE
               END-READ
           END-PERFORM

           CLOSE INPUT-TRANS-FILE
                 OUTPUT-TRANS-FILE

           DISPLAY "Transaction conversion complete."
           DISPLAY "Input file : " WS-INPUT-FILE
           DISPLAY "Output file: " WS-OUTPUT-FILE
           DISPLAY "Records read    : " WS-RECORDS-READ
           DISPLAY "Records written : " WS-RECORDS-WRITTEN
           DISPLAY "Duplicates skipped: " WS-DUPLICATES-SKIPPED

           STOP RUN.

       LOAD-ARGUMENTS.
           ACCEPT WS-CMD-LINE FROM COMMAND-LINE

           IF WS-CMD-LINE NOT = SPACES
               UNSTRING WS-CMD-LINE DELIMITED BY ALL SPACES
                   INTO WS-ARG-INPUT
                        WS-ARG-OUTPUT
               END-UNSTRING

               IF WS-ARG-INPUT NOT = SPACES
                   MOVE WS-ARG-INPUT TO WS-INPUT-FILE
               END-IF

               IF WS-ARG-OUTPUT NOT = SPACES
                   MOVE WS-ARG-OUTPUT TO WS-OUTPUT-FILE
               END-IF
           END-IF.

       OPEN-FILES.
           OPEN INPUT INPUT-TRANS-FILE
           IF WS-IN-STATUS NOT = "00"
               DISPLAY "Unable to open input transaction file: "
                   WS-INPUT-FILE
               DISPLAY "Input status: " WS-IN-STATUS
               STOP RUN
           END-IF

           OPEN OUTPUT OUTPUT-TRANS-FILE
           IF WS-OUT-STATUS NOT = "00"
               DISPLAY "Unable to create indexed transaction file: "
                   WS-OUTPUT-FILE
               DISPLAY "Output status: " WS-OUT-STATUS
               STOP RUN
           END-IF.

       READ-FIRST-RECORD.
           READ INPUT-TRANS-FILE
               AT END
                   SET INPUT-EOF TO TRUE
           END-READ.

       END PROGRAM CREATE-INDEXED-TRANS.
