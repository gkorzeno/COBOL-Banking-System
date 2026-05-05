# COBOL Banking System Simulation

## Overview

This project simulates a simplified banking backend using GnuCOBOL, demonstrating both sequential and indexed file processing techniques commonly used in legacy and mainframe systems.

The system evolves through multiple stages:

1. Data generation using Python
2. Sequential transaction processing
3. Conversion to indexed files
4. Indexed transaction processing
5. Interactive mini database

This progression highlights the differences between sequential and indexed file organizations, as well as batch vs interactive processing.

---

## Project Structure

* **data-generation/**: Python scripts to generate test data
* **sequential-processing/**: Processes transactions using sequential files
* **indexed-conversion/**: Converts sequential files to indexed format
* **indexed-processing/**: Processes transactions using indexed files
* **mini-database/**: Interactive account management system
* **sample-data/**: Example input/output files

---

## Key Concepts Demonstrated

* Sequential file processing
* Indexed file organization
* Batch processing systems
* File sorting and merging logic
* Overdraft detection and handling
* Interactive database-style operations

---

## How to Run

### 1. Generate Data

```bash
python masterFile.py
python transactionFile.py
```

### 2. Run Sequential Processor

```bash
cobc -x transactionProcessor.cbl
./transactionProcessor master.dat transactions.dat updated.dat report.txt error.txt
```

### 3. Convert to Indexed Files

```bash
cobc -x createIndexedFile.cbl
./createIndexedFile master.dat indexedMaster.dat

cobc -x createIndexedTrans.cbl
./createIndexedTrans transactions.dat indexedTransactions.dat
```

### 4. Run Indexed Processor

```bash
cobc -x indexedTransactionProcessor.cbl
./indexedTransactionProcessor indexedMaster.dat indexedTransactions.dat updatedIndexed.dat report.txt error.txt
```

### 5. Run Mini Database

```bash
cobc -x miniDatabase.cbl
./miniDatabase
```

---

# dataGeneration

## Purpose

Generates synthetic banking data for testing.

## Files

* `masterFile.py`: Creates account records
* `transactionFile.py`: Creates sorted transaction records

## Output

* `master.dat`
* `transactions.dat`

## Notes

* Account numbers are zero-padded
* Transactions are sorted by account and timestamp

---

# sequentialProcessing

## Purpose

Processes transactions against a master file using sequential file logic.

## Key Features

* Merge-style processing of master and transaction files
* Overdraft detection with fee application
* Per-account summary reporting
* Error logging for invalid transactions

## Input

* `master.dat`
* `transactions.dat`

## Output

* Updated master file
* Summary report
* Error report

## Notes

Requires both files to be sorted by account number.

---

# indexedConversion

## Purpose

Converts sequential files into indexed files for efficient access.

## Programs

* `createIndexedFile.cbl`: Converts master file
* `createIndexedTrans.cbl`: Converts transaction file

## Key Concepts

* Primary keys
* Alternate keys (with duplicates)
* Indexed file organization

## Output

* `indexedMaster.dat`
* `indexedTransactions.dat`

---

# indexedProcessing

## Purpose

Processes transactions using indexed files instead of sequential scanning.

## Key Features

* Direct access using keys
* START and READ NEXT operations
* Efficient lookup of transactions per account

## Differences from Sequential Version

* No full file scan required
* Faster access for large datasets
* Uses alternate keys for grouping transactions

---

# miniDatabase

## Purpose

Implements an interactive banking database using indexed files.

## Features

* Add account
* View account (by number or name)
* Update account
* Delete account (soft and hard)
* List all accounts
* Filter accounts by balance or type

## Key Concepts

* Indexed file as a database
* Record-level operations (READ, WRITE, REWRITE, DELETE)
* Alternate key searching
* Soft vs hard delete strategies

## Notes

* Soft delete marks records instead of removing them
* Hard delete permanently removes records
* Uses sequential reads for listing and filtering

---

# Optional: docs/architecture.md

## System Flow

1. Generate data
2. Sequential batch processing
3. Convert to indexed format
4. Indexed batch processing
5. Interactive database usage

## Design Insight

This project mimics real-world legacy banking systems where:

* Batch jobs process transactions overnight
* Indexed files provide efficient record access
* Interactive systems allow account management

---

If extended further, this system could incorporate concurrency, locking, or integration with modern databases.
