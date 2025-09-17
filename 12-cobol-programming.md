# COBOL Programming Language

## Table of Contents
- [What is COBOL?](#what-is-cobol)
- [COBOL Structure](#cobol-structure)
- [Data Division](#data-division)
- [Procedure Division](#procedure-division)
- [COBOL Examples](#cobol-examples)
- [File Processing](#file-processing)
- [COBOL Best Practices](#cobol-best-practices)

## What is COBOL?

**COBOL (Common Business-Oriented Language)** is a high-level programming language designed for:
- **Business applications** and data processing
- **Mainframe systems** and enterprise computing
- **Financial systems** and banking
- **Government systems** and administration
- **Legacy system** maintenance and development

### Key Features:
- **English-like syntax** - easy to read and understand
- **Structured programming** - clear program organization
- **Data processing** - excellent for business applications
- **Mainframe integration** - designed for mainframe systems
- **Standardized** - ANSI/ISO standards

## COBOL Structure

### Basic COBOL Program:
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       AUTHOR. PROGRAMMER.
       DATE-WRITTEN. 2024-01-01.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE PIC X(20) VALUE 'HELLO, WORLD!'.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY WS-MESSAGE.
           STOP RUN.
```

### COBOL Divisions:
1. **IDENTIFICATION DIVISION** - Program identification
2. **ENVIRONMENT DIVISION** - Hardware and software environment
3. **DATA DIVISION** - Data definitions
4. **PROCEDURE DIVISION** - Program logic

## Data Division

### 1. **Working-Storage Section**
```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER PIC 9(3) VALUE 0.
       01 WS-TOTAL PIC 9(5)V99 VALUE 0.
       01 WS-NAME PIC X(30) VALUE SPACES.
       01 WS-DATE PIC 9(8) VALUE 20240101.
```

### 2. **File Section**
```cobol
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
           05 CUSTOMER-ID PIC 9(5).
           05 CUSTOMER-NAME PIC X(30).
           05 CUSTOMER-BALANCE PIC 9(7)V99.
       
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           05 OUT-CUSTOMER-ID PIC 9(5).
           05 OUT-CUSTOMER-NAME PIC X(30).
           05 OUT-CUSTOMER-BALANCE PIC 9(7)V99.
```

### 3. **Data Types**
```cobol
       01 WS-NUMERIC PIC 9(5) VALUE 12345.
       01 WS-ALPHANUMERIC PIC X(10) VALUE 'HELLO'.
       01 WS-DECIMAL PIC 9(5)V99 VALUE 123.45.
       01 WS-SIGNED PIC S9(5) VALUE -12345.
       01 WS-COMP-3 PIC 9(5) COMP-3 VALUE 12345.
```

## Procedure Division

### 1. **Basic Statements**
```cobol
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           MOVE 100 TO WS-COUNTER.
           ADD 50 TO WS-COUNTER.
           SUBTRACT 25 FROM WS-COUNTER.
           MULTIPLY WS-COUNTER BY 2.
           DIVIDE WS-COUNTER BY 2.
           DISPLAY 'COUNTER: ' WS-COUNTER.
           STOP RUN.
```

### 2. **Conditional Statements**
```cobol
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           IF WS-COUNTER > 100 THEN
               DISPLAY 'COUNTER IS GREATER THAN 100'
           ELSE
               DISPLAY 'COUNTER IS 100 OR LESS'
           END-IF.
           
           EVALUATE WS-COUNTER
               WHEN 1
                   DISPLAY 'ONE'
               WHEN 2
                   DISPLAY 'TWO'
               WHEN OTHER
                   DISPLAY 'OTHER'
           END-EVALUATE.
```

### 3. **Loops**
```cobol
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 10
               DISPLAY 'COUNT: ' WS-COUNTER
           END-PERFORM.
           
           PERFORM 100-TIMES.
           STOP RUN.
       
       100-TIMES.
           DISPLAY 'HELLO, WORLD!'.
```

## COBOL Examples

### 1. **Hello World Program**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE PIC X(20) VALUE 'HELLO, WORLD!'.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY WS-MESSAGE.
           STOP RUN.
```

### 2. **Simple Calculator**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1 PIC 9(5) VALUE 0.
       01 WS-NUM2 PIC 9(5) VALUE 0.
       01 WS-RESULT PIC 9(10) VALUE 0.
       01 WS-OPERATION PIC X(1) VALUE SPACES.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY 'ENTER FIRST NUMBER: '.
           ACCEPT WS-NUM1.
           DISPLAY 'ENTER SECOND NUMBER: '.
           ACCEPT WS-NUM2.
           DISPLAY 'ENTER OPERATION (+, -, *, /): '.
           ACCEPT WS-OPERATION.
           
           EVALUATE WS-OPERATION
               WHEN '+'
                   ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT
               WHEN '-'
                   SUBTRACT WS-NUM2 FROM WS-NUM1 GIVING WS-RESULT
               WHEN '*'
                   MULTIPLY WS-NUM1 BY WS-NUM2 GIVING WS-RESULT
               WHEN '/'
                   DIVIDE WS-NUM1 BY WS-NUM2 GIVING WS-RESULT
               WHEN OTHER
                   DISPLAY 'INVALID OPERATION'
           END-EVALUATE.
           
           DISPLAY 'RESULT: ' WS-RESULT.
           STOP RUN.
```

### 3. **File Processing Program**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILEPROC.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO INPUT.
           SELECT OUTPUT-FILE ASSIGN TO OUTPUT.
       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
           05 CUSTOMER-ID PIC 9(5).
           05 CUSTOMER-NAME PIC X(30).
           05 CUSTOMER-BALANCE PIC 9(7)V99.
       
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           05 OUT-CUSTOMER-ID PIC 9(5).
           05 OUT-CUSTOMER-NAME PIC X(30).
           05 OUT-CUSTOMER-BALANCE PIC 9(7)V99.
       
       WORKING-STORAGE SECTION.
       01 WS-EOF-FLAG PIC X(1) VALUE 'N'.
       01 WS-RECORD-COUNT PIC 9(5) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           OPEN INPUT INPUT-FILE
                 OUTPUT OUTPUT-FILE.
           
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ INPUT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM.
           
           CLOSE INPUT-FILE OUTPUT-FILE.
           DISPLAY 'TOTAL RECORDS PROCESSED: ' WS-RECORD-COUNT.
           STOP RUN.
       
       PROCESS-RECORD.
           MOVE CUSTOMER-ID TO OUT-CUSTOMER-ID.
           MOVE CUSTOMER-NAME TO OUT-CUSTOMER-NAME.
           MOVE CUSTOMER-BALANCE TO OUT-CUSTOMER-BALANCE.
           WRITE OUTPUT-RECORD.
           ADD 1 TO WS-RECORD-COUNT.
```

## File Processing

### 1. **Sequential File Processing**
```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO INPUT.
           SELECT OUTPUT-FILE ASSIGN TO OUTPUT.
       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(80).
       
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(80).
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           OPEN INPUT INPUT-FILE
                 OUTPUT OUTPUT-FILE.
           
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ INPUT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM.
           
           CLOSE INPUT-FILE OUTPUT-FILE.
```

### 2. **VSAM File Processing**
```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO CUSTOMER
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS CUSTOMER-ID.
       
       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           05 CUSTOMER-ID PIC 9(5).
           05 CUSTOMER-NAME PIC X(30).
           05 CUSTOMER-BALANCE PIC 9(7)V99.
```

## COBOL Best Practices

### 1. **Program Structure**
- Use meaningful paragraph names
- Follow consistent indentation
- Include proper comments
- Use standard COBOL conventions

### 2. **Data Definition**
- Use meaningful data names
- Choose appropriate data types
- Use proper data validation
- Follow naming conventions

### 3. **Error Handling**
- Check file status codes
- Handle end-of-file conditions
- Validate input data
- Provide meaningful error messages

### 4. **Performance**
- Use efficient algorithms
- Minimize I/O operations
- Use appropriate data types
- Optimize program logic

## Quick Reference

### COBOL Divisions:
| Division | Purpose | Example |
|----------|---------|---------|
| `IDENTIFICATION` | Program ID | `PROGRAM-ID. HELLO.` |
| `ENVIRONMENT` | Hardware/Software | `CONFIGURATION SECTION.` |
| `DATA` | Data definitions | `WORKING-STORAGE SECTION.` |
| `PROCEDURE` | Program logic | `MAIN-PARAGRAPH.` |

### Data Types:
| Type | Description | Example |
|------|-------------|---------|
| `PIC 9(n)` | Numeric | `PIC 9(5)` |
| `PIC X(n)` | Alphanumeric | `PIC X(10)` |
| `PIC 9(n)V99` | Decimal | `PIC 9(5)V99` |
| `PIC S9(n)` | Signed | `PIC S9(5)` |
| `PIC 9(n) COMP-3` | Packed decimal | `PIC 9(5) COMP-3` |

### Control Structures:
| Structure | Syntax | Example |
|-----------|--------|---------|
| `IF` | `IF condition THEN ... ELSE ... END-IF` | `IF WS-X > 0 THEN ...` |
| `EVALUATE` | `EVALUATE variable WHEN ... END-EVALUATE` | `EVALUATE WS-OP WHEN '+' ...` |
| `PERFORM` | `PERFORM paragraph-name` | `PERFORM 100-PROCESS.` |
| `PERFORM UNTIL` | `PERFORM UNTIL condition ... END-PERFORM` | `PERFORM UNTIL WS-EOF = 'Y' ...` |

---

*Next: [CICS (Customer Information Control System)](12-cics-comprehensive.md)*
