# CICS (Customer Information Control System)

## Table of Contents
- [What is CICS?](#what-is-cics)
- [CICS Architecture](#cics-architecture)
- [BMS (Basic Mapping Support)](#bms-basic-mapping-support)
- [BMS Map Attributes](#bms-map-attributes)
- [CICS-COBOL Programs](#cics-cobol-programs)
- [CICS Commands](#cics-commands)
- [CICS Examples](#cics-examples)
- [Best Practices](#best-practices)

## What is CICS?

**CICS (Customer Information Control System)** is a transaction processing system that provides:
- **Online transaction processing** (OLTP)
- **Real-time data access** and manipulation
- **User interface** management
- **Database access** and control
- **Security and authorization** management
- **Application development** environment

### Key Features:
- **Transaction processing** - handles online transactions
- **Screen management** - provides user interfaces
- **Database access** - integrates with various databases
- **Security** - built-in access control
- **Scalability** - supports multiple users and transactions
- **Reliability** - fault-tolerant design

## CICS Architecture

### Main Components:
```
┌─────────────────────────────────────┐
│           CICS System               │
├─────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  │
│  │ Transaction │  │   Program   │  │
│  │  Manager    │  │   Manager   │  │
│  └─────────────┘  └─────────────┘  │
│  ┌─────────────┐  ┌─────────────┐  │
│  │   Screen    │  │  Database   │  │
│  │  Manager    │  │   Manager   │  │
│  └─────────────┘  └─────────────┘  │
│  ┌─────────────┐  ┌─────────────┐  │
│  │  Security   │  │   System    │  │
│  │  Manager    │  │  Services   │  │
│  └─────────────┘  └─────────────┘  │
└─────────────────────────────────────┘
```

### Transaction Flow:
1. **User Input** - User enters data
2. **Transaction Processing** - CICS processes the transaction
3. **Program Execution** - Application program runs
4. **Database Access** - Data is retrieved/updated
5. **Screen Output** - Results are displayed to user

## BMS (Basic Mapping Support)

### What is BMS?
**BMS (Basic Mapping Support)** is a CICS facility that provides:
- **Screen definition** and management
- **Field mapping** and validation
- **User interface** generation
- **Input/output** handling
- **Screen formatting** and layout

### BMS Components:
1. **Map Definition** - Screen layout and fields
2. **Map Generation** - Creates executable maps
3. **Map Usage** - Program interaction with maps
4. **Map Attributes** - Field properties and behavior

## BMS Map Attributes

### 1. **Field Attributes**
```cobol
       01 CUSTOMER-MAP.
           05 CUSTOMER-ID-FIELD.
               10 CUSTOMER-ID-LENGTH PIC S9(4) COMP.
               10 CUSTOMER-ID-ATTRB PIC X(1).
               10 CUSTOMER-ID-DATA PIC X(5).
           05 CUSTOMER-NAME-FIELD.
               10 CUSTOMER-NAME-LENGTH PIC S9(4) COMP.
               10 CUSTOMER-NAME-ATTRB PIC X(1).
               10 CUSTOMER-NAME-DATA PIC X(30).
```

### 2. **Attribute Values**
```cobol
       01 ATTRIBUTE-VALUES.
           05 ATTR-NORMAL PIC X(1) VALUE X'00'.
           05 ATTR-BRIGHT PIC X(1) VALUE X'10'.
           05 ATTR-DIM PIC X(1) VALUE X'20'.
           05 ATTR-UNDERSCORE PIC X(1) VALUE X'40'.
           05 ATTR-BLINK PIC X(1) VALUE X'80'.
           05 ATTR-REVERSE PIC X(1) VALUE X'90'.
           05 ATTR-NON-DISPLAY PIC X(1) VALUE X'F0'.
```

### 3. **Field Properties**
```cobol
       01 FIELD-PROPERTIES.
           05 FIELD-INPUT PIC X(1) VALUE 'I'.
           05 FIELD-OUTPUT PIC X(1) VALUE 'O'.
           05 FIELD-BOTH PIC X(1) VALUE 'B'.
           05 FIELD-PROTECTED PIC X(1) VALUE 'P'.
           05 FIELD-UNPROTECTED PIC X(1) VALUE 'U'.
```

## CICS-COBOL Programs

### 1. **Basic CICS-COBOL Program**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICS001.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESPONSE PIC S9(8) COMP.
       01 WS-MESSAGE PIC X(79) VALUE 'HELLO, CICS!'.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           EXEC CICS SEND
               FROM(WS-MESSAGE)
               LENGTH(79)
               RESP(WS-RESPONSE)
           END-EXEC.
           
           EXEC CICS RETURN
           END-EXEC.
```

### 2. **Map Processing Program**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICS002.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESPONSE PIC S9(8) COMP.
       01 WS-MAPNAME PIC X(8) VALUE 'CUSTMAP'.
       01 WS-MAPSET PIC X(8) VALUE 'CUSTSET'.
       
       COPY CUSTMAP.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           EXEC CICS RECEIVE
               MAP(WS-MAPNAME)
               MAPSET(WS-MAPSET)
               INTO(CUSTOMER-MAP)
               RESP(WS-RESPONSE)
           END-EXEC.
           
           PERFORM PROCESS-CUSTOMER.
           
           EXEC CICS SEND
               MAP(WS-MAPNAME)
               MAPSET(WS-MAPSET)
               FROM(CUSTOMER-MAP)
               RESP(WS-RESPONSE)
           END-EXEC.
           
           EXEC CICS RETURN
           END-EXEC.
       
       PROCESS-CUSTOMER.
           MOVE 'CUSTOMER PROCESSED' TO CUSTOMER-MESSAGE.
```

### 3. **Database Access Program**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICS003.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESPONSE PIC S9(8) COMP.
       01 WS-CUSTOMER-ID PIC 9(5).
       01 WS-CUSTOMER-NAME PIC X(30).
       01 WS-CUSTOMER-BALANCE PIC 9(7)V99.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           EXEC CICS RECEIVE
               INTO(WS-CUSTOMER-ID)
               LENGTH(5)
               RESP(WS-RESPONSE)
           END-EXEC.
           
           EXEC CICS READ
               FILE('CUSTOMER')
               RIDFLD(WS-CUSTOMER-ID)
               INTO(WS-CUSTOMER-NAME)
               RESP(WS-RESPONSE)
           END-EXEC.
           
           IF WS-RESPONSE = DFHRESP(NORMAL) THEN
               EXEC CICS SEND
                   FROM(WS-CUSTOMER-NAME)
                   LENGTH(30)
                   RESP(WS-RESPONSE)
               END-EXEC
           ELSE
               EXEC CICS SEND
                   FROM('CUSTOMER NOT FOUND')
                   LENGTH(20)
                   RESP(WS-RESPONSE)
               END-EXEC
           END-IF.
           
           EXEC CICS RETURN
           END-EXEC.
```

## CICS Commands

### 1. **Screen Commands**
```cobol
       EXEC CICS SEND
           FROM(data-area)
           LENGTH(length)
           RESP(response-code)
       END-EXEC.
       
       EXEC CICS RECEIVE
           INTO(data-area)
           LENGTH(length)
           RESP(response-code)
       END-EXEC.
       
       EXEC CICS SEND MAP
           MAP(map-name)
           MAPSET(mapset-name)
           FROM(map-area)
           RESP(response-code)
       END-EXEC.
```

### 2. **Database Commands**
```cobol
       EXEC CICS READ
           FILE(file-name)
           RIDFLD(key-field)
           INTO(data-area)
           RESP(response-code)
       END-EXEC.
       
       EXEC CICS WRITE
           FILE(file-name)
           FROM(data-area)
           RIDFLD(key-field)
           RESP(response-code)
       END-EXEC.
       
       EXEC CICS DELETE
           FILE(file-name)
           RIDFLD(key-field)
           RESP(response-code)
       END-EXEC.
```

### 3. **Control Commands**
```cobol
       EXEC CICS RETURN
           TRANSID(transaction-id)
       END-EXEC.
       
       EXEC CICS XCTL
           PROGRAM(program-name)
       END-EXEC.
       
       EXEC CICS LINK
           PROGRAM(program-name)
           COMMAREA(data-area)
           LENGTH(length)
       END-EXEC.
```

## CICS Examples

### 1. **Customer Inquiry Program**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTINQ.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESPONSE PIC S9(8) COMP.
       01 WS-CUSTOMER-ID PIC 9(5).
       01 WS-CUSTOMER-RECORD.
           05 CUSTOMER-ID PIC 9(5).
           05 CUSTOMER-NAME PIC X(30).
           05 CUSTOMER-BALANCE PIC 9(7)V99.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           EXEC CICS RECEIVE
               INTO(WS-CUSTOMER-ID)
               LENGTH(5)
               RESP(WS-RESPONSE)
           END-EXEC.
           
           EXEC CICS READ
               FILE('CUSTOMER')
               RIDFLD(WS-CUSTOMER-ID)
               INTO(WS-CUSTOMER-RECORD)
               RESP(WS-RESPONSE)
           END-EXEC.
           
           IF WS-RESPONSE = DFHRESP(NORMAL) THEN
               EXEC CICS SEND
                   FROM(WS-CUSTOMER-RECORD)
                   LENGTH(42)
                   RESP(WS-RESPONSE)
               END-EXEC
           ELSE
               EXEC CICS SEND
                   FROM('CUSTOMER NOT FOUND')
                   LENGTH(20)
                   RESP(WS-RESPONSE)
               END-EXEC
           END-IF.
           
           EXEC CICS RETURN
           END-EXEC.
```

### 2. **Customer Update Program**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTUPD.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESPONSE PIC S9(8) COMP.
       01 WS-CUSTOMER-RECORD.
           05 CUSTOMER-ID PIC 9(5).
           05 CUSTOMER-NAME PIC X(30).
           05 CUSTOMER-BALANCE PIC 9(7)V99.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           EXEC CICS RECEIVE
               INTO(WS-CUSTOMER-RECORD)
               LENGTH(42)
               RESP(WS-RESPONSE)
           END-EXEC.
           
           EXEC CICS READ
               FILE('CUSTOMER')
               RIDFLD(CUSTOMER-ID)
               INTO(WS-CUSTOMER-RECORD)
               RESP(WS-RESPONSE)
           END-EXEC.
           
           IF WS-RESPONSE = DFHRESP(NORMAL) THEN
               EXEC CICS REWRITE
                   FILE('CUSTOMER')
                   FROM(WS-CUSTOMER-RECORD)
                   RESP(WS-RESPONSE)
               END-EXEC
               
               EXEC CICS SEND
                   FROM('CUSTOMER UPDATED SUCCESSFULLY')
                   LENGTH(28)
                   RESP(WS-RESPONSE)
               END-EXEC
           ELSE
               EXEC CICS SEND
                   FROM('CUSTOMER NOT FOUND')
                   LENGTH(20)
                   RESP(WS-RESPONSE)
               END-EXEC
           END-IF.
           
           EXEC CICS RETURN
           END-EXEC.
```

## Best Practices

### 1. **Program Design**
- Use meaningful program names
- Follow CICS programming standards
- Include proper error handling
- Use appropriate data structures

### 2. **Error Handling**
- Check response codes
- Handle error conditions
- Provide meaningful error messages
- Implement recovery procedures

### 3. **Performance**
- Use efficient CICS commands
- Minimize database access
- Optimize screen operations
- Monitor transaction performance

### 4. **Security**
- Follow access control rules
- Validate input data
- Protect sensitive information
- Maintain audit trails

## Quick Reference

### CICS Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `SEND` | Send data to screen | `EXEC CICS SEND FROM(data) LENGTH(len) END-EXEC` |
| `RECEIVE` | Receive data from screen | `EXEC CICS RECEIVE INTO(data) LENGTH(len) END-EXEC` |
| `READ` | Read from database | `EXEC CICS READ FILE('file') RIDFLD(key) END-EXEC` |
| `WRITE` | Write to database | `EXEC CICS WRITE FILE('file') FROM(data) END-EXEC` |
| `RETURN` | Return control | `EXEC CICS RETURN END-EXEC` |

### BMS Attributes:
| Attribute | Value | Description |
|-----------|-------|-------------|
| `ATTR-NORMAL` | `X'00'` | Normal display |
| `ATTR-BRIGHT` | `X'10'` | Bright display |
| `ATTR-DIM` | `X'20'` | Dim display |
| `ATTR-UNDERSCORE` | `X'40'` | Underscored |
| `ATTR-BLINK` | `X'80'` | Blinking |
| `ATTR-REVERSE` | `X'90'` | Reverse video |

### Response Codes:
| Code | Description |
|------|-------------|
| `DFHRESP(NORMAL)` | Operation successful |
| `DFHRESP(NOTFND)` | Record not found |
| `DFHRESP(DUPKEY)` | Duplicate key |
| `DFHRESP(INVREQ)` | Invalid request |
| `DFHRESP(LENGERR)` | Length error |

---

*This completes the comprehensive mainframe notes series. Each file provides detailed information and examples for new learners to understand and work with mainframe systems effectively.*
