# VSAM (Virtual Storage Access Method)

## Table of Contents
- [What is VSAM?](#what-is-vsam)
- [VSAM Dataset Types](#vsam-dataset-types)
- [VSAM Components](#vsam-components)
- [VSAM Processing](#vsam-processing)
- [Alternate Index in VSAM](#alternate-index-in-vsam)
- [VSAM Commands](#vsam-commands)
- [VSAM Examples](#vsam-examples)
- [Best Practices](#best-practices)

## What is VSAM?

**VSAM (Virtual Storage Access Method)** is a high-performance access method for mainframe systems that provides:
- **Efficient data access** by key or relative record number
- **Automatic space management** and optimization
- **Built-in security** and access control
- **Recovery and restart** capabilities
- **Integration** with various programming languages

### Key Features:
- **Key-sequenced access** for fast retrieval
- **Automatic space allocation** and management
- **Built-in security** and access control
- **Recovery and restart** capabilities
- **High performance** for large datasets

## VSAM Dataset Types

### 1. **KSDS (Key Sequenced Data Set)**
- **Description**: Records stored in key sequence
- **Access**: By key or relative record number
- **Use Cases**: Database files, indexed data
- **Key Features**: Automatic key sequencing, fast access

### 2. **ESDS (Entry Sequenced Data Set)**
- **Description**: Records stored in entry sequence
- **Access**: By relative record number only
- **Use Cases**: Log files, audit trails
- **Key Features**: Sequential processing, append-only

### 3. **RRDS (Relative Record Data Set)**
- **Description**: Records stored by relative record number
- **Access**: By relative record number
- **Use Cases**: Lookup tables, arrays
- **Key Features**: Direct access by record number

### 4. **LDS (Linear Data Set)**
- **Description**: Continuous data stream
- **Access**: By byte offset
- **Use Cases**: Binary data, images
- **Key Features**: No record structure, byte-level access

## VSAM Components

### 1. **Data Component**
- **Purpose**: Stores actual data records
- **Structure**: Variable-length records
- **Access**: By key or relative record number
- **Features**: Automatic space management

### 2. **Index Component**
- **Purpose**: Provides key-to-address mapping
- **Structure**: B-tree index structure
- **Access**: Automatic during data access
- **Features**: Fast key lookup

### 3. **Catalog Entry**
- **Purpose**: Defines VSAM dataset
- **Structure**: Catalog information
- **Access**: Through catalog
- **Features**: Dataset identification

## VSAM Processing

### 1. **Sequential Processing**
```jcl
//STEP1    EXEC PGM=PROGRAM1
//INPUT    DD DSN=USER.VSAM.CUSTOMER,DISP=SHR
//OUTPUT   DD DSN=USER.DATA.OUTPUT,DISP=(NEW,CATLG)
//SYSPRINT DD SYSOUT=*
```

### 2. **Random Processing**
```jcl
//STEP1    EXEC PGM=PROGRAM2
//INPUT    DD DSN=USER.VSAM.CUSTOMER,DISP=SHR
//OUTPUT   DD DSN=USER.DATA.OUTPUT,DISP=(NEW,CATLG)
//SYSPRINT DD SYSOUT=*
```

### 3. **Key-sequenced Processing**
```jcl
//STEP1    EXEC PGM=PROGRAM3
//INPUT    DD DSN=USER.VSAM.CUSTOMER,DISP=SHR
//OUTPUT   DD DSN=USER.DATA.OUTPUT,DISP=(NEW,CATLG)
//SYSPRINT DD SYSOUT=*
```

## Alternate Index in VSAM

### 1. **Alternate Index Definition**
```jcl
//STEP1    EXEC PGM=IDCAMS
//SYSIN    DD *
  DEFINE AIX(NAME(USER.VSAM.CUSTOMER.AIX) -
  RELATE(USER.VSAM.CUSTOMER) -
  KEYS(20,10) -
  RECORDSIZE(80,80) -
  FREESPACE(10,10) -
  UNIQUEKEY -
  UPGRADE)
/*
```

### 2. **Alternate Index Path**
```jcl
//STEP1    EXEC PGM=IDCAMS
//SYSIN    DD *
  DEFINE PATH(NAME(USER.VSAM.CUSTOMER.PATH) -
  PATHENTRY(USER.VSAM.CUSTOMER.AIX) -
  UPDATE)
/*
```

### 3. **Alternate Index Usage**
```jcl
//STEP1    EXEC PGM=PROGRAM4
//INPUT    DD DSN=USER.VSAM.CUSTOMER.PATH,DISP=SHR
//OUTPUT   DD DSN=USER.DATA.OUTPUT,DISP=(NEW,CATLG)
//SYSPRINT DD SYSOUT=*
```

## VSAM Commands

### 1. **DEFINE Command**
```jcl
//STEP1    EXEC PGM=IDCAMS
//SYSIN    DD *
  DEFINE CLUSTER(NAME(USER.VSAM.CUSTOMER) -
  INDEXED -
  KEYS(10,5) -
  RECORDSIZE(80,80) -
  FREESPACE(10,10) -
  CYLINDERS(10,5) -
  VOLUMES(PRD001))
/*
```

### 2. **DELETE Command**
```jcl
//STEP1    EXEC PGM=IDCAMS
//SYSIN    DD *
  DELETE USER.VSAM.CUSTOMER
  DELETE USER.VSAM.CUSTOMER PURGE
  DELETE USER.VSAM.CUSTOMER FORCE
/*
```

### 3. **REPRO Command**
```jcl
//STEP1    EXEC PGM=IDCAMS
//SYSIN    DD *
  REPRO INFILE(INPUT) OUTFILE(OUTPUT)
/*
//INPUT    DD DSN=USER.DATA.INPUT,DISP=SHR
//OUTPUT   DD DSN=USER.VSAM.CUSTOMER,DISP=SHR
```

### 4. **LISTCAT Command**
```jcl
//STEP1    EXEC PGM=IDCAMS
//SYSIN    DD *
  LISTCAT ENTRIES(USER.VSAM.CUSTOMER) ALL
  LISTCAT ENTRIES(USER.VSAM.CUSTOMER) HISTORY
  LISTCAT ENTRIES(USER.VSAM.CUSTOMER) VOLUME
/*
```

## VSAM Examples

### 1. **KSDS Creation**
```jcl
//KSDSJOB  JOB (12345),'KSDS CREATION',CLASS=A,MSGCLASS=X
//*
//STEP1    EXEC PGM=IDCAMS
//SYSIN    DD *
  DEFINE CLUSTER(NAME(USER.VSAM.CUSTOMER) -
  INDEXED -
  KEYS(10,5) -
  RECORDSIZE(80,80) -
  FREESPACE(10,10) -
  CYLINDERS(10,5) -
  VOLUMES(PRD001) -
  SHAREOPTIONS(2,3))
/*
//SYSPRINT DD SYSOUT=*
//*
//STEP2    EXEC PGM=IDCAMS
//SYSIN    DD *
  REPRO INFILE(INPUT) OUTFILE(OUTPUT)
/*
//INPUT    DD DSN=USER.DATA.INPUT,DISP=SHR
//OUTPUT   DD DSN=USER.VSAM.CUSTOMER,DISP=SHR
//SYSPRINT DD SYSOUT=*
```

### 2. **ESDS Creation**
```jcl
//ESDSJOB  JOB (12345),'ESDS CREATION',CLASS=A,MSGCLASS=X
//*
//STEP1    EXEC PGM=IDCAMS
//SYSIN    DD *
  DEFINE CLUSTER(NAME(USER.VSAM.LOG) -
  NONINDEXED -
  RECORDSIZE(80,80) -
  CYLINDERS(5,2) -
  VOLUMES(PRD001) -
  SHAREOPTIONS(2,3))
/*
//SYSPRINT DD SYSOUT=*
//*
//STEP2    EXEC PGM=IDCAMS
//SYSIN    DD *
  REPRO INFILE(INPUT) OUTFILE(OUTPUT)
/*
//INPUT    DD DSN=USER.DATA.INPUT,DISP=SHR
//OUTPUT   DD DSN=USER.VSAM.LOG,DISP=SHR
//SYSPRINT DD SYSOUT=*
```

### 3. **RRDS Creation**
```jcl
//RRDSJOB  JOB (12345),'RRDS CREATION',CLASS=A,MSGCLASS=X
//*
//STEP1    EXEC PGM=IDCAMS
//SYSIN    DD *
  DEFINE CLUSTER(NAME(USER.VSAM.LOOKUP) -
  NUMBERED -
  RECORDSIZE(80,80) -
  CYLINDERS(2,1) -
  VOLUMES(PRD001) -
  SHAREOPTIONS(2,3))
/*
//SYSPRINT DD SYSOUT=*
//*
//STEP2    EXEC PGM=IDCAMS
//SYSIN    DD *
  REPRO INFILE(INPUT) OUTFILE(OUTPUT)
/*
//INPUT    DD DSN=USER.DATA.INPUT,DISP=SHR
//OUTPUT   DD DSN=USER.VSAM.LOOKUP,DISP=SHR
//SYSPRINT DD SYSOUT=*
```

### 4. **Alternate Index Example**
```jcl
//AIXJOB   JOB (12345),'ALTERNATE INDEX',CLASS=A,MSGCLASS=X
//*
//STEP1    EXEC PGM=IDCAMS
//SYSIN    DD *
  DEFINE AIX(NAME(USER.VSAM.CUSTOMER.AIX) -
  RELATE(USER.VSAM.CUSTOMER) -
  KEYS(20,10) -
  RECORDSIZE(80,80) -
  FREESPACE(10,10) -
  UNIQUEKEY -
  UPGRADE)
/*
//SYSPRINT DD SYSOUT=*
//*
//STEP2    EXEC PGM=IDCAMS
//SYSIN    DD *
  DEFINE PATH(NAME(USER.VSAM.CUSTOMER.PATH) -
  PATHENTRY(USER.VSAM.CUSTOMER.AIX) -
  UPDATE)
/*
//SYSPRINT DD SYSOUT=*
//*
//STEP3    EXEC PGM=IDCAMS
//SYSIN    DD *
  BLDINDEX INDATASET(USER.VSAM.CUSTOMER) -
  OUTDATASET(USER.VSAM.CUSTOMER.AIX)
/*
//SYSPRINT DD SYSOUT=*
```

## Best Practices

### 1. **Dataset Design**
- Choose appropriate VSAM type
- Design efficient key structures
- Use appropriate space allocation
- Consider performance requirements

### 2. **Key Design**
- Use meaningful key fields
- Keep key length reasonable
- Consider key distribution
- Plan for future growth

### 3. **Space Management**
- Allocate appropriate space
- Monitor space usage
- Use FREESPACE parameters
- Plan for expansion

### 4. **Performance**
- Use appropriate access methods
- Optimize key structures
- Monitor performance metrics
- Consider alternate indexes

## Quick Reference

### VSAM Types:
| Type | Access | Use Case | Key Features |
|------|--------|----------|--------------|
| KSDS | By key | Database files | Key sequencing, fast access |
| ESDS | By RRN | Log files | Sequential, append-only |
| RRDS | By RRN | Lookup tables | Direct access by number |
| LDS | By offset | Binary data | No record structure |

### VSAM Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `DEFINE` | Create VSAM | `DEFINE CLUSTER(NAME(...))` |
| `DELETE` | Delete VSAM | `DELETE USER.VSAM.DATA` |
| `REPRO` | Copy data | `REPRO INFILE(...) OUTFILE(...)` |
| `LISTCAT` | List catalog | `LISTCAT ENTRIES(...) ALL` |

### Key Parameters:
| Parameter | Purpose | Values |
|-----------|---------|--------|
| `KEYS` | Key definition | `(length,offset)` |
| `RECORDSIZE` | Record size | `(average,maximum)` |
| `FREESPACE` | Free space | `(CI%,CA%)` |
| `SHAREOPTIONS` | Sharing | `(2,3)` |

---

*Next: [COBOL Programming Language](11-cobol-programming.md)*
