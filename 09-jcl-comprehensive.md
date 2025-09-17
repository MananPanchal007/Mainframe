# JCL (Job Control Language)

## Table of Contents
- [What is JCL?](#what-is-jcl)
- [JCL Structure](#jcl-structure)
- [JCL Statements](#jcl-statements)
- [JCL Execution](#jcl-execution)
- [JCL Libraries](#jcl-libraries)
- [Conditional Processing](#conditional-processing)
- [Generation Data Groups](#generation-data-groups)
- [JCL Examples](#jcl-examples)
- [Best Practices](#best-practices)

## What is JCL?

**JCL (Job Control Language)** is a scripting language used to control batch job execution on mainframe systems. It defines:
- **Job parameters** and execution environment
- **Dataset allocations** and access methods
- **Program execution** and parameters
- **Output handling** and routing
- **Error handling** and recovery procedures

### Key Features:
- **Batch processing** control
- **Resource management** and allocation
- **Program execution** coordination
- **Error handling** and recovery
- **Output management** and routing

## JCL Structure

### Basic JCL Job:
```jcl
//JOBNAME  JOB (ACCOUNT),'JOB DESCRIPTION',CLASS=A,MSGCLASS=X
//STEP1    EXEC PGM=PROGRAM1
//INPUT    DD DSN=USER.DATA.INPUT,DISP=SHR
//OUTPUT   DD DSN=USER.DATA.OUTPUT,DISP=(NEW,CATLG)
//SYSPRINT DD SYSOUT=*
//*
//STEP2    EXEC PGM=PROGRAM2
//INPUT    DD DSN=USER.DATA.OUTPUT,DISP=SHR
//OUTPUT   DD DSN=USER.DATA.FINAL,DISP=(NEW,CATLG)
//SYSPRINT DD SYSOUT=*
```

## JCL Statements

### 1. **JOB Statement**
```jcl
//JOBNAME  JOB (ACCOUNT),'JOB DESCRIPTION',CLASS=A,MSGCLASS=X
```

**Parameters:**
- **JOBNAME**: Unique job identifier (1-8 characters)
- **ACCOUNT**: Accounting information
- **JOB DESCRIPTION**: Job description
- **CLASS**: Job class (A-Z, 0-9)
- **MSGCLASS**: Message class for output

### 2. **EXEC Statement**
```jcl
//STEP1    EXEC PGM=PROGRAM1
//STEP2    EXEC PGM=PROGRAM2,PARM='PARAMETERS'
//STEP3    EXEC PROC=PROCNAME
```

**Parameters:**
- **PGM**: Program name to execute
- **PARM**: Parameters to pass to program
- **PROC**: Procedure name to execute

### 3. **DD Statement**
```jcl
//INPUT    DD DSN=USER.DATA.INPUT,DISP=SHR
//OUTPUT   DD DSN=USER.DATA.OUTPUT,DISP=(NEW,CATLG)
//SYSPRINT DD SYSOUT=*
```

**Parameters:**
- **DSN**: Dataset name
- **DISP**: Disposition (SHR, OLD, NEW, MOD)
- **SYSOUT**: System output destination

## JCL Execution

### 1. **Job Submission**
```jcl
SUBMIT 'USER.JCL.BATCHJOB'
```

### 2. **Job Monitoring**
```jcl
STATUS JOB(JOB123)
```

### 3. **Job Output**
```jcl
OUTPUT JOB(JOB123)
```

## JCL Libraries

### 1. **PROCLIB (Procedure Library)**
```jcl
//STEP1    EXEC PROC=COMPILE
//COMPILE.SYSIN DD DSN=USER.SOURCE.COBOL,DISP=SHR
//COMPILE.SYSLIB DD DSN=USER.LIBRARY.COBOL,DISP=SHR
```

### 2. **JCL Library**
```jcl
//STEP1    EXEC PGM=IEBGENER
//SYSIN    DD DSN=USER.JCL.LIBRARY,DISP=SHR
```

## Conditional Processing

### 1. **IF/THEN/ELSE/ENDIF**
```jcl
//STEP1    EXEC PGM=PROGRAM1
//STEP2    IF STEP1.RC = 0 THEN
//STEP3    EXEC PGM=PROGRAM2
//STEP4    ELSE
//STEP5    EXEC PGM=ERRORHANDLER
//STEP6    ENDIF
```

### 2. **COND Parameter**
```jcl
//STEP1    EXEC PGM=PROGRAM1
//STEP2    EXEC PGM=PROGRAM2,COND=(0,LT,STEP1)
//STEP3    EXEC PGM=PROGRAM3,COND=(4,GT,STEP1)
```

## Generation Data Groups

### 1. **GDG Definition**
```jcl
//STEP1    EXEC PGM=IDCAMS
//SYSIN    DD *
  DEFINE GDG(NAME(USER.BACKUP.DATA) -
  LIMIT(10) -
  SCRATCH)
/*
```

### 2. **GDG Usage**
```jcl
//STEP1    EXEC PGM=PROGRAM1
//INPUT    DD DSN=USER.DATA.INPUT,DISP=SHR
//OUTPUT   DD DSN=USER.BACKUP.DATA(+1),DISP=(NEW,CATLG)
//BACKUP   DD DSN=USER.BACKUP.DATA(0),DISP=SHR
```

## JCL Examples

### 1. **Simple COBOL Compile and Link**
```jcl
//COBOLJOB JOB (12345),'COBOL COMPILE',CLASS=A,MSGCLASS=X
//*
//COMPILE  EXEC PGM=IGYCRCTL
//SYSIN    DD DSN=USER.SOURCE.COBOL(PROG1),DISP=SHR
//SYSLIB   DD DSN=USER.LIBRARY.COBOL,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT2   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT3   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT4   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT5   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT6   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT7   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSLIN   DD DSN=USER.OBJECT.COBOL(PROG1),DISP=(NEW,CATLG),
//            UNIT=SYSDA,SPACE=(TRK,(1,1))
//*
//LINK     EXEC PGM=IEWL
//SYSLIN   DD DSN=USER.OBJECT.COBOL(PROG1),DISP=SHR
//SYSLIB   DD DSN=USER.LIBRARY.LOAD,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSLMOD  DD DSN=USER.LOAD.COBOL(PROG1),DISP=(NEW,CATLG),
//            UNIT=SYSDA,SPACE=(TRK,(1,1))
```

### 2. **Data Processing Job**
```jcl
//DATAPROC JOB (12345),'DATA PROCESSING',CLASS=A,MSGCLASS=X
//*
//STEP1    EXEC PGM=IEBGENER
//SYSUT1   DD DSN=USER.DATA.INPUT,DISP=SHR
//SYSUT2   DD DSN=USER.DATA.OUTPUT,DISP=(NEW,CATLG),
//            UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  GENERATE MAXFLDS=1
  RECORD FIELD=(80,1,,1)
/*
//*
//STEP2    EXEC PGM=PROGRAM1
//INPUT    DD DSN=USER.DATA.OUTPUT,DISP=SHR
//OUTPUT   DD DSN=USER.DATA.FINAL,DISP=(NEW,CATLG),
//            UNIT=SYSDA,SPACE=(CYL,(5,2))
//SYSPRINT DD SYSOUT=*
```

### 3. **Conditional Processing Example**
```jcl
//CONDJOB  JOB (12345),'CONDITIONAL JOB',CLASS=A,MSGCLASS=X
//*
//STEP1    EXEC PGM=PROGRAM1
//INPUT    DD DSN=USER.DATA.INPUT,DISP=SHR
//OUTPUT   DD DSN=USER.DATA.STEP1,DISP=(NEW,CATLG)
//SYSPRINT DD SYSOUT=*
//*
//STEP2    IF STEP1.RC = 0 THEN
//STEP2A   EXEC PGM=PROGRAM2
//INPUT    DD DSN=USER.DATA.STEP1,DISP=SHR
//OUTPUT   DD DSN=USER.DATA.STEP2,DISP=(NEW,CATLG)
//SYSPRINT DD SYSOUT=*
//STEP2B   EXEC PGM=PROGRAM3
//INPUT    DD DSN=USER.DATA.STEP2,DISP=SHR
//OUTPUT   DD DSN=USER.DATA.FINAL,DISP=(NEW,CATLG)
//SYSPRINT DD SYSOUT=*
//         ELSE
//STEP2C   EXEC PGM=ERRORHANDLER
//INPUT    DD DSN=USER.DATA.INPUT,DISP=SHR
//OUTPUT   DD DSN=USER.DATA.ERROR,DISP=(NEW,CATLG)
//SYSPRINT DD SYSOUT=*
//         ENDIF
```

### 4. **GDG Processing Example**
```jcl
//GDGJOB   JOB (12345),'GDG PROCESSING',CLASS=A,MSGCLASS=X
//*
//STEP1    EXEC PGM=PROGRAM1
//INPUT    DD DSN=USER.DATA.INPUT,DISP=SHR
//OUTPUT   DD DSN=USER.BACKUP.DATA(+1),DISP=(NEW,CATLG),
//            UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSPRINT DD SYSOUT=*
//*
//STEP2    EXEC PGM=PROGRAM2
//INPUT    DD DSN=USER.BACKUP.DATA(0),DISP=SHR
//OUTPUT   DD DSN=USER.REPORT.DAILY(+1),DISP=(NEW,CATLG),
//            UNIT=SYSDA,SPACE=(CYL,(5,2))
//SYSPRINT DD SYSOUT=*
```

## Best Practices

### 1. **JCL Design**
- Use meaningful job and step names
- Include proper comments
- Use appropriate job classes
- Follow naming conventions

### 2. **Error Handling**
- Use conditional processing
- Include error handling steps
- Monitor job completion codes
- Implement recovery procedures

### 3. **Resource Management**
- Allocate appropriate space
- Use efficient dataset organizations
- Monitor resource usage
- Clean up temporary datasets

### 4. **Documentation**
- Document job purpose
- Include parameter descriptions
- Maintain change history
- Follow organizational standards

## Quick Reference

### JCL Statements:
| Statement | Purpose | Example |
|-----------|---------|---------|
| `JOB` | Job definition | `//JOB1 JOB (12345),'JOB',CLASS=A` |
| `EXEC` | Step execution | `//STEP1 EXEC PGM=PROG1` |
| `DD` | Dataset definition | `//INPUT DD DSN=USER.DATA,DISP=SHR` |

### DD Parameters:
| Parameter | Purpose | Values |
|-----------|---------|--------|
| `DSN` | Dataset name | `USER.DATA.INPUT` |
| `DISP` | Disposition | `SHR`, `OLD`, `NEW`, `MOD` |
| `SYSOUT` | System output | `*`, `A`, `B`, etc. |

### Job Classes:
| Class | Purpose | Priority |
|-------|---------|----------|
| `A` | High priority | 1 |
| `B` | Medium priority | 2 |
| `C` | Low priority | 3 |

---

*Next: [Introduction to REXX](09-rexx-intro.md)*
