# TSO (Time Sharing Option)

## Table of Contents
- [What is TSO?](#what-is-tso)
- [TSO Logon Process](#tso-logon-process)
- [TSO Commands](#tso-commands)
- [Command Syntax](#command-syntax)
- [Essential TSO Commands](#essential-tso-commands)
- [Dataset Commands](#dataset-commands)
- [System Commands](#system-commands)
- [TSO Profile](#tso-profile)
- [Error Handling](#error-handling)
- [Best Practices](#best-practices)

## What is TSO?

**TSO (Time Sharing Option)** is the interactive environment that allows users to access mainframe resources. It provides a command-line interface for:
- Dataset management
- Job submission
- Program execution
- System administration
- Development and testing

### Key Features:
- **Interactive Commands**: Direct system interaction
- **Dataset Access**: Create, modify, and manage datasets
- **Job Control**: Submit and monitor batch jobs
- **Security**: User authentication and authorization
- **Customization**: Personal settings and preferences

## TSO Logon Process

### 1. **Initial Connection**
```
LOGON userid password
```
or
```
LOGON userid
Password: ********
```

### 2. **Logon Parameters**
- **USERID**: Your unique identifier
- **PASSWORD**: Authentication credential
- **NEWPASSWORD**: For password changes
- **PROC**: Logon procedure (optional)

### 3. **Logon Procedure**
- **IKJACCNT**: Default TSO procedure
- **IKJACCNT.SPF**: ISPF-enabled procedure
- **Custom procedures**: Organization-specific setups

### 4. **Logon Messages**
```
IKJ56520I ENTER LOGON OR LGOFF COMMAND
IKJ56521I TSO READY
```

## TSO Commands

### Command Structure
```
command operand1 operand2 ... operandn
```

### Command Types:
1. **Dataset Commands**: Manage datasets
2. **System Commands**: System information and control
3. **Job Commands**: Submit and monitor jobs
4. **Utility Commands**: Various utilities
5. **Profile Commands**: User settings

## Command Syntax

### General Rules:
- Commands are **case-insensitive**
- Multiple spaces are treated as single space
- Commands can be abbreviated (minimum 4 characters)
- Use quotes for values containing spaces

### Examples:
```
LISTC 'USER.DATASET'
LISTC USER.DATASET
LISTC 'USER.DATASET' ALL
```

## Essential TSO Commands

### 1. **HELP Command**
```
HELP command-name
HELP LISTC
HELP ALL
```

### 2. **LISTC (List Catalog)**
```
LISTC 'dataset-name'
LISTC 'dataset-name' ALL
LISTC 'dataset-name' VOL(volser)
```

**Examples:**
```
LISTC 'USER.PROGRAM.COBOL'
LISTC 'USER.DATA.*' ALL
LISTC 'SYS1.PROCLIB' VOL(PRD001)
```

### 3. **LISTDS (List Dataset)**
```
LISTDS 'dataset-name'
LISTDS 'dataset-name' ALL
LISTDS 'dataset-name' HISTORY
```

**Examples:**
```
LISTDS 'USER.PROGRAM.COBOL'
LISTDS 'USER.DATA.*' ALL
LISTDS 'SYS1.PROCLIB' HISTORY
```

### 4. **ALLOCATE Command**
```
ALLOCATE FILE(ddname) DATASET('dataset-name') SHR
ALLOCATE FILE(ddname) DATASET('dataset-name') OLD
ALLOCATE FILE(ddname) DATASET('dataset-name') NEW
```

**Examples:**
```
ALLOCATE FILE(INPUT) DATASET('USER.DATA.INPUT') SHR
ALLOCATE FILE(OUTPUT) DATASET('USER.DATA.OUTPUT') OLD
ALLOCATE FILE(TEMP) DATASET('USER.TEMP.DATA') NEW
```

### 5. **FREE Command**
```
FREE FILE(ddname)
FREE ALL
FREE FILE(INPUT) FILE(OUTPUT)
```

### 6. **DELETE Command**
```
DELETE 'dataset-name'
DELETE 'dataset-name' PURGE
DELETE 'dataset-name' FORCE
```

**Examples:**
```
DELETE 'USER.TEMP.DATA'
DELETE 'USER.OBSOLETE.DATA' PURGE
DELETE 'USER.LOCKED.DATA' FORCE
```

## Dataset Commands

### 1. **RENAME Command**
```
RENAME 'old-dataset-name' 'new-dataset-name'
```

**Example:**
```
RENAME 'USER.DATA.OLD' 'USER.DATA.NEW'
```

### 2. **COPY Command**
```
COPY 'source-dataset' 'target-dataset'
COPY 'source-dataset' 'target-dataset' REPLACE
```

**Example:**
```
COPY 'USER.DATA.BACKUP' 'USER.DATA.CURRENT' REPLACE
```

### 3. **MOVE Command**
```
MOVE 'source-dataset' 'target-dataset'
```

**Example:**
```
MOVE 'USER.DATA.TEMP' 'USER.DATA.PERMANENT'
```

### 4. **COMPRESS Command**
```
COMPRESS 'dataset-name'
```

**Example:**
```
COMPRESS 'USER.DATA.LARGE'
```

## System Commands

### 1. **STATUS Command**
```
STATUS
STATUS USER
STATUS SYSTEM
```

### 2. **TIME Command**
```
TIME
```

### 3. **DATE Command**
```
DATE
```

### 4. **WHO Command**
```
WHO
WHO userid
```

### 5. **SEND Command**
```
SEND 'message' USER(userid)
SEND 'message' BROADCAST
```

**Example:**
```
SEND 'Meeting at 2 PM' USER(JOHN)
SEND 'System maintenance tonight' BROADCAST
```

## Job Commands

### 1. **SUBMIT Command**
```
SUBMIT 'dataset-name'
SUBMIT 'dataset-name' CLASS(A)
SUBMIT 'dataset-name' HOLD
```

**Example:**
```
SUBMIT 'USER.JCL.BATCHJOB'
SUBMIT 'USER.JCL.URGENT' CLASS(A) HOLD
```

### 2. **STATUS Command (Jobs)**
```
STATUS JOB(jobname)
STATUS USER
STATUS ALL
```

### 3. **CANCEL Command**
```
CANCEL JOB(jobname)
CANCEL JOB(jobname) PURGE
```

## Utility Commands

### 1. **IKJEFT01 (TSO Terminal)**
```
IKJEFT01
```

### 2. **IKJEFT1A (TSO with ISPF)**
```
IKJEFT1A
```

### 3. **IKJEFT1B (TSO with CLIST)**
```
IKJEFT1B
```

## TSO Profile

### Profile Commands:
```
PROFILE PREFIX('prefix')
PROFILE NOPREFIX
PROFILE MSGID
PROFILE NOMSGID
```

### Profile Settings:
- **PREFIX**: Default dataset name prefix
- **MSGID**: Show message IDs
- **PROMPT**: Command prompt settings
- **TIMEOUT**: Session timeout

**Example:**
```
PROFILE PREFIX('USER')
PROFILE MSGID
```

## Error Handling

### Common TSO Messages:

#### **IKJ56400I** - Command not found
```
IKJ56400I COMMAND command-name NOT FOUND
```
**Solution**: Check command spelling or use HELP

#### **IKJ56401I** - Dataset not found
```
IKJ56401I DATASET dataset-name NOT FOUND
```
**Solution**: Verify dataset name and existence

#### **IKJ56402I** - Insufficient authority
```
IKJ56402I INSUFFICIENT AUTHORITY
```
**Solution**: Contact system administrator

#### **IKJ56403I** - Dataset in use
```
IKJ56403I DATASET dataset-name IN USE
```
**Solution**: Wait or use FORCE option

### Error Recovery:
1. **Check command syntax**
2. **Verify dataset names**
3. **Check user authority**
4. **Use HELP command**
5. **Contact system administrator**

## Best Practices

### 1. **Command Usage**
- Use meaningful dataset names
- Always verify before destructive operations
- Use quotes for dataset names with special characters
- Keep commands simple and clear

### 2. **Security**
- Never share passwords
- Log off when finished
- Use appropriate dataset permissions
- Follow organizational security policies

### 3. **Efficiency**
- Use wildcards appropriately
- Free allocated datasets when done
- Use appropriate dataset organizations
- Monitor system resources

### 4. **Documentation**
- Document complex procedures
- Keep command history
- Maintain dataset naming conventions
- Record error messages and solutions

## Quick Reference

### Essential Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `HELP` | Get help | `HELP LISTC` |
| `LISTC` | List catalog | `LISTC 'USER.DATA.*'` |
| `LISTDS` | List dataset | `LISTDS 'USER.DATA'` |
| `ALLOCATE` | Allocate dataset | `ALLOCATE FILE(IN) DATASET('USER.DATA') SHR` |
| `FREE` | Free dataset | `FREE FILE(IN)` |
| `DELETE` | Delete dataset | `DELETE 'USER.TEMP.DATA'` |
| `RENAME` | Rename dataset | `RENAME 'OLD' 'NEW'` |
| `COPY` | Copy dataset | `COPY 'SRC' 'TGT'` |
| `SUBMIT` | Submit job | `SUBMIT 'USER.JCL.JOB'` |
| `STATUS` | Check status | `STATUS` |

### Common Abbreviations:
- `LISTC` → `LISTCAT`
- `LISTDS` → `LISTDS`
- `ALLOCATE` → `ALLOC`
- `DELETE` → `DEL`

---

*Next: [ISPF (Interactive System Productivity Facility)](03-ispf.md)*
