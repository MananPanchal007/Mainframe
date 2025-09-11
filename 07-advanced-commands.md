# Advanced Commands

## Table of Contents
- [What are Advanced Commands?](#what-are-advanced-commands)
- [System Commands](#system-commands)
- [Dataset Management Commands](#dataset-management-commands)
- [Job Management Commands](#job-management-commands)
- [Security Commands](#security-commands)
- [Performance Commands](#performance-commands)
- [Utility Commands](#utility-commands)
- [Programming Commands](#programming-commands)
- [Network Commands](#network-commands)
- [Monitoring Commands](#monitoring-commands)
- [Best Practices](#best-practices)

## What are Advanced Commands?

**Advanced commands** are sophisticated TSO/ISPF commands that provide:
- **System administration** capabilities
- **Performance monitoring** and tuning
- **Security management** functions
- **Advanced dataset operations**
- **Job control** and monitoring
- **Network and communication** features
- **Programming and development** tools

### Key Features:
- **Complex operations** with multiple parameters
- **System-level access** and control
- **Performance optimization** capabilities
- **Security and compliance** functions
- **Integration** with multiple subsystems
- **Automation** and scripting support

## System Commands

### 1. **SYSTEM Command**
```
SYSTEM
SYSTEM STATUS
SYSTEM INFO
SYSTEM VERSION
```
- **Purpose**: Display system information
- **Action**: Shows system status and details
- **Use Case**: System monitoring and troubleshooting

### 2. **WHO Command**
```
WHO
WHO ALL
WHO USER(userid)
WHO SYSTEM
```
- **Purpose**: Display user information
- **Action**: Shows logged-on users
- **Use Case**: User management and monitoring

### 3. **STATUS Command**
```
STATUS
STATUS USER
STATUS SYSTEM
STATUS JOB
STATUS ALL
```
- **Purpose**: Display status information
- **Action**: Shows current status
- **Use Case**: System monitoring

### 4. **TIME Command**
```
TIME
TIME ZONE
TIME FORMAT
TIME DISPLAY
```
- **Purpose**: Display time information
- **Action**: Shows current time and timezone
- **Use Case**: Time reference and logging

### 5. **DATE Command**
```
DATE
DATE FORMAT
DATE DISPLAY
DATE CALENDAR
```
- **Purpose**: Display date information
- **Action**: Shows current date and calendar
- **Use Case**: Date reference and scheduling

## Dataset Management Commands

### 1. **LISTCAT Command**
```
LISTCAT
LISTCAT 'dataset-name'
LISTCAT 'dataset-name' ALL
LISTCAT 'dataset-name' VOLUME(volser)
LISTCAT 'dataset-name' HISTORY
```
- **Purpose**: List catalog information
- **Action**: Shows dataset catalog details
- **Use Case**: Dataset catalog management

### 2. **LISTDS Command**
```
LISTDS
LISTDS 'dataset-name'
LISTDS 'dataset-name' ALL
LISTDS 'dataset-name' HISTORY
LISTDS 'dataset-name' VOLUME(volser)
```
- **Purpose**: List dataset information
- **Action**: Shows dataset details and attributes
- **Use Case**: Dataset information and management

### 3. **ALLOCATE Command (Advanced)**
```
ALLOCATE FILE(ddname) DATASET('dataset-name') SHR
ALLOCATE FILE(ddname) DATASET('dataset-name') OLD
ALLOCATE FILE(ddname) DATASET('dataset-name') NEW
ALLOCATE FILE(ddname) DATASET('dataset-name') MOD
ALLOCATE FILE(ddname) DATASET('dataset-name') SHR REUSE
```
- **Purpose**: Allocate datasets with advanced options
- **Action**: Allocates datasets with specific attributes
- **Use Case**: Advanced dataset allocation

### 4. **FREE Command (Advanced)**
```
FREE FILE(ddname)
FREE ALL
FREE FILE(INPUT) FILE(OUTPUT)
FREE FILE(ddname) KEEP
FREE FILE(ddname) DELETE
```
- **Purpose**: Free datasets with advanced options
- **Action**: Frees datasets with specific options
- **Use Case**: Advanced dataset management

### 5. **DELETE Command (Advanced)**
```
DELETE 'dataset-name'
DELETE 'dataset-name' PURGE
DELETE 'dataset-name' FORCE
DELETE 'dataset-name' NOSCRATCH
DELETE 'dataset-name' SCRATCH
```
- **Purpose**: Delete datasets with advanced options
- **Action**: Deletes datasets with specific options
- **Use Case**: Advanced dataset cleanup

## Job Management Commands

### 1. **SUBMIT Command (Advanced)**
```
SUBMIT 'dataset-name'
SUBMIT 'dataset-name' CLASS(A)
SUBMIT 'dataset-name' HOLD
SUBMIT 'dataset-name' NOTIFY
SUBMIT 'dataset-name' JOBNAME(jobname)
SUBMIT 'dataset-name' USER(userid)
```
- **Purpose**: Submit jobs with advanced options
- **Action**: Submits jobs with specific parameters
- **Use Case**: Advanced job submission

### 2. **STATUS Command (Jobs)**
```
STATUS JOB(jobname)
STATUS USER
STATUS ALL
STATUS JOB(jobname) DETAIL
STATUS JOB(jobname) OUTPUT
```
- **Purpose**: Check job status with advanced options
- **Action**: Shows detailed job information
- **Use Case**: Job monitoring and management

### 3. **CANCEL Command (Advanced)**
```
CANCEL JOB(jobname)
CANCEL JOB(jobname) PURGE
CANCEL JOB(jobname) FORCE
CANCEL JOB(jobname) HOLD
CANCEL JOB(jobname) RELEASE
```
- **Purpose**: Cancel jobs with advanced options
- **Action**: Cancels jobs with specific options
- **Use Case**: Advanced job control

### 4. **OUTPUT Command**
```
OUTPUT
OUTPUT JOB(jobname)
OUTPUT JOB(jobname) LIST
OUTPUT JOB(jobname) PRINT
OUTPUT JOB(jobname) DELETE
```
- **Purpose**: Manage job output
- **Action**: Handles job output operations
- **Use Case**: Job output management

## Security Commands

### 1. **RACF Commands**
```
RACF
RACF LIST
RACF LIST USER(userid)
RACF LIST DATASET('dataset-name')
RACF LIST GROUP(groupname)
```
- **Purpose**: RACF security management
- **Action**: Shows security information
- **Use Case**: Security administration

### 2. **AUTH Command**
```
AUTH
AUTH LIST
AUTH LIST USER(userid)
AUTH LIST DATASET('dataset-name')
AUTH LIST RESOURCE(resource)
```
- **Purpose**: Display authorization information
- **Action**: Shows user authorizations
- **Use Case**: Security auditing

### 3. **PERMIT Command**
```
PERMIT 'dataset-name' ID(userid) ACCESS(READ)
PERMIT 'dataset-name' ID(userid) ACCESS(WRITE)
PERMIT 'dataset-name' ID(userid) ACCESS(ALTER)
PERMIT 'dataset-name' ID(userid) ACCESS(CONTROL)
```
- **Purpose**: Grant dataset permissions
- **Action**: Sets dataset access permissions
- **Use Case**: Security management

## Performance Commands

### 1. **PERFORM Command**
```
PERFORM
PERFORM USER
PERFORM SYSTEM
PERFORM JOB
PERFORM ALL
```
- **Purpose**: Display performance information
- **Action**: Shows performance metrics
- **Use Case**: Performance monitoring

### 2. **MONITOR Command**
```
MONITOR
MONITOR USER
MONITOR SYSTEM
MONITOR JOB
MONITOR RESOURCE
```
- **Purpose**: Monitor system resources
- **Action**: Shows resource usage
- **Use Case**: Performance monitoring

### 3. **TUNE Command**
```
TUNE
TUNE USER
TUNE SYSTEM
TUNE JOB
TUNE RESOURCE
```
- **Purpose**: Tune system performance
- **Action**: Optimizes system performance
- **Use Case**: Performance optimization

## Utility Commands

### 1. **IKJEFT01 Command**
```
IKJEFT01
IKJEFT01 'command'
IKJEFT01 'command' PARM('parameters')
```
- **Purpose**: Execute TSO commands
- **Action**: Runs TSO commands
- **Use Case**: Command execution

### 2. **IKJEFT1A Command**
```
IKJEFT1A
IKJEFT1A 'command'
IKJEFT1A 'command' PARM('parameters')
```
- **Purpose**: Execute TSO with ISPF
- **Action**: Runs TSO commands with ISPF
- **Use Case**: ISPF command execution

### 3. **IKJEFT1B Command**
```
IKJEFT1B
IKJEFT1B 'command'
IKJEFT1B 'command' PARM('parameters')
```
- **Purpose**: Execute TSO with CLIST
- **Action**: Runs TSO commands with CLIST
- **Use Case**: CLIST execution

## Programming Commands

### 1. **COMPILE Command**
```
COMPILE 'source-dataset'
COMPILE 'source-dataset' LANGUAGE(COBOL)
COMPILE 'source-dataset' LANGUAGE(PLI)
COMPILE 'source-dataset' LANGUAGE(ASSEMBLER)
```
- **Purpose**: Compile source code
- **Action**: Compiles source programs
- **Use Case**: Program development

### 2. **LINK Command**
```
LINK 'object-dataset'
LINK 'object-dataset' LIBRARY('library-dataset')
LINK 'object-dataset' MODULE('module-name')
```
- **Purpose**: Link edit programs
- **Action**: Creates load modules
- **Use Case**: Program development

### 3. **RUN Command**
```
RUN 'program-name'
RUN 'program-name' PARM('parameters')
RUN 'program-name' LIBRARY('library-dataset')
```
- **Purpose**: Run programs
- **Action**: Executes programs
- **Use Case**: Program testing

## Network Commands

### 1. **PING Command**
```
PING 'hostname'
PING 'ip-address'
PING 'hostname' COUNT(5)
PING 'hostname' TIMEOUT(10)
```
- **Purpose**: Test network connectivity
- **Action**: Sends network packets
- **Use Case**: Network troubleshooting

### 2. **TRACERT Command**
```
TRACERT 'hostname'
TRACERT 'ip-address'
TRACERT 'hostname' MAXHOPS(30)
```
- **Purpose**: Trace network route
- **Action**: Shows network path
- **Use Case**: Network troubleshooting

### 3. **NETSTAT Command**
```
NETSTAT
NETSTAT -A
NETSTAT -R
NETSTAT -S
```
- **Purpose**: Display network statistics
- **Action**: Shows network information
- **Use Case**: Network monitoring

## Monitoring Commands

### 1. **TOP Command**
```
TOP
TOP USER
TOP SYSTEM
TOP JOB
TOP RESOURCE
```
- **Purpose**: Display top processes
- **Action**: Shows resource usage
- **Use Case**: System monitoring

### 2. **PS Command**
```
PS
PS USER
PS SYSTEM
PS JOB
PS ALL
```
- **Purpose**: Display process information
- **Action**: Shows running processes
- **Use Case**: Process monitoring

### 3. **VMSTAT Command**
```
VMSTAT
VMSTAT 5
VMSTAT 10 5
```
- **Purpose**: Display virtual memory statistics
- **Action**: Shows memory usage
- **Use Case**: Memory monitoring

## Best Practices

### 1. **Command Usage**
- Use appropriate commands for the task
- Understand command parameters and options
- Test commands in non-production environments
- Document complex command sequences

### 2. **Security**
- Follow security policies and procedures
- Use appropriate access controls
- Monitor command usage
- Maintain audit trails

### 3. **Performance**
- Use efficient commands
- Monitor system resources
- Optimize command sequences
- Avoid resource-intensive operations

### 4. **Error Handling**
- Check command return codes
- Handle errors appropriately
- Use error recovery procedures
- Document error conditions

## Quick Reference

### System Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `SYSTEM` | System info | `SYSTEM STATUS` |
| `WHO` | User info | `WHO ALL` |
| `STATUS` | Status info | `STATUS USER` |
| `TIME` | Time info | `TIME` |
| `DATE` | Date info | `DATE` |

### Dataset Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `LISTCAT` | List catalog | `LISTCAT 'USER.DATA.*'` |
| `LISTDS` | List dataset | `LISTDS 'USER.DATA.INPUT'` |
| `ALLOCATE` | Allocate | `ALLOCATE FILE(IN) DATASET('USER.DATA') SHR` |
| `FREE` | Free | `FREE FILE(IN)` |
| `DELETE` | Delete | `DELETE 'USER.DATA.OBSOLETE'` |

### Job Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `SUBMIT` | Submit job | `SUBMIT 'USER.JCL.JOB'` |
| `STATUS` | Job status | `STATUS JOB(JOB123)` |
| `CANCEL` | Cancel job | `CANCEL JOB(JOB123)` |
| `OUTPUT` | Job output | `OUTPUT JOB(JOB123)` |

### Security Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `RACF` | RACF info | `RACF LIST USER(USER123)` |
| `AUTH` | Authorization | `AUTH LIST USER(USER123)` |
| `PERMIT` | Grant access | `PERMIT 'USER.DATA' ID(USER123) ACCESS(READ)` |

### Performance Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `PERFORM` | Performance | `PERFORM USER` |
| `MONITOR` | Monitor | `MONITOR SYSTEM` |
| `TUNE` | Tune | `TUNE USER` |

---

*Next: [JCL (Job Control Language)](08-jcl-comprehensive.md)*
