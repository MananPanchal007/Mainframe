# Datasets and Operations

## Table of Contents
- [What are Datasets?](#what-are-datasets)
- [Dataset Types](#dataset-types)
- [Dataset Naming Conventions](#dataset-naming-conventions)
- [Dataset Organizations](#dataset-organizations)
- [Dataset Attributes](#dataset-attributes)
- [Dataset Operations](#dataset-operations)
- [Dataset Commands](#dataset-commands)
- [Dataset Management](#dataset-management)
- [Dataset Security](#dataset-security)
- [Best Practices](#best-practices)

## What are Datasets?

A **dataset** is a collection of related data records stored on a mainframe system. Datasets are the fundamental storage units in mainframe systems and can contain:
- **Programs**: Source code, object code, load modules
- **Data**: Business data, transaction records, reports
- **Libraries**: Collections of related programs or data
- **System Data**: Operating system files, configuration data

### Key Characteristics:
- **Persistent Storage**: Data remains after system restart
- **Structured Access**: Organized for efficient retrieval
- **Security**: Access control and protection
- **Backup**: Automatic backup and recovery
- **Versioning**: Multiple versions of the same dataset

## Dataset Types

### 1. **Sequential Datasets**
- **Description**: Records stored one after another
- **Access**: Sequential only (start to end)
- **Use Cases**: Large data files, reports, logs
- **Examples**: `USER.DATA.INPUT`, `SYS1.PROCLIB`

### 2. **Partitioned Datasets (PDS)**
- **Description**: Collection of members in a directory
- **Access**: Direct access to individual members
- **Use Cases**: Source code libraries, JCL libraries
- **Examples**: `USER.SOURCE.COBOL`, `SYS1.PROCLIB`

### 3. **Partitioned Dataset Extended (PDSE)**
- **Description**: Enhanced PDS with better performance
- **Access**: Direct access to individual members
- **Use Cases**: Modern source code libraries
- **Examples**: `USER.SOURCE.COBOL`, `SYS1.PROCLIB`

### 4. **VSAM Datasets**
- **Description**: Virtual Storage Access Method
- **Access**: Direct access by key or relative record number
- **Use Cases**: Database files, indexed data
- **Examples**: `USER.VSAM.CUSTOMER`, `USER.VSAM.ORDER`

### 5. **Generation Data Groups (GDG)**
- **Description**: Collection of related datasets with version numbers
- **Access**: Current, previous, or specific generation
- **Use Cases**: Backup files, historical data
- **Examples**: `USER.BACKUP.DATA.G0001V00`, `USER.REPORT.DAILY.G0002V00`

## Dataset Naming Conventions

### Standard Format:
```
qualifier1.qualifier2.qualifier3.qualifier4.qualifier5
```

### Rules:
- **Maximum 44 characters** total
- **Qualifiers separated by dots** (.)
- **Each qualifier 1-8 characters**
- **First character must be alphabetic** (A-Z, @, #, $)
- **Subsequent characters** alphanumeric (A-Z, 0-9, @, #, $)
- **No consecutive dots**
- **No trailing dots**

### Examples:
```
USER.DATA.INPUT
USER.SOURCE.COBOL
SYS1.PROCLIB
USER.BACKUP.DATA.G0001V00
```

### Common Qualifiers:
- **USER**: User-specific datasets
- **SYS1**: System datasets
- **TEST**: Test datasets
- **PROD**: Production datasets
- **BACKUP**: Backup datasets
- **TEMP**: Temporary datasets

## Dataset Organizations

### 1. **Sequential Organization**
```
Record 1 → Record 2 → Record 3 → ... → Record N
```
- **Access**: Sequential only
- **Use**: Large data files, reports
- **Performance**: Good for sequential processing

### 2. **Partitioned Organization**
```
Directory
├── Member1
├── Member2
├── Member3
└── MemberN
```
- **Access**: Direct to members
- **Use**: Source code libraries
- **Performance**: Good for member access

### 3. **VSAM Organization**
```
Index → Data Records
```
- **Access**: Direct by key
- **Use**: Database files
- **Performance**: Excellent for random access

## Dataset Attributes

### 1. **Record Format (RECFM)**
- **F**: Fixed-length records
- **V**: Variable-length records
- **U**: Undefined records
- **FB**: Fixed blocked
- **VB**: Variable blocked

### 2. **Record Length (LRECL)**
- **Fixed**: All records same length
- **Variable**: Records can be different lengths
- **Maximum**: 32,760 bytes

### 3. **Block Size (BLKSIZE)**
- **Size of physical blocks** on storage device
- **Multiple of record length**
- **Affects I/O performance**

### 4. **Space Allocation**
- **Primary**: Initial space allocation
- **Secondary**: Additional space when needed
- **Directory**: Space for PDS directory

### 5. **Device Type**
- **3390**: Standard DASD device
- **3390-3**: Extended capacity
- **3390-9**: High capacity

## Dataset Operations

### 1. **Create Dataset**
```
ALLOCATE DATASET('dataset-name') NEW
```

**Example:**
```
ALLOCATE DATASET('USER.DATA.NEW') NEW
```

### 2. **Delete Dataset**
```
DELETE 'dataset-name'
```

**Example:**
```
DELETE 'USER.DATA.OBSOLETE'
```

### 3. **Rename Dataset**
```
RENAME 'old-name' 'new-name'
```

**Example:**
```
RENAME 'USER.DATA.OLD' 'USER.DATA.NEW'
```

### 4. **Copy Dataset**
```
COPY 'source-dataset' 'target-dataset'
```

**Example:**
```
COPY 'USER.DATA.BACKUP' 'USER.DATA.CURRENT'
```

### 5. **Move Dataset**
```
MOVE 'source-dataset' 'target-dataset'
```

**Example:**
```
MOVE 'USER.DATA.TEMP' 'USER.DATA.PERMANENT'
```

### 6. **Compress Dataset**
```
COMPRESS 'dataset-name'
```

**Example:**
```
COMPRESS 'USER.DATA.LARGE'
```

## Dataset Commands

### 1. **LISTC (List Catalog)**
```
LISTC 'dataset-name'
LISTC 'dataset-name' ALL
LISTC 'dataset-name' VOL(volser)
```

**Examples:**
```
LISTC 'USER.DATA.*'
LISTC 'USER.DATA.INPUT' ALL
LISTC 'SYS1.PROCLIB' VOL(PRD001)
```

### 2. **LISTDS (List Dataset)**
```
LISTDS 'dataset-name'
LISTDS 'dataset-name' ALL
LISTDS 'dataset-name' HISTORY
```

**Examples:**
```
LISTDS 'USER.DATA.INPUT'
LISTDS 'USER.DATA.*' ALL
LISTDS 'SYS1.PROCLIB' HISTORY
```

### 3. **ALLOCATE Command**
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

### 4. **FREE Command**
```
FREE FILE(ddname)
FREE ALL
FREE FILE(INPUT) FILE(OUTPUT)
```

### 5. **DELETE Command**
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

## Dataset Management

### 1. **Space Management**
- **Primary Space**: Initial allocation
- **Secondary Space**: Additional space when needed
- **Directory Space**: For PDS directories
- **Space Monitoring**: Track space usage

### 2. **Version Control**
- **Generation Data Groups**: Multiple versions
- **Backup Strategies**: Regular backups
- **Archive Policies**: Long-term storage
- **Retention Rules**: Data lifecycle management

### 3. **Performance Optimization**
- **Block Size**: Optimize I/O performance
- **Record Format**: Choose appropriate format
- **Device Selection**: Use appropriate storage
- **Access Patterns**: Optimize for usage

### 4. **Maintenance**
- **Compression**: Reclaim unused space
- **Reorganization**: Optimize structure
- **Validation**: Check data integrity
- **Cleanup**: Remove obsolete data

## Dataset Security

### 1. **Access Control**
- **User Authorization**: Who can access
- **Dataset Permissions**: Read, write, execute
- **Resource Classes**: Dataset protection
- **Security Profiles**: User profiles

### 2. **Data Protection**
- **Encryption**: Data at rest
- **Audit Trails**: Access logging
- **Backup Security**: Protected backups
- **Recovery Procedures**: Data restoration

### 3. **Compliance**
- **Regulatory Requirements**: Industry standards
- **Data Classification**: Sensitivity levels
- **Retention Policies**: Legal requirements
- **Audit Requirements**: Compliance reporting

## Best Practices

### 1. **Naming Conventions**
- Use meaningful, descriptive names
- Follow organizational standards
- Include environment indicators
- Use consistent qualifiers

### 2. **Space Management**
- Allocate appropriate space
- Monitor space usage
- Clean up unused datasets
- Use compression when beneficial

### 3. **Security**
- Follow access control rules
- Use appropriate permissions
- Protect sensitive data
- Maintain audit trails

### 4. **Performance**
- Choose appropriate organization
- Optimize block sizes
- Use appropriate record formats
- Monitor performance metrics

### 5. **Maintenance**
- Regular cleanup
- Monitor for errors
- Keep documentation current
- Follow change management

## Quick Reference

### Essential Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `LISTC` | List catalog | `LISTC 'USER.DATA.*'` |
| `LISTDS` | List dataset | `LISTDS 'USER.DATA'` |
| `ALLOCATE` | Allocate dataset | `ALLOCATE FILE(IN) DATASET('USER.DATA') SHR` |
| `FREE` | Free dataset | `FREE FILE(IN)` |
| `DELETE` | Delete dataset | `DELETE 'USER.TEMP.DATA'` |
| `RENAME` | Rename dataset | `RENAME 'OLD' 'NEW'` |
| `COPY` | Copy dataset | `COPY 'SRC' 'TGT'` |
| `MOVE` | Move dataset | `MOVE 'SRC' 'TGT'` |
| `COMPRESS` | Compress dataset | `COMPRESS 'USER.DATA'` |

### Dataset Organizations:
| Type | Access | Use Case | Example |
|------|--------|----------|---------|
| Sequential | Sequential | Large files | `USER.DATA.INPUT` |
| PDS | Direct to members | Source libraries | `USER.SOURCE.COBOL` |
| PDSE | Direct to members | Modern libraries | `USER.SOURCE.COBOL` |
| VSAM | Direct by key | Database files | `USER.VSAM.CUSTOMER` |
| GDG | Current/Previous | Versioned data | `USER.BACKUP.DATA.G0001V00` |

### Record Formats:
| Format | Description | Use Case |
|--------|-------------|----------|
| F | Fixed | All records same length |
| V | Variable | Records different lengths |
| U | Undefined | Binary data |
| FB | Fixed Blocked | Fixed records in blocks |
| VB | Variable Blocked | Variable records in blocks |

---

*Next: [ISPF Edit Line Commands](05-ispf-edit-commands.md)*
