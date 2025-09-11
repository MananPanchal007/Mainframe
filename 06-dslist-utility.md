# DSLIST Utility Primary Commands

## Table of Contents
- [What is DSLIST?](#what-is-dslist)
- [Accessing DSLIST](#accessing-dslist)
- [DSLIST Interface](#dslist-interface)
- [Primary Commands](#primary-commands)
- [Dataset Operations](#dataset-operations)
- [Member Operations](#member-operations)
- [Display Commands](#display-commands)
- [Sort and Filter Commands](#sort-and-filter-commands)
- [Utility Commands](#utility-commands)
- [Line Commands](#line-commands)
- [Best Practices](#best-practices)

## What is DSLIST?

**DSLIST (Dataset List)** is an ISPF utility that provides a comprehensive interface for managing datasets and their members. It offers:
- **Dataset browsing** and management
- **Member operations** for partitioned datasets
- **Dataset information** display
- **Bulk operations** on multiple datasets
- **Search and filter** capabilities
- **Integration** with other ISPF utilities

### Key Features:
- **Visual interface** for dataset management
- **Bulk operations** on multiple datasets
- **Member management** for PDS/PDSE
- **Search and filter** capabilities
- **Integration** with edit and browse
- **Customizable display** options

## Accessing DSLIST

### 1. **From ISPF Primary Menu**
```
Option 3 - UTILITIES
Option 3.4 - DSLIST
```

### 2. **Direct Access**
```
=3.4
```

### 3. **Command Line**
```
DSLIST
```

### 4. **With Parameters**
```
DSLIST 'dataset-name'
DSLIST 'dataset-pattern'
```

## DSLIST Interface

### Main Screen Layout:
```
┌─────────────────────────────────────────────────────────────┐
│                    DATASET LIST UTILITY                    │
│                                                             │
│ Dataset Name . . . . . . . . . . . . . . . . . . . . . . . │
│ Volume Serial . . . . . . . . . . . . . . . . . . . . . .  │
│ Generic Level . . . . . . . . . . . . . . . . . . . . . .  │
│ Member Name . . . . . . . . . . . . . . . . . . . . . . .  │
│                                                                 │
│ Command ===>                                                    │
│                                                                 │
│ ┌─────────────────────────────────────────────────────────┐   │
│ │ Dataset Name                    | Vol | Org | Recfm | LRECL │
│ ├─────────────────────────────────────────────────────────┤   │
│ │ USER.DATA.INPUT                 | PRD1| PS  | FB    | 80   │
│ │ USER.DATA.OUTPUT                | PRD1| PS  | FB    | 80   │
│ │ USER.SOURCE.COBOL               | PRD1| PO  | FB    | 80   │
│ │ USER.SOURCE.COBOL(MEMBER1)      | PRD1| PO  | FB    | 80   │
│ │ USER.SOURCE.COBOL(MEMBER2)      | PRD1| PO  | FB    | 80   │
│ └─────────────────────────────────────────────────────────┘   │
│                                                                 │
│ F1=Help    F2=Split    F3=Exit     F4=Return                   │
│ F5=RFind   F6=RChange  F7=Up       F8=Down                     │
│ F9=Swap    F10=Left    F11=Right   F12=Cursor                  │
└─────────────────────────────────────────────────────────────┘
```

## Primary Commands

### 1. **LIST Command**
```
LIST
LIST 'dataset-name'
LIST 'dataset-pattern'
```
- **Purpose**: List datasets matching criteria
- **Action**: Displays datasets in the list
- **Use Case**: Browse available datasets

### 2. **SORT Command**
```
SORT NAME
SORT VOLUME
SORT SIZE
SORT DATE
SORT CREATED
SORT REFERRED
```
- **Purpose**: Sort the dataset list
- **Action**: Reorders datasets by specified criteria
- **Use Case**: Organize dataset display

### 3. **FILTER Command**
```
FILTER 'pattern'
FILTER 'pattern' VOLUME(volser)
FILTER 'pattern' ORGANIZATION(PS)
```
- **Purpose**: Filter datasets by criteria
- **Action**: Shows only matching datasets
- **Use Case**: Focus on specific datasets

### 4. **RESET Command**
```
RESET
RESET SORT
RESET FILTER
```
- **Purpose**: Reset display options
- **Action**: Clears sort and filter settings
- **Use Case**: Return to default view

## Dataset Operations

### 1. **BROWSE Command**
```
BROWSE 'dataset-name'
BROWSE 'dataset-name' MEMBER(member-name)
```
- **Purpose**: Browse dataset contents
- **Action**: Opens dataset in browse mode
- **Use Case**: View dataset contents

### 2. **EDIT Command**
```
EDIT 'dataset-name'
EDIT 'dataset-name' MEMBER(member-name)
```
- **Purpose**: Edit dataset contents
- **Action**: Opens dataset in edit mode
- **Use Case**: Modify dataset contents

### 3. **COPY Command**
```
COPY 'source-dataset' 'target-dataset'
COPY 'source-dataset' 'target-dataset' REPLACE
```
- **Purpose**: Copy dataset
- **Action**: Creates a copy of the dataset
- **Use Case**: Backup or duplicate datasets

### 4. **MOVE Command**
```
MOVE 'source-dataset' 'target-dataset'
MOVE 'source-dataset' 'target-dataset' REPLACE
```
- **Purpose**: Move dataset
- **Action**: Moves dataset to new location
- **Use Case**: Reorganize datasets

### 5. **DELETE Command**
```
DELETE 'dataset-name'
DELETE 'dataset-name' PURGE
DELETE 'dataset-name' FORCE
```
- **Purpose**: Delete dataset
- **Action**: Removes dataset from system
- **Use Case**: Clean up obsolete datasets

### 6. **RENAME Command**
```
RENAME 'old-name' 'new-name'
```
- **Purpose**: Rename dataset
- **Action**: Changes dataset name
- **Use Case**: Update dataset names

## Member Operations

### 1. **MEMBER Command**
```
MEMBER 'dataset-name'
MEMBER 'dataset-name' 'member-name'
```
- **Purpose**: List members of partitioned dataset
- **Action**: Shows members in the dataset
- **Use Case**: Browse PDS/PDSE members

### 2. **BROWSE Member**
```
BROWSE 'dataset-name' MEMBER('member-name')
```
- **Purpose**: Browse specific member
- **Action**: Opens member in browse mode
- **Use Case**: View member contents

### 3. **EDIT Member**
```
EDIT 'dataset-name' MEMBER('member-name')
```
- **Purpose**: Edit specific member
- **Action**: Opens member in edit mode
- **Use Case**: Modify member contents

### 4. **COPY Member**
```
COPY 'source-dataset' MEMBER('member-name') 'target-dataset'
COPY 'source-dataset' MEMBER('member-name') 'target-dataset' MEMBER('new-name')
```
- **Purpose**: Copy member
- **Action**: Creates a copy of the member
- **Use Case**: Duplicate members

### 5. **MOVE Member**
```
MOVE 'source-dataset' MEMBER('member-name') 'target-dataset'
MOVE 'source-dataset' MEMBER('member-name') 'target-dataset' MEMBER('new-name')
```
- **Purpose**: Move member
- **Action**: Moves member to new location
- **Use Case**: Reorganize members

### 6. **DELETE Member**
```
DELETE 'dataset-name' MEMBER('member-name')
DELETE 'dataset-name' MEMBER('member-name') PURGE
```
- **Purpose**: Delete member
- **Action**: Removes member from dataset
- **Use Case**: Clean up obsolete members

## Display Commands

### 1. **SHOW Command**
```
SHOW
SHOW ALL
SHOW VOLUME
SHOW SIZE
SHOW DATE
```
- **Purpose**: Show additional information
- **Action**: Displays extra dataset details
- **Use Case**: Get more information about datasets

### 2. **HIDE Command**
```
HIDE
HIDE VOLUME
HIDE SIZE
HIDE DATE
```
- **Purpose**: Hide information columns
- **Action**: Removes columns from display
- **Use Case**: Simplify display

### 3. **WIDTH Command**
```
WIDTH 80
WIDTH 132
WIDTH 200
```
- **Purpose**: Set display width
- **Action**: Changes screen width
- **Use Case**: Optimize display for different screen sizes

## Sort and Filter Commands

### 1. **SORT Options**
```
SORT NAME          # Sort by dataset name
SORT VOLUME        # Sort by volume serial
SORT SIZE          # Sort by dataset size
SORT DATE          # Sort by last modified date
SORT CREATED       # Sort by creation date
SORT REFERRED      # Sort by last referenced date
SORT ORGANIZATION  # Sort by dataset organization
SORT RECFM         # Sort by record format
```

### 2. **FILTER Options**
```
FILTER 'pattern'                    # Filter by name pattern
FILTER 'pattern' VOLUME(volser)     # Filter by volume
FILTER 'pattern' ORGANIZATION(PS)   # Filter by organization
FILTER 'pattern' SIZE(>1000)        # Filter by size
FILTER 'pattern' DATE(>20240101)    # Filter by date
```

### 3. **RESET Options**
```
RESET              # Reset all filters and sorts
RESET SORT         # Reset only sort
RESET FILTER       # Reset only filter
```

## Utility Commands

### 1. **COMPRESS Command**
```
COMPRESS 'dataset-name'
COMPRESS ALL
```
- **Purpose**: Compress dataset
- **Action**: Reclaims unused space
- **Use Case**: Optimize dataset storage

### 2. **VERIFY Command**
```
VERIFY 'dataset-name'
VERIFY ALL
```
- **Purpose**: Verify dataset integrity
- **Action**: Checks dataset for errors
- **Use Case**: Validate dataset health

### 3. **CATALOG Command**
```
CATALOG 'dataset-name'
CATALOG 'dataset-name' VOLUME(volser)
```
- **Purpose**: Catalog dataset
- **Action**: Adds dataset to catalog
- **Use Case**: Make dataset accessible

### 4. **UNCATALOG Command**
```
UNCATALOG 'dataset-name'
```
- **Purpose**: Un catalog dataset
- **Action**: Removes dataset from catalog
- **Use Case**: Remove dataset from catalog

## Line Commands

### 1. **B - Browse**
```
B 000100
```
- **Purpose**: Browse dataset
- **Action**: Opens dataset in browse mode
- **Use Case**: View dataset contents

### 2. **E - Edit**
```
E 000100
```
- **Purpose**: Edit dataset
- **Action**: Opens dataset in edit mode
- **Use Case**: Modify dataset contents

### 3. **C - Copy**
```
C 000100
```
- **Purpose**: Copy dataset
- **Action**: Initiates copy operation
- **Use Case**: Duplicate dataset

### 4. **M - Move**
```
M 000100
```
- **Purpose**: Move dataset
- **Action**: Initiates move operation
- **Use Case**: Relocate dataset

### 5. **D - Delete**
```
D 000100
```
- **Purpose**: Delete dataset
- **Action**: Initiates delete operation
- **Use Case**: Remove dataset

### 6. **R - Rename**
```
R 000100
```
- **Purpose**: Rename dataset
- **Action**: Initiates rename operation
- **Use Case**: Change dataset name

## Best Practices

### 1. **Dataset Management**
- Use meaningful dataset names
- Organize datasets logically
- Regular cleanup of obsolete datasets
- Monitor dataset usage

### 2. **Member Management**
- Use consistent member naming
- Regular cleanup of obsolete members
- Document member purposes
- Version control for important members

### 3. **Performance**
- Use appropriate filters to reduce display
- Sort datasets for easier navigation
- Use bulk operations when possible
- Monitor system resources

### 4. **Security**
- Follow access control rules
- Use appropriate permissions
- Protect sensitive datasets
- Maintain audit trails

## Quick Reference

### Primary Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `LIST` | List datasets | `LIST 'USER.DATA.*'` |
| `SORT` | Sort datasets | `SORT NAME` |
| `FILTER` | Filter datasets | `FILTER 'USER.DATA.*'` |
| `RESET` | Reset display | `RESET` |
| `BROWSE` | Browse dataset | `BROWSE 'USER.DATA.INPUT'` |
| `EDIT` | Edit dataset | `EDIT 'USER.DATA.INPUT'` |
| `COPY` | Copy dataset | `COPY 'SRC' 'TGT'` |
| `MOVE` | Move dataset | `MOVE 'SRC' 'TGT'` |
| `DELETE` | Delete dataset | `DELETE 'USER.DATA.OBSOLETE'` |
| `RENAME` | Rename dataset | `RENAME 'OLD' 'NEW'` |

### Line Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `B` | Browse | `B 000100` |
| `E` | Edit | `E 000100` |
| `C` | Copy | `C 000100` |
| `M` | Move | `M 000100` |
| `D` | Delete | `D 000100` |
| `R` | Rename | `R 000100` |

### Sort Options:
| Option | Purpose | Description |
|--------|---------|-------------|
| `NAME` | Sort by name | Alphabetical order |
| `VOLUME` | Sort by volume | Volume serial order |
| `SIZE` | Sort by size | Size order |
| `DATE` | Sort by date | Last modified date |
| `CREATED` | Sort by created | Creation date |
| `REFERRED` | Sort by referred | Last referenced date |

### Filter Options:
| Option | Purpose | Example |
|--------|---------|---------|
| `'pattern'` | Name pattern | `'USER.DATA.*'` |
| `VOLUME(volser)` | Volume filter | `VOLUME(PRD001)` |
| `ORGANIZATION(PS)` | Org filter | `ORGANIZATION(PS)` |
| `SIZE(>1000)` | Size filter | `SIZE(>1000)` |
| `DATE(>20240101)` | Date filter | `DATE(>20240101)` |

---

*Next: [Advanced Commands](07-advanced-commands.md)*
