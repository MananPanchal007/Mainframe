# ISPF (Interactive System Productivity Facility)

## Table of Contents
- [What is ISPF?](#what-is-ispf)
- [ISPF Architecture](#ispf-architecture)
- [Starting ISPF](#starting-ispf)
- [ISPF Primary Menu](#ispf-primary-menu)
- [ISPF Panels](#ispf-panels)
- [Navigation](#navigation)
- [ISPF Settings](#ispf-settings)
- [ISPF Utilities](#ispf-utilities)
- [ISPF Commands](#ispf-commands)
- [Customization](#customization)
- [Best Practices](#best-practices)

## What is ISPF?

**ISPF (Interactive System Productivity Facility)** is a comprehensive development environment for mainframe systems. It provides:
- **Menu-driven interface** for easy navigation
- **Text editor** for program development
- **Dataset management** tools
- **Job submission** and monitoring
- **System utilities** and tools
- **Customizable environment** for different users

### Key Features:
- **User-friendly interface** with menus and panels
- **Powerful text editor** with advanced features
- **Dataset browser** and manager
- **Job management** capabilities
- **System utilities** integration
- **Customizable settings** and preferences

## ISPF Architecture

### Main Components:
```
┌─────────────────────────────────────┐
│           ISPF Environment          │
├─────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  │
│  │   Primary   │  │   Utility   │  │
│  │    Menu     │  │   Panels    │  │
│  └─────────────┘  └─────────────┘  │
│  ┌─────────────┐  ┌─────────────┐  │
│  │    Edit     │  │   Browse    │  │
│  │   Panels    │  │   Panels    │  │
│  └─────────────┘  └─────────────┘  │
│  ┌─────────────┐  ┌─────────────┐  │
│  │   Dataset   │  │    Job      │  │
│  │   Manager   │  │   Manager   │  │
│  └─────────────┘  └─────────────┘  │
└─────────────────────────────────────┘
```

### Panel Types:
1. **Primary Panels**: Main menu system
2. **Utility Panels**: System utilities
3. **Edit Panels**: Text editing
4. **Browse Panels**: Read-only viewing
5. **Dataset Panels**: Dataset management
6. **Job Panels**: Job management

## Starting ISPF

### 1. **From TSO**
```
ISPF
```
or
```
ISPSTART
```

### 2. **With Specific Options**
```
ISPF PARM('option1,option2')
```

### 3. **Logon Process**
```
LOGON userid
Password: ********
IKJ56521I TSO READY
ISPF
```

### 4. **Initial Screen**
```
┌─────────────────────────────────────────────────────────────┐
│                    ISPF/PDF PRIMARY OPTION MENU            │
│                                                             │
│ 0  ISPF PARMS  - Specify terminal and user parameters      │
│ 1  BROWSE      - View source data or output listings       │
│ 2  EDIT        - Create or change source data              │
│ 3  UTILITIES   - Perform utility functions                 │
│ 4  FOREGROUND  - Invoke language-sensitive processors      │
│ 5  BATCH       - Submit job for language processing        │
│ 6  COMMAND     - Enter TSO command, CLIST, or REXX exec    │
│ 7  DIALOG TEST - Perform dialog testing                    │
│ 8  LM UTILITIES- Perform library management functions      │
│ 9  IBM PRODUCTS- Additional IBM program development products│
│ 10 SCLM        - Software Configuration and Library Manager│
│ 11 HARDCOPY    - Print hardcopy output                     │
│ 12 FORMAT      - Format print output                       │
│ 13 HARDCOPY    - Print hardcopy output                     │
│ 14 SUPERC      - CICS transaction development              │
│ 15 FORECAST    - System performance monitor                │
│ 16 PERFORM     - TSO performance monitor                   │
│ 17 RADIO      - System and network monitor                 │
│ 18 ORACLE     - Oracle database management                 │
│ 19 SCLM       - Software Configuration and Library Manager │
│ 20 SCLM       - Software Configuration and Library Manager │
│                                                             │
│ SELECT OPTION ===>                                          │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## ISPF Primary Menu

### Main Options:

#### **0 - ISPF PARMS**
- Terminal settings
- User preferences
- Display options
- Security settings

#### **1 - BROWSE**
- View datasets in read-only mode
- Navigate through large datasets
- Search and locate specific data
- Print or copy data

#### **2 - EDIT**
- Create new datasets
- Modify existing datasets
- Advanced text editing features
- Line commands and primary commands

#### **3 - UTILITIES**
- Dataset management
- Dataset list (DSLIST)
- Dataset information
- Dataset operations

#### **4 - FOREGROUND**
- Compile programs
- Link edit programs
- Run programs interactively
- Language-specific processing

#### **5 - BATCH**
- Submit batch jobs
- Monitor job status
- View job output
- Job management

#### **6 - COMMAND**
- Execute TSO commands
- Run CLISTs
- Execute REXX programs
- System commands

## ISPF Panels

### Panel Structure:
```
┌─────────────────────────────────────────────────────────────┐
│                    Panel Title                             │
│                                                             │
│ Field1: ===> value1                                        │
│ Field2: ===> value2                                        │
│ Field3: ===> value3                                        │
│                                                             │
│ Command ===>                                                │
│                                                             │
│ F1=Help    F2=Split    F3=Exit     F4=Return               │
│ F5=RFind   F6=RChange  F7=Up       F8=Down                 │
│ F9=Swap    F10=Left    F11=Right   F12=Cursor              │
└─────────────────────────────────────────────────────────────┘
```

### Panel Elements:
- **Title**: Panel identification
- **Fields**: Input areas for data
- **Command Line**: For entering commands
- **Function Keys**: Quick access to common functions
- **Status Line**: System messages and information

## Navigation

### Function Keys:

#### **F1 - Help**
- Context-sensitive help
- Panel information
- Command help

#### **F2 - Split**
- Split screen display
- View multiple panels simultaneously
- Compare datasets

#### **F3 - Exit**
- Return to previous panel
- Exit current function
- Close current session

#### **F4 - Return**
- Return to previous panel
- Save current settings
- Continue with current function

#### **F5 - RFind**
- Repeat last find operation
- Continue searching
- Find next occurrence

#### **F6 - RChange**
- Repeat last change operation
- Continue replacing
- Change next occurrence

#### **F7 - Up**
- Scroll up
- Move to previous page
- Navigate backward

#### **F8 - Down**
- Scroll down
- Move to next page
- Navigate forward

#### **F9 - Swap**
- Switch between split screens
- Toggle between panels
- Exchange panel positions

#### **F10 - Left**
- Move left
- Scroll left
- Previous column

#### **F11 - Right**
- Move right
- Scroll right
- Next column

#### **F12 - Cursor**
- Move cursor to command line
- Position cursor
- Cursor control

### Navigation Commands:
- **=**: Go to specific panel
- **=3.1**: Go to utility panel 3.1
- **=2.1**: Go to edit panel 2.1
- **=1.1**: Go to browse panel 1.1

## ISPF Settings

### Accessing Settings:
```
Option 0 - ISPF PARMS
```

### Key Settings:

#### **Terminal Settings**
- Screen size
- Character set
- Display options
- Color settings

#### **User Settings**
- Default libraries
- Dataset naming conventions
- Editor preferences
- Security settings

#### **Display Settings**
- Line numbers
- Column markers
- Ruler display
- Status information

#### **Edit Settings**
- Tab settings
- Case sensitivity
- Auto-save options
- Backup settings

## ISPF Utilities

### Utility Menu (Option 3):
```
┌─────────────────────────────────────────────────────────────┐
│                    UTILITY SELECTION MENU                  │
│                                                             │
│ 1  LIBRARY    - Library utility                            │
│ 2  DATASET    - Dataset utility                            │
│ 3  MOVE/COPY  - Move or copy members                       │
│ 4  DSLIST     - Dataset list utility                       │
│ 5  RESET      - Reset statistics for members               │
│ 6  CATALOG    - Catalog utility                            │
│ 7  VTOC       - VTOC utility                               │
│ 8  HARDCOPY   - Print utility                              │
│ 9  OUTLIST    - Output list utility                        │
│ 10 SORT       - Sort utility                               │
│ 11 FORMAT     - Format utility                             │
│ 12 COMPRESS   - Compress partitioned data set              │
│ 13 SEARCH-FOR - Search for string in data set              │
│ 14 VERIFY     - Verify data set integrity                  │
│ 15 DITTO      - DITTO utility                              │
│ 16 COMPARE    - Compare data sets                          │
│ 17 RENAME     - Rename data set                            │
│ 18 DELETE     - Delete data set                            │
│ 19 RECEIVE    - Receive data set                           │
│ 20 TRANSMIT  - Transmit data set                           │
│                                                             │
│ SELECT OPTION ===>                                          │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Key Utilities:

#### **3.1 - Library Utility**
- Manage partitioned datasets
- List members
- Copy members
- Delete members

#### **3.2 - Dataset Utility**
- Dataset information
- Dataset operations
- Dataset attributes
- Dataset statistics

#### **3.4 - DSLIST Utility**
- List datasets
- Dataset operations
- Dataset management
- Dataset information

## ISPF Commands

### Primary Commands:
- **=**: Navigate to panel
- **HELP**: Get help
- **END**: Exit current function
- **RETURN**: Return to previous panel
- **SPLIT**: Split screen
- **SWAP**: Swap split screens

### Edit Commands:
- **FIND**: Search for text
- **CHANGE**: Replace text
- **COPY**: Copy lines
- **MOVE**: Move lines
- **DELETE**: Delete lines
- **INSERT**: Insert lines

### Browse Commands:
- **FIND**: Search for text
- **LOCATE**: Go to specific line
- **PRINT**: Print data
- **COPY**: Copy data

## Customization

### User Profile:
- **ISPF Profile**: User-specific settings
- **Default Libraries**: Frequently used libraries
- **Editor Settings**: Personal preferences
- **Display Options**: Screen appearance

### Panel Customization:
- **Panel Libraries**: Custom panels
- **Menu Options**: Additional functions
- **Help Text**: Custom help information
- **Messages**: User-defined messages

### Command Customization:
- **Command Aliases**: Shortcuts for commands
- **Command Tables**: Custom command processing
- **Macros**: Automated command sequences
- **CLISTs**: Command procedures

## Best Practices

### 1. **Navigation**
- Use function keys for common operations
- Learn keyboard shortcuts
- Use split screen for comparison
- Keep track of current panel

### 2. **Dataset Management**
- Use meaningful dataset names
- Organize datasets logically
- Use appropriate dataset organizations
- Keep datasets clean and organized

### 3. **Editing**
- Use line commands for efficiency
- Save work frequently
- Use backup datasets
- Follow coding standards

### 4. **Security**
- Follow access control rules
- Use appropriate dataset permissions
- Log off when finished
- Protect sensitive data

### 5. **Performance**
- Use appropriate dataset organizations
- Avoid unnecessary operations
- Monitor system resources
- Optimize dataset access

## Quick Reference

### Essential Function Keys:
| Key | Function | Description |
|-----|----------|-------------|
| F1 | Help | Context-sensitive help |
| F2 | Split | Split screen display |
| F3 | Exit | Return to previous panel |
| F4 | Return | Continue with current function |
| F5 | RFind | Repeat find operation |
| F6 | RChange | Repeat change operation |
| F7 | Up | Scroll up |
| F8 | Down | Scroll down |
| F9 | Swap | Switch split screens |
| F10 | Left | Move left |
| F11 | Right | Move right |
| F12 | Cursor | Move to command line |

### Common Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `=` | Navigate | `=3.1` |
| `HELP` | Get help | `HELP` |
| `END` | Exit | `END` |
| `RETURN` | Return | `RETURN` |
| `SPLIT` | Split screen | `SPLIT` |
| `SWAP` | Swap screens | `SWAP` |

---

*Next: [Datasets and Operations](04-datasets-operations.md)*
