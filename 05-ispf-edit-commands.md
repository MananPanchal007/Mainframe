# ISPF Edit Line Commands

## Table of Contents
- [What are Line Commands?](#what-are-line-commands)
- [Line Command Syntax](#line-command-syntax)
- [Basic Line Commands](#basic-line-commands)
- [Advanced Line Commands](#advanced-line-commands)
- [Range Commands](#range-commands)
- [Copy and Move Commands](#copy-and-move-commands)
- [Delete Commands](#delete-commands)
- [Insert Commands](#insert-commands)
- [Change Commands](#change-commands)
- [Search Commands](#search-commands)
- [Primary Commands](#primary-commands)
- [Best Practices](#best-practices)

## What are Line Commands?

**Line commands** are single-character or two-character commands that you enter in the line number area (prefix area) of the ISPF editor. They provide quick and efficient ways to:
- **Manipulate lines** of text
- **Copy, move, and delete** lines
- **Insert and modify** content
- **Search and replace** text
- **Format and organize** data

### Key Features:
- **Single character** commands for speed
- **Range operations** for multiple lines
- **Immediate execution** when you press Enter
- **Visual feedback** on the screen
- **Undo capability** for most operations

## Line Command Syntax

### Basic Format:
```
Command Line Number
```

### Examples:
```
C 000100
D 000200
I 000300
M 000400 000500
```

### Command Types:
1. **Single Line Commands**: Operate on one line
2. **Range Commands**: Operate on multiple lines
3. **Block Commands**: Operate on blocks of text
4. **Global Commands**: Operate on entire dataset

## Basic Line Commands

### 1. **C - Copy Line**
```
C 000100
```
- **Purpose**: Copy a single line
- **Action**: Creates a copy of the line after the current line
- **Use Case**: Duplicate lines for modification

### 2. **D - Delete Line**
```
D 000200
```
- **Purpose**: Delete a single line
- **Action**: Removes the line from the dataset
- **Use Case**: Remove unwanted lines

### 3. **I - Insert Line**
```
I 000300
```
- **Purpose**: Insert a new line
- **Action**: Creates an empty line after the specified line
- **Use Case**: Add new content

### 4. **M - Move Line**
```
M 000400 000500
```
- **Purpose**: Move a line to a new position
- **Action**: Moves the line from source to destination
- **Use Case**: Reorganize content

### 5. **R - Repeat Line**
```
R 000600
```
- **Purpose**: Repeat a line
- **Action**: Creates a copy of the line after itself
- **Use Case**: Duplicate lines for modification

## Advanced Line Commands

### 1. **A - After**
```
A 000100
```
- **Purpose**: Insert line after specified line
- **Action**: Creates empty line after the line
- **Use Case**: Add content after specific line

### 2. **B - Before**
```
B 000200
```
- **Purpose**: Insert line before specified line
- **Action**: Creates empty line before the line
- **Use Case**: Add content before specific line

### 3. **E - Exclude**
```
E 000300
```
- **Purpose**: Exclude line from display
- **Action**: Hides the line from view
- **Use Case**: Focus on specific content

### 4. **F - Find**
```
F 000400
```
- **Purpose**: Find and display line
- **Action**: Locates and displays the line
- **Use Case**: Locate specific content

### 5. **G - Get**
```
G 000500
```
- **Purpose**: Get line from another dataset
- **Action**: Retrieves line from specified dataset
- **Use Case**: Import content from other datasets

## Range Commands

### 1. **CC - Copy Range**
```
CC 000100 000200
```
- **Purpose**: Copy multiple lines
- **Action**: Copies the range of lines
- **Use Case**: Duplicate blocks of text

### 2. **DD - Delete Range**
```
DD 000300 000400
```
- **Purpose**: Delete multiple lines
- **Action**: Removes the range of lines
- **Use Case**: Remove blocks of text

### 3. **MM - Move Range**
```
MM 000500 000600 000700
```
- **Purpose**: Move multiple lines
- **Action**: Moves the range to new position
- **Use Case**: Reorganize blocks of text

### 4. **RR - Repeat Range**
```
RR 000800 000900
```
- **Purpose**: Repeat multiple lines
- **Action**: Creates copies of the range
- **Use Case**: Duplicate blocks of text

## Copy and Move Commands

### 1. **Copy Commands**
```
C 000100          # Copy single line
CC 000100 000200  # Copy range
C 000100 000200   # Copy range (alternative)
```

### 2. **Move Commands**
```
M 000100 000200   # Move single line
MM 000100 000200 000300  # Move range
M 000100 000200 000300   # Move range (alternative)
```

### 3. **Copy to Another Dataset**
```
C 000100 'USER.DATA.TARGET'
CC 000100 000200 'USER.DATA.TARGET'
```

### 4. **Move to Another Dataset**
```
M 000100 'USER.DATA.TARGET'
MM 000100 000200 'USER.DATA.TARGET'
```

## Delete Commands

### 1. **Single Line Delete**
```
D 000100
```

### 2. **Range Delete**
```
DD 000100 000200
D 000100 000200
```

### 3. **Block Delete**
```
BD 000100 000200
```

### 4. **Delete with Confirmation**
```
D 000100 CONFIRM
DD 000100 000200 CONFIRM
```

## Insert Commands

### 1. **Single Line Insert**
```
I 000100
```

### 2. **Multiple Line Insert**
```
II 000100 5
I 000100 5
```

### 3. **Insert with Content**
```
I 000100 'CONTENT TO INSERT'
```

### 4. **Insert from Another Dataset**
```
I 000100 'USER.DATA.SOURCE'
```

## Change Commands

### 1. **Change Line**
```
X 000100
```
- **Purpose**: Mark line for change
- **Action**: Highlights line for modification
- **Use Case**: Mark lines for editing

### 2. **Change Range**
```
XX 000100 000200
```
- **Purpose**: Mark range for change
- **Action**: Highlights range for modification
- **Use Case**: Mark blocks for editing

### 3. **Change with Confirmation**
```
X 000100 CONFIRM
XX 000100 000200 CONFIRM
```

## Search Commands

### 1. **Find Line**
```
F 000100
```
- **Purpose**: Find and display line
- **Action**: Locates and displays the line
- **Use Case**: Locate specific content

### 2. **Find Range**
```
FF 000100 000200
```
- **Purpose**: Find and display range
- **Action**: Locates and displays the range
- **Use Case**: Locate blocks of content

### 3. **Find with Pattern**
```
F 000100 'PATTERN'
FF 000100 000200 'PATTERN'
```

## Primary Commands

### 1. **FIND Command**
```
FIND 'search-string'
FIND 'search-string' ALL
FIND 'search-string' NEXT
```

### 2. **CHANGE Command**
```
CHANGE 'old-string' 'new-string'
CHANGE 'old-string' 'new-string' ALL
CHANGE 'old-string' 'new-string' NEXT
```

### 3. **COPY Command**
```
COPY 'source-dataset' 'target-dataset'
COPY 'source-dataset' 'target-dataset' REPLACE
```

### 4. **MOVE Command**
```
MOVE 'source-dataset' 'target-dataset'
MOVE 'source-dataset' 'target-dataset' REPLACE
```

### 5. **DELETE Command**
```
DELETE 'dataset-name'
DELETE 'dataset-name' PURGE
```

## Best Practices

### 1. **Command Efficiency**
- Use single-character commands when possible
- Combine commands for complex operations
- Use range commands for multiple lines
- Learn keyboard shortcuts

### 2. **Error Prevention**
- Verify line numbers before executing
- Use CONFIRM for destructive operations
- Save work frequently
- Use undo when available

### 3. **Organization**
- Use consistent line numbering
- Group related commands together
- Document complex operations
- Follow coding standards

### 4. **Performance**
- Use appropriate commands for the task
- Avoid unnecessary operations
- Use range commands for multiple lines
- Optimize for the specific task

## Quick Reference

### Single Line Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `C` | Copy line | `C 000100` |
| `D` | Delete line | `D 000200` |
| `I` | Insert line | `I 000300` |
| `M` | Move line | `M 000400 000500` |
| `R` | Repeat line | `R 000600` |
| `A` | After | `A 000700` |
| `B` | Before | `B 000800` |
| `E` | Exclude | `E 000900` |
| `F` | Find | `F 001000` |
| `G` | Get | `G 001100` |
| `X` | Change | `X 001200` |

### Range Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `CC` | Copy range | `CC 000100 000200` |
| `DD` | Delete range | `DD 000300 000400` |
| `MM` | Move range | `MM 000500 000600 000700` |
| `RR` | Repeat range | `RR 000800 000900` |
| `XX` | Change range | `XX 001000 001100` |
| `FF` | Find range | `FF 001200 001300` |

### Primary Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `FIND` | Search text | `FIND 'search-string'` |
| `CHANGE` | Replace text | `CHANGE 'old' 'new'` |
| `COPY` | Copy dataset | `COPY 'SRC' 'TGT'` |
| `MOVE` | Move dataset | `MOVE 'SRC' 'TGT'` |
| `DELETE` | Delete dataset | `DELETE 'dataset'` |

### Common Combinations:
| Task | Commands | Example |
|------|----------|---------|
| Copy block | `CC` | `CC 000100 000200` |
| Move block | `MM` | `MM 000100 000200 000300` |
| Delete block | `DD` | `DD 000100 000200` |
| Insert multiple | `II` | `II 000100 5` |
| Change block | `XX` | `XX 000100 000200` |

---

*Next: [DSLIST Utility Primary Commands](06-dslist-utility.md)*
