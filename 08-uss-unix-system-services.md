# USS (Unix System Services)

## Table of Contents
- [What is USS?](#what-is-uss)
- [USS Architecture](#uss-architecture)
- [USS Shell Environment](#uss-shell-environment)
- [File System Structure](#file-system-structure)
- [Basic USS Commands](#basic-uss-commands)
- [File Operations](#file-operations)
- [Directory Operations](#directory-operations)
- [Process Management](#process-management)
- [Text Processing](#text-processing)
- [USS and MVS Integration](#uss-and-mvs-integration)
- [Security in USS](#security-in-uss)
- [USS Examples](#uss-examples)
- [Best Practices](#best-practices)

## What is USS?

**USS (Unix System Services)** is a component of z/OS that provides:
- **Unix-like environment** on mainframe systems
- **POSIX compliance** for Unix applications
- **Shell interface** for command-line operations
- **File system integration** with MVS datasets
- **Network services** and TCP/IP support
- **Application porting** from Unix systems

### Key Features:
- **POSIX-compliant** Unix environment
- **Shell scripting** capabilities
- **Hierarchical file system** (HFS/zFS)
- **Integration** with traditional MVS
- **Network services** and protocols
- **Multi-user** environment

## USS Architecture

### Main Components:
```
┌─────────────────────────────────────┐
│            z/OS System              │
├─────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  │
│  │     MVS     │  │     USS     │  │
│  │ Traditional │  │ Unix System │  │
│  │   System    │  │  Services   │  │
│  └─────────────┘  └─────────────┘  │
│  ┌─────────────┐  ┌─────────────┐  │
│  │    VSAM     │  │   HFS/zFS   │  │
│  │  Datasets   │  │ File System │  │
│  └─────────────┘  └─────────────┘  │
│  ┌─────────────────────────────────┐ │
│  │        TCP/IP Stack             │ │
│  └─────────────────────────────────┘ │
└─────────────────────────────────────┘
```

### Integration Points:
- **File System**: HFS/zFS integrated with MVS datasets
- **Security**: RACF integration for access control
- **Networking**: TCP/IP stack shared with MVS
- **Process Management**: Unix processes and MVS tasks

## USS Shell Environment

### 1. **Accessing USS Shell**
```bash
# From TSO/ISPF
OMVS

# From TSO command line
TSO OMVS

# From ISPF Option 6
OMVS
```

### 2. **Shell Types**
```bash
# Available shells
/bin/sh      # Bourne shell (default)
/bin/ksh     # Korn shell
/bin/bash    # Bash shell (if installed)
/bin/csh     # C shell (if available)
```

### 3. **Environment Variables**
```bash
# Display environment
env
printenv

# Common environment variables
echo $HOME        # Home directory
echo $PATH        # Command search path
echo $USER        # Current user
echo $SHELL       # Current shell
echo $PWD         # Current directory
```

### 4. **Shell Configuration**
```bash
# Profile files
~/.profile       # Login shell profile
~/.bashrc        # Bash configuration
~/.kshrc         # Korn shell configuration
```

## File System Structure

### 1. **Root File System**
```
/
├── bin/           # System binaries
├── dev/           # Device files
├── etc/           # System configuration
├── home/          # User home directories
├── lib/           # System libraries
├── tmp/           # Temporary files
├── usr/           # User programs
├── var/           # Variable data
└── u/             # User directories (z/OS specific)
```

### 2. **z/OS Specific Directories**
```
/usr/lpp/          # Licensed program products
/etc/zos/          # z/OS specific configuration
/usr/include/      # System header files
/usr/lib/          # System libraries
/var/log/          # System log files
```

### 3. **File Types**
```bash
# Regular files
-rw-r--r--  file.txt

# Directories
drwxr-xr-x  directory/

# Symbolic links
lrwxrwxrwx  link -> target

# Device files
crw-rw-rw-  /dev/null
```

## Basic USS Commands

### 1. **Navigation Commands**
```bash
# Change directory
cd /home/user
cd ~              # Home directory
cd ..             # Parent directory
cd -              # Previous directory

# List directory contents
ls                # List files
ls -l             # Long format
ls -la            # Include hidden files
ls -lt            # Sort by time
ls -lh            # Human readable sizes

# Print working directory
pwd
```

### 2. **File Information Commands**
```bash
# File information
file filename.txt
stat filename.txt
ls -l filename.txt

# Disk usage
df                # File system usage
df -h             # Human readable
du                # Directory usage
du -h             # Human readable
du -s             # Summary only
```

### 3. **Help Commands**
```bash
# Manual pages
man ls            # Manual for ls command
man -k keyword    # Search manual pages

# Command information
which ls          # Location of command
type ls           # Command type
```

## File Operations

### 1. **Creating Files**
```bash
# Create empty file
touch filename.txt

# Create file with content
echo "Hello World" > file.txt
cat > file.txt << EOF
Line 1
Line 2
EOF
```

### 2. **Viewing Files**
```bash
# Display entire file
cat filename.txt

# Display with line numbers
cat -n filename.txt

# Page through file
more filename.txt
less filename.txt

# Display first/last lines
head filename.txt          # First 10 lines
head -n 20 filename.txt    # First 20 lines
tail filename.txt          # Last 10 lines
tail -n 20 filename.txt    # Last 20 lines
tail -f filename.txt       # Follow file changes
```

### 3. **Copying and Moving Files**
```bash
# Copy files
cp source.txt destination.txt
cp source.txt /path/to/destination/
cp -r directory/ /path/to/destination/

# Move/rename files
mv oldname.txt newname.txt
mv file.txt /path/to/destination/
mv directory/ /path/to/destination/
```

### 4. **Deleting Files**
```bash
# Remove files
rm filename.txt
rm -f filename.txt         # Force removal
rm -i filename.txt         # Interactive removal

# Remove directories
rmdir empty_directory/
rm -r directory/           # Recursive removal
rm -rf directory/          # Force recursive removal
```

## Directory Operations

### 1. **Creating Directories**
```bash
# Create directory
mkdir directory_name
mkdir -p path/to/directory  # Create parent directories
mkdir -m 755 directory_name # Set permissions
```

### 2. **Directory Information**
```bash
# List directory contents
ls directory_name/
ls -la directory_name/

# Directory size
du -sh directory_name/

# Directory permissions
ls -ld directory_name/
```

### 3. **Finding Files and Directories**
```bash
# Find files
find /path -name "*.txt"
find /path -type f -name "pattern"
find /path -type d -name "pattern"
find /path -mtime -7          # Modified in last 7 days
find /path -size +1M          # Files larger than 1MB

# Locate files
locate filename
whereis command
```

## Process Management

### 1. **Process Information**
```bash
# List processes
ps                # Current user processes
ps -ef            # All processes
ps aux            # BSD format
ps -u username    # User specific processes

# Process tree
ps -ef --forest
pstree
```

### 2. **Process Control**
```bash
# Background processes
command &         # Run in background
jobs             # List background jobs
fg %1            # Bring job to foreground
bg %1            # Send job to background

# Process termination
kill pid         # Terminate process
kill -9 pid      # Force termination
killall process_name
```

### 3. **System Monitoring**
```bash
# System activity
top              # Real-time process monitor
iostat           # I/O statistics
vmstat           # Virtual memory statistics
netstat          # Network statistics
```

## Text Processing

### 1. **Text Search**
```bash
# Search in files
grep "pattern" filename.txt
grep -i "pattern" filename.txt    # Case insensitive
grep -r "pattern" directory/      # Recursive search
grep -n "pattern" filename.txt    # Show line numbers
grep -v "pattern" filename.txt    # Invert match
```

### 2. **Text Manipulation**
```bash
# Sort text
sort filename.txt
sort -r filename.txt              # Reverse sort
sort -n filename.txt              # Numeric sort
sort -k2 filename.txt             # Sort by column 2

# Remove duplicates
uniq filename.txt
sort filename.txt | uniq

# Count lines, words, characters
wc filename.txt
wc -l filename.txt                # Count lines only
wc -w filename.txt                # Count words only
```

### 3. **Text Editors**
```bash
# vi/vim editor
vi filename.txt
vim filename.txt

# Basic vi commands
i                # Insert mode
:w               # Save file
:q               # Quit
:wq              # Save and quit
:q!              # Quit without saving
```

## USS and MVS Integration

### 1. **Dataset Access from USS**
```bash
# List MVS datasets
ls "//'USER.DATA.SET'"
ls "//'SYS1.PROCLIB'"

# Copy from MVS to USS
cp "//'USER.DATA.INPUT'" /tmp/input.txt

# Copy from USS to MVS
cp /tmp/output.txt "//'USER.DATA.OUTPUT'"
```

### 2. **TSO Commands from USS**
```bash
# Execute TSO commands
tsocmd "LISTC 'USER.DATA.*'"
tsocmd "LISTDS 'USER.DATA.INPUT'"
tsocmd "DELETE 'USER.TEMP.DATA'"

# Submit JCL from USS
submit "//'USER.JCL.BATCHJOB'"
```

### 3. **REXX from USS**
```bash
# Execute REXX programs
rexx /path/to/rexx_program.rex
oorexx /path/to/oo_rexx_program.rex
```

## Security in USS

### 1. **File Permissions**
```bash
# Permission format: rwxrwxrwx (owner,group,other)
# r=read(4), w=write(2), x=execute(1)

# Change permissions
chmod 755 filename.txt        # rwxr-xr-x
chmod u+x filename.txt        # Add execute for owner
chmod go-w filename.txt       # Remove write for group/other
chmod a+r filename.txt        # Add read for all
```

### 2. **Ownership**
```bash
# Change ownership
chown user:group filename.txt
chown user filename.txt
chgrp group filename.txt
```

### 3. **Access Control**
```bash
# Check user identity
whoami
id
groups

# Switch user (if authorized)
su - username
sudo command
```

## USS Examples

### 1. **File Processing Script**
```bash
#!/bin/sh
# Process daily reports

INPUT_DIR="/u/reports/input"
OUTPUT_DIR="/u/reports/output"
DATE=$(date +%Y%m%d)

# Create output directory
mkdir -p $OUTPUT_DIR

# Process each file
for file in $INPUT_DIR/*.txt; do
    if [ -f "$file" ]; then
        echo "Processing $file"
        # Sort and remove duplicates
        sort "$file" | uniq > "$OUTPUT_DIR/$(basename $file .txt)_$DATE.txt"
        echo "Completed $(basename $file)"
    fi
done

echo "All files processed"
```

### 2. **System Monitoring Script**
```bash
#!/bin/sh
# System monitoring script

LOG_FILE="/var/log/system_monitor.log"
DATE=$(date)

echo "=== System Monitor Report - $DATE ===" >> $LOG_FILE

# Disk usage
echo "Disk Usage:" >> $LOG_FILE
df -h >> $LOG_FILE

# Memory usage
echo "Memory Usage:" >> $LOG_FILE
ps aux --sort=-%mem | head -10 >> $LOG_FILE

# Process count
echo "Process Count:" >> $LOG_FILE
ps aux | wc -l >> $LOG_FILE

echo "Report completed" >> $LOG_FILE
```

### 3. **Data Transfer Script**
```bash
#!/bin/sh
# Transfer data between MVS and USS

MVS_DATASET="USER.DATA.INPUT"
USS_FILE="/tmp/processing_data.txt"
OUTPUT_DATASET="USER.DATA.OUTPUT"

# Copy from MVS to USS
echo "Copying from MVS dataset to USS file"
cp "//'$MVS_DATASET'" "$USS_FILE"

# Process the data
echo "Processing data"
sort "$USS_FILE" | uniq > "${USS_FILE}.processed"

# Copy back to MVS
echo "Copying processed data back to MVS"
cp "${USS_FILE}.processed" "//'$OUTPUT_DATASET'"

# Cleanup
rm "$USS_FILE" "${USS_FILE}.processed"

echo "Data transfer completed"
```

## Best Practices

### 1. **File Management**
- Use meaningful file and directory names
- Organize files in logical directory structures
- Set appropriate permissions for security
- Regular cleanup of temporary files

### 2. **Shell Scripting**
- Include proper error handling
- Use meaningful variable names
- Add comments for complex logic
- Test scripts thoroughly

### 3. **Security**
- Follow principle of least privilege
- Regularly review file permissions
- Use secure file transfer methods
- Monitor system access logs

### 4. **Performance**
- Use efficient commands and pipelines
- Avoid unnecessary file operations
- Monitor system resources
- Optimize scripts for large datasets

## Quick Reference

### Navigation Commands:
| Command | Purpose | Example |
|---------|---------|---------|
| `cd` | Change directory | `cd /home/user` |
| `ls` | List files | `ls -la` |
| `pwd` | Print working directory | `pwd` |
| `find` | Find files | `find /path -name "*.txt"` |

### File Operations:
| Command | Purpose | Example |
|---------|---------|---------|
| `cp` | Copy files | `cp source dest` |
| `mv` | Move/rename | `mv old new` |
| `rm` | Remove files | `rm filename` |
| `chmod` | Change permissions | `chmod 755 file` |

### Text Processing:
| Command | Purpose | Example |
|---------|---------|---------|
| `cat` | Display file | `cat filename` |
| `grep` | Search text | `grep "pattern" file` |
| `sort` | Sort lines | `sort filename` |
| `wc` | Count lines/words | `wc -l filename` |

### Process Management:
| Command | Purpose | Example |
|---------|---------|---------|
| `ps` | List processes | `ps -ef` |
| `kill` | Terminate process | `kill pid` |
| `jobs` | List jobs | `jobs` |
| `top` | System monitor | `top` |

### File Permissions:
| Permission | Numeric | Description |
|------------|---------|-------------|
| `rwx` | 7 | Read, write, execute |
| `rw-` | 6 | Read, write |
| `r-x` | 5 | Read, execute |
| `r--` | 4 | Read only |
| `---` | 0 | No permissions |

---

*Next: [JCL (Job Control Language)](09-jcl-comprehensive.md)*
