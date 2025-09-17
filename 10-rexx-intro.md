# Introduction to REXX

## Table of Contents
- [What is REXX?](#what-is-rexx)
- [REXX Features](#rexx-features)
- [REXX Syntax](#rexx-syntax)
- [Variables and Data Types](#variables-and-data-types)
- [Control Structures](#control-structures)
- [Functions](#functions)
- [REXX Examples](#rexx-examples)
- [REXX in Mainframe](#rexx-in-mainframe)
- [Best Practices](#best-practices)

## What is REXX?

**REXX (Restructured Extended Executor)** is a high-level programming language designed for:
- **System administration** and automation
- **Text processing** and manipulation
- **Data analysis** and reporting
- **Application development** and scripting
- **Mainframe operations** and utilities

### Key Features:
- **Interpreted language** - no compilation required
- **Simple syntax** - easy to learn and use
- **Powerful string manipulation** capabilities
- **Built-in functions** for common operations
- **Cross-platform** compatibility
- **Integration** with mainframe systems

## REXX Features

### 1. **Interpreted Language**
- No compilation step required
- Immediate execution
- Easy debugging and testing
- Rapid development cycle

### 2. **Simple Syntax**
- English-like commands
- Minimal punctuation
- Case-insensitive
- Free-format code

### 3. **String Processing**
- Powerful string manipulation
- Pattern matching
- Text parsing
- Data extraction

### 4. **Built-in Functions**
- Mathematical functions
- String functions
- Date/time functions
- System functions

## REXX Syntax

### Basic Structure:
```rexx
/* REXX Program */
SAY 'Hello, World!'
EXIT
```

### Comments:
```rexx
/* This is a comment */
/* Multi-line
   comment */
```

### Case Sensitivity:
```rexx
/* REXX is case-insensitive */
say 'Hello'
SAY 'Hello'
Say 'Hello'
```

## Variables and Data Types

### 1. **Variable Declaration**
```rexx
/* Variables are declared by assignment */
name = 'John'
age = 25
salary = 50000.50
```

### 2. **Data Types**
```rexx
/* REXX has only one data type: string */
string_var = 'Hello World'
numeric_var = 123
decimal_var = 123.45
boolean_var = 1  /* 1 = true, 0 = false */
```

### 3. **Variable Names**
```rexx
/* Variable naming rules */
valid_name = 'John'
valid_name2 = 'Mary'
valid_name_3 = 'Bob'
/* Invalid: 2invalid, name-with-dash */
```

## Control Structures

### 1. **IF/THEN/ELSE**
```rexx
/* Simple IF statement */
IF age >= 18 THEN
    SAY 'Adult'
ELSE
    SAY 'Minor'

/* Nested IF */
IF age >= 65 THEN
    SAY 'Senior'
ELSE IF age >= 18 THEN
    SAY 'Adult'
ELSE
    SAY 'Minor'
```

### 2. **DO Loops**
```rexx
/* DO loop with counter */
DO i = 1 TO 10
    SAY 'Count:' i
END

/* DO loop with step */
DO i = 1 TO 10 BY 2
    SAY 'Count:' i
END

/* DO WHILE loop */
i = 1
DO WHILE i <= 10
    SAY 'Count:' i
    i = i + 1
END

/* DO UNTIL loop */
i = 1
DO UNTIL i > 10
    SAY 'Count:' i
    i = i + 1
END
```

### 3. **SELECT/CASE**
```rexx
/* SELECT statement */
SELECT
    WHEN age < 18 THEN
        SAY 'Minor'
    WHEN age < 65 THEN
        SAY 'Adult'
    OTHERWISE
        SAY 'Senior'
END
```

## Functions

### 1. **String Functions**
```rexx
/* String length */
length = LENGTH('Hello World')  /* Returns 11 */

/* String concatenation */
result = 'Hello' || ' ' || 'World'  /* Returns 'Hello World' */

/* String extraction */
substring = SUBSTR('Hello World', 1, 5)  /* Returns 'Hello' */

/* String search */
position = POS('World', 'Hello World')  /* Returns 7 */

/* String replacement */
new_string = TRANSLATE('Hello World', 'H', 'h')  /* Returns 'Hello World' */
```

### 2. **Mathematical Functions**
```rexx
/* Basic arithmetic */
sum = 10 + 20
difference = 20 - 10
product = 10 * 20
quotient = 20 / 10
remainder = 20 // 10

/* Mathematical functions */
absolute = ABS(-10)  /* Returns 10 */
maximum = MAX(10, 20, 30)  /* Returns 30 */
minimum = MIN(10, 20, 30)  /* Returns 10 */
```

### 3. **Date/Time Functions**
```rexx
/* Current date */
current_date = DATE()  /* Returns current date */

/* Current time */
current_time = TIME()  /* Returns current time */

/* Date formatting */
formatted_date = DATE('S')  /* Returns date in standard format */
```

## REXX Examples

### 1. **Hello World Program**
```rexx
/* Hello World in REXX */
SAY 'Hello, World!'
EXIT
```

### 2. **Simple Calculator**
```rexx
/* Simple Calculator */
SAY 'Enter first number:'
PULL num1
SAY 'Enter second number:'
PULL num2
SAY 'Enter operation (+, -, *, /):'
PULL operation

SELECT
    WHEN operation = '+' THEN
        result = num1 + num2
    WHEN operation = '-' THEN
        result = num1 - num2
    WHEN operation = '*' THEN
        result = num1 * num2
    WHEN operation = '/' THEN
        result = num1 / num2
    OTHERWISE
        SAY 'Invalid operation'
        EXIT
END

SAY 'Result:' result
EXIT
```

### 3. **File Processing**
```rexx
/* File Processing Example */
filename = 'USER.DATA.INPUT'
'ALLOCATE FILE(INPUT) DATASET('filename') SHR'

DO WHILE LINES(INPUT) > 0
    line = LINEIN(INPUT)
    SAY 'Processing:' line
    /* Process the line */
END

'FREE FILE(INPUT)'
EXIT
```

### 4. **Data Validation**
```rexx
/* Data Validation Example */
SAY 'Enter your age:'
PULL age

/* Validate age */
IF DATATYPE(age, 'N') = 0 THEN DO
    SAY 'Age must be a number'
    EXIT
END

IF age < 0 THEN DO
    SAY 'Age cannot be negative'
    EXIT
END

IF age > 150 THEN DO
    SAY 'Age seems too high'
    EXIT
END

SAY 'Valid age:' age
EXIT
```

## REXX in Mainframe

### 1. **TSO REXX**
```rexx
/* TSO REXX Example */
'ALLOCATE FILE(INPUT) DATASET(USER.DATA.INPUT) SHR'
'ALLOCATE FILE(OUTPUT) DATASET(USER.DATA.OUTPUT) OLD'

DO WHILE LINES(INPUT) > 0
    line = LINEIN(INPUT)
    /* Process line */
    LINEOUT(OUTPUT, line)
END

'FREE FILE(INPUT)'
'FREE FILE(OUTPUT)'
EXIT
```

### 2. **ISPF REXX**
```rexx
/* ISPF REXX Example */
ADDRESS ISREDIT
'MACRO'
'LINE_AFTER 1 = "This is a new line"'
'SAVE'
EXIT
```

### 3. **JCL REXX**
```rexx
/* JCL REXX Example */
'SUBMIT USER.JCL.BATCHJOB'
'STATUS JOB'
EXIT
```

## Best Practices

### 1. **Code Structure**
- Use meaningful variable names
- Include proper comments
- Follow consistent indentation
- Use appropriate control structures

### 2. **Error Handling**
- Validate input data
- Check return codes
- Handle errors gracefully
- Provide meaningful error messages

### 3. **Performance**
- Use efficient algorithms
- Minimize I/O operations
- Avoid unnecessary loops
- Optimize string operations

### 4. **Maintenance**
- Document complex logic
- Use modular design
- Test thoroughly
- Follow coding standards

## Quick Reference

### Control Structures:
| Structure | Syntax | Example |
|-----------|--------|---------|
| IF | `IF condition THEN statement` | `IF age >= 18 THEN SAY 'Adult'` |
| DO | `DO i = 1 TO 10 ... END` | `DO i = 1 TO 10; SAY i; END` |
| SELECT | `SELECT ... WHEN ... END` | `SELECT; WHEN age < 18 THEN SAY 'Minor'; END` |

### String Functions:
| Function | Purpose | Example |
|----------|---------|---------|
| `LENGTH` | String length | `LENGTH('Hello')` |
| `SUBSTR` | Extract substring | `SUBSTR('Hello', 1, 3)` |
| `POS` | Find position | `POS('lo', 'Hello')` |
| `TRANSLATE` | Replace characters | `TRANSLATE('Hello', 'H', 'h')` |

### Mathematical Functions:
| Function | Purpose | Example |
|----------|---------|---------|
| `ABS` | Absolute value | `ABS(-10)` |
| `MAX` | Maximum value | `MAX(10, 20, 30)` |
| `MIN` | Minimum value | `MIN(10, 20, 30)` |

---

*Next: [VSAM (Virtual Storage Access Method)](10-vsam-comprehensive.md)*
