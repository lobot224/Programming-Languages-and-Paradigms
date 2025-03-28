## **Prefix Calculator**

### **Description**
The Prefix Calculator is a program designed to evaluate mathematical expressions written in prefix notation. It supports interactive and batch modes, keeps a history of results for reuse, and gracefully handles errors during evaluation.

### **Features**
- **Supported Operations:**
  - **Addition (+)**: Adds two expressions.
  - **Multiplication (*)**: Multiplies two expressions.
  - **Division (/)**: Divides one expression by another (division by zero is handled as an error).
  - **Negation (-)**: Negates the value of an expression.
- **History Usage:** Reuse previous results in new calculations using the `$n` syntax (e.g., `$1` for the first result in history)
- **Error Handling:** Provides clear messages for issues such as invalid input, division by zero, or referencing nonexistent history values.
- **Modes:** Interactive and Batch.

---

## **How to Use**

### **Prefix Notation Overview**
In prefix notation, the operator appears **before** its operands, and expressions are read from left to right. This format allows the calculation to be performed without parentheses or ambiguity. The syntax supports:

1. **Binary Operators**:
   - `+`: Adds two expressions.
   - `*`: Multiplies two expressions.
   - `/`: Divides the first expression by the second (division by zero results in an error).

2. **Unary Operators**:
   - `-`: Negates a single expression.

3. **History References**:
   - `$n`: Refers to the result of a previous calculation. `n` must be a valid integer, referencing the history by ID.

4. **Values**:
   - Numbers (e.g., `5`, `3.14`) are valid values.

---

### **Expression Syntax**
#### **General Format**
1. **Binary Operators**:
   ```plaintext
   (+ expr1 expr2)
   (* expr1 expr2)
   (/ expr1 expr2)
   ```
   Each operator is followed by its two operands (which can be numbers, history references, or nested expressions).

2. **Unary Operator**:
   ```plaintext
   (- expr)
   ```
   Negates the value of `expr`.

3. **Nested Functions**:
   Nested expressions are processed from left to right. For example:
   ```plaintext
   (+ (* 2 3) (/ 10 2))
   ```
   Here:
   - `(* 2 3)` evaluates to `6`.
   - `(/ 10 2)` evaluates to `5`.
   - The final result is `6 + 5 = 11`.

4. **History References**:
   ```plaintext
   (+ $1 $2)
   ```
   Retrieves the first (`$1`) and second (`$2`) results from history to evaluate.

---

### **Examples**

#### **Basic Expressions**
- **Addition**: `(+ 2 3)`  
  Evaluates to `5`.

- **Multiplication**: `(* 4 2)`  
  Evaluates to `8`.

- **Unary Negation**: `(- 5)`  
  Evaluates to `-5`.

#### **Nested Functions**
- Example: `(+ (* 2 3) (/ 15 5))`  
  - Step 1: `(* 2 3)` → `6`.
  - Step 2: `(/ 15 5)` → `3`.
  - Step 3: `(+ 6 3)` → `9`.

#### **History References**
- Assume history contains:
  - `$1 = 5`  
  - `$2 = 10`

- Expression: `(* $1 $2)`  
  - Retrieves `$1` (`5`) and `$2` (`10`).
  - Calculates `5 * 10` → `50`.

#### **Complex Nested Example**
- Expression: `(+ (* (- (/ 16 4) 2) 5) (/ (* 6 3) (+ 1 2)))`  
  Step-by-step breakdown:
  - `(/ 16 4)` → `4`.
  - `(- 4 2)` → `2`.
  - `(* 2 5)` → `10`.
  - `(* 6 3)` → `18`.
  - `(+ 1 2)` → `3`.
  - `(/ 18 3)` → `6`.
  - `(+ 10 6)` → `16`.

---

### **Error Scenarios**
1. **Division by Zero**:
   - `(/ 5 0)` → `Error: Division by zero is undefined`.

2. **Invalid Tokens**:
   - `(+ 5 invalid)` → `Invalid Expression`.

3. **History Reference Out of Range**:
   - `$100` (when there are fewer than 100 results) → `Invalid history reference $100: Out of range`.

4. **Improper Syntax**:
   - `(+ 1 2 3)` → `Invalid Expression` (extra token `3` after evaluation).

#### **1. Interactive Mode**
1. Run the program without any arguments:
   ```bash
   racket "Prefix Calc.rkt"
   ```
2. You’ll see:
   ```
   Prefix Calculator (Interactive Mode)
   > 
   ```
   Enter expressions in prefix notation (e.g., `(+ 1 2)`).

3. Use history by referencing results via `$` (e.g., `$1` for the first result in history).

4. Type `quit` to exit.

#### **2. Batch Mode**
1. Create a file (e.g., `test_cases.txt`) containing expressions, one per line.
2. Run the program with the `-b` flag and pipe the input file:
   ```bash
   racket "Prefix Calc.rkt" --batch < test_cases.txt
   ```
3. Results (or error messages) will be displayed in order for each line of input.

---

### **Examples**

#### **Interactive Mode**
```plaintext
Prefix Calculator (Interactive Mode)
> (+ 2 3)
5
> (/ 15 3)
5
> (* $1 2)
10
> quit
Exiting... Goodbye!
```

#### **Batch Mode**
For a file `test_cases.txt`:
```plaintext
(+ 2 3)
(/ 15 3)
(* $1 2)
(/ 10 0)
```

Run:
```bash
racket "Prefix Calc.rkt" --batch < test_cases.txt
```

Output:
```plaintext
5
5
10
Error: Division by zero is undefined
```

---

### **Error Handling**

- **Division by Zero**: `Error: Division by zero is undefined`
- **Invalid Input**: `Invalid Expression`
- **History Reference Errors**:
  - Nonexistent reference: `Invalid history reference $n: Out of range`
  - Improper formatting: `Invalid history reference format`

---

### **Project Requirements Covered**
- Supports prefix notation evaluation.
- Maintains and references calculation history.
- Handles errors with clear messages prefixed by `Error:`.
- Operates seamlessly in both interactive and batch modes.

---

### **Dependencies**
- Racket programming language (install [here](https://racket-lang.org/)).

---
