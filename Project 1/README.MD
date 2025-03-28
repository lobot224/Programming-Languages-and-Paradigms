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
- Expre  - `(/ 16 4)` → `4`.
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

--Ah, right, Logan—got it! Since it’s based on our conversation, I’ll weave the necessary details into the revised `README.md`. Here’s a polished version of it that aligns perfectly with your project:

---

### **Prefix Calculator**

Welcome to the Prefix Calculator project! This program is a simple yet robust implementation of a prefix (Polish notation) calculator, capable of evaluating mathematical expressions using basic operators (`+`, `-`, `*`, `/`) and managing a history of results. Designed in **Racket**, the calculator supports both interactive and batch mode operations, ensuring flexibility and ease of use.

---

### **Features**
1. **Interactive Mode**:
   - Allows users to input prefix expressions dynamically.
   - Tracks and prints the results of calculations with unique history IDs.
   - Supports history references (`$n`) for previously calculated results.

2. **Batch Mode**:
   - Processes a text file containing prefix expressions line by line.
   - Handles errors gracefully and continues processing subsequent lines.
   - Skips blank lines automatically and omits invalid expressions.

3. **Mathematical Operators**:
   - Basic arithmetic operations: addition (`+`), subtraction (`-`), multiplication (`*`), and division (`/`).
   - Division includes safeguards against division by zero errors.
   - Handles both unary and multi-operand operations.

4. **Floating Point Precision**:
   - Results are calculated with floating-point precision, ensuring accuracy even for complex calculations.

5. **Error Handling**:
   - Provides descriptive error messages for invalid input, division by zero, and out-of-range history references.
   - Continues processing without interruption in batch mode.

---

### **How to Use**
#### **Interactive Mode**
To launch the program in interactive mode:
1. Open a terminal or command prompt.
2. Run the program using the command:
   ```
   racket prefix_calc.rkt
   ```
3. Enter prefix expressions interactively, one at a time.
4. Type `quit` to exit the program.

#### **Batch Mode**
To process a text file of expressions:
1. Prepare a `.txt` file with each prefix expression on a new line.
2. Run the program in batch mode using:
   ```
   racket prefix_calc.rkt --batch < test_cases.txt
   ```
3. The program evaluates each expression and outputs results with history IDs. Errors are reported but processing continues.

---

### **Input Format**
- Prefix notation: Operators precede their operands.
  - Example: `(+ 1 2)` evaluates to `3.0`.
- History references (`$n`): Refer to results of previous calculations.
  - Example: `(* $1 2)` doubles the result of the first expression.
- Nested expressions are supported:
  - Example: `(+ (* 2 3) (- 10 4))`.

---

### **Example Output**
For the input file `test_cases.txt`:
```
(+ 1 2)
(- 5 3)
(* $1 2)
(/ $1 0)   ; Error
(+ invalid)
```

The output would be:
```
1: 3.0
2: 2.0
3: 6.0
Error: Division by zero is undefined
Error: Invalid Expression
Finished Processing All Lines
```

---

### **Technical Notes**
- **No Rounding of Results**: Outputs like `6.300000000000001` reflect floating-point precision and are intentionally unrounded for accuracy.
- **Error Resilience**: Errors don't halt execution in batch mode; the program skips invalid expressions gracefully.
- **Blank Lines**: Automatically ignored during processing.

---

Feel free to update or refine this `README.md` further—it’s your project, after all! Let me know if you’d like additional tweaks or sections added. 🚀

