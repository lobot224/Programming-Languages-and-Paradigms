#PROJECT 1 - Prefix Calculator

## March 26, 2025 

-- **Goal:** Detail the progress of the report since its initial creation about a week ago. 
-- **Progress:** Up to this week I have been able to acomplish the following: 
                1. Implemented all of the arithmitc operators within the prefix calculator 
                2. Implemented the history feature of the calculator 
                3. Implemented the nesting feature of the calculator 
                4. Implemented interactive mode functionality 
                5. Implemented bath mode functionality 
                6. Began testing cases for both modes
-- **Plan:** To catch everything up to speed in this log and finish last rounds of testing. 
--**Problems:** As mentioned, I had forgotten to document the changes to the code, its initial creation and updates. 
I will describe some of the many issues encountered while working with Racket: 

1. Nested Operations Handling
Issue: The calculator struggled with parsing and evaluating deeply nested operations.

Resolution: Improved the recursive process function to handle all levels of nesting and correctly evaluate sub-expressions before applying operators.

2. Unary Operator Handling
Issue: Unary operators like - (e.g., (- 5) or nested cases like (- (* 2 3))) caused errors or unexpected results.

Resolution: Explicitly adjusted the process function to distinguish between single-argument and multi-argument lists for the - operator.

3. Floating-Point Precision
Issue: Floating-point arithmetic introduced minor inaccuracies (e.g., results like 3.3333333).

Resolution: Implemented a round-number function to round all results to two decimal places.

4. Division by Zero
Issue: Division by zero returned +inf.0 or other unexpected values instead of raising an error.

Resolution: Enhanced the / operator logic in the process function to explicitly check for zero in the denominator and raise a clear error message: 
"Error: Division by zero is undefined".

5. Invalid Input
Issue: The program failed to handle invalid expressions or operators gracefully (e.g., "invalid" or malformed operations).

Resolution: Strengthened error handling for invalid operators or arguments, providing a general error message: "Invalid operator or arguments".

6. History Reference Resolution
Issue: History references ($n) caused issues when referring to out-of-range or malformed indices, leading to crashes or incorrect results.

Resolution:

Added bounds checking in the get-from-history function to handle out-of-range indices with a descriptive error: 
"Invalid history reference $n: Out of range".

Improved the replace-history-ref function to check for valid $n syntax and raise a "Invalid history reference format" error for malformed references.

7. Batch Mode Parsing
Issue: Lines starting with # or empty lines in the input file caused parsing errors.

Resolution: Updated the batch-mode function to ignore comments (#) and skip over blank lines.

8. #<eof> Errors
Issue: The program processed #<eof> (end-of-file) as an invalid expression, resulting in errors.

Resolution: Enhanced batch-mode to gracefully terminate processing when reaching the end of the file.

9. Debugging Outputs
Issue: Debugging outputs were too verbose, cluttering the results.

Resolution: Created a clean, production-ready version of the program with debugging outputs removed.

10. Quitting Functionality
Issue: The program lacked a way to exit gracefully when the user inputs "quit" in either interactive or batch mode.

Resolution: Added checks for the "quit" keyword in both modes to stop execution and exit cleanly.

11. Enhanced Error Messaging
Issue: Some error messages were generic and lacked clarity.

Resolution: Improved error messages across the board to be more descriptive and provide better feedback (e.g., distinguishing between out-of-range 
references and invalid syntax).

This concludes the problems report... For now. 

**Author's Notes:** 
Programming in Racket is certainly somehting different than what I had expected. But so far I'm really intrigued by what it has to offer.
The prefix calculator has come a long way since its basic 'mode' function that we were given before and I continue to grow fascinated with how its
able to operate. Even if it makes me rip my hair out... 

I hope to add more as time goes on even after this project is over, maybe something like remainder results would be fun to work with, but we'll see. :) 
