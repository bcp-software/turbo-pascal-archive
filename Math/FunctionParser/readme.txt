 The unit contains the TParsedFunction class and descriptions of types needed to
 working with it.

 The TParsedFunction (PF) class is an interpreter (parser) of functions.
 The ParseFunction method allows to recognize any mathematical expression,
 containing (optionally) x, y, z, various math functions and numeric
 constants (see list below), written as a string. The recognized expression
 is written in a special way in the form of 4 arrays of variables. The method
 contains the ErCode parameter, which allows to check if the recognition operation

 The ErCode parameter indicates whether the recognition operation was successful or not:
   ErCode =
     0 - The operation was a success
     1 - An unknown function or operator is encountered
     2 - The expression contains an unequal number of opening and closing brackets.
         closing brackets
     3 - Invalid character detected
         (valid characters are: Latin letters of any case, digits, parentheses, space, + - * / ^ and dot).
          brackets, space, symbols + - * / ^ and dot).

 The Compute method allows to calculate the value of a previously recognized function for
 values given for x, y and z (if the parameter is omitted, it is considered equal to 0).

 The ImportParsed and ExportParsed methods allow you to import/export
 recognized functions in the visibility of the TPFRecord.

 Parser's principle and algorithm:
 Consider the following expression (as an example):

 (5+у)*sin(x^3)+х

 This expression can be written as a1 = a2 + a3,
 where a2 = (5+u)*sin(x^3), and a3 = x.
 In turn, a2 = a4*a5, where a4 = 5+u, and a5 = sin(x^3)...
 And so on. Eventually we get a chain of simple operations
 on two or one operand, each of which can be either
 a number/variable or another pair of operands.
 The chain is technically written in three arrays: a one-dimensional array,
 containing an indication of an operation (in my unit, each operation is labeled
 number and is stored in type byte - you can hardly think of more than 255 operations :)
 I personally remember only 23 :)). The second array is two-dimensional and
 stores references to the operands participating in the operation. The references are
 are numbers of chain links and are stored in the word type.
 The third array contains variables, numeric constants and numbers found
 in the expression. This array is linked with the first two assignment operations, which
 in my case has the number 0. In this case, the number of the first operand refers not to
 element of the operand array, but to the element of this third array.
 As you can see, everything is simple!
 To calculate the value of an expression, we need to enter another array
 (in my case it is labeled a). Note that the operands involved in each
 operations always have a number greater than that operand. Therefore, in order to
 calculate the value of the expression itself (i.e., element a1), it is sufficient
 calculate the value of each element of a, starting from the end of the array.


 Calculation speed:
 Due to the fact that the function is recognized only once,
 and the entire calculation operation is essentially reduced to actions on arrays,
 it is possible to reach a computing speed, which can be compared to the speed of calculating
 of the same expression by the Delphi compiler itself.
 Calculation speeds themselves vary, depending on the complexity of the expression
 (Sometimes parser's calculation speed is even higher than that of the compiler!
  This is caused by a certain optimization of the expression during parsing), but on average
  Parser's computation time is 150-200% of the compiler's computation time.

  Accuracy of calculations:
  The information is stored in type Single. There is no point in having more accuracy, because
  real numbers in the string are transmitted with an obvious error. In any case
  In any case, to get more accuracy, you just need to change single to
  to the right type, wherever it occurs.
  The accuracy of calculations is on average e-5


  String presentation format:
  - String length is limited to 255 characters
    (the limit can be removed by replacing shortString with ansiString in the
    method definition)
  - Letter case is NOT important
  - Spaces are allowed
  - The decimal part must be separated by a period (not a comma)
  - Expressions must be written using the normal rules for writing mathematical expressions
    for computers. (For example: x^2 + sin(5*y)/exp(4*z) )
  - The program takes into account the priority of operations (in descending order: calculating
    functions, exponentiation, multiplication and division, addition and subtraction)
  - The program keeps brackets in mind
  - The program knows the following mathematical operations:

      + : addition
      - : subtraction
      / : division
      * : multiplication
      ^ : arbitrary exponentiation
  - The program knows the following math functions:
      sin - sine
      cos - cosine
      tan - tangent
      cot - cotangent
      exp - exponent
      ln - the nat. logarithm
      sqr - square
      sqrt - square root
      round - rounding
      trunc - integer part
      asin - arcsine
      acos - arc cosine
      atan - arctangent
      acot - arcotangent
      sinh - hyp. sine
      cosh cosine
      tanh - hyp. tangent
      coth - hyp. cotangent

  - The program knows the following numerical constants:
      pi - number pi
      e - number e

  - The program understands functions up to 3 variables.
    The variables are denoted by letters x, y, z

  - The program understands only numbers presented in decimal form.
    decimal form

   Note (array capacity)
   The required capacity of an array is defined as the sum of:
   number of operators (functions) + number of occurrences of values + 2.
   For example:  5*sin(x^3)+x
   4 operators (multiplication, sine, addition, reduction)
   +
   4 occurrences of variables (5, 3, x, x)
   +
   2 = 10.
   It seems to the author that the capacity of 100 should be enough to write
   a very complex expression. In any case the capacity can be increased
   by changing the constant Capacity.
   (Originally, dynamic arrays were used in the program,
   but then I gave it up, because it was too much hassle :) )

   !WARNINGS!

  It is necessary to control the following things yourself:
  - Calling the Compute method for an unrecognized function will cause a
    an execution time error.
  - Exceeding a possible array capacity will of course also cause an error.
    Adjust the Capacity value to your own needs.
  - Errors like division by zero, which may be in the expression, are
    entirely on the conscience of the user

   LICENSE:
   This unit and the classes and methods it contains, as well as the algorithm
   recognition functions are intellectual property
   OF ILYA A. SHCHEGLOV <franzy@comail.ru>.
   The given class, its methods and algorithm are distributed free of charge for
   non-commercial use. Any other distribution
   This information for profit is prohibited. Use of
   of the class, its methods or recognition algorithm in programs
   distributed by commercial means is possible only with written
   permission of the above copyright holder.

   This information must be specified in the body of any program using the
   recognition algorithm, class, or its methods.

   Acknowledgements:
   Yuri Lapukhov for his help in finding errors in the algorithm
