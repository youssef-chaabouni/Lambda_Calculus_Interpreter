# Lambda_Calculus_Interpreter
An interactive console for lambda calculus.

## Clone the project

```bash
  $ git clone https://github.com/youssef-chaabouni/Lambda_Calculus_Interpreter
```

## Usage

### Run the interpreter
Once you clone project (section <b>"Clone the project"</b> above), you can preceed to creating the executable for the Lambda Calculus Interpreter:

<ol>
  <li>
Move into the project directory:

```bash
  $ cd Lambda_Calculus_Interpreter
```
  </li>
  <li>
Compile the project:

```bash
  $ make
```
  </li>
  <li>
Run the interpreter:

```bash
  $ ./MyLambda
```
  </li>
</ol>

### Front-End
<ul>
  <li>
  <b>The console:</b><br>

The interpreter offers an interactive console for Lambda calculus to the user.

```
  >
```
  </li>

  <li>
  <b>Lambda Expressions:</b><br>

Lambda Expressions are represented in a <a href="https://en.wikipedia.org/wiki/Lambda_calculus">human-friendly format</a>.

e.g.

```
  > my_lambda_expression = \f.\x.f x
```
  </li>

  <li>
  <b>Line-editing with history and Backward/Forward editing:</b><br>

The console supports the following operations:
- Up button `↑` : Go backward in the history of commands.
- Down button `↓` : Go forward in the history of commands.
- Left button `←` : Go backward in the line currently being edited.
- Right button `→` : Go forward in the line currently being edited.

  </li>
</ul>

### Commands

Two types of commands are supported by the interpreter:

<ul>
  <li>
  <b>Variable definition:</b><br>
  Variables can be defined using either of the syntaxes:

```
  > 'variable_name' = 'expression'
```
or

```
  > let 'variable_name' = 'expression'
```
  where:
  - `'variable_name'` stands for the name of the variable.
  - `'expression'` stands for the Lambda-expression we want to assign.
  
  Once the variable is defined, it can then be used to refer to the assigned expression in the rest of the interaction with the interpreter (unless it is shadowed by another assignment).
  </li>
  <li>
  <b>Expression evaluation:</b><br>
  Expressions can be evaluated using the syntax:

```
  > 'expression'
```
  where `'expression'` stands for the Lambda-expression we want to assign.
  
  This will evaluate the expression to its <a href="https://en.wikipedia.org/wiki/Beta_normal_form">beta-normal form</a>, and print the result.
  </li>
</ul>

## Credits

### Authors
Jean Sébastien Gaultier<br>
John Levy<br>
Youssef Chaabouni<br>

### University
This project was done in the scope of the course <i>CSE301: Functional Programming</i> at <i>Ecole Polytechnique</i>.

Some parts of the code in this repository were provided in the assignment, and some of the ideas were inspired from materials seen in class.

### Others resources
- <a href="https://hackage.haskell.org/package/haskeline-0.8.2/docs/System-Console-Haskeline-IO.html">Haskeline Documentation</a>
- <a href="https://en.wikipedia.org/wiki/Beta_normal_form">Stack Overflow</a>