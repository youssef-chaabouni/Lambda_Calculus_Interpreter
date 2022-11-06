# Lambda_Calculus_Interpreter
An interactive console for lambda calculus.

## <div id="clone">Clone the project</div>

```bash
  $ git clone https://github.com/youssef-chaabouni/Lambda_Calculus_Interpreter
```

## Usage

In order to be able to use <b>Lambda_Calculus_Interpreter</b>, make sure you have <a href="https://www.haskell.org/ghc/">GHC</a>.

### Run the interpreter
Once you <a href="#clone">clone the project</a>, you can proceed to creating the executable for the Lambda Calculus Interpreter:

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

The interpreter offers an interactive console for Lambda calculus:

```
  Lambda>
```
  </li>

  <li>
  <b>Lambda Expressions:</b><br>

Lambda Expressions are represented in a <a href="https://en.wikipedia.org/wiki/Lambda_calculus">human-friendly format</a>.

Example:

```
  Lambda> my_lambda_expression = \f.\x.f x
```

Different other sytaxes are also supported by the interpreter:
<ul>
  <li>

  Use of the backslash `\` equivalently to the lambda symbol `λ`.

  Example:

```
  Lambda> \f.\x.f x
  \f.\x.f x
  Lambda> λf.λx.f x
  \f.\x.f x
```
  </li>
  <li>
  Support for <a href="https://en.wikipedia.org/wiki/Lambda_calculus">Haskell</a>-like syntax.

  Example:

```
  Lambda> \f x y -> f y x
  \f.\x.\y.f y x
```
  </li>
</ul>
  </li>
  <li>
  <b>Line-editing with history and Backward/Forward editing:</b>

The console supports the following operations:
<ul>
<li>

Up button `↑` : Go backward in the history of commands.</li>
<li>

Down button `↓` : Go forward in the history of commands.</li>
<li>

Left button `←` : Go backward in the line currently being edited.</li>
<li>

Right button `→` : Go forward in the line currently being edited.</li>
</ul>
  </li>
  <li>
  <b>Parsing errors:</b><br>

  Whenever a parsing error is encountered, the whole line is ignored and the interpreter keeps listening to the standard input.

Examples:

```
  Lambda> x = -1
  Parsing error in: `x = -1`
  Lambda> ▮
```

```
  Lambda> \f.\x.f y
  Variable not in scope: y
  Lambda> ▮
```
  </li>
</ul>

### Commands

Four types of commands are supported by the interpreter:

<ul>
  <li>
  <b>Variable definition:</b><br>
  
  Variables can be defined using either of the syntaxes:

```
  Lambda> 'variable_name' = 'expression'
```
or

```
  Lambda> let 'variable_name' = 'expression'
```
  where:
  - `'variable_name'` stands for the name of the variable.
  - `'expression'` stands for the Lambda Expression we want to assign.
  
  Once the variable is defined, it can then be used to refer to the assigned expression in the rest of the interaction with the interpreter (unless it is shadowed by another assignment).
  </li>
  <li>
  <b>Expression evaluation:</b><br>

  Expressions can be evaluated using the syntax:

```
  Lambda> 'expression'
```
  where `'expression'` stands for the Lambda-expression we want to assign.
  
  This will evaluate the expression to its <a href="https://en.wikipedia.org/wiki/Beta_normal_form">beta-normal form</a>, and print the result.
  </li>
  <li>
  <b>Load instructions from file:</b><br>

  You can load instructions from a file using the syntax:

```
  Lambda> :load 'filename'
```
  where `'filename'` stands for the name of the file.

  Example:

```
  Lambda> :load resources/church.lam
```
  </li>
  <li>
  <b>Quit the interpreter:</b><br>

  You can quit the interpreter using either of the syntaxes:

```
  Lambda> :quit
```
or

```
  Lambda> :q
```
  </li>
</ul>

Example:

```
  $ ./MyLambda
  Lambda> zero = \f.\x.x
  Lambda> one = \f.\x.f x
  Lambda> succ = \n.\f.\x.f (n f x)
  Lambda> two = suc one
  Variable not in scope: suc
  Lambda> two = succ one
  Lambda> three = succ two
  Lambda> add = \m.\n.m succ n
  Lambda> mul = \m.\n.m (add n) zero
  Lambda> exp = \m.\n.n (mul m) one
  Lambda> add two three
  \f.\x.f (f (f (f (f x))))
  Lambda> mul two three
  \f.\x.f (f (f (f (f (f x)))))
  Lambda> exp two three
  \f.\x.f (f (f (f (f (f (f (f x)))))))
  Lambda> :quit
  Leaving Lambda Calculus Interpreter.
  Goodbye :)
```

## Acknowledgements

### Authors
Jean Sébastien Gaultier<br>
John Levy<br>
Youssef Chaabouni<br>

### University
This project was done in the scope of the course <i>CSE301: Functional Programming</i> at <i>Ecole Polytechnique</i>.

Some parts of the code in this repository were provided in the assignment, and some of the ideas were inspired from materials seen in class.

### Other resources
- <a href="https://hackage.haskell.org/package/haskeline-0.8.2/docs/System-Console-Haskeline-IO.html">Haskeline Documentation</a>
- <a href="https://stackoverflow.com/a/23070727">Stack Overflow</a>
