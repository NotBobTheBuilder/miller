Miller
======

Miller is a static analysis tool for JavaScript.

It infers the types of expressions and looks for contradictions where it can.

Take a look at the test scripts for some examples of what kinds of type errors can be checked at compile time.

## Usage

`sbt assembly` will generate a JAR that you can run.

`./miller.jar filename.js` will run miller on a file, alternatively just `miller.jar` will open a REPL (close with ctrl-D)
