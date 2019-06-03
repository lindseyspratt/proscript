# proscript
A Javascript implementation of Prolog

This is a fork of the original Proscript project by Matt Lilley.
There are two major enhancements in this fork: builtins to work with the DOM (Document Object Model) of HTML and
a proper interactive Prolog tracing debugger that can be used in the client web page (using a JQuery Terminal).
For the DOM enhancements there is a javascript function proscript_init("goal") that can be 
called in <body> onload to run a Prolog query to initialize the HTML environment.
proscript("goal") can be used in handlers in the HTML page where the evaluation
shares the same predicate assertions as that set up by the proscript_init function.

As with the original project by Lilley, it still needs a lot of tidying and organization.

## Organization
### The WAM implementation
This is implemented primarily in wam.js. Extra stuff is also present in:
   * fli.js: SWI-Prolog-like foreign langauge interface. Allows escaping to Javascript from Prolog, so you can call low(er) level functions. Huge chunks of this (like PL_cut_query!) are not implemented
   * foreign.js: This implements a lot of core WAM building blocks directly in javascript. For example, you will find implemntations for univ, writeln and halt here.
   * memory_files.js: stream IO with memory files.
   * gc.js: Implements a garbage collector
   * read.js: Handles input and output of terms, including parsing Prolog terms
   * record.js: Handles dynamic adjustment of the state: assert and friends
   * stream.js: Handles reading and writing to streams, and all the ISO predicates (the ones implemented anyway) like get_char/2 and put_code/2.
   * dom.js, object_method.js, object_property.js: Builtins for working with the DOM.
   * debugger.pl, debugger.js: The interactive Prolog tracing debugger.

### Bits you must implement, and the stubs provided
   * standalone.js: Contains implementations of stdout and flush_stdout/1. You can either include this (in which case you will get output printed to a variable called stdout_buffer), or implement them yourself to do something /with/ the stuff written to stdout.

You might think that was all you needed, but then you need some code to run on your WAM, which is where the compiler comes in!

### The compiler
The compiler is itself written in Prolog. We must go deeper.

   * wam_compiler.pl: The guts of the compiler. Exports build_saved_state/2 and bootstrap/2, both actually located in wam_boostrap.pl
   * wam_boostrap.pl: This is the part of the compiler only executed in the bootstrapping process to generate the boostrapped compiler.
   * bootstrap_js.pl: This is the part of the compiler compiled by the bootstrapping compiler to generate the saved state for the actual compiled system
   * testing.pl: Contains implementations of debugging predicates used for debugging the compiler

Compiling the compiler produces:
   * proscriptls_state.js (the saved state)
   * proscriptls_engine.js    (the executable runtime)

You must include both of these if you want a working system. See test.html for an example.

### Tidying things up
   * js_preprocess.pl: This is a minification process that combines several files together to form proscriptls_engine.js, which is the final system used for execution

### Debugging terminal
A debugging terminal can be displayed in an HTML page using the 
terminal/proscript_interpreter_terminal.js. An example with just the terminal is terminal/terminal_test.html.
Another example is examples/calculator.html.
The debugging terminal is a command-line Prolog interpreter.
It also supports the 'trace' predicate to enable the 
interactive trace/debug mode. E.g. 'trace,mem(X,[a,b])' is a query that 
activates tracing while evaluating the mem/2 predicate.

## Trying it out
There are several examples in the examples directory: test.html, 
simple_test.html, console_button_test.html, and calculator.html.

test.html provides an execution environment for you to try out the final state

terminal_test.html (with proscript_interpreter_terminal.js) is an example using the JQuery Terminal 
to run the interactive Prolog debugger in a web page.

simple_test.html uses simple_test.pl to write "Hello World".