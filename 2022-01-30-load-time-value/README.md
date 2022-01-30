# `LOAD-TIME-VALUE`, or How I Started Worrying About `STATIC-LET` Again

Usually you are not supposed to know whether a piece of Lisp code has been compiled or is being interpreted without compilation. In particular, compiling a Lisp program shouldn't change its semantics in any way, right?

See, there is one thing that *surely* is an exception to that rule. It is specified in a way that allows it to differ in terms of how it behaves between compiled and non-compiled code: `LOAD-TIME-VALUE`, which I recently used to implement `STATIC-LET`.

And it's not a nice thing to discover.

## Rationale

CLHS 3.2.2.2 Minimal Compilation states (emphasis mine):

> The first argument in a `load-time-value` form in source code processed by `compile` is evaluated at compile time; in source code processed by `compile-file`, the compiler arranges for it to be evaluated at load time. **In either case, the result of the evaluation is remembered and used later as the value of the `load-time-value` form at execution time**.

More, CLHS Special Operator `LOAD-TIME-VALUE` states (emphasis mine):

>  If a `load-time-value` expression is processed by `compile-file`, the compiler performs its normal semantic processing (such as macro expansion and translation into machine code) on form, but arranges for the execution of form to occur at load time in a null lexical environment, with the result of this evaluation then being treated as a literal object at run time. **It is guaranteed that the evaluation of form will take place only once when the file is loaded**, but the order of evaluation with respect to the evaluation of top level forms in the file is implementation-dependent.
>
> If a load-time-value expression appears within a function compiled with `compile`, the form is evaluated at compile time in a null lexical environment. **The result of this compile-time evaluation is treated as a literal object in the compiled code.** 
>
> If a load-time-value expression is processed by `eval`, form is evaluated in a null lexical environment, and one value is returned. Implementations that implicitly compile (or partially compile) expressions processed by `eval` **might** evaluate form only once, at the time this compilation is performed.

`COMPILE-FILE` and `COMPILE` have a "must", where `EVAL` only has a "might". This means that functions defined using `EVAL`, without compilation, can cause a new object to be instantiated every time, which will both call the initialization form every time the body is entered and break all code that depends on the static binding values to stay static.

So, in order to get the behavior we want (in which an object is only allocated once), the code containing the `STATIC-LET` form must be compiled, at which point the load-time values will either be instantiated (in case of `COMPILE`) or stored in the resulting FASLs to be instantiated at load time (in case of `COMPILE-FILE`).

## Test

We can make a quick test to figure out how different Lisp implementations handle this.

```lisp
;;; the test goes like this:

;;; let's grab two functions with the same body
;;; which uses the LOAD-TIME-VALUE trick
(defun test-function ()
  (let ((counter-var (load-time-value (cons 0 nil))))
    (symbol-macrolet ((counter (car counter-var)))
      (incf counter))))

(defun test-function-2 ()
  (let ((counter-var (load-time-value (cons 0 nil))))
    (symbol-macrolet ((counter (car counter-var)))
      (incf counter))))

;;; let's compile only the second one
;;; and leave the first one possibly uncompiled
(compile 'test-function-2)

;;; let's call each one a few times
(format t "Possibly not compiled code: ~D ~D ~D~%"
        (test-function) (test-function) (test-function))

(format t "Compiled code: ~D ~D ~D~%"
        (test-function-2) (test-function-2) (test-function-2))
```

The results divide the CL world pretty much in half, speaking numerically.

* SBCL 2.1.11
  * Possibly not compiled code: 1 2 3
  * Compiled code: 1 2 3
* CCL 1.12
  * Possibly not compiled code: 1 2 3
  * Compiled code: 1 2 3
* ECL 21.2.1
  * Possibly not compiled code: 1 2 3
  * Compiled code: 1 2 3
* Clasp current
  * Possibly not compiled code: 1 2 3
  * Compiled code: 1 2 3

* ABCL 1.8.0:
  * Possibly not compiled code: 1 1 1
  * Compiled code: 1 2 3
* CLISP 2.49.93+:
  * Possibly not compiled code: 1 1 1
  * Compiled code: 1 2 3
* ACL 10.1 Express:
  * Possibly not compiled code: 1 1 1
  * Compiled code: 1 2 3
* LW 7.1.2 Personal:
  * Possibly not compiled code: 1 1 1
  * Compiled code: 1 2 3

So, it's time to update the `STATIC-LET` article I wrote [yesterday](https://github.com/phoe/articles/blob/main/2022-01-29-static-let/README.md) and add a warning to it that it's only safe to use `STATIC-LET` in compiled code, or on implementations which are compiler-only.

## Detection

But alas! It's also possible to use this behavior to *check* if a piece of code has been minimally compiled. The following example signals an error *only* on the latter four implementations and *only* in code that has not been compiled.

```lisp
;;; I used the stones to destroy the stones
(flet ((fn () (let ((x (load-time-value (list 0)))) (incf (car x)))))
  (declare (notinline fn))
  (when (= (fn) (fn))
    (error "This will not work in uncompiled code.")))
```

On non-compiler-only implementations, it's possible to splice such a piece of code into a macroexpansion in order to have a check which will signal an error (or possibly do something else) on code which was not minimally compiled.

Or, in other words, I just realized that `(flet ((fn () (let ((x (load-time-value (list 0)))) (incf (car x))))) (= (fn) (fn)))` is a non-portable but luckily working poor man's "was this code minimally compiled" predicate.

I bet $3 that it's useless other than for the above.
