# The mystery regarding `UIOP:DEFINE-PACKAGE` `:UNINTERN`

## Advertisement time

`UIOP:DEFINE-PACKAGE` is the part of UIOP that I personally use the most - it fills (IMO) the biggest hole in the Common Lisp package system, which is [CLHS Macro DEFPACKAGE](http://clhs.lisp.se/Body/m_defpkg.htm) saying:

> If the new definition is at variance with the current state of that package, the consequences are undefined; (...)

This means that removing an export from a `DEFPACKAGE` can cause your implementation to wag a finger at you, and also ignore your attempt at removing it.

```lisp
CL-USER> (defpackage #:foo (:use) (:export #:bar))
#<PACKAGE "FOO">

CL-USER> (defpackage #:foo (:use) (:export))
WARNING: FOO also exports the following symbols:
  (FOO:BAR)
See also:
  The ANSI Standard, Macro DEFPACKAGE
  The SBCL Manual, Variable *ON-PACKAGE-VARIANCE*
#<PACKAGE "FOO">

CL-USER> (loop for sym being the external-symbols of :foo 
               collect sym)
(FOO:BAR)
```

The solution is to manually call `UNEXPORT` on `FOO::BAR`, at which point SBCL will calm down and let you evaluate the second `DEFPACKAGE` form in peace.

`DEFINE-PACKAGE`, in the same situation, will do "the right thing" (read: the thing I personally expect it to) and adjust the package's export list to be consistent with the one provided to it.

```lisp
CL-USER> (uiop:define-package #:foo (:use) (:export #:bar))
#<PACKAGE "FOO">

CL-USER> (uiop:define-package #:foo (:use) (:export))
#<PACKAGE "FOO">

CL-USER> (loop for sym being the external-symbols of :foo 
               collect sym)
NIL
```

There's plenty of other useful options, such as `:MIX`, `:REEXPORT` and all, but there's one of them that looks... A bit off.

## Mystery time

The option `:UNINTERN` is specified to call `CL:UNINTERN` on some symbols when the package is defined.

Hold up, wait a second, though. Uninterning symbols? During package definition?

When a package is defined for the first time, there are no symbols to unintern. This means that this option is only useful when a package already exists, and therefore `UIOP:DEFINE-PACKAGE` is used to *redefine* it.

This, and uninterning cannot be used to achieve "partial `:use`", that is, to remove symbols from packages that are `:use`d in the current package in order to only "use a part of" this other package. That simply isn't doable in Common Lisp - `:use` imports all of the symbols exported by another package, except those that are explicitly `:shadow`ed.

So, again, what's the point? Scroll down only if you'd like the mystery to be spoiled to you.

-------

## Story time

Let's assume a very simple situation:

```lisp
(defpackage #:bar
  (:use)
  (:export #:symbol))
```

We have a single package which exports a single symbol. That package was created by some software which we use, and the symbol `BAR:SYMBOL` is useful to us in some way.

And then, while our Lisp image is still running, we'd like to *upgrade* this software to a new version. That is, we'd like to load a new version of that software and disregard the old one. In the new version of our software, the package structure looks like this:

```lisp
(defpackage #:foo
  (:use)
  (:export #:symbol))

(defpackage #:bar
  (:use #:foo)
  (:export #:symbol))
```

It seems that the symbol named `SYMBOL` was moved into another package, possibly because that is where the implementation of that symbol has been moved to. Oh well, looks understandable from a software architecture point of view!

...and then trying to load the upgraded version will fail at the very beginning. Worse - it *might fail*, since we have just stepped into undefined behavior area, as stated in the beginning of this post.

In particular, `DEFPACKAGE FOO` will be evaluated without any problem, but a keen eye will notice an error which will be signaled the moment we evaluate `DEFPACKAGE BAR`. The currently existing package contains its own version of the symbol named `SYMBOL`, whereas the new requirement is to `:USE` the package `FOO`, which has its own symbol named `SYMBOL` - a classic package name conflict.

What is the producer of this piece of software to do now in order to ensure a smooth transition?

One way forward is to `DELETE-PACKAGE` before moving on with the upgrade, but that's pretty explosive - if `BAR` exported any other symbols, naming e.g. class definitions, then this means trouble for us. Another way forward is to manually call `UNINTERN` before calling `DEFPACKAGE`, but *only* if the package already exists - and that is a little bit messy.

And this is exactly the problem that is meant to be solved by `UIOP:DEFINE-PACKAGE`. In particular, this utility is capable of automatically changing the structure of the underlying package to resolve conflicts in favor of the newly added symbols. We can simply use it as a drop-in replacement for `DEFPACKAGE`, like this:

```lisp
(defpackage #:foo
  (:use)
  (:export #:symbol))

(uiop:define-package #:bar
  (:use #:foo)
  (:export #:symbol))
```

That change allows this code to compile and load without errors. In particular, we can verify that `BAR:SYMBOL` correctly resolves to the new symbol from package `FOO`:

```lisp
CL-USER> 'bar:symbol
FOO:SYMBOL
```

So, that's one upgrading problem less, solved by using `UIOP:DEFINE-PACKAGE` instead of `DEFPACKAGE`.

...but, uh, what about `DEFINE-PACKAGE :UNINTERN`? That's still not the end of the story.

## Edge case time

Let us assume that you are the *developer* of Lisp software who is working on it and you are testing the scenario in which you upgrade one version of software to another. The technique described above works well with regard to upgrading software, but let's say that your package definition looked like this:

```lisp
(defpackage #:foo
  (:use)
  (:intern #:some #:totally-random #:stuff))
```

And you want to replace it with the following:

```lisp
(uiop:define-package #:foo
  (:use)
  (:intern #:some #:totally-randomized #:stuff))
```

The explanation is that `TOTALLY-RANDOM` was a symbol that was useful (and used) in the previous version of software, but the new version uses something better, which also has a better name - `TOTALLY-RANDOMIZED`.

And all is fine and well, until you go into your REPL and see *this*:

![image1](image1.png)

The syntax completion is suggesting the *old* symbol even though it no longer bears any meaning. It means that you, as the programmer, need to hit the `↓` key to navigate downwards and select the proper symbol, which can annoy you to no avail. That's a pet peeve.

But it also means that you have the *possibility* of introducing bugs into the system by using the old version of a function - or, worse, breaking the build by using a symbol that is only present on systems *upgraded from the old version* and not ones which had the new version loaded start from scratch.

That's actually scary.

And that's the concrete edge case solved by `:UNINTERN`!

```lisp
(uiop:define-package #:foo
  (:use)
  (:intern #:totally-randomized)
  (:unintern #:totally-random))
```

Using this fixes the syntax completion:

![image2](image2.png)

In particular, evaluating this `DEFINE-PACKAGE` form will either be a no-op (if the symbol doesn't exist, e.g. when defining the package from scratch) or automatically unintern the old symbol from the system (if it exists, e.g. when upgrading the package to a newer version).

In particular, the second option will happen *even if the current shape of the source code no longer has any other mentions of it* and even if this `:UNINTERN` call seems to make no sense. In this context, `:UNINTERN` is something protecting the programmer from a danger that may no longer be relevant for current versions of the software, but was once something that the programmer considered important enough to remove during a software upgrade.

Hell of an edge case, eh? As always, it's an edge case until you hit it and need a tool for solving it - and `:UNINTERN` fits that description pretty damn well.

Thanks to Robert Goldman and Phoebe Goldman for helping me solve this mystery.