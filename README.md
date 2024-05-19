# Learning the Scheme language

Preferred Scheme implementation to use is [GNU Guile](https://www.gnu.org/software/guile/).

Note that the Guile REPL is a little awkward to use without the Readline library. You can enable Readline like this:

```
(use-modules (ice-9 readline))
(activate-readline)
```

You can set these in the Guile init file `~/.guile` so they are executed automatically when starting Guile.

Sample session:

```
$ guile
scheme@(guile-user)> (load "fact.scm")
scheme@(guile-user)> (fact 5)
$1 = 120
scheme@(guile-user)> (fact2 6)
$2 = 720
scheme@(guile-user)> (fact3 7)
$3 = 5040
```

## The Little Schemer

The file *little.scm* contains exercises from [The Little Schemer](https://felleisen.org/matthias/BTLS-index.html) book. Very useful book for learning Scheme and Lisp more generally.
