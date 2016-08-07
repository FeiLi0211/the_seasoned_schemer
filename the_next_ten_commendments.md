# The Next Ten Commendment

## The Eleventh Commendment
Use additional arguments when a function needs to know other arguments to the function have been like so far.

## The Twelfth Commendment
Use **(letrec ...)** to remove arguments that do not change for recursive applications.

## The Thirteenth Commendment
Use **(letrec ...)** to hide and to protect functions.

## The Fourteenth Commendment
Use **(letrec ...)** to return values abruptly and promptly.


## The Fifteenth Commendment
Use **(letrec ...)** to name the values of repeated expressions in a function definition if they may be evaluated twice for one and the same use of the function. And use **(let ...)** to name that values of expressions (without **set!**) that are re-evaluated every time a function is used.

## The Sixteenth Commendment
Use **(set! ...)** only with names defined in **(let ...)**s.

## The Seventeenth Commendment
Use **(set! x ...)** for **(let ((x ...)) ...)** only if there is at least one **(lambda ...** between it and the **(let ...)**, or if the new value for **x** is a function that refers to **x**.

## The Eighteenth Commendment
Use **(set! x ...)** only when the value that **x** refers to is no longer needed.

## The Nineteenth Commendment
Use **(set! ...)** to remember valuable things between two distinct uses of a function.

## Then Twentieth Commendment
When thinking about a value created with **(letcc ...)**, write down the function that is equivalent but does not forget. Then, when you use it, rember to forget.