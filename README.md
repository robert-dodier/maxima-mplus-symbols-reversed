# maxima-mplus-symbols-reversed

This project provides Lisp code for Maxima to change the order of
terms in "+" expressions displayed in the console and also in TeX output.
Specifically, "+" expressions are output with symbols in dictionary order,
instead of reverse dictionary order (the default). However, the ordering
of terms with exponents (with the same base) and other constructs is
not changed. Therefore this is not the same effect as the global
variable `powerdisp`.

The effect of this code is controlled by a global variable `reverse_symbols_order`,
which is `false` (disabled) by default. Reordering is enabled when
`reverse_symbols_order` is `true`.

Example:
```
(%i1) load ("form-mplus-symbols-reversed.lisp") $
(%i2) reverse_symbols_order;
(%o2)                                false
(%i3) e : 1 + 2*b^2 + 3*c^3 + b + 3*b^3 + 5*b^5 + a + z;
                          3      5      3      2
(%o3)              z + 3 c  + 5 b  + 3 b  + 2 b  + b + a + 1
(%i4) reverse_symbols_order : true $
(%i5) e;
                          5      3      2          3
(%o5)              a + 5 b  + 3 b  + 2 b  + b + 3 c  + z + 1    
```
This code works by replacing the function which is called to rework
every "+" expression for display. The main function for that purpose
is named NFORMAT and the specific one for "+" expressions is named
FORM-MPLUS. This code replaces FORM-MPLUS with a modified function,
FORM-MPLUS-SYMBOLS-REVERSED-MAYBE.

Aside from display output and TeX output, NFORMAT is called by `part`
and other functions that work on parts of expressions. These functions
will yield different results when `reverse_symbols_order` is enabled.

`run_testsuite` yields some errors when reordering is enabled; 
a cursory review seems to show that these are all due to a fixed
dependence on a specific order of terms, and therefore it is possible
all these errors could be avoided by updating the expected result in
each test. A case by case review would be needed to establish that.
