
This is a minimal Racket program to experiment how the Racket module system
can be used in a DSL.

The file `main.rkt` defines the following macros:

* `(assign dest src)`, where `dest` is a variable name and `src` is either a literal or a variable name, expands to a `provide` form and a `define` form for the given variable.
* `(show src)`, where `src` is a variable name, expands to `println`.
* `(use require-spec)` is equivalent to a `require` form in our DSL.
* `(begin-mini-dsl body ...)` is a wrapper form for our DSL. It calls the `check` function whose purpose is to perform semantic checking and code transformation.

The function `check`:

* creates a new scope for the body of the `begin-mini-dsl` form;
* transforms the destination of each `assign` form with a generated variable name:
    * a binding is created in the current scope for the original destination name;
    * this binding is mapped to an instance of `struct var` that stores the generated variable name;
* transforms the source of each `assign` or `show`, when the source is a name:
    * by looking up the binding for the source name;
    * by inspecting the corresponding instance of `struct var` to retrieve the generated variable name.

The relevance of this DSL is not the main concern here.

The question I want to address is: what would be a correct implementation of the `use` form?
We need to `provide` the bindings that we created in the `check` function,
and we need to access the instances of `struct var` from the module that
contains `use` forms.

At the moment, the implementation works for a standalone example:

```
racket examples/standalone.rkt
45
```

Without a complete implementation, a two-module example will fail in the
look-up step:

```
racket examples/module-main.rkt
main.rkt:43:52: v: variable not found
```

Uncommenting the `syntax-local-lift-provide` in function `bind-var!`
will result in the following error:

> SIGSEGV MAPERR si_code 1 fault on addr 0x300
> Abandon (core dumped)
