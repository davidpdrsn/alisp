# alisp

This is a small toy language I made to practice compiling stuff into JavaScript.

## Example

Write some code like this:

```lisp
(defun add (a b)
  (+ a b))

(defun main ()
  (print (filter (lambda (x) (< x 2)) [1 2 3 4]))
  (print (fold add 0 [1 2 3 4])))
```

Compile it like this:

`$ alisp foo.lisp`

And get some JavaScript like this:

```javascript
var add = (function (a, b) {
    return a + b
});
var main = (function () {
    (function () {
        console.log([1, 2, 3, 4].filter((function (x) {
            return x < 2
        })));
        return [1, 2, 3, 4].filter((function (x) {
            return x < 2
        }))
    })();
    return (function () {
        console.log([1, 2, 3, 4].reduce(add, 0));
        return [1, 2, 3, 4].reduce(add, 0)
    })()
});
main();
```

Which you can then run and get the following result:

```javascript
[ 1 ]
10
```

## Todo list

- [ ] Comments
- [ ] Strings
- [ ] Add options for either compiling or interpreting the given file
- [ ] Hashes
- [x] Break up code generation mode into three modules. (1) js ast, (2) lisp ast -> js ast, (3) js ast -> js code
- [ ] Don't compile expression twice in print
- [ ] Type checking (type inference)
- [ ] Interpreter
- [ ] Include positions in the program for each expression
