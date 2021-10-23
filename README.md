# `jsgo` is parser for some of javascript features in `Go-lang`. It is for education purpose only and is done as a practice project.

## Supported javascript features
- Variable declarations using `let`, `var` and `const`
```js
    let a = 10;
    var b = 20;
    const c = 30;
```
- Functions and Arrow functions
```js
function hello(name) {
    return "Hello " + name;
}

const hello = (name)=> {
    return "Hello " + name;
}
```
- Supported data-types are `undefined`, `number`, `string` and `number`
- String templates
```js
    const name = "John";
    const greeting = `Hello ${name}`;
```

- Supported Operators
```js
    // Mathematical Operators
    + // PLUS
    - // MINUS
    * // ASTERISK
    ^ // POWER
    % // MODULUS
    / // SLASH
    // Mathematical Assignment Operators
    +=
    -=
    *=
    ^=
    %=
    /=
    // The dot operator -> Member Expression in the AST
    one.two.three.four
    // The Spread operator
    ...x
    // Boolean Operators
    a == b
    a != b
    a || b
    a && b
    // Ternary Operator
    a ? b : c
```

- Objects

```js
    // Maybe implemented as a Map<Identifier, Expression> 
    let person = {Name: "Jane", Age: 20, Friend: {Name: "Jane", Age: 30}}
    p = {...person, Name: "Henry"}

    // Object Destructuring in function params
    function hello({Name: _name}) {
        return `Hello ${name}`;
    }

```

- Arrays
```js
    let grades = [10,20,30];
    [a,b,c] = grades;
```

- String Templates
```js
    let name = "Hellen";
    let greeting = `Hello ${name}`;
```

- `for`,`do-while` and `while` loops
- `if`, `else` and `if-else` statements
- `swithc-case` statements

- `in-built` functions and objects
```js
    console.log("Hello World")
```
