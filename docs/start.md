# 4. Tutorial and Exercises (200 pts)

## 4.1 Getting Started Tutorial (100 pts)

```superclean
// Variable declaration
let x: Int = 5;

// Function definition
fun add(a: Int, b: Int): Int {
    return a + b;
}

// Print statement
print(add(x, 3));

// If-else expression
if x > 3 {
    print("x is greater than 3");
} else {
    print("x is less than or equal to 3");
};

// While loop
let counter: Int = 0;
while counter < 5 {
    print(counter);
    counter = counter + 1;
}
```

---

## 4.2 Five Programming Exercises (100 pts)

### Example: Fibonacci

**Problem**: Write a recursive function `fib(n: Int): Int` that returns the nth Fibonacci number.

**Solution**:

```superclean
fun fib(n: Int): Int {
    if n <= 1 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}
```

---

### **TODOs for Future Iterations**

- Add examples for lists and `for` loops once implemented.
