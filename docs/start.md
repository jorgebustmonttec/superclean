# 4. Tutorial and Exercises (200 pts)

## 4.1 Getting Started Tutorial (100 pts)

> **TODO**: Write a short tutorial guiding a new user.

```superclean
let x: Int = 5;

fun add(a: Int, b: Int): Int {
    return a + b;
}

print(add(x, 3));
```

## 4.2 Five Programming Exercises (100 pts)

> **TODO**: Add problem + solution code for each.

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
