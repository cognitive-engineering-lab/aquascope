# Chapter 1

```aquascope,receiver-types
fn main() {
<<<<<<< HEAD
    let mut x = 1;
    let y = &x;
    x += 1;
    println!("{} = {}", x, *y);
}
=======
    let s = [(0, 1), (2, 3)];
    let x = &s[1].0;
}
>>>>>>> upstream/main
```

<!-- ```aquascope,interpreter
fn main() {
    let n = Box::new(1);`[]`
    let y = plus_one(&n);`[]`
    println!("The value of y is: {y}");
}

fn plus_one(x: &i32) -> i32 {
    `[]`*x + 1
}
``` -->

<!-- ```aquascope,interpreter
fn main() {
    let x = 1;
    let y = &x;`[]`
}
``` -->
