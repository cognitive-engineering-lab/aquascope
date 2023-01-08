# Chapter 1

```aquascope,receiver-types
fn main() {
    let mut x = 1;
    let y = &x;
    x += 1;
    println!("{} = {}", x, *y);
}
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
