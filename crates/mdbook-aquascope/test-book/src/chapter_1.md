# Chapter 1

```aquascope,interpreter,horizontal=true
fn main() {
    let a = Box::new([0; 1_000_000]);
    let b = a;
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