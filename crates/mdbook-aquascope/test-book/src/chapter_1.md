# Chapter 1

```aquascope,interpreter
fn main() {
    let m1 = String::from("Hello");
    let m2 = String::from("world");`[]`
    greet(&m1, &m2);`[]` // note the ampersands
    let n = m1.len() + m2.len();
}

fn greet(g1: &String, g2: &String) { // note the ampersands
    `[]`println!("{} {}!", g1, g2);
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
