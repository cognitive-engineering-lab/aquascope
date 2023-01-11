# Chapter 1

```aquascope,stepper
# fn main() {
let mut x = 1;
let y = &x;
println!("{} = {}", x, *y);`(focus,paths:x)`
#}
```

```aquascope,boundaries
# fn main() {
let mut x = 1;
let y = &mut x;
println!("{} = {}", x, *y);`(focus,paths:x)`
#}
```

```aquascope,interpreter,concreteTypes=true
fn main() {
    let n = Box::new(1);`[]`
    let y = plus_one(&n);`[]`
    println!("The value of y is: {y}");
}

fn plus_one(x: &i32) -> i32 {
    `[]`*x + 1
}
```

<!-- ```aquascope,interpreter
fn main() {
    let x = 1;
    let y = &x;`[]`
}
``` -->
