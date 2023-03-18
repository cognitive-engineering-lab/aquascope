# Chapter 1

## Some introduction

```aquascope,interpreter
#struct User {
#    active: bool,
#    username: String,
#    email: String,
#    sign_in_count: u64,
#}
fn main() {
    let user1 = User {
        email: String::from("someone@example.com"),
        username: String::from("someusername123"),
        active: true,
        sign_in_count: 1,
    };`[]`
    let x = 1;
}
```

```aquascope,interpreter+permissions,stepper,boundaries,shouldFail,horizontal
#fn main() {
let mut v = vec![1, 2, 3];
let n = &v[0];`[]`
v.push(0);`[]`
let x = *n;`[]`
#}
```

```aquascope,permissions,stepper
#fn main() {
let mut x = 1;
let y = &x;
println!("{} = {}", x, *y); `(focus,paths:x)`
#}
```

```aquascope,permissions,boundaries,stepper
#fn main() {
let mut x = 1;
let y = &mut x;
println!("{} = {}", x, *y);
#}
```

```aquascope,permissions,stepper,boundaries
#fn main() {
let mut x = 1;
let y = &x;
println!("{} = {}", x, *y);
#}
```

```aquascope,permissions,stepper,boundaries
#fn main() {
let mut x = String::from("Hello");
let y = &mut x;
let z = &*y;
println!("{} {}", y, z);
#}
```

```aquascope,interpreter,concreteTypes
fn main() {
    let n = Box::new(1);`[]`
    let y = plus_one(&n);`[]`
    println!("The value of y is: {y}");
}
fn plus_one(x: &i32) -> i32 {
    `[]`*x + 1
}
```

```aquascope,permissions,boundaries,showFlows
fn add_ref(v: &mut Vec<&i32>, n: i32) {
  let r = &n;
  v.push(r);
}
```
