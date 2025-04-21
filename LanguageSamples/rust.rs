use std::collections::HashMap;

struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn show(&self) {
        println!("({}, {})", self.x, self.y);
    }
}

fn greet(name: &str) {
    println!("Hi, {}!", name);
}

fn square(n: i32) -> i32 {
    n * n
}

fn main() {
    let name = "Alice";
    let x = 5;
    let y = 3.14;
    let active = true;

    greet(name);
    println!("Square of {} is {}", x, square(x));

    let p = Point { x: 3, y: 4 };
    p.show();

    let nums = vec![1, 2, 3];
    for n in nums {
        print!("{} ", n);
    }
    println!();

    let mut ages = HashMap::new();
    ages.insert("Alice", 30);
    ages.insert("Bob", 25);
    println!("Bob is {} years old.", ages["Bob"]);

    let mut count = 0;
    while count < 3 {
        println!("While loop: {}", count);
        count += 1;
    }
}
