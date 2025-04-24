import std.stdio, std.string, std.container;

struct Point {
    int x, y;
    void show() const {
        writeln("(", x, ", ", y, ")");
    }
}

enum Color { Red, Green, Blue }

void greet(string name) {
    writeln("Hi, ", name, "!");
}

int square(int n) {
    return n * n;
}

void main() {
    string name = "Alice";
    int x = 5;
    double y = 3.14;
    bool active = true;

    greet(name);
    writeln("Square of ", x, " is ", square(x));

    Point p = Point(3, 4);
    p.show();

    int[] nums = [1, 2, 3];
    foreach (n; nums) write(n, " ");
    writeln();

    auto ages = ["Alice": 30, "Bob": 25];
    writeln("Bob is ", ages["Bob"], " years old.");

    int count = 0;
    while (count < 3) {
        writeln("While loop: ", count);
        count++;
    }

    Color c = Color.Green;
    if (c == Color.Green)
        writeln("Color is Green");
}

