using System;
using System.Collections.Generic;

struct Point {
    public int x, y;
    public void Show() => Console.WriteLine($"({x}, {y})");
}

enum Color { Red, Green, Blue }

class Program {
    static void Greet(string name) => Console.WriteLine($"Hi, {name}!");

    static int Square(int n) => n * n;

    static void Main() {
        string name = "Alice";
        int x       = 5;
        double y    = 3.14;
        bool active = true;

        Greet(name);
        Console.WriteLine($"Square of {x} is {Square(x)}");

        Point p = new Point { x = 3, y = 4 };
        p.Show();

        var nums = new List<int> { 1, 2, 3 };
        nums.ForEach(n => Console.Write($"{n} "));
        Console.WriteLine();

        var ages = new Dictionary<string, int> { ["Alice"] = 30, ["Bob"] = 25 };
        Console.WriteLine($"Bob is {ages["Bob"]} years old.");

        int count = 0;
        while (count < 3) {
            Console.WriteLine($"While loop: {count}");
            count++;
        }

        Color c = Color.Green;
        if (c == Color.Green) Console.WriteLine("Color is Green");
    }
}
