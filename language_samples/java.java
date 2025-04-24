import java.util.*;

class Point {
    int x, y;
    Point(int x, int y) { this.x = x; this.y = y; }
    void show() {
        System.out.println("(" + x + ", " + y + ")");
    }
}

enum Color { RED, GREEN, BLUE }

public class Showcase {
    static void greet(String name) {
        System.out.println("Hi, " + name + "!");
    }

    static int square(int n) {
        return n * n;
    }

    public static void main(String[] args) {
        String name = "Alice";
        int x = 5;
        double y = 3.14;
        boolean active = true;

        greet(name);
        System.out.println("Square of " + x + " is " + square(x));

        Point p = new Point(3, 4);
        p.show();

        List<Integer> nums = Arrays.asList(1, 2, 3);
        for (int n : nums) System.out.print(n + " ");
        System.out.println();

        Map<String, Integer> ages = new HashMap<>();
        ages.put("Alice", 30);
        ages.put("Bob", 25);
        System.out.println("Bob is " + ages.get("Bob") + " years old.");

        int count = 0;
        while (count < 3) {
            System.out.println("While loop: " + count);
            count++;
        }

        Color c = Color.GREEN;
        if (c == Color.GREEN)
            System.out.println("Color is Green");
    }
}

