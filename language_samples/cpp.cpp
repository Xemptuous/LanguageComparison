#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

struct Point {
    int x, y;
    void show() const { std::cout << "(" << x << ", " << y << ")\n"; }
};

enum class Color { Red, Green, Blue };

void greet(const std::string& name) {
    std::cout << "Hi, " << name << "!\n";
}

int square(int n) {
    return n * n;
}

int main() {
    std::string name = "Alice";
    int x            = 5;
    double y         = 3.14;
    bool active      = true;

    greet(name);
    std::cout << "Square of " << x << " is " << square(x) << "\n";

    Point p{3, 4};
    p.show();

    std::vector<int> nums = {1, 2, 3};
    for (int n : nums)
        std::cout << n << " ";
    std::cout << "\n";

    std::unordered_map<std::string, int> ages = {
        {"Alice", 30},
        {"Bob",   25}
    };
    std::cout << "Bob is " << ages["Bob"] << " years old.\n";

    int count = 0;
    while (count < 3) {
        std::cout << "While loop: " << count << "\n";
        ++count;
    }

    Color c = Color::Green;
    if (c == Color::Green) {
        std::cout << "Color is Green\n";
    }
    return 0;
}
