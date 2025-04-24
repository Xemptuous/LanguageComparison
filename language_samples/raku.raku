class Point {
    has Int $.x;
    has Int $.y;
    method show {
        say "($.x, $.y)";
    }
}

enum Color <Red Green Blue>;

sub greet($name) {
    say "Hi, $name!";
}

sub square($n) {
    $n * $n;
}

my $name = "Alice";
my $x = 5;
my $y = 3.14;
my $active = True;

greet($name);
say "Square of $x is { square($x) }";

my $p = Point.new(x => 3, y => 4);
$p.show;

my @nums = 1, 2, 3;
for @nums -> $n { print "$n " }
say "";

my %ages = Alice => 30, Bob => 25;
say "Bob is { %ages<Bob> } years old.";

my $count = 0;
while $count < 3 {
    say "While loop: $count";
    $count++;
}

my $c = Color::Green;
say "Color is Green" if $c == Color::Green;

