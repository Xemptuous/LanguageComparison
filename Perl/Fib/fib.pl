use strict;
use warnings;
use 5.010; # Enables 'say' function

sub fib {
    my ($n) = @_;

    return 0 if $n == 0;
    return 1 if $n == 1;
    return fib($n - 1) + fib($n - 2);
}

for my $i (0..30) {
    say "fib($i): " . fib($i);
}
