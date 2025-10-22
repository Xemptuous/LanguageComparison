program fibonacci;
var
  i: LongInt;

function fib(n: LongInt): LongInt;
begin
  if n <= 1 then
    fib := n
  else
    fib := fib(n - 1) + fib(n - 2);
end;

begin
  for i := 0 to 30 do
  begin
    writeln('fib(', i, '): ', fib(i));
  end;
end.
