with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
    function fib(n: in integer) return integer is
    begin
        if n = 0 then return 0;
        elsif n = 1 then return 1;
        else return fib(n - 1) + fib(n - 2);
        end if;
    end fib;

    Nth_Term : integer;

begin
    for i in 0 .. 30 loop
        Ada.Text_IO.Put("fib(");
        Ada.Integer_Text_IO.Put(i);
        Ada.Text_IO.Put("): ");
        Ada.Integer_Text_IO.Put(fib(i));
        Ada.Text_IO.Put_Line("");
    end loop;
end Main;

