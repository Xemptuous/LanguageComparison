DECLARE FUNCTION fib (n AS INTEGER) AS INTEGER
DIM i AS INTEGER


FOR i = 0 TO 30
    PRINT "fib(";i;"): "; fib(i); CHR$(10);
NEXT i
END

FUNCTION fib (n AS INTEGER) AS INTEGER
    IF n = 0 THEN
        fib = 0
    ELSEIF n = 1 THEN
        fib = 1
    ELSE
        fib = fib(n - 1) + fib(n - 2)
    END IF
END FUNCTION
