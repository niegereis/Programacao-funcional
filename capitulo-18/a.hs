[return 18.1, readLn, do {x <- readLn; return (2*x :: Float)}]

(Monad m, Fractional m, Read a) => [m, IO a, IO Float]
