distDoisPontos :: (Floating a) => (a, a, a) -> (a, a, a) -> a
distDoisPontos (x1, y1, z1) (x2, y2, z2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2)