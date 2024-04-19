-- calcPontos :: (Num a, Ord a) => [a] -> a
-- calcPontos (p : ps) =
--   if null ps
--     then p
--     else p + calcPontos ps

melhorouPontuacao :: (Num a, Ord a) => [a] -> [a] -> Bool
melhorouPontuacao partida1 partida2 = totalPartida2 > totalPartida1
  where
    totalPartida1 = sum partida1
    totalPartida2 = sum partida2

-- [ 22, 14, 26, 18, 17, 32, 26, 10, 21, 28 ] [ 20, 16, 18, 20, 24, 22, 24, 20, 12, 16 ]