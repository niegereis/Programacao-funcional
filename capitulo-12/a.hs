saudacao :: String -> String
saudacao "Joana" = saudacaoLegal ++ " Joana!"
saudacao "Ferando" = saudacaoLegal ++ " Fernando!"
saudacao nome = saudacaoInfeliz ++ " " ++ nome

saudacaoLegal = "Olá! Que bom encontrar você, "

saudacaoInfeliz = "Oh! Pfft. É você, "