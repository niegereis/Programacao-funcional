type Nome = String

type Preco = Int

type Codigo = Int

type Mercadorias = [(Codigo, Nome, Preco)]

tabelaMercadorias :: Mercadorias
tabelaMercadorias =
  [ (4719, "Fish Fingers", 121),
    (5643, "Nappies", 1010),
    (3814, "Orange Jelly", 56),
    (1111, "Hula Hoops", 21),
    (1112, "Hula Hoops (Giant)", 133),
    (1234, "Dry Sherry, 1lt", 540)
  ]

type Carrinho = [Codigo]

type Conta = [(Nome, Preco)]

formataCentavos :: Preco -> String
formataCentavos preco = precoString
  where
    precoString = show reais ++ "." ++ (if centavos < 10 then "0" else "") ++ show centavos
      where
        reais = div preco 100
        centavos = mod preco 100

tamanhoLinha :: Int
tamanhoLinha = 30

formataLinha :: (Nome, Preco) -> String
formataLinha (nome, preco) = linhaConta
  where
    linhaConta = nome ++ replicate qtdPontos '.' ++ precoString ++ "\n"
    qtdPontos = tamanhoLinha - length nome + length precoString
    precoString = formataCentavos preco

formataLinhas :: [(Nome, Preco)] -> String
formataLinhas listaCompras = show linhas
  where
    linhas = foldl (++) "" (map formataLinha listaCompras)

formataTotal :: Preco -> String
formataTotal preco = precoString
  where
    precoString = "\nTotal" ++ replicate qtdPontos '.' ++ valString
    qtdPontos = tamanhoLinha - length "\nTotal" + length valString
    valString = formataCentavos preco

formataConta :: Conta -> String
formataConta listaCompra = linhas ++ "\n" ++ formataTotal valorTotal
  where
    linhas = formataLinhas listaCompra
    valorTotal = foldl (+) 0 (map snd listaCompra)

calculaTotal :: Conta -> Preco
calculaTotal listaCompras = precoTotal
  where
    precoTotal = sum (map snd listaCompras)

procuraCodigo :: Mercadorias -> Codigo -> (Nome, Preco)
procuraCodigo [] _ = ("Unknown item", 0)
procuraCodigo ((codigo, nome, preco) : restoLista) cod
  | codigo == cod = (nome, preco)
  | otherwise = procuraCodigo restoLista cod

criaConta :: Mercadorias -> Carrinho -> Conta
criaConta tabela listaCod = map (procuraCodigo tabela) listaCod

fazCompra :: Mercadorias -> Carrinho -> String
fazCompra tabelaMercadorias listaCodigos = formataConta (criaConta tabelaMercadorias listaCodigos)