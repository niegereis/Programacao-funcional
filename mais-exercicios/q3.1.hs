data DescMov = Abertura | Saque | Depósito
              deriving (Eq, Ord, Show)
type Data = String
type Movimento = (Data, DescMov, Double, Double)
type Histórico = [ Movimento ]
type Conta = (Integer, Histórico)
type Carteira = [ Conta ]

exemplo :: Carteira
-- exemplo = [ (10, [ ("6/6/18", Depósito, 20, 90)
--                   , ("6/5/18", Saque , 80, 70)
--                   , ("3/4/18", Depósito, 50, 150)
--                   , ("1/2/18", Abertura, 100, 100)
--                   ] )
--             , (20, [ ("5/5/18", Depósito, 35, 50)
--                   , ("9/2/18", Saque, 5, 15)
--                   , ("4/2/18", Depósito, 2, 20)
--                   , ("4/2/18", Depósito, 18, 18)
--                   , ("3/2/18", Abertura, 0, 0)
--                   ] )
--             , (30, [ ("9/2/18", Saque, 8, 2)
--                   , ("3/5/18", Abertura, 10, 10)
--                   ] )
-- ]

somaDepositos :: Carteira -> Double
somaDepositos carteira = soma
  where
    listaDeHistoricos = map snd carteira 
    listaDeMovimentos = concat listaDeHistoricos
    listaDeDepósitos =  filter (\( _, deposito, _, _) -> deposito == Depósito) listaDeMovimentos
    listaDeValores = map (\( _, _, val, _) -> val) listaDeDepósitos
    soma = sum listaDeValores


saldoTotal :: Carteira -> Double
saldoTotal carteira = soma
  where 
    listaDeHistoricos = map (head . snd) carteira
    listaDeSaldoBanco = map (\( _, _, _, val) -> val) listaDeHistoricos
    soma = sum listaDeSaldoBanco

saldoTotal' :: Carteira -> Double
saldoTotal' carteira = foldl1 (+) listaDeSaldoBanco
  where
    listaDeHistoricos = map (head . snd) carteira
    listaDeSaldoBanco = map (\( _, _, _, val) -> val) listaDeHistoricos

