data Expre 
  = Cte Double -- contante
  | Var String -- variável
  | Som Expre Expre -- soma
  | Sub Expre Expre -- subtração
  | Mul Expre Expre -- multiplicação
  | Div Expre Expre -- divisão
  | Neg Expre -- simétrico
  deriving (Show)


contaCte :: Expre -> Double
contaCte (Var _) = 0
contaCte (Cte val) = val
contaCte (Neg expre) = contaCte expre
contaCte (Som expre1 expre2) = contaCte expre1 + contaCte expre2
contaCte (Sub expre1 expre2) = contaCte expre1 + contaCte expre2
contaCte (Mul expre1 expre2) = contaCte expre1 + contaCte expre2
contaCte (Div expre1 expre2) = contaCte expre1 + contaCte expre2

-- Exemplo 
-- contaCte (Mul (Neg (Cte 5.6)) (Som (Cte 1.4) (Cte 2))) = 3
