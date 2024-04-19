data Casa
  = Numero Int
  | Nome String
  deriving (Show)

data Endereco = Endereco String String Casa

transforma :: Casa -> String
transforma (Nome n) = n
transforma (Numero n) = show n