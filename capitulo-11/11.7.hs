module Main (main) where

import Distribution.Simple.Setup (sdistCommand)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

lerGabarito :: [Char] -> Integer -> Integer -> IO [Char]
lerGabarito gabarito qtdQuestoes questaoAtual = do
  if qtdQuestoes == 0
    then return gabarito
    else do
      putStrLn ("Digite o gabarito da questão " ++ show questaoAtual)
      resposta <- getChar
      erro <- getChar
      lerGabarito (resposta : gabarito) (qtdQuestoes - 1) (questaoAtual + 1)

obterGabaritoAluno :: [Char] -> Integer -> Integer -> IO [Char]
obterGabaritoAluno gabaritoAluno qtdQuestoes questaoAtual = do
  if qtdQuestoes == 0
    then return gabaritoAluno
    else do
      putStrLn ("Digite o gabarito da questão " ++ show questaoAtual)
      resposta <- getChar
      erro <- getChar
      obterGabaritoAluno (resposta : gabaritoAluno) (qtdQuestoes - 1) (questaoAtual + 1)

obterDadosAlunos :: Integer -> Integer -> [(Integer, [Char])] -> IO [(Integer, [Char])]
obterDadosAlunos qtdAlunos qtdQuestoes dadosAlunos = do
  if qtdAlunos == 0
    then return dadosAlunos
    else do
      putStrLn "Digite a matricula do aluno "
      matricula <- readLn
      respAluno <- obterGabaritoAluno [] qtdQuestoes 1
      obterDadosAlunos (qtdAlunos - 1) qtdQuestoes ((matricula, respAluno) : dadosAlunos)

somaPontos :: [Char] -> [Char] -> Integer -> Integer -> Integer -> Double
somaPontos gabaritoAluno gabaritoCorreto nota qtdQuestoes i =
  if qtdQuestoes == i
    then (fromIntegral nota / fromIntegral qtdQuestoes) * 10
    else do
      if (gabaritoAluno !! fromIntegral i) == (gabaritoCorreto !! fromIntegral i)
        then somaPontos gabaritoAluno gabaritoCorreto (nota + 1) qtdQuestoes (i + 1)
        else somaPontos gabaritoAluno gabaritoCorreto nota qtdQuestoes (i + 1)

obterResultadoAlunos :: [(Integer, [Char])] -> [Char] -> [(Integer, Double)] -> Integer -> Integer -> [(Integer, Double)]
obterResultadoAlunos dadosAlunos gabaritoCorreto resultadosAlunos qtdAlunos qtdQuestoes =
  if qtdAlunos == 0
    then resultadosAlunos
    else do
      let matricula = fst (head dadosAlunos)
      let nota = somaPontos (snd (head dadosAlunos)) gabaritoCorreto 0 qtdQuestoes 0
      obterResultadoAlunos (tail dadosAlunos) gabaritoCorreto ((matricula, nota) : resultadosAlunos) (qtdAlunos - 1) qtdQuestoes

obterPorcentagemAprovacao :: [(Integer, Double)] -> Integer -> Integer -> Double
obterPorcentagemAprovacao resultados contAprovados qtdAlunos =
  if qtdAlunos == 0
    then fromIntegral contAprovados
    else do
      let nota = snd (head resultados)
      if nota >= 7
        then obterPorcentagemAprovacao (tail resultados) (contAprovados + 1) (qtdAlunos - 1)
        else obterPorcentagemAprovacao (tail resultados) contAprovados (qtdAlunos - 1)

imprimeAlunos :: [(Integer, Double)] -> Integer -> IO ()
imprimeAlunos resultadosAlunos qtdAlunos = do
  if qtdAlunos == 0
    then return ()
    else do
      let matricula = fst (head resultadosAlunos)
      let nota = snd (head resultadosAlunos)
      putStrLn ("Matricula: " ++ show matricula ++ " Nota: " ++ show nota)
      imprimeAlunos (tail resultadosAlunos) (qtdAlunos - 1)

imprimeAlunosSemNota :: (Eq t, Num t, Show a1, Show a2) => [(a1, a2)] -> t -> IO ()
imprimeAlunosSemNota resultadosAlunos qtdAlunos = do
  if qtdAlunos == 0
    then return ()
    else do
      let matricula = fst (head resultadosAlunos)
      let nota = snd (head resultadosAlunos)
      putStrLn ("Matricula: " ++ show matricula ++ " Nota: " ++ show nota)
      imprimeAlunosSemNota (tail resultadosAlunos) (qtdAlunos - 1)

main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Digite a quantidade de questoes: "
  qtdQuestoes <- readLn
  gabaritoCorreto <- lerGabarito [] qtdQuestoes 1
  -- gabaritoAluno <- obterGabaritoAluno [] qtdQuestoes 1
  putStrLn "Digite a quantidade de alunos: "
  qtdAlunos <- readLn
  listaComDadoAlunos <- obterDadosAlunos qtdAlunos qtdQuestoes []
  let listaComResultadosAlunos = obterResultadoAlunos listaComDadoAlunos gabaritoCorreto [] qtdAlunos qtdQuestoes
  imprimeAlunosSemNota listaComDadoAlunos qtdAlunos
  imprimeAlunos listaComResultadosAlunos qtdAlunos
  let aprovados = obterPorcentagemAprovacao listaComResultadosAlunos 0 qtdAlunos
  let percent = (aprovados / fromIntegral qtdAlunos) * 100
  putStrLn (show percent ++ "% alunos aprovados!!!")
