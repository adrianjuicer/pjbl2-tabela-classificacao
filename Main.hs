-- =====================================================================
-- PjBL 2: Tabela de Classificacao de Campeonato (Haskell)
-- Arquivo unico, rota playground: as secoes abaixo seguem a mesma ordem
-- dos modulos da secao 6 do enunciado.
--
--   Camada 1  tipos e consultas basicas
--   Camada 2  classificacao com funcoes de ordem superior
--   Camada 3  leitura, validacao e saida
--   Dados     o campeonato de teste, como texto dentro do codigo
--   Main      a demonstracao das tres camadas
-- =====================================================================
module Main where

import Data.List (sort, sortBy)
import Data.Ord (comparing, Down (..))
import Text.Read (readMaybe)

-- =====================================================================
-- CAMADA 1: TIPOS E DADOS DO CAMPEONATO
-- =====================================================================

type Time = String

data Resultado = Vitoria | Empate | Derrota
  deriving (Show, Eq)

data Partida = Partida
  { rodada        :: Int
  , mandante      :: Time
  , golsMandante  :: Int
  , golsVisitante :: Int
  , visitante     :: Time
  } deriving (Show, Eq)

-- Uma linha da tabela. Pontos e saldo sao calculados na Camada 2.
data Linha = Linha
  { time       :: Time
  , jogos      :: Int
  , vitorias   :: Int
  , empates    :: Int
  , derrotas   :: Int
  , golsPro    :: Int
  , golsContra :: Int
  } deriving (Show, Eq)

-- O time jogou essa partida?
participou :: Time -> Partida -> Bool
participou t p = elem t [mandante p, visitante p]

-- O resultado da partida para o time. Nothing se ele nao jogou.
resultadoPara :: Time -> Partida -> Maybe Resultado
resultadoPara t p
  | t == mandante p  = Just (comparar (golsMandante p) (golsVisitante p))
  | t == visitante p = Just (comparar (golsVisitante p) (golsMandante p))
  | otherwise        = Nothing

-- Compara os gols do time com os do adversario.
comparar :: Int -> Int -> Resultado
comparar meus deles
  | meus > deles  = Vitoria
  | meus == deles = Empate
  | otherwise     = Derrota

-- Todos os times, sem repetir, em ordem alfabetica.
times :: [Partida] -> [Time]
times ps = sort (foldr incluir [] (map mandante ps ++ map visitante ps))

-- Poe o time na lista so se ele ainda nao estiver nela.
incluir :: Time -> [Time] -> [Time]
incluir t lista = if elem t lista then lista else t : lista

-- =====================================================================
-- CAMADA 2: CLASSIFICACAO (sem recursao explicita)
-- =====================================================================

pontos :: Resultado -> Int
pontos Vitoria = 3
pontos Empate  = 1
pontos Derrota = 0

-- A linha de um time: pega so os jogos dele e soma um por um com foldl.
linhaDoTime :: [Partida] -> Time -> Linha
linhaDoTime ps t = foldl (somarJogo t) (Linha t 0 0 0 0 0 0) (filter (participou t) ps)

-- Soma uma partida na linha do time.
somarJogo :: Time -> Linha -> Partida -> Linha
somarJogo t l p =
  l { jogos      = jogos l + 1
    , vitorias   = vitorias l + conta Vitoria
    , empates    = empates l + conta Empate
    , derrotas   = derrotas l + conta Derrota
    , golsPro    = golsPro l + feitos
    , golsContra = golsContra l + sofridos
    }
  where
    conta r  = if resultadoPara t p == Just r then 1 else 0
    feitos   = if mandante p == t then golsMandante p else golsVisitante p
    sofridos = if mandante p == t then golsVisitante p else golsMandante p

pontosLinha :: Linha -> Int
pontosLinha l = pontos Vitoria * vitorias l + pontos Empate * empates l

saldo :: Linha -> Int
saldo l = golsPro l - golsContra l

-- Porcentagem dos pontos disputados que o time ganhou. 0 se nao jogou.
aproveitamento :: Linha -> Double
aproveitamento l
  | jogos l == 0 = 0
  | otherwise    = 100 * fromIntegral (pontosLinha l) / fromIntegral (3 * jogos l)

-- O aproveitamento do primeiro colocado, com todas as casas decimais.
-- Nothing para uma tabela vazia, em vez de quebrar com head.
aproveitamentoDoLider :: [Linha] -> Maybe Double
aproveitamentoDoLider []      = Nothing
aproveitamentoDoLider (l : _) = Just (aproveitamento l)

-- A tabela ordenada.
classificacao :: [Partida] -> [Linha]
classificacao ps = sortBy criterio (map (linhaDoTime ps) (times ps))

-- Pontos, vitorias, saldo, gols pro (maior primeiro, por isso o Down) e,
-- no fim, o nome. O <> so olha o proximo criterio quando o anterior empata.
criterio :: Linha -> Linha -> Ordering
criterio =
     comparing (Down . pontosLinha)
  <> comparing (Down . vitorias)
  <> comparing (Down . saldo)
  <> comparing (Down . golsPro)
  <> comparing time

-- So as partidas ate a rodada n.
ateRodada :: Int -> [Partida] -> [Partida]
ateRodada n = filter ((<= n) . rodada)

-- A partida com a maior diferenca de gols. Nothing se a lista for vazia.
maiorGoleada :: [Partida] -> Maybe Partida
maiorGoleada = foldr maior Nothing
  where
    maior p Nothing  = Just p
    maior p (Just q) = if diferenca p >= diferenca q then Just p else Just q
    diferenca x      = abs (golsMandante x - golsVisitante x)

-- Media de gols por partida. Nothing se a lista for vazia.
mediaGols :: [Partida] -> Maybe Double
mediaGols [] = Nothing
mediaGols ps = Just (fromIntegral (sum (map gols ps)) / fromIntegral (length ps))
  where
    gols p = golsMandante p + golsVisitante p

-- =====================================================================
-- CAMADA 3: LEITURA, VALIDACAO E SAIDA
-- =====================================================================

-- Cada erro guarda o numero da linha; TimeRepetidoNaRodada guarda a rodada e o
-- time. Os cinco primeiros sao os que o enunciado exige ("no minimo"); os dois
-- ultimos foram acrescentados pelo grupo, porque sem eles uma rodada 0 ou
-- negativa e um nome de time vazio passavam em silencio.
data Erro
  = CamposInvalidos Int
  | NumeroInvalido Int
  | PlacarNegativo Int
  | MesmoTime Int
  | TimeRepetidoNaRodada Int Time
  | RodadaInvalida Int
  | NomeVazio Int
  deriving (Show, Eq)

-- Quebra o texto em pedacos a cada caractere c.
-- O foldr anda da direita para a esquerda guardando (pedaco atual, pedacos prontos).
separar :: Char -> String -> [String]
separar c texto = atual : prontos
  where
    (atual, prontos) = foldr passo ("", []) texto
    passo x (pedaco, resto)
      | x == c    = ("", pedaco : resto)
      | otherwise = (x : pedaco, resto)

-- A linha numero n vira uma partida. O primeiro Left para tudo.
lerPartida :: Int -> String -> Either Erro Partida
lerPartida n linha = do
  (r, casa, gc, gf, fora) <- campos n (separar ';' linha)
  numeroRodada <- numero n r
  golsCasa     <- numero n gc
  golsFora     <- numero n gf
  validar n (Partida numeroRodada casa golsCasa golsFora fora)

-- A linha precisa ter exatamente 5 campos.
campos :: Int -> [String] -> Either Erro (String, String, String, String, String)
campos _ [r, casa, gc, gf, fora] = Right (r, casa, gc, gf, fora)
campos n _                       = Left (CamposInvalidos n)

-- Texto para numero com readMaybe. Se der Nothing, vira Left.
numero :: Int -> String -> Either Erro Int
numero n texto = maybe (Left (NumeroInvalido n)) Right (readMaybe texto)

-- As guardas seguem a ordem dos campos da linha: rodada, nomes, placar.
validar :: Int -> Partida -> Either Erro Partida
validar n p
  | rodada p < 1              = Left (RodadaInvalida n)
  | mandante p == ""          = Left (NomeVazio n)
  | visitante p == ""         = Left (NomeVazio n)
  | golsMandante p < 0        = Left (PlacarNegativo n)
  | golsVisitante p < 0       = Left (PlacarNegativo n)
  | mandante p == visitante p = Left (MesmoTime n)
  | otherwise                 = Right p

-- Numera as linhas e tira as vazias e os comentarios (#).
linhasNumeradas :: String -> [(Int, String)]
linhasNumeradas texto = filter (contaComoPartida . snd) (zip [1 ..] (separar '\n' texto))

contaComoPartida :: String -> Bool
contaComoPartida ""        = False
contaComoPartida ('#' : _) = False
contaComoPartida _         = True

-- Le tudo, para no primeiro erro, e depois confere time repetido na rodada.
lerCampeonato :: String -> Either Erro [Partida]
lerCampeonato texto = do
  partidas <- traverse (\(n, linha) -> lerPartida n linha) (linhasNumeradas texto)
  case repetidos partidas of
    []         -> Right partidas
    (erro : _) -> Left erro

-- Um erro para cada time que aparece de novo na mesma rodada.
repetidos :: [Partida] -> [Erro]
repetidos ps = snd (foldl verificar ([], []) ps)
  where
    verificar (vistos, erros) p =
      let novos = [(rodada p, mandante p), (rodada p, visitante p)]
          ja    = filter (\par -> elem par vistos) novos
      in (vistos ++ novos, erros ++ map (\(r, t) -> TimeRepetidoNaRodada r t) ja)

-- Todos os erros do texto, e nao so o primeiro.
todosOsErros :: String -> [Erro]
todosOsErros texto = erros ++ repetidos partidas
  where
    leituras          = map (\(n, linha) -> lerPartida n linha) (linhasNumeradas texto)
    (erros, partidas) = foldr dividir ([], []) leituras
    dividir (Left e)  (es, ps) = (e : es, ps)
    dividir (Right p) (es, ps) = (es, p : ps)

-- A maior goleada em texto. Nothing se nao houve partida.
-- O <$> aplica descrever DENTRO da caixa Maybe, sem tirar o valor dela:
-- Maybe como functor, que a secao 3.3 do enunciado pede.
descricaoGoleada :: [Partida] -> Maybe String
descricaoGoleada ps = descrever <$> maiorGoleada ps

-- Uma partida em uma linha: "Cascavel 0 x 4 Coritiba (rodada 5)".
descrever :: Partida -> String
descrever p =
  mandante p ++ " " ++ show (golsMandante p) ++ " x " ++ show (golsVisitante p)
    ++ " " ++ visitante p ++ " (rodada " ++ show (rodada p) ++ ")"

-- Tira o texto da caixa; se nao houver nada, devolve o aviso.
textoOu :: String -> Maybe String -> String
textoOu aviso Nothing      = aviso
textoOu _     (Just texto) = texto

-- Enche o texto com espacos a DIREITA ate ter n caracteres.
-- replicate com numero negativo devolve "", entao nome maior que n nao quebra.
esquerda :: Int -> String -> String
esquerda n texto = texto ++ replicate (n - length texto) ' '

-- Enche o texto com espacos a ESQUERDA ate ter n caracteres.
direita :: Int -> String -> String
direita n texto = replicate (n - length texto) ' ' ++ texto

-- O saldo sempre com sinal, para a coluna ficar legivel.
comSinal :: Int -> String
comSinal n = if n > 0 then '+' : show n else show n

-- Largura da coluna do time: o nome mais comprido, nunca menor que "Time".
-- foldr com max nao quebra na lista vazia, ao contrario de maximum.
larguraNome :: [Linha] -> Int
larguraNome linhas = foldr max 4 (map (length . time) linhas)

-- Aproveitamento com uma casa decimal, SEM printf (que esta fora dos modulos
-- permitidos pela secao 5) e SEM round: a conta e feita com div e mod sobre os
-- pontos, entao a casa decimal e truncada, nao arredondada.
aproveitamentoTexto :: Linha -> String
aproveitamentoTexto l
  | jogos l == 0 = "0.0"
  | otherwise    = show inteiro ++ "." ++ show decimo
  where
    disputados = 3 * jogos l
    inteiro    = (100 * pontosLinha l) `div` disputados
    decimo     = ((1000 * pontosLinha l) `div` disputados) `mod` 10

-- A tabela pronta para impressao: cabecalho, traco e uma linha por time.
-- Campeonato vazio devolve texto vazio, em vez de um cabecalho sem tabela.
tabelaFormatada :: [Linha] -> String
tabelaFormatada []     = ""
tabelaFormatada linhas = unlines (cabecalho larg : traco larg : corpo)
  where
    larg  = larguraNome linhas
    corpo = zipWith (formatar larg) [1 ..] linhas

cabecalho :: Int -> String
cabecalho larg =
  direita 3 "Pos" ++ "  " ++ esquerda larg "Time"
    ++ direita 4 "P" ++ direita 4 "J" ++ direita 4 "V" ++ direita 4 "E"
    ++ direita 4 "D" ++ direita 5 "GP" ++ direita 5 "GC" ++ direita 5 "SG"
    ++ direita 8 "%"

traco :: Int -> String
traco larg = replicate (length (cabecalho larg)) '-'

formatar :: Int -> Int -> Linha -> String
formatar larg posicao l =
  direita 3 (show posicao) ++ "  " ++ esquerda larg (time l)
    ++ direita 4 (show (pontosLinha l))
    ++ direita 4 (show (jogos l))
    ++ direita 4 (show (vitorias l))
    ++ direita 4 (show (empates l))
    ++ direita 4 (show (derrotas l))
    ++ direita 5 (show (golsPro l))
    ++ direita 5 (show (golsContra l))
    ++ direita 5 (comSinal (saldo l))
    ++ direita 8 (aproveitamentoTexto l ++ "%")

-- =====================================================================
-- DADOS: o campeonato de teste, como texto dentro do codigo
-- =====================================================================

-- 8 times, 7 rodadas, 28 partidas: turno unico completo, montado pelo
-- metodo do circulo, em que cada time joga exatamente uma vez por rodada
-- e uma vez contra cada adversario.
--
-- O que este campeonato cobre de proposito:
--   - um 0 x 0 (rodada 7, Coritiba x Arapongas);
--   - quatro vitorias por 3 gols ou mais (a maior e Cascavel 0 x 4 Coritiba);
--   - tres times empatados em 11 pontos e 3 vitorias: Toledo passa a frente
--     do Coritiba pelos gols pro (mesmo saldo), e o Coritiba passa a frente
--     do Arapongas pelo saldo;
--   - o Cascavel com mais vitorias que o Azuriz e ainda assim atras dele,
--     porque pontos vem antes de vitorias;
--   - o Cianorte sem nenhuma vitoria;
--   - a tabela da rodada 3 bem diferente da final.
dadosCampeonato :: String
dadosCampeonato = unlines
  [ "# rodada;mandante;gols mandante;gols visitante;visitante"
  , "1;Arapongas;4;1;Toledo"
  , "1;Londrina;2;3;Athletico"
  , "1;Azuriz;2;2;Coritiba"
  , "1;Cianorte;0;1;Cascavel"
  , ""
  , "2;Toledo;2;4;Athletico"
  , "2;Azuriz;2;2;Arapongas"
  , "2;Londrina;2;1;Cascavel"
  , "2;Cianorte;0;2;Coritiba"
  , ""
  , "3;Azuriz;1;1;Toledo"
  , "3;Athletico;1;0;Cascavel"
  , "3;Cianorte;1;2;Arapongas"
  , "3;Londrina;1;0;Coritiba"
  , ""
  , "4;Toledo;2;0;Cascavel"
  , "4;Cianorte;0;1;Azuriz"
  , "4;Athletico;3;4;Coritiba"
  , "4;Londrina;3;4;Arapongas"
  , ""
  , "5;Cianorte;1;4;Toledo"
  , "5;Cascavel;0;4;Coritiba"
  , "5;Londrina;1;2;Azuriz"
  , "5;Athletico;3;1;Arapongas"
  , ""
  , "6;Toledo;4;1;Coritiba"
  , "6;Londrina;1;1;Cianorte"
  , "6;Cascavel;3;1;Arapongas"
  , "6;Athletico;2;2;Azuriz"
  , ""
  , "7;Londrina;1;1;Toledo"
  , "7;Coritiba;0;0;Arapongas"
  , "7;Athletico;1;0;Cianorte"
  , "7;Cascavel;2;1;Azuriz"
  ]

-- Um erro de cada tipo, com o numero da linha ao lado:
--   3  NumeroInvalido         "dois" no lugar do placar
--   4  CamposInvalidos        falta um campo
--   5  PlacarNegativo         -1 gol
--   6  MesmoTime              o Azuriz jogando contra o Azuriz
--   8  RodadaInvalida         rodada 0
--   9  NomeVazio              mandante sem nome
--   -  TimeRepetidoNaRodada   a linha 7 poe o Coritiba de novo na rodada 1
-- As linhas 2 e 7 sao as unicas que viram partida, e e entre elas que aparece
-- o time repetido na rodada.
dadosComErros :: String
dadosComErros = unlines
  [ "# rodada;mandante;gols mandante;gols visitante;visitante"
  , "1;Athletico;2;1;Coritiba"
  , "1;Londrina;dois;0;Cascavel"
  , "1;Cianorte;1;Azuriz"
  , "2;Coritiba;-1;0;Londrina"
  , "2;Azuriz;1;1;Azuriz"
  , "1;Coritiba;0;0;Cascavel"
  , "0;Toledo;1;0;Cianorte"
  , "3;;2;1;Londrina"
  ]

-- =====================================================================
-- MAIN: mostra as tres camadas, na ordem pedida pela secao 5 do
-- enunciado. E a unica funcao com IO.
-- =====================================================================

main :: IO ()
main = do
  case lerCampeonato dadosCampeonato of
    Left erro -> putStrLn ("Erro no campeonato: " ++ show erro)
    Right partidas -> do
      putStrLn "=== 1. Classificacao final ==="
      putStr (tabelaFormatada (classificacao partidas))
      putStrLn ""
      putStrLn "Legenda: P pontos, J jogos, V vitorias, E empates, D derrotas,"
      putStrLn "         GP gols pro, GC gols contra, SG saldo, % aproveitamento."
      putStrLn ""
      putStrLn "=== 2. Classificacao apos a rodada 3 ==="
      putStr (tabelaFormatada (classificacao (ateRodada 3 partidas)))
      putStrLn ""
      putStrLn "=== 3. Estatisticas do campeonato ==="
      putStrLn ("Times: " ++ show (length (times partidas))
                 ++ " | Partidas: " ++ show (length partidas))
      putStrLn ("Maior goleada: " ++ textoOu "nenhuma partida" (descricaoGoleada partidas))
      putStrLn ("Media de gols por partida: "
                 ++ maybe "nenhuma partida" show (mediaGols partidas))
      putStrLn ("Aproveitamento do lider, com todas as casas: "
                 ++ maybe "campeonato vazio" show
                          (aproveitamentoDoLider (classificacao partidas)))
      putStrLn ""
  putStrLn "=== 4. Entrada com erros ==="
  mapM_ print (todosOsErros dadosComErros)
