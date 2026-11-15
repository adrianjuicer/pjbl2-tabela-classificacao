-- =====================================================================
-- PjBL 2: Tabela de Classificacao de Campeonato (Haskell)
-- Arquivo unico, rota playground: as secoes abaixo seguem a mesma ordem
-- dos modulos da secao 6 do enunciado.
--
--   Camada 1  tipos e consultas basicas
--   Camada 2  classificacao com funcoes de ordem superior
--   Dados     o campeonato de teste, como texto dentro do codigo
--   Main      provisorio, ate a Camada 3 existir
-- =====================================================================
module Main where

import Data.List (sort, sortBy)
import Data.Ord (comparing, Down (..))

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

-- =====================================================================
-- MAIN: provisorio. A demonstracao das tres camadas chega junto com a
-- Camada 3, que e quem sabe transformar o texto de dadosCampeonato em
-- partidas. Por enquanto a prova de que as camadas 1 e 2 funcionam esta
-- no Testes.hs.
-- =====================================================================

main :: IO ()
main = putStrLn "Camadas 1 e 2 prontas. Rode o Testes.hs para ver os testes."
