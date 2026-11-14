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
  <> comparing saldo
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
-- TESTES: os casos obrigatorios da secao 7 do enunciado, mais os casos
-- de borda da tabela de armadilhas da secao 4 (campeonato vazio, lista
-- vazia, time que nao jogou, divisao por zero no aproveitamento).
--
-- Cada linha da saida comeca com OK ou com FALHOU.
-- =====================================================================

testar :: (Eq a, Show a) => String -> a -> a -> IO ()
testar nome obtido esperado
  | obtido == esperado = putStrLn ("OK      " ++ nome)
  | otherwise          = putStrLn ("FALHOU  " ++ nome ++ ": " ++ show obtido)

-- As 28 partidas de dadosCampeonato, escritas a mao.
partidasDeTeste :: [Partida]
partidasDeTeste =
  [ Partida 1 "Arapongas" 4 1 "Toledo"
  , Partida 1 "Londrina" 2 3 "Athletico"
  , Partida 1 "Azuriz" 2 2 "Coritiba"
  , Partida 1 "Cianorte" 0 1 "Cascavel"
  , Partida 2 "Toledo" 2 4 "Athletico"
  , Partida 2 "Azuriz" 2 2 "Arapongas"
  , Partida 2 "Londrina" 2 1 "Cascavel"
  , Partida 2 "Cianorte" 0 2 "Coritiba"
  , Partida 3 "Azuriz" 1 1 "Toledo"
  , Partida 3 "Athletico" 1 0 "Cascavel"
  , Partida 3 "Cianorte" 1 2 "Arapongas"
  , Partida 3 "Londrina" 1 0 "Coritiba"
  , Partida 4 "Toledo" 2 0 "Cascavel"
  , Partida 4 "Cianorte" 0 1 "Azuriz"
  , Partida 4 "Athletico" 3 4 "Coritiba"
  , Partida 4 "Londrina" 3 4 "Arapongas"
  , Partida 5 "Cianorte" 1 4 "Toledo"
  , Partida 5 "Cascavel" 0 4 "Coritiba"
  , Partida 5 "Londrina" 1 2 "Azuriz"
  , Partida 5 "Athletico" 3 1 "Arapongas"
  , Partida 6 "Toledo" 4 1 "Coritiba"
  , Partida 6 "Londrina" 1 1 "Cianorte"
  , Partida 6 "Cascavel" 3 1 "Arapongas"
  , Partida 6 "Athletico" 2 2 "Azuriz"
  , Partida 7 "Londrina" 1 1 "Toledo"
  , Partida 7 "Coritiba" 0 0 "Arapongas"
  , Partida 7 "Athletico" 1 0 "Cianorte"
  , Partida 7 "Cascavel" 2 1 "Azuriz"
  ]

-- Tabela final conferida a mao (time, J, V, E, D, gols pro, gols contra).
-- Os tres empatados em 11 pontos e 3 vitorias: Toledo e Coritiba tem o mesmo
-- saldo (+3) e os gols pro decidem (15 contra 13); entre Coritiba e Arapongas
-- decide o saldo (+3 contra +1). O Cascavel tem 3 vitorias e fica atras do
-- Azuriz, que tem 2, porque o Azuriz somou 4 empates.
tabelaFinal :: [Linha]
tabelaFinal =
  [ Linha "Athletico" 7 5 1 1 17 11
  , Linha "Toledo"    7 3 2 2 15 12
  , Linha "Coritiba"  7 3 2 2 13 10
  , Linha "Arapongas" 7 3 2 2 14 13
  , Linha "Azuriz"    7 2 4 1 11 10
  , Linha "Cascavel"  7 3 0 4  7 11
  , Linha "Londrina"  7 2 2 3 11 12
  , Linha "Cianorte"  7 0 1 6  3 12
  ]

-- Uma linha de time que nao jogou, para os casos de divisao por zero.
semJogos :: Linha
semJogos = Linha "Novato" 0 0 0 0 0 0

main :: IO ()
main = do
  let jogo   = Partida 1 "Arapongas" 4 1 "Toledo"
      linhaDe = linhaDoTime partidasDeTeste
      ordemFinal = map time (classificacao partidasDeTeste)
      so ts = filter (\t -> elem t ts) ordemFinal

  putStrLn "--- Camada 1: tipos e consultas basicas ---"
  testar "resultadoPara mandante" (resultadoPara "Arapongas" jogo) (Just Vitoria)
  testar "resultadoPara visitante" (resultadoPara "Toledo" jogo) (Just Derrota)
  testar "resultadoPara quem nao jogou" (resultadoPara "Londrina" jogo) Nothing
  testar "participou: o mandante jogou" (participou "Arapongas" jogo) True
  testar "participou: o visitante jogou" (participou "Toledo" jogo) True
  testar "participou: quem nao estava na partida" (participou "Londrina" jogo) False
  testar "times sem repeticao e em ordem alfabetica" (times partidasDeTeste)
    ["Arapongas", "Athletico", "Azuriz", "Cascavel", "Cianorte", "Coritiba", "Londrina", "Toledo"]
  testar "times de campeonato vazio" (times []) []

  putStrLn "--- Camada 2: classificacao e estatisticas ---"
  testar "pontos da vitoria" (pontos Vitoria) 3
  testar "pontos do empate" (pontos Empate) 1
  testar "pontos da derrota" (pontos Derrota) 0
  testar "classificacao final igual a tabela conferida a mao"
    (classificacao partidasDeTeste) tabelaFinal

  -- Os desempates provados no mesmo dado: primeiro que o criterio anterior
  -- realmente empatou, depois que o criterio seguinte difere e decide a ordem.
  testar "desempate 1: Coritiba e Arapongas empatam em pontos e em vitorias"
    (map (\l -> (pontosLinha l, vitorias l)) [linhaDe "Coritiba", linhaDe "Arapongas"])
    [(11, 3), (11, 3)]
  testar "desempate 1: o SALDO difere (+3 contra +1) e decide a ordem"
    (saldo (linhaDe "Coritiba"), saldo (linhaDe "Arapongas"), so ["Coritiba", "Arapongas"])
    (3, 1, ["Coritiba", "Arapongas"])
  testar "desempate 2: Toledo e Coritiba empatam em pontos, vitorias e saldo"
    (map (\l -> (pontosLinha l, vitorias l, saldo l)) [linhaDe "Toledo", linhaDe "Coritiba"])
    [(11, 3, 3), (11, 3, 3)]
  testar "desempate 2: os GOLS PRO diferem (15 contra 13) e decidem a ordem"
    (golsPro (linhaDe "Toledo"), golsPro (linhaDe "Coritiba"), so ["Toledo", "Coritiba"])
    (15, 13, ["Toledo", "Coritiba"])
  testar "pontos vem antes de vitorias: Cascavel tem 3 V e fica atras do Azuriz, com 2 V"
    ( (vitorias (linhaDe "Cascavel"), pontosLinha (linhaDe "Cascavel"))
    , (vitorias (linhaDe "Azuriz"), pontosLinha (linhaDe "Azuriz"))
    , so ["Azuriz", "Cascavel"] )
    ((3, 9), (2, 10), ["Azuriz", "Cascavel"])

  testar "classificacao ate a rodada 3 (diferente da final)"
    (map time (classificacao (ateRodada 3 partidasDeTeste)))
    ["Athletico", "Arapongas", "Londrina", "Coritiba", "Cascavel", "Azuriz", "Toledo", "Cianorte"]
  testar "ateRodada 0 nao devolve nenhuma partida" (ateRodada 0 partidasDeTeste) []
  testar "classificacao de campeonato vazio" (classificacao []) []
  testar "aproveitamento de quem nao jogou e 0 (sem divisao por zero)"
    (aproveitamento semJogos) 0
  testar "aproveitamento do lider: 16 de 21 pontos disputados"
    (aproveitamentoDoLider (classificacao partidasDeTeste))
    (Just (100 * 16 / 21 :: Double))
  testar "aproveitamentoDoLider de tabela vazia" (aproveitamentoDoLider []) Nothing
  testar "maiorGoleada do campeonato de teste"
    (maiorGoleada partidasDeTeste) (Just (Partida 5 "Cascavel" 0 4 "Coritiba"))
  testar "maiorGoleada com empate de diferenca fica com a primeira da lista"
    (maiorGoleada [Partida 1 "Toledo" 3 0 "Azuriz", Partida 1 "Cianorte" 0 3 "Cascavel"])
    (Just (Partida 1 "Toledo" 3 0 "Azuriz"))
  testar "maiorGoleada []" (maiorGoleada []) Nothing
  testar "mediaGols do campeonato de teste (91 gols em 28 partidas)"
    (mediaGols partidasDeTeste) (Just 3.25)
  testar "mediaGols []" (mediaGols []) Nothing
