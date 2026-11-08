-- =====================================================================
-- CAMADA 1: TIPOS E DADOS DO CAMPEONATO
-- =====================================================================
module Tipos where

import Data.List (sort)

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
