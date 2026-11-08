-- =====================================================================
-- TESTES: os casos obrigatorios da secao 7 do enunciado
-- =====================================================================
module Main (main) where

import Tipos

testar :: (Eq a, Show a) => String -> a -> a -> IO ()
testar nome obtido esperado
  | obtido == esperado = putStrLn ("OK      " ++ nome)
  | otherwise          = putStrLn ("FALHOU  " ++ nome ++ ": " ++ show obtido)

-- As 15 partidas de dadosCampeonato, escritas a mao.
partidas :: [Partida]
partidas =
  [ Partida 1 "Athletico" 2 1 "Coritiba"
  , Partida 1 "Londrina" 0 0 "Cascavel"
  , Partida 1 "Cianorte" 1 3 "Azuriz"
  , Partida 2 "Athletico" 4 0 "Londrina"
  , Partida 2 "Coritiba" 2 2 "Cianorte"
  , Partida 2 "Cascavel" 1 0 "Azuriz"
  , Partida 3 "Athletico" 1 1 "Cascavel"
  , Partida 3 "Coritiba" 3 1 "Azuriz"
  , Partida 3 "Londrina" 2 1 "Cianorte"
  , Partida 4 "Athletico" 0 1 "Cianorte"
  , Partida 4 "Coritiba" 2 0 "Cascavel"
  , Partida 4 "Londrina" 1 1 "Azuriz"
  , Partida 5 "Athletico" 2 0 "Azuriz"
  , Partida 5 "Coritiba" 1 1 "Londrina"
  , Partida 5 "Cascavel" 2 2 "Cianorte"
  ]

main :: IO ()
main = do
  -- Camada 1
  let jogo = Partida 1 "Athletico" 2 1 "Coritiba"
  testar "resultadoPara mandante" (resultadoPara "Athletico" jogo) (Just Vitoria)
  testar "resultadoPara visitante" (resultadoPara "Coritiba" jogo) (Just Derrota)
  testar "resultadoPara quem nao jogou" (resultadoPara "Londrina" jogo) Nothing
  testar "times sem repeticao" (times partidas)
    ["Athletico", "Azuriz", "Cascavel", "Cianorte", "Coritiba", "Londrina"]
