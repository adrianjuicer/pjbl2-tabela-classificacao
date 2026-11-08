-- =====================================================================
-- DADOS: o campeonato de teste, como texto dentro do codigo
-- =====================================================================
module Dados where

-- 6 times, 5 rodadas, 15 partidas. Tem um 0 x 0 (rodada 1), um 4 x 0
-- (rodada 2), e Cascavel e Londrina terminam empatados em pontos.
dadosCampeonato :: String
dadosCampeonato = unlines
  [ "# rodada;mandante;gols mandante;gols visitante;visitante"
  , "1;Athletico;2;1;Coritiba"
  , "1;Londrina;0;0;Cascavel"
  , "1;Cianorte;1;3;Azuriz"
  , ""
  , "2;Athletico;4;0;Londrina"
  , "2;Coritiba;2;2;Cianorte"
  , "2;Cascavel;1;0;Azuriz"
  , ""
  , "3;Athletico;1;1;Cascavel"
  , "3;Coritiba;3;1;Azuriz"
  , "3;Londrina;2;1;Cianorte"
  , ""
  , "4;Athletico;0;1;Cianorte"
  , "4;Coritiba;2;0;Cascavel"
  , "4;Londrina;1;1;Azuriz"
  , ""
  , "5;Athletico;2;0;Azuriz"
  , "5;Coritiba;1;1;Londrina"
  , "5;Cascavel;2;2;Cianorte"
  ]
