# Decisões de modelagem

## Ambiente e estrutura

- **Playground** (play.haskell.org), indicado como a seção 2 exige. Compila também no GHC local
  9.10.3 com `-Wall` sem nenhum aviso, que usamos só para conferir.
- **Arquivo único**, que a seção 6 autoriza: o playground cola um arquivo por vez e não resolve
  `import` de módulo vizinho. O `Testes.hs` repete o programa pelo mesmo motivo, byte a byte.
- **`module Main where` sem lista de exportação**, senão o `-Wall` reclamaria de toda função
  auxiliar que o `main` não alcança.

## Camada 1: tipos e dados

- `Partida` e `Linha` são records, e não tuplas: não dá para trocar os gols dos dois times.
- `Resultado` é tipo soma, então só existem `Vitoria`, `Empate` e `Derrota`. Um `"v"` nem compila.
- `Linha` não guarda pontos nem saldo; `pontosLinha` e `saldo` calculam, e nunca discordam dela.
- `type Time = String` é o que a seção 3.1 pede; um `newtype` mudaria a assinatura exigida.
- `times` tira os repetidos com `foldr` e ordena com `sort`, em vez de `nub`.

## Camada 2: classificação

- **Sem recursão explícita**, como a seção 3.2 exige: `map`, `filter`, `foldl`, `foldr`, `sortBy`.
- `linhaDoTime` usa `foldl`, e não `foldr`: é um acumulador que anda na ordem das rodadas.
- **Desempate:** pontos, vitórias, saldo, gols pró, nome. Uma comparação só, com `comparing`,
  `Down` e `<>`, e não vários `sortBy` encadeados.
- `aproveitamento` usa `fromIntegral` e `/`, nunca `div`, que truncaria; com `jogos = 0` uma
  guarda devolve 0.
- `maiorGoleada` e `mediaGols` devolvem `Nothing` na lista vazia, em vez de `head` ou `maximum`.
