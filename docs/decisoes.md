# Decisões de modelagem

## Ambiente

GHC local 9.10.3, com um módulo por camada (seção 6 do enunciado).

## Camada 1

- `Partida` e `Linha` são records, para não trocar os gols do mandante com os do visitante.
- `Resultado` é um tipo soma: só existem `Vitoria`, `Empate` e `Derrota`.
- `Linha` não guarda pontos nem saldo. Eles são calculados por `pontosLinha` e `saldo`, então
  nunca ficam diferentes das vitórias e dos gols.
- `times` junta mandantes e visitantes, tira os repetidos com `foldr` e ordena com `sort`.
