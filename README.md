# PjBL 2: Tabela de Classificação de Campeonato (Haskell)

## Integrantes

- Adrian Arthur Carvalho Stoco
- Luan Eduardo Fermino Orlovski
- Matheus Francisco Trevisan Del Zotto
- Matheus Silva de Quadros

## Ambiente

**Playground online, [play.haskell.org](https://play.haskell.org)**, que a seção 2 do enunciado
manda o grupo indicar. Por isso a entrega é um arquivo único, como a seção 6 autoriza para quem
usa o playground.

## Arquivos

- `Main.hs`: o programa, com as três camadas em seções comentadas.
- `Testes.hs`: o mesmo programa mais a bateria de testes.
- `docs/decisoes.md`: decisões de modelagem e limitações conhecidas.
- `README.md`: este arquivo.

## Como rodar

Abra [play.haskell.org](https://play.haskell.org), apague o editor, cole **todo** o `Main.hs` e
clique em **Run**. Para ver os testes, mesma coisa com o `Testes.hs`. Cada arquivo roda sozinho.

## O que o `Main.hs` mostra

1. a classificação final, em tabela alinhada;
2. a classificação até a rodada 3, diferente da final;
3. as estatísticas da Camada 2: maior goleada, média de gols e aproveitamento do líder;
4. uma entrada com erros, listando todos eles.

O `Testes.hs` imprime `OK` ou `FALHOU` por linha, nos 52 casos: os obrigatórios da seção 7 e os
casos de borda da seção 4.

## Campeonato de teste e formato da entrada

8 times, 7 rodadas, 28 partidas: turno único completo (a seção 3.1 pede 6 times e 5 rodadas). Uma
partida por linha, campos separados por `;`; linhas em branco e linhas que começam com `#` são
ignoradas. Os dados ficam no próprio código, como uma `String`, porque o playground não tem
entrada pelo teclado.

```
# rodada;mandante;gols mandante;gols visitante;visitante
1;Arapongas;4;1;Toledo
```
