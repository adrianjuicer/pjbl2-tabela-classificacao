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

## Camada 3: leitura, validação e saída

- `lerPartida` é um bloco `do` em `Either`: o primeiro `Left` para a linha, sem `case` aninhado.
- Números com `readMaybe`, nunca `read`; o `Nothing` vira `Left` pela função `maybe`.
- `lerCampeonato` usa `traverse` e para no primeiro erro; `todosOsErros` junta todos.
- A numeração da linha vem **antes** do filtro de `#` e linhas vazias, para bater com o editor.
- **Sete erros:** os cinco exigidos mais `RodadaInvalida` e `NomeVazio`, que o "no mínimo"
  permite. Sem eles, `0;Toledo;1;0;Cianorte` entrava na tabela calada.
- **Tabela alinhada sem `printf`**, que está fora dos módulos da seção 5: `replicate` com `++`, e
  a largura da coluna sai de `foldr max 4`, não de `maximum`.
- **A coluna `%` é truncada de propósito**, com `div` e `mod`: uma casa decimal sem `printf`
  exigiria `round`, que a disciplina não ensinou. Não é a armadilha da seção 4, porque o *valor*
  vem de `fromIntegral` e `/`, impresso inteiro nas estatísticas.
- **Casos de borda, todos sem exceção e com teste:** `tabelaFormatada []`, `classificacao []`,
  `times []`, `lerCampeonato ""`, `todosOsErros ""` e o aproveitamento de quem não jogou nenhum
  jogo, que não divide por zero.
- Os dados ficam no código, como `String`: o playground não tem teclado. `main` é o único com `IO`.

## Campeonato de teste

8 times, 7 rodadas, 28 partidas: turno único completo, conferido à mão. Cobre um 0 x 0, quatro
vitórias por 3 gols ou mais, desempate pelo saldo e pelos gols pró, um time sem nenhuma vitória e
uma tabela da rodada 3 diferente da final.

## Limitações conhecidas

- Confronto direto não é critério de desempate: depois dos gols pró decide o nome do time.
- Espaços contam no nome: `"Coritiba "` e `"Coritiba"` seriam times diferentes.
- A coluna `%` é truncada, não arredondada. O valor exato aparece nas estatísticas.
- `TimeRepetidoNaRodada` acusa repetição dentro da mesma rodada, que é o que o enunciado pede, e
  não o mesmo confronto duas vezes no campeonato.
- Não há leitura de arquivo: trocar o campeonato é editar `dadosCampeonato` no código.
