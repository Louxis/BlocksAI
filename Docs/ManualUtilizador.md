# Manual de utilizador

![Cover](https://i.imgur.com/cTWjasD.png)

**Realizado por**

- José Pereira nº 150221044
- Lyudmyla Todoriko nº 150221059

**Docentes** 

- Prof. Hugo Silva
- Prof. Joaquim Silva
- Eng. Filipe Mariano





Para a cadeira de Inteligência Artificial

<div style="page-break-after: always;"></div> 

## 1. Introdução

No âmbito da unidade curricular de Inteligência Artificial foi-nos proposta para esta primeira fase do projeto uma simulação de resolução de um puzzle , Blocks, utilizando algoritmos leccionados na devida unidade curricular.

O objetivo do programa realizado nesta fase é percorrer os diferentes tabuleiros, guardados num ficheiro fornecido pelo utilizador, com os diferentes algoritmos implementados.

Logo de início o utilizador deverá introduzir o caminho onde está  seu ficheiro e escolher o tabuleiro com o qual deseja fazer uma simulação.

Seguidamente será pedido ao utilizador que escolha que algoritmo deseja que percorra o tabuleiro escolhido e algumas informações adicionais, como heurísticas e profundidade máxima para determinados algoritmos.

Após o tabuleiro ser percorrido, o programa pede o caminho para o qual o utilizador deseja guardar o resultado e após este procedimento o  programa volta à mensagem inicial. 

## 2. Início e caminho de ficheiro

Para iniciar o programa, poderá certificar-se que os ficheiros que poderá utilizar para guardar os resultados estão vazios. Saltar este passo não irá afetar de forma alguma o funcionamento do programa. 

Deverá abrir o ficheiro programa.lisp e iniciar o programa com o comando (start). 

Após esse comando irá aparecer uma mensagem de apresentação, como a apresentada abaixo.

```
CL-USER 1 > (start)

Welcome to BlocksAI!
Please insert the filepath you want to simulate.

```

Nesta mensagem deverá introduzir o caminho do ficheiro no seguinte formato. Não se esqueça das aspas. Pode obter o caminho do seu ficheiro observando as propriedades do mesmo.

```
"D:/3_Ano/IA/BlocksAI/problemas.dat"
```

O ficheiro deverá ser composto por listas que representam os tabuleiros, separadas por um separador legal.

O programa irá verificar se os ficheiros necessários ao seu funcionamento estão compilados. Caso não estejam, o programa irá pedir que insira o caminho para esses ficheiros. Deverá proceder da mesma maneira como com o ficheiro que contém os tabuleiros, inserindo o nome dos ficheiros que o mesmo necessita.

```
Please insert the filepath to the file with PUZZLE logic:
"D:/3_Ano/IA/BlocksAI/puzzle"
```



## 3. Escolha de Tabuleiro

Após a introdução correta do caminho do ficheiro, a sua janela deverá mostrar as seguintes mensagens:

```
CL-USER 1 > (start)

Welcome to BlocksAI!
Please insert the filepath you want to simulate.
"D:/3_Ano/IA/BlocksAI/problemas.dat"
Type restart to go back to begining.
Type exit to end program.
Choose the board you want to test
```

Iremos abordar neste ponto apenas a primeira mensagem: *Choose the board you want to test*.

Observando o ficheiro fornecido, poderá decidir que tabuleiro deseja analisar, introduzindo na consola o número associado ao tabuleiro desejado.

```
Type restart to go back to begining.
Type exit to end program.
Choose the board you want to test
1
```

Neste caso o programa irá percorrer o primeiro tabuleiro do ficheiro.

## 4. Escolha de Algoritmo

Após a escolha do tabuleiro, o utilizador poderá escolher o algoritmo que deseja utilizar para percorrer o tabuleiro.

```
Please choose the algorithm you want to work with.
1 - BFS 
2 - DFS
3 - A*
4 - IDA* 
```

Caso escolha o primeiro algoritmo, o programa irá apresentar logo os resultados para o tabuleiro escolhido.

```
1 - BFS 
2 - DFS
3 - A*
4 - IDA* 
1
Tamanho da solução: 1

Nós gerados: 6

Nós explorados: 1

Tempo de execução: 3ms

Penetrância: 1/6

Ramificação média: 6
```

1. Caso escolha o segundo algoritmo, o programa irá pedir a introdução da profundidade máxima para este algoritmo.

```
1 - BFS 
2 - DFS
3 - A*
4 - IDA* 
2

Please insert maximum depth 
23
```

2. Quando introduzir um valor na consola, o programa apresentará os resultados do algoritmo.

```
Please choose the algorithm you want to work with.
1 - BFS 
2 - DFS
3 - A*
4 - IDA* 
2

Please insert maximum depth 
23
Tamanho da solução: 1

Nós gerados: 6

Nós explorados: 1

Tempo de execução: 2ms

Penetrância: 1/6

Ramificação média: 6
```

Caso escolha o terceiro ou quarto algoritmo, o programa irá pedir que escolha que heurística deseja aplicar no algoritmo:

```
Please choose the algorithm you want to work with.
1 - BFS 
2 - DFS
3 - A*
4 - IDA* 
3

Please insert wanted heuristic 
1 - Default 
2 - Student Custom

```

A opção *1 - Default* corresponde à heurística fornecida pelos professores da unidade curricular e a opção *2 - Student Custom* corresponde à heurística criada pelos estudantes que realizaram este projeto.

Introduzindo o número da opção desejada é escolhida a heurística a ser utilizada pelo algoritmo A* e IDA*, sendo posteriormente apresentados os resultados do tabuleiro percorrido por um destes algoritmos.

```
Please choose the algorithm you want to work with.
1 - BFS 
2 - DFS
3 - A*
4 - IDA* 
3

Please insert wanted heuristic 
1 - Default 
2 - Student Custom
1
Tamanho da solução: 2

Nós gerados: 10

Nós explorados: 3

Tempo de execução: 3ms

Penetrância: 1/5

Ramificação média: 2.7015693
```

## 4. Finalização da pesquisa e escrita de resultados no ficheiro

Após a apresentação dos resultados da pesquisa feita por qualquer um dos algoritmos o programa pede ao utilizador para inserir o caminho do ficheiro para o qual deseja guardar o resultado obtido.

```
Please choose the algorithm you want to work with.
1 - BFS 
2 - DFS
3 - A*
4 - IDA* 
3

Please insert wanted heuristic 
1 - Default 
2 - Student Custom
1
Tamanho da solução: 2

Nós gerados: 10

Nós explorados: 3

Tempo de execução: 3ms

Penetrância: 1/5

Ramificação média: 2.7015693

Please insert the filepath where you want to save the results
```

Deverá inserir o caminho do ficheiro no qual deseja guardar os resultados no mesmo formato que o caminho do ficheiro com os tabuleiros. Abaixo é apresentado um exemplo.

```
"D:/3_Ano/IA/BlocksAI/a-star-solution.txt"
```

Após a correta introdução do caminho para o ficheiro desejado, será apresentada uma mensagem de sucesso e o programa voltará ao momento em que pede ao utilizador para escolher o tabuleiro que quer utilizar na simulação.

```
Please insert the filepath where you want to save the results
"D:/3_Ano/IA/BlocksAI/a-star-solution.txt"
Results were saved in "D:/3_Ano/IA/BlocksAI/a-star-solution.txt"


Type restart to go back to begining.
Type exit to end program.
Choose the board you want to test:

```

Quando abrir o ficheiro onde guardou os resultados, deverá ser apresentada a seguinte informação:

```
Tamanho da solução: 1

Nós gerados: 6

Nós explorados: 1

Tempo de execução: 3ms

Penetrância: 1/6

Ramificação média: 6

Peças finais:
        1x1 = 0
        2x2 = 10
     Cruzes = 15

Inicio:
0 0 0 0 2 2 0 0 2 0 2 0 2 0 
0 0 0 0 2 2 0 2 2 2 0 2 2 2 
0 0 0 2 0 0 2 0 2 0 2 0 2 0 
0 2 2 0 0 2 2 2 0 2 2 2 0 2 
0 2 2 0 2 0 2 0 2 0 2 0 2 0 
0 0 0 2 2 2 0 2 2 2 0 2 2 2 
0 2 2 0 2 0 2 0 2 0 2 0 2 0 
0 2 2 0 0 2 2 2 0 2 2 2 0 2 
0 0 0 0 2 0 2 0 2 0 2 0 2 0 
0 0 0 2 2 2 0 2 2 2 0 2 2 2 
0 0 2 1 2 1 2 0 2 0 2 0 2 0 
1 2 2 2 1 2 2 2 0 2 2 2 0 0 
0 1 2 1 0 0 2 0 2 0 2 0 2 2 
1 0 1 2 1 2 0 2 0 2 0 0 2 2 

1- Colocou uma peça 1x1

Fim:
0 0 0 0 2 2 0 0 2 0 2 0 2 0 
0 0 0 0 2 2 0 2 2 2 0 2 2 2 
0 0 0 2 0 0 2 0 2 0 2 0 2 0 
0 2 2 0 0 2 2 2 0 2 2 2 0 2 
0 2 2 0 2 0 2 0 2 0 2 0 2 0 
0 0 0 2 2 2 0 2 2 2 0 2 2 2 
0 2 2 0 2 0 2 0 2 0 2 0 2 0 
0 2 2 0 0 2 2 2 0 2 2 2 0 2 
0 0 0 0 2 0 2 0 2 0 2 0 2 0 
0 0 1 2 2 2 0 2 2 2 0 2 2 2 
0 0 2 1 2 1 2 0 2 0 2 0 2 0 
1 2 2 2 1 2 2 2 0 2 2 2 0 0 
0 1 2 1 0 0 2 0 2 0 2 0 2 2 
1 0 1 2 1 2 0 2 0 2 0 0 2 2 
```

Este ficheiro contém não só a informação dos resultados, como também o estado inicial e final do tabuleiro e as peças que foram colocadas no tabuleiro a cada jogada.

## 5. Sair do programa e reiniciar

Caso deseje sair do programa, poderá fazê-lo a qualquer altura onde a interface permita ao utilizador a introdução da palavra exit

```
Welcome to BlocksAI!
Please insert the filepath you want to simulate or type exit to leave.
exit

Ended Session
-------------------------------------------------------
Type restart to go back to begining.
Type exit to end program.
Choose the board you want to test:
exit

Ended Session
```

O comando restart retorná-lo-á ao ponto inicial do programa.

