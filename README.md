# Trabalho Prático 1 - G08_08

## Representação interna dos polinómios
- No nosso trabalho, criámos dois novos tipos para a representação de polinómios. Um tipo para monómios (`Nomial`), e um tipo para polinómios (`Polynomial`), que consiste num conjunto de monómios.
- Um monómio (Nomial) é do tipo `(Int, [(Char, Int)])`, ou seja, é constituído por um um par em que o primeiro elemento é o coeficiente do monómio representado por um inteiro. 
- E em que o segundo elemento do par é uma lista de variáveis e expoentes, suportando assim, múltiplas variáveis. Essa lista de váriaveis é constituída por pares do tipo `(Char,Int)` em que o primeiro elemento representa uma variável (por exemplo: 'x', 'y' ou 'z') e que a segunda variável representa o expoente da variável. 
- Decidimos representar a estrutura principal do monómio como um par porque todos os monómios podem ter apenas, no máximo, um coeficiente (`Int`) e variáveis (`Char`) com expoente (`Int`), tendo um tamanho fixo de dois componentes que podem ser facilmente separados.
- De forma a podermos representar monómios do tipo `2*x^2*y^3` em vez de termos uma estrutura do tipo `(Char, Int)` para as variáveis e os seus expoentes, representámos essa estrutura como uma lista de pares `[(Char, Int)]` para podermos representar múltiplas variáveis juntas dos seus expoentes associados.
- Os polinómios (`Polynomial`) são um conjunto de monómios somados pelo que escolhemos representá-los pelo tipo `[Nomial]`, uma lista de monómios.

## Funcionalidades
- 


## Exemplos de utilização:
- 





O ficheiro README.pdf deverá conter:
- Uma descrição (e justificação) da escolha de representação interna de polinómios;
- Uma breve descrição da estratégia de implementação de cada funcionalidade;
- Exemplos de utilização que permitam testar todas as funcionalidades do programa.
