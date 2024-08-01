-- 1)  Escreva a função osQuatroSaoIguais que possui tipo
-- Int -> Int -> Int -> Int -> Bool
-- que retorna True se seus quatro argumentos s˜ao iguais

osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d = (a == b) && (b == c) && (c == d)


-- 2) Defina a função quantosSaoIguais :: Int -> Int -> Int -> Int que
-- recebe 3 valores e diz quantos desses valores s˜ao iguais

quantosSaoIguais :: Int -> Int -> Int -> Int 
quantosSaoIguais a b c
    | (a == b) && (b == c) = 3
    | (a == b) || (b == c) || (a == c) = 2
    | otherwise = 0


-- 3. Defina a fun¸c˜ao
-- todosDiferentes :: Int -> Int -> Int -> Bool
-- que retorna True se todos os seus argumentos s˜ao diferentes. Obs: m /= n
-- retorna True se m e n s˜ao diferentes

todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = (quantosSaoIguais a b c) == 0
-- todosDiferentes a b c = not((a == b) && (b == c))


-- 4. O que est´a errado com a seguinte defini¸c˜ao de todosDiferentes:
-- todosDiferentes n m p = ( ( n/=m ) && ( m/=p ) )
-- Resposta: A função não verifica se n e p são diferentes, 1 2 1 retornaria True, mas não deveria


-- 5. Escreva uma definição de quantosSaoIguais que use a fun¸c˜ao todosDiferentes
-- e a fun¸c˜ao todosIguais

todosIguais :: Int -> Int -> Int -> Bool
todosIguais a b c = (quantosSaoIguais a b c) == 3

quantosSaoIguais2 :: Int -> Int -> Int -> Int 
quantosSaoIguais2 a b c
    | (todosDiferentes a b c) == True = 0
    | (todosIguais a b c) == True = 3
    | otherwise = 2

-- 6. Defina a fun¸cao elevadoDois :: Int -> Int que recebe um argumento n
-- e devolve como resposta n2
elevadoDois :: Int -> Int
elevadoDois n = n * n


-- 7. Defina a fun¸c˜ao elevadoQuatro :: Int -> Int que recebe um argumento
-- n e devolve como resposta n^4
-- . Use elevadoDois para definir elevadoQuatro
elevadoQuatro :: Int -> Int
elevadoQuatro n = (elevadoDois n) * (elevadoDois n)

-- 8. Supondo que exista uma fun¸c˜ao vendas:
-- vendas :: Int -> Int
-- que devolve a venda semanal de uma loja (ex: vendas 0 devolve as vendas
-- na semana 0, vendas 1 devolve as vendas na semana 1, etc. Implemente
-- uma fun¸c˜ao chamada vendaTotal, que recebe um argumento n e calcula
-- todas as vendas da semana 0 at´e a semana n. Observe que essa fun¸c˜ao deve
-- ser recursiva. Exemplo de calculo: As vendas da semana 0 at´e a semana 2,
-- podem ser calculados usando a seguinte formula:
-- vendas 0 + vendas 1 + vendas 2
vendas :: Int -> Int
vendas 0 = 100
vendas 1 = 150
vendas 2 = 200
vendas 3 = 250
vendas _ = 0  

vendaTotal :: Int -> Int
vendaTotal 0 = vendas 0
vendaTotal n = vendas n + vendaTotal (n - 1)
