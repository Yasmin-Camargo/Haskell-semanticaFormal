-- 1) Implementar uma função que verifica se uma string contém um palindromo

eh_palindromo :: String -> Bool
eh_palindromo s = s == reverse s


-- 2) A soma do comprimento de qualquer dois lados de um triângulo é sempre maior do que
-- o comprimento do terceiro lado. Fazer uma função que recebe o comprimento dos três
-- lados de um triângulo e verifica essa condição

verifica_triangulo :: Int -> Int -> Int -> Bool
verifica_triangulo a b c = ((a + b) > c) && ((a + c) > b) && ((b + c) > a)


-- 3) Defina, usando guardas, a função sinal
-- sinal :: Int -> Int
-- que recebe um inteiro como entrada e devolve: -1 se a entrada for um número negativo,
-- 1, caso seja positivo ou 0 caso a entrada seja o número zero

sinal :: Int -> Int
sinal n
    | n < 0 = -1
    | otherwise = 1
    
    
-- 4) Implemente a função:
-- menorTres :: Int -> Int -> Int -> Int
-- que recebe três inteiros e devolve o menor entre os três

menorTres :: Int -> Int -> Int -> Int
menorTres a b c
    | (a < b) && (a < c) = a
    | (b < c) && (b < a) = b
    | otherwise = c
    
    
-- Em Haskell, podemos implementar uma função que calcula o fatorial de um número da
-- seguinte forma:
fat :: Int -> Int
fat 0 = 1
fat 1 = 1
fat n = n * fat (n-1)


-- 5) Implementar uma função recursiva que recebe a base e o expoente e calcula a potência
potencia :: Int -> Int -> Int
potencia a b
    | b == 0 = 1
    | b == 1 = a
    | otherwise = a * (potencia (a) (b-1))



