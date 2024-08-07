-- ORDENANDO UMA LISTA USANDO INSERÇÃO
iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins a [] = [a] 
ins a (x:xs) 
    | a <= x =  a : x : xs
    | otherwise = x : ins a xs



-- 1. Implementar a função pegaPosicao
-- > pegaPosicao 3 [10,2,11,4,5] 11
pegaPosicao :: Int -> [Int] -> [Int]
pegaPosicao n (x:xs)
    | n == 1 = [x]
    | otherwise = pegaPosicao (n-1) xs

pegaPosicao2 :: Int -> [Int] -> Int
pegaPosicao2 1 (x:xs) = x
pegaPosicao2 n (x:xs) = pegaPosicao2 (n-1) xs


-- 2. Implementar a função pega:
-- > pega 3 [1,2,3,4,5]
-- [1,2,3]
pega :: Int -> [Int] -> [Int]
pega n (x:xs) 
    | n == 0 = []
    | otherwise = x : pega (n-1) xs


-- 3. Implementar a função retira:
-- > retira 3 [1,2,3,4,5]
-- [4,5]
retira :: Int -> [Int] -> [Int]
retira _ [] = []
retira n (x:xs)
    | n == 0 = (x:xs)
    | otherwise = retira (n-1) xs

-- 4. Implementar a função mediaLista que cacula a média dos elementos de
-- uma lista
mediaLista :: [Float] -> Float
mediaLista [] = 0.0
mediaLista (x:xs) = sumElementos (x:xs)  / numElementos (x:xs)

numElementos :: [Float] -> Float
numElementos [] = 0.0
numElementos (x:xs) = 1 + (numElementos xs) 

sumElementos :: [Float] -> Float
sumElementos [] = 0.0
sumElementos (x:xs) = x + (sumElementos xs)


-- 5. Implementar a função pegaMaiores
-- > pegaMaiores 3 [10,2,3,4,5]
-- [10,4,5]

