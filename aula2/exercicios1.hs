-- 1) Implementar a função
-- multLista :: Int -> [Int] -> [Int]
-- que recebe um inteiro e uma lista e multiplica todos os elementos dessa lista pelo inteiro
multLista :: Int -> [Int] -> [Int]
multLista _ [] = []  
multLista n (x:xs) = (n * x) : multLista n xs


-- 2) Implemente a função elemento :: Int -> [Int] -> Bool
-- que recebe um inteiro e uma lista, e devolve um booleano dizendo se o inteiro se encontra na lista
elemento :: Int -> [Int] -> Bool
elemento _ [] = False  
elemento n (x:xs)
    | n == x    = True   
    | otherwise = elemento n xs  


-- 3) Implemente a função
-- conta :: Int -> [Int] -> Int
-- que recebe um inteiro e uma lista, e diz quantas vezes o inteiro ocorre dentro da lista
conta :: Int -> [Int] -> Int
conta _ [] = 0
conta n (x:xs) 
    | n == x    = 1 + conta n xs  
    | otherwise = conta n xs 


-- 4) Implemente a função:
-- contaMaiores :: Int -> [Int]-> Int
-- que recebe um inteiro e uma lista e conta quantos elementos da lista são maiores que o
-- inteiro passado como argumento
contaMaiores :: Int -> [Int]-> Int
contaMaiores _ [] = 0
contaMaiores n (x:xs) 
    | n < x    = 1 + contaMaiores n xs  
    | otherwise = contaMaiores n xs 
