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


-- 5) Implemente a função
-- maiores :: Int -> [Int] -> [Int]
-- que recebe um inteiro e uma lista e devolve uma lista contendo somente os valores que
-- estavam na lista inicial e que são maiores do que o inteiro passado como argumento
maiores :: Int -> [Int]-> [Int]
maiores _ [] = []
maiores n (x:xs) 
    | n < x    = x : maiores n xs  
    | otherwise = maiores n xs 


-- 6) Implementar a função
-- geraLista :: Int -> Int -> [Int]
-- que recebe um inteiro m e um inteiro n e devolve uma lista contendo m vezes n
-- > geraLista 3 7
-- [7, 7, 7]
geraLista :: Int -> Int -> [Int]
geraLista 0 _ = []
geraLista n m = m : geraLista (n-1) m


-- 7) Implementar a função
-- addFim :: Int -> [Int] -> [Int]
-- que recebe um inteiro, uma lista e adiciona o elemento no fim da lista (sem usar o ++):
-- > addFim 10 [1,2]
-- [1,2,10]
addFim :: Int -> [Int] -> [Int]
addFim n [] = n : []
addFim n (x:xs) = x : addFim n xs


-- (8) Implementar a função
-- join :: [Int] -> [Int] -> [Int]
-- que recebe duas listas e concatena as mesmas gerando uma nova lista:
-- > join [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]
join :: [Int] -> [Int] -> [Int]
join x y = x ++ y


-- (9) Implementar a função inverte
-- inverte :: [Int] -> [Int]
-- que recebe uma lista e devolve a mesma invertida
inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:xs) = (inverte xs) ++ (x:[])