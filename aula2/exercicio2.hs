-- 1. Defina a função
-- membro :: Int -> [Int] -> Bool
-- que retorna um booleando que diz se um inteiro está presente na lista
membro :: Int -> [Int] -> Bool
membro _ [] = False  
membro n (x:xs)
    | n == x    = True   
    | otherwise = membro n xs  


-- 2. Implemente a função
-- membroNum :: Int -> [Int] -> Int
-- que conta o número de vezes que um inteiro aparece em uma lista
membroNum :: Int -> [Int] -> Int
membroNum _ [] = 0
membroNum n (x:xs) 
    | n == x    = 1 + membroNum n xs  
    | otherwise = membroNum n xs 


-- 3. Defina a função membro usando a função membroNum
membro2 :: Int -> [Int] -> Bool
membro2 n x = (membroNum n x) > 0 


-- 4. Implemente a função
-- unico :: [Int] -> [Int]
-- que retorna uma lista com os números que aparecem apenas uma vez na
-- lista argumento. Ex:
-- *Main> unico [2,4,1,4,1,3]
-- [2,3]
-- A função memberNum deve ser usada na defnição de unico
unico :: [Int] -> [Int]
unico [] = []
unico (x:xs) 
    | membroNum x (x:xs) == 1 = x : unico xs
    | otherwise = unico xs


-- 5. Observe a implementação da função de ordenação quikSort:
-- quikSort :: [Int] -> [Int]
-- quikSort [] = []
-- quikSort (x:xs) = quikSort (menores x xs)
-- ++ [x] ++
-- quikSort (maiores x xs)
-- A intuição dessa solução é a seguinte: deixar o primeiro elemento da lista
-- (pivo) no meio, então oloar antes dele todos os elementos menores que
-- o pivo ordenados e, depois dele, todos os elementos mairoes que o pivo
-- ordenados. Implemente as funções
-- menores :: Int -> [Int] -> [Int]
-- maiores :: Int -> [Int] -> [Int]
-- e veja o quik sort funionando!

quikSort :: [Int] -> [Int]
quikSort [] = []
quikSort (x:xs) = quikSort (menores x xs) ++ [x] ++ quikSort (maiores x xs)

menores :: Int -> [Int] -> [Int]
menores _ [] = []
menores n (x:xs)
    | n > x = x : menores n xs
    | otherwise = menores n xs

maiores :: Int -> [Int] -> [Int]
maiores _ [] = []
maiores n (x:xs)
    | n < x = x : maiores n xs
    | otherwise = maiores n xs
