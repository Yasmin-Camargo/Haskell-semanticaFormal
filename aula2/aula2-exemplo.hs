somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

multDois:: [Int] -> [Int]
multDois [] = []
multDois (x:xs) = 2*x : multDois xs
