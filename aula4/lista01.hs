-- Para os próximos exercícios, use a seguinte definição de Arvore:
data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq,Show)


-- 1) Implementar a função
-- multArvore:: Int -> Arvore -> Arvore
-- que recebe um inteiro e uma árvore, e multiplica todos os valores contidos na árvore pelo inteiro

multArvore:: Int -> Arvore -> Arvore
multArvore a (Folha n) = Folha (a*n)
multArvore a (Nodo n a1 a2) = Nodo (a*n) (multArvore a a1) (multArvore a a2)


-- 2) Implemente a função
-- contaFolhas :: Arvore -> Int
-- que recebe uma árvore e conta quantas folhas existem nessa árvore

contaFolhas :: Arvore -> Int
contaFolhas (Folha n) = 1
contaFolhas (Nodo n a1 a2) = contaFolhas a1 + contaFolhas a2


-- 3) Implemente a função
-- contaNodos :: Árvore -> Int
-- que conta quantos Nodos uma árvore possui

contaNodos :: Arvore -> Int
contaNodos (Folha n) = 1
contaNodos (Nodo n a1 a2) = 1 + contaNodos a1 + contaNodos a2


-- 4) Implemente a função:
-- quantasVezes :: Int -> Arvore -> Int
-- que recebe um inteiro e uma árvore, e conta quantas vezes esse inteiro aparece na árvore

quantasVezes :: Int -> Arvore -> Int
quantasVezes a (Folha n) 
    | a == n = 1
    | otherwise = 0
quantasVezes a (Nodo n a1 a2)
    | a == n = 1 + (quantasVezes a a1) + (quantasVezes a a2)
    | otherwise =  (quantasVezes a a1) + (quantasVezes a a2)


-- (5) A função max do Haskell, recebe dois inteiros e devolve o maior entre eles:
-- Main*> max 4 33
-- 33
-- Main*> max 10 10
-- 10
-- Usando a função max implemente a função:
-- maxArvore :: Arvore -> Int
-- que encontra o maior inteiro em uma árvore

maxInt :: Int -> Int -> Int
maxInt a b
   | a > b = a
   | otherwise = b

maxArvore :: Arvore -> Int
maxArvore (Folha n) = n
maxArvore (Nodo n a1 a2) = maxInt n (maxInt (maxArvore a1) (maxArvore a2))


-- 6) Uma árvore refletida, é uma árvore com todos os seus ramos esquerdos e direitos trocados.
-- Defina a função:
-- refleteArvore :: Arvore -> Arvore

refleteArvore :: Arvore -> Arvore
refleteArvore (Folha n) = Folha (n)
refleteArvore (Nodo n a1 a2) = Nodo (n) (refleteArvore a2) (refleteArvore a1) 


-- (7) Implementar a função
-- geraLista :: Arvore -> [Int]
-- que transforma uma árvore em uma lista de inteiros. Não importa a ordem dos inteiros
-- na lista, apenas que todos os inteiros dos Nodos e Folhas estejam na lista resultante

geraLista :: Arvore -> [Int]
geraLista (Folha n) = [n]
geraLista (Nodo n a1 a2) = n : (geraLista a1) ++ (geraLista a2)
