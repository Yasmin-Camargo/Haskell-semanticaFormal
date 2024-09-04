----------------- TRABALHO DE SEMÂNTICA FORMAL ------------------
--                                                             --    
-- Alunos: Caroline Souza Camargo e Yasmin Souza Camargo       --
-- Professor: André Rauber Du Bois                             --
--                                                             --
-----------------------------------------------------------------


-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E         -- menor ou igual
      | Igual E E       -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | DoWhile C B       ---- Do C While B: executa C enquanto B avalie para verdadeiro
    | Unless B C C      ---- Unless B C1 C2: se B avalia para falso, então executa C1, caso contrário, executa C2
    | Loop E C          ---- Loop E C: Executa E vezes o comando C
    | Swap E E          ---- recebe duas variáveis e troca o conteúdo delas
    | DAtrrib E E E E   ---- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
   deriving(Eq,Show)                


-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]


mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------


--- EXPRESSÕES ARITMÉTICAS ---
ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s) = procuraVar s x
ebigStep (Num n,s) = n
ebigStep (Soma e1 e2,s) = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s) = ebigStep (e1,s) - ebigStep (e2,s)        -- (subtração)
ebigStep (Mult e1 e2,s) = ebigStep (e1,s) * ebigStep (e2,s)       -- (multiplicação)
ebigStep (Div e1 e2,s) = ebigStep (e1,s) `div` ebigStep (e2,s)    -- (divisão)


--- EXPRESSÕES BOOLEANAS ---
bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE,s)  = True
bbigStep (FALSE,s) = False

bbigStep (Not b,s) 
   | bbigStep (b,s) == True     = False
   | otherwise                  = True 

bbigStep (And b1 b2,s )                                           -- (and)
   | bbigStep (b1,s) == True      = bbigStep (b2,s)
   | otherwise                    =  False

bbigStep (Or b1 b2,s )                                            -- (or)
   | bbigStep (b1,s) == True     = True
   | otherwise                   = bbigStep (b2,s)

bbigStep (Leq e1 e2,s ) = ebigStep (e1,s) <= ebigStep (e2,s)      -- (Leq)
bbigStep (Igual e1 e2,s) = ebigStep (e1,s) == ebigStep (e2,s)     -- (Igual) -> recebe duas expressões aritméticas e devolve um valor booleano dizendo se são iguais



--- COMANDOS ---
cbigStep :: (C,Memoria) -> (C,Memoria)
cbigStep (Skip,s) = (Skip,s)

cbigStep (If b c1 c2,s)                                                 -- (If)
   | bbigStep (b,s) == True     = cbigStep (c1,s)
   | otherwise                  = cbigStep (c2,s)

cbigStep (Seq c1 c2,s) =                                                -- (Seq)
   let (_, s') = cbigStep (c1, s)
   in cbigStep (c2, s')       

cbigStep (Atrib (Var x) e, s) = (Skip, mudaVar s x (ebigStep (e, s)))   -- (Atrib)

cbigStep (While b c,s)                                                  -- (While)
   | bbigStep (b,s) == True     = cbigStep (Seq c (While b c),s)
   | otherwise                  = (Skip,s)

cbigStep (DoWhile c b,s) =   cbigStep (Seq c (While b c), s)            -- (Do While) -> executa C enquanto B avalie para verdadeiro

cbigStep (Unless b c1 c2,s)                                             -- (Unless) -> se B avalia para falso, então executa C1, caso contrário, executa C2
   | bbigStep (b,s) == True     = cbigStep (c2,s)
   | otherwise                  = cbigStep (c1,s)

cbigStep (Loop e c,s)                                                   --- (Loop) -> Executa E vezes o comando C
   | bbigStep(Leq e (Num 0), s) == True       = (Skip,s)
   | otherwise                                = cbigStep (Seq c (Loop (Sub e (Num 1)) c),s)

cbigStep (Swap (Var x) (Var y),s) =                                     --- (Swap) recebe duas variáveis e troca o conteúdo delas
   let temp = ebigStep (Var x, s)
   in cbigStep (Seq (Atrib (Var x) (Var y)) (Atrib (Var y) (Num temp)), s)
--cbigStep (Swap (Var x) (Var y),s) = cbigStep (Seq (Atrib (Var "temp") (Var x)) (Seq (Atrib (Var x) (Var y)) (Atrib (Var y) (Var "temp"))), s)                                --- (Swap) recebe duas variáveis e troca o conteúdo delas

cbigStep (DAtrrib (Var x) (Var y) e1 e2,s) = cbigStep (Seq (Atrib (Var x) e1) (Atrib (Var y) e2), s) -- (DAtrrib) Dupla atribuição: recebe duas variáveis x e y e duas expressões "e1" e "e2". Faz x:=e1 e y:=e2.
   

--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop 
--- * Dupla Atribuição
--- * Do While
-------------------------------------

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica
---

---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))


------------------------------------- NOSSOS PROGRAMAS --------------------------------------

memoriaTeste :: Memoria
memoriaTeste = [("a", 10), ("b", 5), ("c", 4), ("d", 6), ("x", 0), ("y", 0)]

-- TESTE Expressões Aritméticas: Div, Mult, Soma, Sub, Var, Num
progTestE :: E
progTestE =  Div (Mult (Soma (Var "a") (Num 5)) (Sub (Var "b") (Num 3))) (Soma (Var "c") (Var "d"))
-- Programa: (a + 5) * (b - 3) / (c + d)
-- ghci> ebigStep (progTestE, memoriaTeste)
-- Resultado esperado: 3


-- TESTE Expressões Booleanas: And, Or, Not, Leq, Igual
progTestB :: B
progTestB = And (Leq (Num 3) (Num 3)) (Not (Or (Igual (Num 1) (Num 2)) FALSE))
-- Programa: 3 <= 3 and ~(1 == 2 or False) 
-- ghci> bbigStep (progTestB, memoriaTeste)
-- Resultado esperado: True


-- TESTE Comandos: If, While, Seq, Atrib, DAtrrib, Skip
progTestC :: C
progTestC = If FALSE (While (Leq (Var "x") (Num 10)) (Atrib (Var "x") (Soma (Var "x") (Num 1)))) (Seq (DAtrrib (Var "x") (Var "y") (Num 4) (Num 2)) (Swap (Var "x") (Var "y")))
-- Programa: If FALSE/TRUE 
--             While x <= 10 
--               x := x + 1
--           Else 
--             x,y := 4,2 
--             Swap x y
-- ghci> cbigStep (progTestC, memoriaTeste)
-- Resultado esperado: FALSE (x = 11 e y = 0)  TRUE (x = 2 e y = 4)


-- TESTE Comandos: Unless, DoWhile, Loop
progTestC2 :: C
progTestC2 = Unless FALSE (DoWhile (Atrib (Var "x") (Soma (Var "x") (Num 1))) (Leq (Var "x") (Num 5))) (Loop (Num 3) (Atrib (Var "y") (Soma (Var "y") (Num 1))))
-- Programa: Unless FALSE/TRUE  
--             DoWhile 
--                x := x + 1 
--                While x <= 5 
--             Loop 3 
--                y := y + 1
-- ghci> cbigStep (progTestC2, memoriaTeste)
-- Resultado esperado: TRUE (x = 6 e y = 0) FALSE (x = 0 e y = 3)



