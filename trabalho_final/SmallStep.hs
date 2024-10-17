
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E       ---   verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | DoWhile C B       ----  Do C While B: executa C enquanto B avalie para verdadeiro
    | Unless B C C      ----  Unless B C1 C2: se B avalia para falso, então executa C1, caso contrário, executa C2
    | Loop E C          ----  Loop E C: Executa E vezes o comando C
    | Swap E E          ----  recebe duas variáveis e troca o conteúdo delas
    | DAtrrib E E E E   ----  Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
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
smallStepE :: (E, Memoria) -> (E, Memoria)
smallStepE (Var x, s)                  = (Num (procuraVar s x), s)

smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Soma el e2,sl)

smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Mult el e2,sl)

smallStepE (Sub (Num n1) (Num n2), s) = (Num (n1 - n2), s)                  -- (subtração)
smallStepE (Sub (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                        in (Sub (Num n) el, sl)
smallStepE (Sub e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                        in (Sub el e2,sl)                                 


--- EXPRESSÕES BOOLEANAS ---
smallStepB :: (B,Memoria) -> (B, Memoria)
smallStepB (Not FALSE,s)    = (TRUE, s)                                     -- (not)
smallStepB (Not TRUE,s)     = (FALSE, s)      
smallStepB (Not b,s)        = let (b1,sl) = smallStepB (b,s)
                              in (Not b1,sl)  

smallStepB (And FALSE b,s )  = (FALSE, s)                                  -- (and)
smallStepB (And TRUE b,s )   = (b, s)  
smallStepB (And b1 b2,s )    = let (b1l,sl) = smallStepB (b1,s)
                               in (And b1l b2,sl)

smallStepB (Or FALSE b,s )   = (b, s)                                      -- (or)
smallStepB (Or TRUE b,s )    = (TRUE, s)
smallStepB (Or b1 b2,s )     = let (b1l,sl) = smallStepB (b1,s)
                               in (Or b1l b2,sl)

smallStepB (Leq (Num n1) (Num n2), s)  = (convertB(n1 <= n2), s)                       -- (leq)
smallStepB (Leq (Num n) e, s)          = let (el,sl) = smallStepE (e,s)
                                         in (Leq (Num n) el, sl)
smallStepB (Leq e1 e2, s)              = let (e1l,sl) = smallStepE (e1,s)
                                         in (Leq e1l e2, sl)

smallStepB (Igual (Num n1) (Num n2), s)  = (convertB(n1 == n2), s)                       -- (igual) recebe duas expressões aritméticas e devolve um valor booleano dizendo se são iguais
smallStepB (Igual (Num n) e, s)          = let (el,sl) = smallStepE (e,s)
                                           in (Igual (Num n) el, sl)
smallStepB (Igual e1 e2, s)              = let (e1l,sl) = smallStepE (e1,s)
                                           in (Igual e1l e2, sl)

convertB :: Bool -> B   -- Converte um valor booleano padrão (do tipo Bool) para o tipo B
convertB True    = TRUE
convertB False   = FALSE


--- COMANDOS ---
smallStepC :: (C,Memoria) -> (C,Memoria)
smallStepC (If FALSE c1 c2,s) = (c2,s)                               -- (if)
smallStepC (If TRUE c1 c2,s) = (c1,s)
smallStepC (If b c1 c2,s) = let (bl,sl) = smallStepB (b,s)
                            in (If bl c1 c2,sl)

smallStepC (Seq Skip c2,s) = (c2, s)                                 -- (seq)
smallStepC (Seq c1 c2,s) = let (c1l,sl) = smallStepC (c1,s)
                           in (Seq c1l c2,sl)

smallStepC (Atrib (Var x) (Num n),s) = (Skip, mudaVar s x n)         -- (atrib)
smallStepC (Atrib (Var x) e,s) = let (el,sl) = smallStepE (e,s)
                                 in (Atrib (Var x) el,sl) 

smallStepC (While b c, s) = (If b (Seq c (While b c)) Skip,s)        -- (while)

smallStepC (DoWhile c b,s) =  (Seq c (If b (DoWhile c b) Skip),s)    -- (dowhile) Do C While B: executa C enquanto B avalie para verdadeiro

smallStepC (Unless b c1 c2,s) = (If b c2 c1,s)                       -- (unless) Unless B C C   ---- Unless B C1 C2: se B avalia para falso, então executa C1, caso contrário, executa C2

smallStepC (Loop e c,s) =  (If (Leq (Num 1) e) (Seq c (Loop (Sub e (Num 1)) c)) Skip,s)   -- (loop) Loop E C    --- Loop E C: Executa E vezes o comando C
 
smallStepC (Swap (Var x) (Var y),s) = let sl = mudaVar s x (procuraVar s y)               -- (swap) Swap E E --- recebe duas variáveis e troca o conteúdo delas   
                                      in (Skip, mudaVar sl y (procuraVar s x))

smallStepC (DAtrrib (Var e1) (Var e2) e3 e4,s) = (Seq (Atrib (Var e1) e3) (Atrib (Var e2) e4),s)   -- (dAtrrib) DAtrrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.


----------------------
--  INTERPRETADORES
----------------------


--- Interpretador para Expressões Aritméticas:
isFinalE :: E -> Bool
isFinalE (Num n) = True
isFinalE _       = False


interpretadorE :: (E,Memoria) -> (E, Memoria)
interpretadorE (e,s) = if (isFinalE e) then (e,s) else interpretadorE (smallStepE (e,s))

--- Interpretador para expressões booleanas
isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

-- Descomentar quanto a função smallStepB estiver implementada:
interpretadorB :: (B,Memoria) -> (B, Memoria)
interpretadorB (b,s) = if (isFinalB b) then (b,s) else interpretadorB (smallStepB (b,s))


-- Interpretador da Linguagem Imperativa
isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

-- Descomentar quando a função smallStepC estiver implementada:
interpretadorC :: (C,Memoria) -> (C, Memoria)
interpretadorC (c,s) = if (isFinalC c) then (c,s) else interpretadorC (smallStepC (c,s))


--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS *** DIFERENTES *** DE PROGRAMAS QUE USEM:
--  * Unless  
--  * Loop   
--  * Swap 
--  * DAtrrib 

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação que fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- A função smallStepE anda apenas um passo na avaliação da Expressão

-- *Main> smallStepE (progExp1, exSigma)
-- (Soma (Num 3) (Soma (Num 10) (Var "y")),[("x",10),("temp",0),("y",0)])

-- Note que no exemplo anterior, o (Var "x") foi substituido por (Num 10)

-- Para avaliar a expressão até o final, deve-se usar o interpretadorE:

-- *Main> interpretadorE (progExp1 , exSigma)
-- (Num 13,[("x",10),("temp",0),("y",0)])

-- *Main> interpretadorE (progExp1 , exSigma2)
-- (Num 6,[("x",3),("y",0),("z",0)])


--- Para rodar os próximos programas é necessário primeiro implementar as regras que faltam
--- e descomentar os respectivos interpretadores


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
--  ghci> interpretadorC (fatorial, exSigma2)


------------------------------------- NOSSOS PROGRAMAS --------------------------------------

memoriaTeste :: Memoria
memoriaTeste = [("a", 10), ("b", 5), ("c", 4), ("d", 6), ("x", 0), ("y", 0)]

-- TESTE Expressões Aritméticas: Mult, Soma, Sub, Var, Num
progTestE :: E
progTestE =  Mult (Soma (Var "a") (Num 5)) (Sub (Var "b") (Num 3))
-- Programa: (a + 5) * (b - 3) 
--  ghci> smallStepE (progTestE, memoriaTeste)
--      Resultado esperado: (10 + 5) * (b - 3) 
--  ghci> interpretadorE (progTestE, memoriaTeste)
--      Resultado final: 30


-- TESTE Expressões Booleanas: And, Or, Not, Leq, Igual
progTestB :: B
progTestB = And (Leq (Num 3) (Num 3)) (Not (Or (Igual (Num 1) (Num 2)) FALSE))
-- Programa: 3 <= 3 and ~(1 == 2 or False) 
--  ghci> smallStepB (progTestB, memoriaTeste)
--    Resultado esperado: TRUE and ~(1 == 2 or False)
--  ghci> interpretadorB (progTestB, memoriaTeste)
--    Resultado final: True


-- TESTE Comandos: If, While, Seq, Atrib, DAtrrib, Skip
progTestC :: C
progTestC = If FALSE (While (Leq (Var "x") (Num 10)) (Atrib (Var "x") (Soma (Var "x") (Num 1)))) (Seq (DAtrrib (Var "x") (Var "y") (Num 4) (Num 2)) (Swap (Var "x") (Var "y")))
-- Programa: If FALSE/TRUE 
--             While x <= 10 
--               x := x + 1
--           Else 
--             x,y := 4,2 
--             Swap x y
--  ghci> smallStepC (progTestC, memoriaTeste)
--    Resultado esperado: TRUE  =>  While x <= 10 
--                                  x := x + 1
--                        FALSE =>  x,y := 4,2 
--                                  Swap x y               
--  ghci> interpretadorC (progTestC, memoriaTeste)
--    Resultado final: TRUE   => (x = 11 e y = 0)  
--                     FALSE  => (x = 2 e y = 4)


-- TESTE Comandos: Unless, DoWhile, Loop
progTestC2 :: C
progTestC2 = Unless TRUE (DoWhile (Atrib (Var "x") (Soma (Var "x") (Num 1))) (Leq (Var "x") (Num 5))) (Loop (Num 3) (Atrib (Var "y") (Soma (Var "y") (Num 1))))
-- Programa: Unless FALSE/TRUE  
--             Do 
--                x := x + 1 
--             While x <= 5 
--             Loop 3 
--                y := y + 1
--  ghci> smallStepC (progTestC2, memoriaTeste)
--    Resultado esperado: TRUE/FALSE => IF FALSE/TRUE
--                                         Loop 3 
--                                           y := y + 1  
--                                      ELSE 
--                                        Do
--                                           x := x + 1 
--                                        While x <= 5

--  ghci> interpretadorC (progTestC2, memoriaTeste)
--    Resultado final: TRUE  => (x = 0 e y = 3)
--                     FALSE => (x = 6 e y = 0) 