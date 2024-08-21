-- Dada a definição de números naturais a seguir:

data Nat = Zero | Suc Nat
 deriving(Eq,Show)


-- 1) Implementar a função to_int que converte um Nat para um inteiro:
to_int :: Nat -> Int
to_int Zero = 0
to_int (Suc n) = 1 + to_int n
-- to_int (Suc (Suc Zero))


-- 2) Implementar a função to_nat que transforma um inteiro em um Nat
to_nat :: Int -> Nat
to_nat 0 = Zero
to_nat n = Suc(to_nat (n-1))


-- 3) Implementar usando recursão a função mult que multiplica dois Nats. 
-- (Você NÃO pode usar to_int ou to_nat):

soma :: Nat -> Nat -> Nat
soma Zero n = n
soma (Suc n1) n2 = soma n1 (Suc n2)
--to_int (soma (to_nat (20)) (to_nat (50)))

mult :: Nat -> Nat -> Nat
mult Zero (Suc n2) = Zero
mult (Suc n1) n2 = soma (mult n1 n2) n2
--to_int (mult (to_nat (2)) (to_nat (5)))


--4) Implementar a função leq (menor ou igual) que recebe dois Nats e devolve um booleano.
-- (Você NÃO pode usar to_int ou to_nat):
leq :: Nat -> Nat -> Bool
leq Zero n1 = True
leq n2 Zero = False
leq (Suc n1) (Suc n2) = leq n1 n2
--(leq (to_nat (6)) (to_nat (5)))


-- 5) Implementar a função sub que executa a subtração de dois naturais (Você NÃO pode
-- usar to_int ou to_nat):
sub :: Nat -> Nat -> Nat
sub n1 Zero = n1
sub n1 (Suc n2) = sub n1 n2