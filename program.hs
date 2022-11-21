module Main (main) where
main :: IO ()
main = do putStrLn "Qual seu nome?"
          nome <- getLine
          putStr nome
          putStrLn ", seja bem vind@!"



-- funções --

-- calcula o dobro de um número
dobro x = x * 2

-- calcula a potência de um número
potencia x n = x ^ n

-- calcula a área de um círculo
areaCirculo r = pi * (r ^ 2)

-- calcula a área de um triângulo
areaTriangulo a b c = let s = (a + b + c) / 2
            in sqrt (s * (s - a) * (s - b) * (s - c))

-- calcula média aritmética
media n1 n2 n3 = (soma / 3)
            where soma = n1 + n2 + n3

-- calcula conta de energia elétrica = 1/5 do salário
contaLuz salario quilowatts = (custo * ((100 - desconto) / 100))
            where
                custo = (salario / 5) * quilowatts
                desconto = 15

-- guarda
guarda n
    | n < 0 = -1
    | n == 0 = 0
    | otherwise = 1

-- função com lista
firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst)
                   then head lst
                   else "empty"

-- verifica se o valor informado é par ou ímpar
ehPar :: Int -> String
ehPar n
    | n `mod` 2 == 0 = "Par"
    | otherwise      = "Impar"

-- verifica se a letra informada é maiúscula ou minúscula
ehMaiuscula :: Char -> String
ehMaiuscula letra
    | (letra >= 'a') && (letra <= 'z') = "Minuscula"
    | (letra >= 'A') && (letra <= 'Z') = "Maiuscula"
    | otherwise = "Desconhecida"

-- verifica e retorna conforme a regra
abc :: Int -> Int -> Int -> Int
abc a b c
    | a == 0 = (b*b) + (3*c)
    | a == 1 = (2*(c*c)) - (3*c)
    | a == 2 = (3*c) - (b*b)
    | otherwise = 0;

-- recursividade
fatorial :: Integer -> Integer
fatorial n
    | n == 0 = 1
    | n > 0  = n * fatorial (n-1)

-- resto da divisão recursiva
divRec :: Int -> Int -> Int
divRec a b
    | b > a     = a
    | b == a    = 0
    | otherwise = divRec (a-b) b

-- multiplicação por recursividade
mult :: Int -> Int -> Int
mult m n
    | n == 0 = 0
    | n >= 1 = m + mult m (n-1)

-- calcula o tamanho de uma lista
tamanho :: [Int] -> Int
tamanho list =
    if null list
    then 0
    else 1 + tamanho (tail list)

-- concatena listas
(+++) :: [Int] -> [Int] -> [Int]
list1 +++ list2 =
    if null list1
    then list2
    else (head list1) : (tail list1 +++ list2)

-- inverte a lista
invertList :: [Int] -> [Int]
invertList list =
    if null list
    then []
    else invertList (tail list) +++ [head list]

-- fibonacci
fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = (fibo (n-1)) + (fibo (n-2))

-- tamanho da lista
listLength :: [Int] -> Int
listLength list
    | null list = 0
    | otherwise = 1 + listLength (tail list)

-- soma dos elementos da lista
listSoma :: [Int] -> Int
listSoma list
    | null list = 0
    | otherwise = (head list) + (listSoma (tail list))

-- maior elemento da lista
listBigger :: [Int] -> Int
listBigger [] = -1
listBigger (head:tail) =
    if head >= tailBigger
    then head
    else tailBigger
    where
        tailBigger = listBigger tail

-- verifica se na lista contém o elemento especificado
containsChar :: [Char] -> Char -> Bool
containsChar [] ch = False
containsChar (head:tail) ch
    | head == ch = True
    | otherwise = containsChar tail ch

raizes :: Float -> Float -> Float -> [Float]
raizes a b c
    | delta < 0  = []
    | delta == 0 = [(-b) / (2*a)]
    | delta > 0  = [(-b - (sqrt delta)) / (2*a), (-b + (sqrt delta)) / (2*a)]
    where
        delta = b*b - 4*a*c