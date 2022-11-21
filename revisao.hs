-- exercicio 1
salario :: Float -> Float
salario x = x + (acrescimo * x)
    where
        acrescimo = (10 - 7) / 100

-- exercicio 2
triangulo :: Int -> Int -> Int -> Bool
triangulo a b c
    | a + b > c = True
    | otherwise = False

-- exercicio 3
mediaFinal :: Float -> Float -> Float -> Char
mediaFinal n1 n2 n3
    | (media >= 8.0) && (media <= 10.0) = 'A'
    | (media >= 7.0) && (media <   8.0) = 'B'
    | (media >= 6.0) && (media <   7.0) = 'C'
    | (media >= 5.0) && (media <   6.0) = 'D'
    | (media >= 0.0) && (media <   5.0) = 'E'
    | otherwise = '_'
    where
        media = (n1 * 0.2) + (n2 * 0.3) + (n3 * 0.5)

-- exercicio 4
precoRetrato :: Integer -> String -> Double
precoRetrato qtdPess dia = custo + (custo * acrescimo)
    where
        custo
            | qtdPess == 1 = 100.0
            | qtdPess == 2 = 130.0
            | qtdPess == 3 = 150.0
            | qtdPess == 4 = 165.0
            | qtdPess == 5 = 175.0
            | qtdPess == 6 = 180.0
            | qtdPess >= 7 = 185.0
        acrescimo =
            if (dia == "sabado") || (dia == "domingo")
            then 0.2
            else 0.0

-- exercicio 5
produto :: Float -> Float -> Float -> Float
produto a b c = a * b * c

-- exercicio 6
pesoIdeal :: Float -> String -> Float
pesoIdeal h s
    | s == "masculino" = (72.7 * (h - 58.0)) / 100
    | s == "feminino"  = (62.1 * (h - 44.7)) / 100
    | otherwise = 0

-- exercicio 7
mediaAritmetica :: Float -> Float -> Float -> String
mediaAritmetica n1 n2 n3
    | (media < 3)                 = "reprovado"
    | (media >= 3) && (media < 7) = "exame especial"
    | (media >= 7)                = "aprovado"
    | otherwise                   = "desconhecido"
    where
        media = (n1 + n2 + n3) / 3

-- exercicio 8
inverteLista :: [a] -> [a]
inverteLista [] = []
inverteLista (head:tail) = (inverteLista tail) ++ [head]

palindromo :: String -> Bool
palindromo frase = frase == inverteLista frase

-- exercicio 9
emprestimo :: Float -> Float -> Bool
emprestimo salario prestacao =
    prestacao <= maxPrestacao
    where
        maxPrestacao = salario * 0.3

-- exercicio 10
eleitor :: Int -> String
eleitor idade
    | (idade <  16)                  = "não eleitor"
    | (idade >= 18) && (idade <  65) = "eleitor obrigatorio"
    | (idade >= 16) || (idade >= 65) = "eleitor facultativo"

-- exercicio 11
fatorialDuplo :: Int -> Int
fatorialDuplo n
    | (n-2 == 0) || (n == 1) = n
    | otherwise = fatorialDuplo (n-2) * n

-- exercicio 12
potencia :: Int -> Int -> Int
potencia x n
    | n == 0 = 0
    | n == 1 = x
    | otherwise = (potencia x (n-1)) * x

-- exercicio 13
--aumento :: Float -> Int -> Int -> Float
--aumento inicial anoContr anoAtual

-- exercicio 14
ultimo :: [Int] -> Int
ultimo [] = -1
ultimo (head:tail)
    | null tail = head
    | otherwise = ultimo tail

-- exercicio 15
primeiros :: [Int] -> [Int]
primeiros [] = []
primeiros (head:tail)
    | null tail = []
    | otherwise = [head] ++ primeiros tail

-- exercicio 16
listSoma :: [Int] -> Int
listSoma [] = 0
listSoma (head:tail) =
    head + listSoma tail

-- exercicio 17
listProduto :: [Int] -> [Int] -> [Int]
listProduto list1 list2
    | (null list1) || (null list2) = []
    | otherwise = [head list1 * head list2] ++ (listProduto (tail list1) (tail list2))

-- exercicio 18
data Produto = Perecivel Int String Int Bool Comercializacao
             | NaoPerecivel Int String String Int Comercializacao
    deriving Show

nomeProduto :: Produto -> String
nomeProduto produto =
    case produto of
        Perecivel    _ nome _ _ _ -> nome
        NaoPerecivel _ nome _ _ _ -> nome

-- exercicio 19
data Comercializacao = Unidade | Peso 
    deriving Show

-- exercicio 20
validade :: Produto -> Int -> Bool
validade (Perecivel _ _ val _ _)    ano = val >= ano
validade (NaoPerecivel _ _ _ val _) ano = val >= ano

-- exercicio 21
