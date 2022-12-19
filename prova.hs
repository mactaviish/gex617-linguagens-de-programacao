type Matricula = Integer
type Nome = String
type Notas = (Double,Double,Double)
type Estudante = (Matricula, Nome, Notas)
type Turma = [ Estudante ]




t1 :: Turma
t1 = [
    (1, "Pedro", (5.0,7.5,4.5)),
    (2, "Maria", (9.0,8.0,10.0)),
    (3, "João",  (3.0,7.0,4.5)),
    (4, "Paulo", (7.0,5.0,9.5)),
    (5, "Ana",   (8.5,8.0,9.0))
    ]

t2 :: Turma
t2 = [
    (1, "Aline",  (9.0,7.5,6.5)),
    (2, "Juan",   (9.0,8.0,10.0)),
    (3, "Roger",  (9.0,7.0,6.5)),
    (4, "Dilson", (9.0,8.0,9.5)),
    (5, "Nelson", (9.5,8.0,9.0))
    ]


mediaAluno :: Notas -> Double
mediaAluno (n1, n2, n3) = (n1 + n2 + n3) / 3

mediaTurma :: Turma -> Double
mediaTurma turma = sum(map (\(_,_,notas) -> mediaAluno notas) turma) / fromIntegral (length turma)

mediaAcima :: Turma -> Double -> Turma
mediaAcima turma n = filter (\(_,_,notas) -> (mediaAluno notas) >= n) turma

maiorMedia :: Turma -> Turma -> Turma
maiorMedia t1 t2
    | mediaTurma t1 > mediaTurma t2 = t1
    | otherwise = t2

medias :: Turma -> [(String, Double)]
medias turma = map (\(_,nome,notas) -> (nome, mediaAluno notas)) turma