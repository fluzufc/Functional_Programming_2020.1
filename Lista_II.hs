-- Função que retorna uma lista com os elementos entre as posições passadas
segmento :: Int -> Int -> [Int] -> [Int]
segmento n m xs = drop (n-1) (take m xs)

-- Função que computa a soma dos quadrados até um valor passado
somaQuadrados :: Int -> Int
somaQuadrados n = sum (map (^2) [1..n])

-- Função fatorial
fact :: Int -> Int
fact n = product [1..n]

-- Função que retorna a lista de divisores de n
divisores :: Int -> [Int]
divisores n = filter ((==0).rem n) [1..n]

-- Função que retorna o vencedor de um jogo de Zerinho ou Um
zeroUm :: Int -> Int -> Int -> Char
zeroUm a b c | a == b && b == c = '*'
             | a == b && b /= c = 'C'
             | a /= b && b == c = 'A'
             | otherwise = 'B'

-- Função resolução da Questão "Copos Quebrados"
coposQuebrados :: [(Int, Int)] -> Int
coposQuebrados xs = sum (map (f1) xs)
f1 :: (Int, Int) -> Int
f1 (x,y) | x > y = y
        | otherwise = 0

-- Função resolução da Questão "Par ou Ímpar"
parImpar :: [(Int, Int)] -> Int
parImpar xs = sum (map (f2) xs)
f2 :: (Int, Int) -> Int
f2 (x,y) | (x+y) `mod` 2 == 0 = 1
        | otherwise = 0

-- Função resolução da Questão "Campeonato"
campeonato :: Int -> Int -> Int -> Int -> Int -> Int -> Char
campeonato cv ce cs fv fe fs | (cv*3 + ce) > (fv*3 + fe) = 'C'
                             | (cv*3 + ce) < (fv*3 + fe) = 'F'
                             | (cv*3 + ce) == (fv*3 + fe) && cs > fs = 'C'
                             | (cv*3 + ce) == (fv*3 + fe) && cs < fs = 'F'
                             | otherwise = '='
