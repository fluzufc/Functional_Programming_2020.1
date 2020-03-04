-- Função Soma
soma :: Int -> Int -> Int
soma x y = x + y

-- Função que retira as extremidades de uma lista
interior :: [Int] -> [Int]
interior xs = init (tail xs)

-- Função que diz quantos de 3 elementos são iguais
iguais3 :: Int -> Int -> Int -> Int
iguais3 a b c | (a == b) && (a == c) && (b == c) = 3
              | (a == b) || (a == c) || (b == c) = 2
              | otherwise = 0

-- Função que diz qual o maior de 3 elementos
max3 :: Int -> Int -> Int -> Int
max3 x y z = max x (max y z)

-- Função que soma apenas os números impares de uma lista
somaImpares :: [Int] -> Int
somaImpares xs = sum (filter odd xs)

-- Função que computa o número de elementos negativos em uma lista
neglist :: [Int] -> Int
neglist xs = length (filter (<0) xs)

-- Função que retorna uma lista formada pelos n últimos elementos de uma lista
final :: Int -> [Int] -> [Int]
final x xs = drop (len-x) xs
    where
        len = length xs

-- Função que calcula o equilíbrio de uma gangorra fictícia de acordo com os pesos e os braços potente e resistente
gangorra :: Int -> Int -> Int -> Int -> Int
gangorra p1 c1 p2 c2 | p1*c1>p2*c2 = -1
                     | p1*c1<p2*c2 = 1
                     | otherwise = 0
