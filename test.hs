square x = x * x

fib 0 = 0
fib 1 = 1

fib x = (fib (x-2) + (fib (x-1)))

primerArreglo (x:arr) = x

segundoArreglo(_:x:_) = x

suma [] = 0
suma (x:arr) = x + (suma arr)

mapiar _ [] = []

mapiar fn (x:arr) = (fn x):(mapiar fn arr) 


swap (x,y) = (y, x)

--filter

filtrar fn [] = []

filtrar fn (x:arr)
	| (fn x) == True = (x):(filtrar fn arr)
	| otherwise = (filtrar fn arr)


getElement (x:arr) a
	|a == 0 = x
	|otherwise = ((arr)!! (a-1))

getMayor arr = mayor arr
	where
		mayor [] =[]
		mayor [x] = [x]
		mayor (x1:x2:arr)
			| (x1 > x2) == True = mayor (x1: arr)
			|otherwise =  mayor (x2: arr)

getMenor arr = menor arr
	where
		menor [] =[]
		menor [x] = [x]
		menor (x1:x2:arr)
			| (x1 > x2) == True = menor (x2: arr)
			|otherwise =  menor (x1: arr)

eliminar n (x:arr)
	| n == x = arr
	|otherwise = [x]++ (eliminar n arr)

ordenar2 arr = ordenando arr
	where
		ordenando [] = []
		ordenando [x] =[x]
		ordenando (x:arr)
			|(x>(getMayor arr)!!0) = ((ordenando arr)++[x]) 
			|otherwise = (ordenando ([x]++(eliminar ((getMayor arr)!!0) arr))) ++ (getMayor arr)


remove_dups xs = remove xs
  where
    remove []  = []
    remove [x] = [x]
    remove (x1:x2:xs)
      | x1 == x2  = remove (x1:xs)
      | otherwise = x1 : remove (x2:xs) 

buscame [] _ = False
buscame (x:arr) a = if (a == x) then True else (buscame arr a)

tam arr = long arr
	where
		long [] =0
		long [x] =1
		long (x:arr) = (long arr) +1

getPromedio arr = prom arr
	where
		prom [] =0
		prom [x] = x
		prom arr = (suma arr) / (tam arr)

trunquear a arr = truncar a arr
	where
		truncar _ [] = []
		truncar a (x:arr)
			| (tam (x:arr)) < a = []
			|a ==0 = []
			|a ==1 = [x]
			|(a >1) = (x):(truncar (a-1) arr)

contiene _ [] = False
contiene a (b:arr)
	| a==(trunquear (tam a) (b:arr)) = True
	|otherwise = (contiene a arr)

--solo puede reemplazar 1 elemento del arreglo
reemplazar n newVal (x:arr)
     | n == 0 = newVal:arr
     | otherwise = x:reemplazar (n-1) newVal arr

remover _ [] = []
remover n (x:arr)
	|n == 0 = (x:arr)
	|n ==1 = arr
	|(n>1) = (remover (n-1) arr) 
	
--reemplaza mas de un elemento del arreglo
reemplazar2 [] _ _ =[]
reemplazar2 (x:arr) a b
	|(a == (trunquear (tam a) (x:arr))) = b ++ (remover (tam a) (x:arr))
	|otherwise = [x] ++ (reemplazar2 arr a b)

nthElement :: [a] -> Int -> Maybe a 
nthElement [] a = Nothing
nthElement (x:xs) a | a <= 0 = Nothing
                    | a == 1 = Just x
                    | a > 1 = nthElement xs (a-1)


replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)



