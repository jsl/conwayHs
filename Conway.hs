import qualified Data.List as L

vecinosVivos :: [Bool] -> Int
vecinosVivos vecinos = length $ filter (\x -> x == True) vecinos

reglaUno :: Bool -> Int -> Bool
reglaUno elemento numeroVivos = numeroVivos < 2

reglaDos :: Bool -> Int -> Bool
reglaDos elemento numeroVivos = elemento && L.elem numeroVivos [2,3]

reglaTres :: Bool -> Int -> Bool
reglaTres elemento numeroVivos
          | elemento == True &&
            numeroVivos > 3 = False
          | otherwise       = elemento

reglaCuatro :: Bool -> Int -> Bool
reglaCuatro elemento numeroVivos
    | elemento == False &&
      numeroVivos == 3 = True
    | otherwise        = elemento

conway :: Bool -> [Bool] -> Bool
conway elemento vecinos = undefined
