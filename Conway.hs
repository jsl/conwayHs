import qualified Data.List as L

vecinosEstado :: Bool -> [Bool]->Int
vecinosEstado estado vecinos = length $
                               filter (\x -> x == estado)
                               vecinos

vecinosVivos :: [Bool] -> Int
vecinosVivos vecinos = vecinosEstado True vecinos

vecinosMuertos :: [Bool] -> Int
vecinosMuertos vecinos = vecinosEstado False vecinos

reglaUno :: Bool -> [Bool] -> Bool
reglaUno elemento vecinos =
    if vecinosVivos vecinos < 2 then
       False
    else
       True

reglaDos :: Bool -> [Bool] -> Bool
reglaDos elemento vecinos =
    if L.elem numeroVivos [2,3] && elemento then
       True
    else
       False
    where numeroVivos = vecinosVivos vecinos

reglaTres :: Bool -> [Bool] -> Bool
reglaTres elemento vecinos
          | elemento == True &&
            vecinosVivos vecinos > 3 = False
          | otherwise                = elemento

reglaCuatro :: Bool -> [Bool] -> Bool
reglaCuatro elemento vecinos
    | elemento == False &&
      vecinosVivos vecinos == 3 = True
    | otherwise                 = elemento

conway :: Bool -> [Bool] -> Bool
conway elemento vecinos = undefined
