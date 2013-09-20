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


elementBy :: (Int, Int) -> (Int, Int) -> [[a]] -> Maybe a
elementBy (curX, curY) (chgX, chgY) juego =
    if newX > -1 && newY > -1 && newY < length juego &&
       newX < length (juego !! newY) then
        Just $ juego !! newY !! newX
    else
        Nothing
    where newX = curX + chgX
          newY = curY + chgY

posicionesDeVecinos = [ (-1, -1), (0, -1), (-1, 1)
                      , (-1, 0),  (1, 0)
                      , (1, -1),  (0, 1),  (1, 1)]

vecinos :: (Int, Int) -> [[a]] -> [a]
vecinos (x, y) juego = foldl
                (\vs (x', y') ->
                     case elementBy (x, y) (x', y') juego of
                       Nothing  -> vs
                       Just elt -> elt : vs)
                []
                posicionesDeVecinos
