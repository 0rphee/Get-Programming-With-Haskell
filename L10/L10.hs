cup floz = \msg -> msg floz

getoz aCup = aCup (\floz -> floz)

drink aCup ozDrank = if ozDiff >= 0
                      then cup ozDiff
                      else cup 0
    where flOz = getoz aCup
          ozDiff = flOz - ozDrank

robot (name,attack,hp) = \msg -> (name,attack,hp)

name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,hp) = hp
getName aRobot = aRobot name