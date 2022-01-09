import Data.List

-- 4.1.2 Example--custom sorting
names = [("Ian", "Curtis"),
         ("Abel", "Curtis"),
         ("Bernard", "Sumner"),
         ("Peter", "Hook"),
         ("Stephen", "Morris")]

comparePeople name1 name2 = if lComp /= EQ
                            then lComp
                            else fComp
    where lComp = compare (snd name1) (snd name2)
          fComp = compare  (fst name1) (fst name2)

-- 4.2 Returning functions 
addressLetter name office = function name
    where
        function = getLocation office

getLocation location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "dc" -> dcOffice
  _ -> (\name -> fst name ++ " " ++ snd name)

sfOffice name = if take 1 nameText  <= "L"
                then nameText ++ " - PO BOX 1234 - San Francisco, CA, 94111"
                else nameText ++ " - PO BOX 1010 - San Francisco, CA, 91109"
    where lastName = snd name
          nameText = fst name ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV, 89523"
  where nameText = snd name

dcOffice name = "Esq. " ++ nameText ++ " - PO Bbox 755 - Washington, DC, 06259"
  where nameText = snd name