data NewEngland = ME | VT | NH | MA | RI | CT
instance Show NewEngland where
    show ME = "Maine"
    show VT = "Vermont"
    show NH = "New Hampshire"
    show MA = "Massachusetts"
    show RI = "Rhode Island"
    show CT = "Connecticut"

helloState :: Show a => a -> [Char]
helloState state = "hello " ++ show state