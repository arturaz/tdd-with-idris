import Data.Vect

data PowerSource = Pedal | Petrol | Electric

data Vehicle : PowerSource -> Type where
  Unicycle : Vehicle Pedal
  Bicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motorcycle fuel) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 20

vectTake : (n : Fin m) -> Vect m a -> Vect (finToNat n) a
vectTake _ [] impossible
vectTake FZ _ = []
vectTake (FS y) (x :: xs) = x :: (vectTake y xs)

sumEntries : Num a => (idx : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} idx xs ys =
  map
    (\idx_fin => (Vect.index idx_fin xs) + (Vect.index idx_fin ys))
    (integerToFin idx n)
