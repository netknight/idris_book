data PowerSource = Electric | Petrol | Pedal

data Vehicle: PowerSource -> Type where
  Unicycle: Vehicle Pedal
  Bycicle: Vehicle Pedal
  Motorcycle: (fuel: Nat) -> Vehicle Petrol
  Car: (fuel: Nat) -> Vehicle Petrol
  Bus: (fuel: Nat) -> Vehicle Petrol
  -- There could be an assumption that Electirc vehicles aren't rechargeable
  ElectricCar: (charge: Nat) -> Vehicle Electric
  Tram: (charge: Nat) -> Vehicle Electric

wheels: Vehicle power -> Nat
wheels Unicycle = 1
wheels Bycicle = 2
wheels Motorcycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (ElectricCar charge) = 4
wheels (Tram charge) = 4


refuel: Vehicle Petrol -> Vehicle Petrol
refuel Unicycle impossible
refuel Bycicle impossible
refuel (Motorcycle fuel) = Motorcycle 20
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
-- refuel: Vehicle Electric -> Vehicle Electric
--refuel (ElectricCar charge) = ElectricCar 5000
--refuel (Tram charge) = Tram 10000
