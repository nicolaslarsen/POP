type Car(yearOfModel: int, make: string) =
    class
        // Speed and gas
        let mutable spdAmount = 0
        let mutable gasAmount = 0

        // Attributes
        member x.yearOfModel = yearOfModel
        member x.make        = make
        member x.speed       = spdAmount
        member x.gas         = gasAmount

        // Methods
        member x.Accelerate =
            if x.gas > 0 then
                spdAmount <- spdAmount + 5
                gasAmount <- gasAmount - 1

        // The amount of gas should not really affect the ability to brake.
        // However, this is stated in the assignment text,
        // so I will leave it as comments.
        member x.Brake =
            if spdAmount > 0 // && x.gas > 0
            then
                spdAmount <- spdAmount - 5
                // gasAmount <- gasAmount -1

        member x.getSpeed = x.speed

        member x.addGas amount =
            gasAmount <- gasAmount + amount

        member x.gasLeft = x.gas
    end

let derp = new Car(1982, "memes")
