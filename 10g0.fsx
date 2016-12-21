open System

// Randomizer
let rand = new System.Random()
            
type Animal(weight, max_speed) = class
    let mutable current_speed = 0.0
    let mutable daily_required_intake = 0.0
    let mutable intake_percent = 0.0
        
    member this.daily_intake
        with get() = daily_required_intake
        and set(value) = daily_required_intake <- value
    member this.intake_prct 
        with get() = intake_percent
        and set(value) = intake_percent <- value
    member this.weight = weight
    member this.max_speed = max_speed
    member this.curr_speed = current_speed
    
    // Constructor with speed only, generates random weight between 70 and 300
    new(max_speed) =
        let weight = float(rand.Next(70,300))
        Animal(weight, max_speed)
        
    // Set current speed based on food eat eaten with respect to food needed
    abstract member setSpeed: unit -> unit
    default this.setSpeed() =
        current_speed <- max_speed/100.0 * this.intake_prct

    // Set a required daily intake with respect to the weight of the animal
    abstract member setIntake: unit -> unit
    default this.setIntake() =
        daily_required_intake <- weight/2.0

    // For testing purposes
    member this.print_attributes() =
        printfn "Amount of daily food needed: %f" this.daily_intake
        printfn "Percent of needed intake eaten: %f" this.intake_prct
        printfn "Weight: %f" this.weight
        printfn "Max speed: %f" this.max_speed
        printfn "Current speed: %f" this.curr_speed
end

type Carnivore(weight , speed) = class
    inherit Animal(weight, speed)

    // Carnivores only need to eat 8% of their weight per day
    override this.setIntake() =
        this.daily_intake <- weight/100.0 * 8.0
end

type Herbivore(weight, speed) = class
    inherit Animal(weight, speed)

    // Herbivores need to eat 40% of their weight per day
    override this.setIntake() =
        this.daily_intake <- weight/100.0 * 40.0
end

let main() =
    let memes = new Animal(100.0, 50.0)
    let cheetah = new Carnivore(50.0, 114.0)
    let antelope = new Herbivore(50.0, 95.0)
    let wildebeest = new Herbivore(200.0, 80.0)
    
    memes.setSpeed()
    memes.setIntake()
    cheetah.setSpeed()
    cheetah.setIntake()
    antelope.setSpeed()
    antelope.setIntake()
    wildebeest.setSpeed()
    wildebeest.setIntake()
    memes.print_attributes()
    cheetah.print_attributes()
    antelope.print_attributes()
    wildebeest.print_attributes()
    
main()
