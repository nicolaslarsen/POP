open System

// Randomizer
let rand = new System.Random()
            
type Animal(weight : int, max_speed : int) =
    class
        let mutable current_speed = 0;
        let mutable daily_required_intake = 0;
        let mutable intake_percent = 0;

        member this.daily_intake = daily_required_intake
        member this.intake_prct = intake_percent
        member this.weight = weight
        member this.max_speed = max_speed
        member this.curr_speed = current_speed
        
        // Constructor with speed only, generates random weight between 70 and 300
        new(max_speed) =
            let weight = rand.Next(70,300)
            Animal(weight, max_speed)

        // Set current speed based on food eat eaten with respect to food needed
        member this.setSpeed() =
            current_speed <- max_speed/100 * this.intake_prct

        // Set a required daily intake with respect to the weight of the animal
        member this.setIntake() =
            daily_required_intake <- weight/2

        // For testing purposes
        member this.print_attributes() =
            printfn "Amount of daily food needed: %d" this.daily_intake
            printfn "Percent of needed intake eaten: %d" this.intake_prct
            printfn "Weight: %d" this.weight
            printfn "Max speed: %d" this.max_speed
            printfn "Current speed: %d" this.curr_speed
    end

let main() =
    let memes = new Animal(100, 50)
    memes.setSpeed()
    memes.setIntake()
    memes.print_attributes()
    
main()
