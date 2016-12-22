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
    let mutable winner = false
    while(not winner) do
        let mutable cheetah_average    = 0.0
        let mutable antelope_average   = 0.0
        let mutable wildebeest_average = 0.0
        for i in 1..3 do
            printfn "-----------------------------------------------------------------------"
            printfn "Round: %d" i
            printfn "-----------------------------------------------------------------------\n"
            let cheetah = new Carnivore(50.0, 114.0)
            let antelope = new Herbivore(50.0, 95.0)
            let wildebeest = new Herbivore(200.0, 80.0)
            cheetah.intake_prct    <- float(rand.Next(1,100))
            antelope.intake_prct   <- float(rand.Next(1,100))
            wildebeest.intake_prct <- float(rand.Next(1,100))
            
            cheetah.setIntake()
            cheetah.setSpeed()
            antelope.setIntake()
            antelope.setSpeed()
            wildebeest.setIntake()
            wildebeest.setSpeed()
            
            // Get the times in minutes
            let cheetah_time = 10.0/cheetah.curr_speed * 60.0
            let antelope_time = 10.0/antelope.curr_speed * 60.0
            let wildebeest_time = 10.0/wildebeest.curr_speed * 60.0

            // Printing
            printfn "Cheetah"
            printfn "intake percentage: %2.2f%%" cheetah.intake_prct
            printfn "actual intake: %2.2fkg" (cheetah.daily_intake/100.0 * cheetah.intake_prct)
            printfn "required intake: %2.2fkg" cheetah.daily_intake 
            printfn "time to cover 10km: %2.2f minutes\n" cheetah_time
            cheetah_average <- cheetah_average + cheetah_time
            
            printfn "Antelope"
            printfn "intake percentage: %2.2f%%" antelope.intake_prct
            printfn "actual intake: %2.2fkg" (antelope.daily_intake/100.0 * antelope.intake_prct)
            printfn "required intake: %2.2fkg" antelope.daily_intake 
            printfn "time to cover 10km: %2.2f minutes\n" antelope_time
            antelope_average <- antelope_average + antelope_time
            
            printfn "Wildebeest"
            printfn "intake percentage: %.2f%%" wildebeest.intake_prct
            printfn "actual intake: %.2fkg" (wildebeest.daily_intake/100.0 * wildebeest.intake_prct)
            printfn "required intake: %.2fkg" wildebeest.daily_intake 
            printfn "time to cover 10km: %.2f minutes\n" wildebeest_time
            wildebeest_average <- wildebeest_average + wildebeest_time

        // Calculate averages
        cheetah_average    <- cheetah_average / 3.0
        antelope_average   <- antelope_average / 3.0
        wildebeest_average <- wildebeest_average / 3.0

        // Small function to find the fastest animal
        let findFastest (x,xname) (y,yname) (z,zname) =
            let mutable fastest = x
            let mutable name = xname
            
            // find the fastest time
            if y < fastest then
                fastest <- y
                name <- yname
            if z < fastest then
                fastest <- z
                name <- zname
            (fastest, name)

        // Small function that checks
        // if there are any animals as fast as the fastest
        let checkFastest fastest x y z =
            let mutable numFastest = 0
            if x = fastest then
                numFastest <- numFastest + 1
            if y = fastest then
                numFastest <- numFastest + 1
            if z = fastest then
                numFastest <- numFastest + 1
            // Checks if there is indeed only 1 fastest animal
            numFastest = 1
        
        
        let (time, winnerName) = findFastest (cheetah_average, "Cheetah") (antelope_average, "Antelope") (wildebeest_average, "Wildebeest")
        winner <- checkFastest time cheetah_average antelope_average wildebeest_average
        if winner then
            printfn "-----------------------------------------------------------------------"
            printfn "%s won with an average time of %.2f minutes pr 10 km" winnerName time
            printfn "-----------------------------------------------------------------------\n"
        else
            printfn "-----------------------------------------------------------------------"
            printfn "No winner, trying again"
            printfn "-----------------------------------------------------------------------\n"
main()
