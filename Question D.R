# Question d)
MontyHall <- function(){
    # Set the winning door to 2 to simplify bit arithmetic later
    winDoor = 2
    
    # Choose randomly out of 0, 1 and 2
    firstChoice = sample(c(0,1,2), 1)
    
    if(firstChoice != 2){
        # Open the door with the only other goat remaining
        # On second viewing this statement of opening the door is unnecessary, but
        # tells us a lot qualitatively
        openDoor = as.integer(!firstChoice)
        
        # For the strategy, we switch to the door not opened and not chosen first
        firstChoice = 2
    }
    else{
        # Host can open either door with a goat
        openDoor = sample(c(0,1), 1)
        
        # We still switch
        firstChoice = as.integer(!openDoor)
    }
    return(firstChoice == winDoor)
}

dVector <- rep(1, times = 1000)
for(i in 1:1000){
    dVector[i] = MontyHall()
}
ansd <- mean(dVector)
