# Question c)

tennis <- function(prob){
    scoreA = 0
    scoreB = 0
    matches = 0
    while(scoreA < 3 && scoreB < 3){
        matches = matches + 1
        if (rbinom(1, 1, prob)){
            # 1 indicates a win by player A
            scoreA = scoreA + 1
        }
        else {
            scoreB = scoreB + 1
        }
    }
    return(matches)
}

constProb = 0.70
matches <- rep(1, times = 1000)
for(i in 1: 1000){
    matches[i] <- tennis(constProb)
} 
ansc <- mean(matches)
