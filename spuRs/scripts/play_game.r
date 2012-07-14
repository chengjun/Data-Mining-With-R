# Program spuRs/resources/scripts/play_game.r

play_game <- function(a, b) {
    state <- c(0, 0, 1)
    while (status(state[1], state[2]) == "unfinished") {
        # show(state)
        state <- play_point(state, a, b)
    }
    if (status(state[1], state[2]) == "player 1 win") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
