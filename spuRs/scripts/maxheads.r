maxheads <- function(n.toss) {
  # returns the length of the longest sequence of heads
  # in a sequence of n.toss coin tosses
  n_heads = 0   # length of current head sequence
  max_heads = 0 # length of longest head sequence so far
  for (i in 1:n.toss) {
    # toss a coin and work out length of current head sequence
    if (runif(1) < 0.5) { # a head, sequence of heads increases by 1
      n_heads <- n_heads + 1
    } else { # a tail, sequence of heads goes back to 0
      n_heads <- 0
    };
    # see if current sequence of heads is the longest
    if (n_heads > max_heads) {
      max_heads <- n_heads
    }
  }
  return(max_heads)
}
