# program: spuRs/resources/scripts/search.r
# Search for a string in a file.

# Clear the work space
rm(list=ls())

# Input
# Assume that the file file_name consists of character strings
# separated by newlines. target is the string to search for
file_name = "../data/data2.txt"
target = "Triumph"

# Open connection
input_file = file(file_name, open = "r")

# Scan until target found or we've reached the end of the file
found = FALSE  # has the target been found yet
eof = FALSE    # are we at the end of the file
n = 0          # number of lines read
while (!found && !eof) {
    str <- scan(input_file, what="a", n=1, sep="\n", quiet=TRUE)
    n = n + 1
    if (length(str) == 0) {
        eof <- TRUE
    } else if (str == target) {
        found <- TRUE
    }
}
close(input_file)

# Output
if (eof) {
    cat("End of file reached without finding target\n")
} else {
    cat("Target found at line", n, "\n")
}
