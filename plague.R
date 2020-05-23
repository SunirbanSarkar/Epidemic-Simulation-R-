# Setting the working directory

set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  # the following line is for getting the path of your current open file
  current_path <- getActiveDocumentContext()$path 
  # The next line set the working directory to the relevant one:
  setwd(dirname(current_path ))
  # The next line set the working directory to the relevant one:
  print( getwd() )
}
set_wd()

######     Case studies: Epidemic    #####

## SIR(Susceptible, Infected and Removed) Model

SIRsim <- function(a, b, N, T){
  S <- rep(0,T+1)
  I <- rep(0,T+1)
  R <- rep(0,T+1)
  
  S[1] <- N
  I[1] <- 1
  R[1] <- 0
  
  for (i in 1:T) {
    S[i+1] <- rbinom(1, S[i], (1 - a)^I[i])
    R[i+1] <- R[i] + rbinom(1, I[i], b)
    I[i+1] <- N + 1 -R[i+1] - S[i+1]
  }
  return(matrix(c(S, I, R), ncol = 3))
}

# Setting parameter values
N <- 1000
T <- 100
a <- 0.0005
b <- 0.1



(N*a);b
Z <- SIRsim(a,b,N,T)
colnames(Z) <- c("S", "I", "R")

par(mfrow=c(3,1))
plot(Z[,"S"], type = "n",
     xlab = "Days",
     ylab = "Total Population")
lines(Z[,"S"], col="blue")
plot(Z[,"I"], type = "n",
     xlab = "Days",
     ylab = "Number Infected")
lines(Z[,"I"], col="red")
plot(Z[,"R"], type = "n",
     xlab = "Days",
     ylab = "Total Removed")
lines(Z[,"R"])
par(mfrow=c(1,1))

# Can run many
par(mfrow=c(1,1))
plot(Z[,"S"], type = "n",
     xlab = "Days",
     ylab = "POP / INF / REM")
for (i in 1:20){
  Z <- SIRsim(a,b,N,T)
  colnames(Z) <- c("S", "I", "R")
  lines(Z[,"S"], col="blue")
  lines(Z[,"I"], col="red")
  lines(Z[,"R"])
}

SIR <- function(a, b, N, T){
  S <- N
  I <- 1
  R <- 0
  
  for (i in 1:T) {
    S <- rbinom(1, S, (1 - a)^I)
    R <- R + rbinom(1, I, b)
    I <- N + 1 - R - S
  }
  return(c(S, I, R))
}

# Setting parameter values
N <- 1000
T <- 100
a <- seq(0.0001, 0.001, by = 0.0001)
b <- seq(0.1, 0.5, by = 0.05)

SIR(a,b,N,T)

n.reps <- 400

f.name <- "SIR_grid.dat"

write(c("a","b","S_T"),file = f.name,
      ncolumns = 3)
for (i in 1:length(a)){
  for (j in 1:length(b)){
    S.sum <- 0
    for (k in 1:n.reps){
      S.sum <- S.sum + SIR(a[i], b[j], N, T)[1]
    }
    write(c(a[i], b[j], S.sum/n.reps),
          file = f.name,
          ncolumns = 3, append = TRUE)
  }
}


# Plot estimates in 3D
g <- read.table(f.name, header = TRUE)
library(lattice)
print(wireframe(S_T ~ a*b, data = g,
                scales = list(arrows = FALSE),
                aspect = c(.5, 1), drape = TRUE,
                xlab = "a", ylab = "b",
                zlab = "E(S[T])"))

