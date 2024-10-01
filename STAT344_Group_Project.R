# Read the data
dat <- read.csv("vgsales.csv", header = T)

dat.fil <- dat[c("Genre", "Global_Sales")]

N <- nrow(dat.fil)
n <- 1000

N.h <- table(dat.fil$Genre)

# population proportion for each strata
N.h/N

# Determine sample sizes for each strata using population allocation
print((N.h/N)*n, 2)

set.seed(1)

## Extract random samples from each strata
genres <- c("Action", "Adventure", "Fighting", "Misc", "Platform", 
            "Puzzle", "Racing", "Role-Playing", "Shooter", 
            "Simulation", "Sports", "Strategy")
samples <- list()

# sample size = 1000 using population allocation
sample_sizes <- c(200, 78, 51, 105, 53, 35, 75, 90, 79, 52, 141, 41)

for (i in seq_along(genres)) {
  genre <- genres[i]
  sample_size <- sample_sizes[i]
  genre_sample <- dat.fil[sample(which(dat.fil$Genre == genre), sample_size, replace = FALSE), ]
  samples[[genre]] <- genre_sample
}


# Sample data
head(samples, 6)


# Sample means
str_means <- numeric(length(genres))
variable <- "Global_Sales"

for (i in seq_along(genres)) {
  genre <- genres[i]
  genre_sample <- samples[[genre]]
  str_means[i] <- mean(genre_sample[[variable]], na.rm = TRUE)
}

str_means

## sample strata variance
str_variances <- numeric(length(genres))
variable <- "Global_Sales"

for (i in seq_along(genres)) {
  genre <- genres[i]
  genre_sample <- samples[[genre]]
  str_variances[i] <- var(genre_sample[[variable]], na.rm = TRUE)
}

str_variances


wt.strata = N.h/N

## overall stratified estimate and its SE
Str.est = sum(wt.strata*str_means)
Str.se = sqrt(sum((wt.strata^2)*(1-(sample_sizes/N.h))*(str_variances/sample_sizes)))

print(c(Str.est, Str.se))
[1] 0.51695794 0.03181284


# Confidence Interval
lower.b = Str.est - 1.96*Str.se
upper.b = Str.est + 1.96*Str.se

print(c(lower.b, upper.b))


