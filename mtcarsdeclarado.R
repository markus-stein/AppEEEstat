data(mtcars)

mtcars$am <- factor(mtcars$am, ordered = FALSE)
levels(mtcars$am) <- c("Automático", "Manual")

mtcars$vs <- factor(mtcars$vs, ordered = FALSE)
levels(mtcars$vs) <- c("V-engine", "Straight")

mtcars$cyl <- factor(mtcars$cyl, ordered = TRUE)

mtcars$gear <- factor(mtcars$gear, ordered = TRUE)

mtcars$hp <- as.integer(mtcars$hp)

mtcars$carb <- factor(mtcars$carb, ordered = TRUE)

mtcars$mpg <- as.numeric(mtcars$mpg)

mtcars$disp <- as.numeric(mtcars$disp)

mtcars$drat <- as.numeric(mtcars$drat)

mtcars$wt <- as.numeric(mtcars$wt)

mtcars$qsec <- as.numeric(mtcars$qsec)
