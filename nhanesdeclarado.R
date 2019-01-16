library(survey)

data(nhanes)

nhanes <- nhanes

nhanes$SDMVPSU <- factor(nhanes$SDMVPSU, ordered = FALSE)

nhanes$SDMVSTRA <- as.integer(nhanes$SDMVSTRA)

nhanes$WTMEC2YR <- as.numeric(nhanes$WTMEC2YR)

nhanes$HI_CHOL <- factor(nhanes$HI_CHOL, ordered = FALSE)

nhanes$race <- factor(nhanes$race, ordered = TRUE)

nhanes$agecat <- factor(nhanes$agecat, ordered = TRUE)

nhanes$RIAGENDR <- factor(nhanes$RIAGENDR, ordered = FALSE)

##### na's em nhanes serão transformados em 0

nhanes[is.na(nhanes)] <- 0


