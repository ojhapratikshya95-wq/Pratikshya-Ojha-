dbh<- 28.4
ba <-pi * (dbh^2) / 40000
dbh = 28.1 
ba <- pi * (dbh^2) / 40000
ba 
dbh_num <-30
ht <- 11.3
tree_count <-15L
species_name <- "Dalbergia Sisoo"
tree_is_healthy <- TRUE

class(dbh_num)
typeof(dbh_num)

class(ht)
typeof(ht)

class(tree_count)
typeof(tree_count)

class(species_name)
typeof(species_name)

class(tree_is_healthy)
typeof(tree_is_healthy)
is.numeric(dbh_num)
is.integer(tree_count)
is.character(species_name)
is.logical(tree_is_healthy)

messy_dbh <-30
pi * (is.numeric(messy_dbh)^2)

a = c("5",5,78,"987")
as.numeric(a)
as.character(a)
#Scenerio E: The classic Factor-to-Numeric Problem 
#If numeric codes are stored as a factor, convert to character FIRST, then numeric:
plot_codes_factor<- factor(c("101", "102", "105"))
as.numeric(plot_codes_factor)
as.numeric(as.character(plot_codes_factor))

forest_types <- c("Community", "Private", "Community","Protected)
forest_factor <-as.factor(forest_types)
class(forest_factor)
levels(forest_factor)


# 4. Implicit Coercion (When R changes types automatically in a vector)
# A vector can only hold ONE data type. Mixing types forces R to convert them

# 1. Relational (comparison) Operators: >, <, ><, >=, <=, == (equal),!=(not equal)

dbh > 20
dbh <= 10
species_chr 
species_chr == "Shorea robusta" 
species_chr!= "Pinux roxburghii"

(dbh > 30) & (height >18)

(dbh >=10) & (dbh ,<= 30)

(dbh <=0) |(dbh >150)
# Conditional Decision-Making with if/ else (for a single)
if (dbh >= 30) {
  Cat("Classification: Sawtimber tree\n")
} else if (dbh >=10) {
  cat("Classification: Pole-sized tree\n")
  } else {
  cat("Classification: Sapling\n")
  }

# Combine multiple DBH measutrements into a numeric vector using c()
tree_dbh <- c(28.4, 3.1, 17.8, 42.0, 22.4)
# Vectorised math: R applies the basal area formula to all 5 trees simultaneously
trees_ba <- pi * (trees_dbh^2)/40000
round(trees-ba, digits =4)

#Handling Missing Values (NA) in field Data
ifelse(test = trees_dbh >= 30, yes = "large stem", no = "small/mediumstem")

#Default: Fails safely so you know data are missing 
heights <- c(19.2, 22.4, NA, 25.1, 17.1)

mean(height)

# The fix: tell R to remove NAs before calculating 
mean(heights, na.rm = TRUE)

# How many trees had missing height measurement?
# ( is.na() produces TRUE/FALSE; sum() counts the TRUEs because TRUE == 1)
is.na(heights)
sum(is.na(heights))

# 1. test now logical checks behave on a vector with planted errors 
test_trees <- c(28.4, -12.0, 0, 999,NA)

# Which trees have impossible non-positive diameter?
test_trees <= 0

#Which trees are suspiciously large (> 150 cm) AND not missing?
test_tress > 150 & !is.not(test_trees)

# Do ANY trees in the vector violate the > 0 rule? 
any(test_trees <= 0, na.rm = TRUE)

# 2. Build a Defensive Basal Area Function 

calculate_basal_area <- function(dbh_cm) {

# Guard  1: Check data type 
if (!is.numeric (dbh_cm)) {
stop ("Error: DBH must be numeric (measured in cm)!")
}
# Gaurd 2: stop on biologically impossible negative or zero values 
if (any (dbh_cm <=0, na.rm = TRUE)) {
 stop("Error: DBH must be strictly greater than 0!")
}
# Guard 3: Warn on potentail unit errors 9e. g., mm recorded instead of cm)
if (any(dbh_cm > 150, na.rm = TRUE)) {
 stop("Error: DBH must be strictly greater than 0!")
}
# Guard 3: Warn on potential unit errors (e.g., mm recorded instead of cm)
if (any (dbh_cm > 150, na.rm = TRUE)) {
    warning("Warning: DBH > 150 cm detected. Did you record in mm?")
}

# ba_m2 <- pi * (dbh_cm^2) / 40000
return(ba_m2)
} 
# Test the 
calculate_basal_area 

trees <- read.csv(data/raw/unit2_starter_trees.csv
head(trees) 
head (trees, 10)
nrow(trees)
ncol(trees)
names(trees)
str(trees)

trees$dbh_cm 

calc_cv(tree$dbh_cm)
trees$ba_m2 <- calculate_basal_area(trees$dbh_cm0)
head(trees)

trees$dbh_cm[trees$dbh_cm > 30]

trees$species[trees$dbh_cm > 30]

which.max(trees$dbh_cm)
trees[which.max(trees$dbh_cm),]
