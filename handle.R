### +++ Question 1: Find a dataset

# Read Data set From File
importDataset <- read.csv(file = 'C:/Users/Tuan/Desktop/19IT210_RMidterm_Project/WorldHappiness_2015_2020.csv')
countries_continents <- read.csv(file = 'C:/Users/Tuan/Desktop/19IT210_RMidterm_Project/Countries-Continents.csv')

### +++ Question 2: Explain variables (columns) in dataset

# importDataset is the WorldHappiness_2015_2020
# Country => Name of the country.
# happiness_score => Average of responses to the primary life evaluation question from the Gallup World Poll (GWP). 0-10
# gdp_per_capita => The extent to which GDP contributes to the calculation of the Happiness Score.
# family => The extent to which Family contributes to the calculation of the Happiness Score.
# health => The extent to which Life expectancy contributed to the calculation of the Happiness Score.
# freedom => The extent to which Freedom contributed to the calculation of the Happiness Score.
# generosity => A numerical value calculated based on poll participants' perceptions of generosity in their country.
# government_trust => The extent to which Perception of Corruption contributes to Happiness Score.
# dystopia_residual => A score based on a hypothetical comparison to the world's saddest country.
# Year => Year of research data take place

### +++ Question 3: Data cleaning (processing missing data)

# Count how many NA value in dataset
missingValue <- as.data.frame(
  # as.data.frame - convert an object to dataframe for cleaner view
  cbind(
    # lappy() returns a list of the similar length as input list object
    # each element of which is the result of applying FUN (function) to the corresponding element of list
    # lapply(X, FUN)
    lapply( # this lapply to count each (TRUE) block (sum FUN)
      lapply(importDataset, is.na), sum)  # Return array of TRUE or FALSE values base on the selected block is NA or not
  )
)
missingValue

# Get only the col name that contain NA value
# subset() => Return subsets of vectors, matrices or data frames which meet conditions.
# rownames() => Return row names from subset
rownames(subset(missingValue, missingValue$V1 != 0))


# Data Cleaning - Changing All NA value into 0
# is.na(importDataset) return list of cordinate, or example ([83,]: FALSE) and it's NA status
importDataset[is.na(importDataset)] <- 0 # Iterate each TRUE cordinate (NA) and replace it by 0

# Merge and clean dataset
# Add new columm (continents) by merge two dataset together
# merge => Merge two data frames by common columns or row names

mergeDataset <- merge(countries_continents, importDataset, by="Country", all.x=TRUE)
# Since in the continents dataset there are countries that not reported in happiness dataset , so we clean those
# complete.cases() => Return a logical vector indicating which cases are complete
mergeDataset <- mergeDataset[complete.cases(mergeDataset), ] # Keep only the complete rows

### +++ Question 4: Create new variables based on existing variables and conditions

# Create a new col base on existing value (most_factor)
# The factor that effect the most on happiness_score
for(i in rownames(mergeDataset)) {
  mostFactors <- list() # Create list for selection
  # Get value in each col
  mostFactors["government_trust"] <- mergeDataset[i, "government_trust"]
  mostFactors["generosity"] <- mergeDataset[i, "generosity"]
  mostFactors["freedom"] <- mergeDataset[i, "freedom"]
  mostFactors["gdp_per_capita"] <- mergeDataset[i, "gdp_per_capita"]
  mostFactors["health"] <- mergeDataset[i, "health"]
  mostFactors["family"] <- mergeDataset[i, "family"]
  # names() function in R Language is used to get or set the name of an Object
  # which.max returns the position of the element with the maximal value in a vector
  # The maximal factor is the factor that have the most effection on happiness
  mergeDataset[i, "most_factor"] <- names(mostFactors[which.max(mostFactors)])
}

### +++ Question 5: Ask research questions (problems to know from the dataset) and answer them with codes

# Is dataset consistent over year?

# table() function in R can be used to quickly create frequency (Freq) tables.
# unique() The unique() function in R is used to eliminate or delete the duplicate values
# length() Get the length of vectors or lists

# Explain: Table() create frequency value of dataset over the years (rows length of data over year)
# We have to make sure that the data over the year is enough (rows) for fair comparision (consistent)
# unique() eliminate duplicate values , return only the unique value, if the length of unique value is equal to 1 which mean
# length of data over year is not change (consistent)
length(unique(table(importDataset$Year))) == 1

# Functions
# list()
# A list in R can contain many different data types inside it.
# A list is a collection of data which is ordered and changeable.

avgHappyScoreBaseOnYear <- function() {
  exportData <- list() # Create list for export
  years <- rownames(table(mergeDataset$Year)) # Get all the years that exist in the dataset
  for( year in years ) # Iterate though each year
    # Save to the list with key as year and the data is average happiness_score
    # mean() => calculate the average value
    exportData[year] <- (mean(mergeDataset[mergeDataset$Year == strtoi(year), ]$happiness_score)) # Filter By Year
  return (exportData) # Export the list to perform further
}

avgHappyScoreBaseOnContinent <- function() {
  exportData <- list() # Create list for export
  continents <- rownames(table(mergeDataset$Continent)) # Get all the continents that exist in the dataset
  for(continent in continents) # Iterate though each continent
    # Save to the list with key as year and the data is average happiness_score
    exportData[continent] <- (mean(mergeDataset[mergeDataset$Continent == continent, ]$happiness_score)) # Filter By Continent
  return (exportData) # Export the list to perform further
}

#TRUE == HAPPY , FALSE == SAD
queryBaseOnYears <- function(happyOrSad , attributeTaken) {
  exportData <- list() # Create list for export
  years <- rownames(table(mergeDataset$Year)) # Get all the years that exist in the dataset
  for( year in years ) { # Iterate though each year
      tmpSelect <- importDataset[importDataset$Year == year, ] # Take dataset that have been filtered by year
      # Save to the list with key as year and data is the attributeTaken (country or continents)
      exportData[year] <- tmpSelect[order(tmpSelect$happiness_score , decreasing = happyOrSad), ][1,attributeTaken]
  }
  return (exportData) # Export the list to perform further
}

# What average happiness_score on each year
# Fuction return a list of average happiness_score though each year
AvgHappyScoreBaseOnYear = avgHappyScoreBaseOnYear()

# What average happiness_score on each continents
# Function return a list of average happiness_score between each continents
AvgHappyScoreBaseOnContinent = avgHappyScoreBaseOnContinent()

# Does ppls be more happy over time
# Explain: Compare happiness_score of the first year in dataset with the lastest year in dataset to see
# First index of list in R is 1 , last index we got from length() function
AvgHappyScoreBaseOnYear[1] < AvgHappyScoreBaseOnYear[[length(AvgHappyScoreBaseOnYear)]]

# Which year people is happiest
# which.max returns the position of the element with the maximal value in a vector.
AvgHappyScoreBaseOnYear[which.max(AvgHappyScoreBaseOnYear)]

# Which year people is saddest
# which.max returns the position of the element with the minimal value in a vector.
AvgHappyScoreBaseOnYear[which.min(AvgHappyScoreBaseOnYear)]

# Which country is happiest
# order() returns a permutation which rearranges its first argument into ascending or descending order
# In other word , sort stuffs by ascending or descending
# Since we need to find the happiest country, we sort the dataset by decreasing
# The first row in sorted dataset will be the happiest country
mergeDataset[order(mergeDataset$happiness_score , decreasing = TRUE), ][1,"Country"]

# Which country is saddest
# Since we need to find the saddest country, we sort the dataset by ascending (decreasing = FALSE)
# The first row in sorted dataset will be the saddest country
mergeDataset[order(mergeDataset$happiness_score , decreasing = FALSE), ][1,"Country"]


# Which continent is happiest
# which.max returns the position of the element with the maximal value in a vector.
AvgHappyScoreBaseOnContinent[which.max(AvgHappyScoreBaseOnContinent)]

#Which continent is saddest
# which.max returns the position of the element with the minimal value in a vector.
AvgHappyScoreBaseOnContinent[which.min(AvgHappyScoreBaseOnContinent)]

# ---------------------------queryBaseOnYears----------------------------
# Which country is happiest over the years
# Function take 2 parameter
# First is ascending or decreasing (TRUE or FALSE)
# Second is attributeTaken (The col that we want to get)
MostHappyCountryBaseOnYears <- queryBaseOnYears("TRUE" , "Country")

# Which continent is happiest over the years
MostHappyContinentBaseOnYears <- queryBaseOnYears("TRUE" , "Continent")

# Which country is saddest over the years
MostSadCountryBaseOnYears <- queryBaseOnYears("FALSE" , "Country")

# Which continent is saddest over the years
MostSadContinentBaseOnYears <- queryBaseOnYears("FALSE" , "Continent")
# end------------------------queryBaseOnYears----------------------------


# Which country that doesn't have data, doesn't appear in the dataset
# Since countries_continents include all countries in existing
# And WorldHappiness_2015_2020 doesn't cover all of them
# So we merge both them together and count empty rows ( doesn't have data )
# Since we already clean the dataset above , we create a new mergeDataset again
# Which called as mergeDatasetX
mergeDatasetX <- merge(countries_continents, importDataset, by="Country", all.x=TRUE)
# new_DF select only empty rows , ["Country"] get only the col Country
new_DF <- mergeDatasetX [rowSums(is.na(mergeDatasetX )) > 0,]["Country"]

# Does a happy family contribute to the happiness of a person
# Get happiness_score in the lowest family row
# And then compare it with happiness_score in the highest family row
# If it smaller then a happy family does contribute to the happiness of a person
# if it bigger then a happy family does NOT contribute to the happiness of a person
mergeDataset[order(mergeDataset$family , decreasing = FALSE), ][1,"happiness_score"] < mergeDataset[order(mergeDataset$family , decreasing = TRUE), ][1,"happiness_score"]

# Does health contribute to the happiness of a person
# Get happiness_score in the lowest health row
# And then compare it with happiness_score in the highest health row
# If it smaller then health does contribute to the happiness of a person
# If it bigger then health does NOT contribute to the happiness of a person
mergeDataset[order(mergeDataset$health , decreasing = FALSE), ][1,"happiness_score"] < mergeDataset[order(mergeDataset$health , decreasing = TRUE), ][1,"happiness_score"]

# Does money important as a factor of happiness
# Get happiness_score in the lowest money row
# And then compare it with happiness_score in the highest money row
# If it smaller then money is important as a factor of happiness
# If it bigger then money is NOT important as a factor of happiness
mergeDataset[order(mergeDataset$gdp_per_capita , decreasing = FALSE), ][1,"happiness_score"] < mergeDataset[order(mergeDataset$gdp_per_capita , decreasing = TRUE), ][1,"happiness_score"]

# Does Freedom important as a factor of happiness
# Get happiness_score in the lowest Freedom row
# And then compare it with happiness_score in the highest Freedom row
# If it smaller then Freedom is important as a factor of happiness
# If it bigger then Freedom is NOT important as a factor of happiness
mergeDataset[order(mergeDataset$freedom , decreasing = FALSE), ][1,"happiness_score"] < mergeDataset[order(mergeDataset$freedom , decreasing = TRUE), ][1,"happiness_score"]

# Does the generosity of a person affect their hapiness
# Get happiness_score in the lowest generosity row
# And then compare it with happiness_score in the highest generosity row
# If it smaller then generosity would affect their hapiness
# If it bigger then generosity would NOT affect their hapiness
mergeDataset[order(mergeDataset$generosity , decreasing = FALSE), ][1,"happiness_score"] < mergeDataset[order(mergeDataset$generosity , decreasing = TRUE), ][1,"happiness_score"]

# Does government trust affect to hapiness
# Get happiness_score in the lowest government_trust row
# And then compare it with happiness_score in the highest government_trust row
# If it smaller then government would affect their hapiness
# If it bigger then government would NOT affect their hapiness
mergeDataset[order(mergeDataset$government_trust , decreasing = FALSE), ][1,"happiness_score"] < mergeDataset[order(mergeDataset$government_trust , decreasing = TRUE), ][1,"happiness_score"]

# Which is the most factor that affect happiness
factors <- list() # Create list for sorting
factors["government_trust"] <- mergeDataset[order(mergeDataset$government_trust , decreasing = TRUE), ][1,"happiness_score"]
factors["generosity"] <- mergeDataset[order(mergeDataset$generosity , decreasing = TRUE), ][1,"happiness_score"]
factors["freedom"] <- mergeDataset[order(mergeDataset$freedom , decreasing = TRUE), ][1,"happiness_score"]
factors["gdp_per_capita"] <- mergeDataset[order(mergeDataset$gdp_per_capita , decreasing = TRUE), ][1,"happiness_score"]
factors["health"] <- mergeDataset[order(mergeDataset$health , decreasing = TRUE), ][1,"happiness_score"]
factors["family"] <- mergeDataset[order(mergeDataset$family , decreasing = TRUE), ][1,"happiness_score"]
# Get the highest happiness_score among all factor
# The maximal factor is the factor that have the most effection on happiness
factors[which.max(factors)]

#View stuffs
View(importDataset)
View(countries_continents)