df$Cabin_letter <- ifelse(is.na(df$Cabin), "NA", str_sub(df$Cabin, 1, 1))
df$Cabin_letter[df$Cabin_letter != "NA"] <- "Y"

# df <- replace_na(df, list(Cabin = "NA", Embarked = "NA", Fare = median(df$Fare, na.rm = TRUE)))
# df$Cabin_letter[df$Cabin_letter == "T"] <- "NA"
# df$Cabin_letter[df$Cabin_letter %in% c("B", "D", "E")] <- "high"
# df$Cabin_letter[df$Cabin_letter %in% c("A", "C", "F", "G")] <- "low"

df$FamilySize <- df$Parch + df$SibSp + 1


# Convert to a string
df$Name <- as.character(df$Name)

# Engineered variable: Title
df$Title <- sapply(df$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
df$Title <- sub(' ', '', df$Title)
# dfne small title groups
df$Title[df$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
df$Title[df$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
df$Title[df$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor

# Engineered variable: Family
df$Surname <- sapply(df$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
df$FamilyID <- paste(as.character(df$FamilySize), df$Surname, sep="")
df$FamilyID[df$FamilySize <= 3] <- 'Small'
# Delete erroneous family IDs
famIDs <- data.frame(table(df$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 3,]
df$FamilyID[df$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor


df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)

df$Kid <- factor(df$Age < 18)


