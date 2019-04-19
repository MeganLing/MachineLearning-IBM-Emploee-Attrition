# **** Data Cleansing ****
rm(list=ls())

# ---- Loading ----
data.file <- "WA_Fn-UseC_-HR-Employee-Attrition.csv"
data.ori <- read.csv(data.file)

# ---- N.A. Handling ----
num.na <- sum(is.na(data.ori))
print(sprintf("Number of Missing Value: %d", num.na))

# ---- Meaningless Feature ----
# check colnums if all values in are the same
colnum.same.value <- unlist(lapply(data.ori, 
                                   function (x) length(unique(x)) <= 1))


# remove colnums that all values are the same
data.ori <- data.ori[!colnum.same.value]

for (feat in names(colnum.same.value)[colnum.same.value]) {
  print(sprintf("Feature [%s] has been removed.", feat))
}

# ---- Binary Features ----
# convert the datatype of binary features into Logic
# data.ori$Attrition <- ifelse(data.ori$Attrition=="Yes", TRUE, FALSE)
data.ori$OverTime <- ifelse(data.ori$OverTime=="Yes", TRUE, FALSE)
data.ori$Gender <- ifelse(data.ori$Gender=="Male", TRUE, FALSE)

# ---- Discrete Features ----
# convert the datatype of discrete features into ordered factor
data.ord <- data.ori

# - Business Travel -
lv.travel <- c("Non-Travel", "Travel_Rarely", "Travel_Frequently")
data.ord$BusinessTravel <- ordered(data.ord$BusinessTravel, levels = lv.travel)

# - Education -
lv.education <- c('Below_College', 'College', 'Bachelor', 'Master', 'Doctor')
data.ord$Education <- ordered(data.ord$Education, 
                              levels = 1:length(lv.education), 
                              labels = lv.education)

# - Environment Satisfaction -
lv.environment <- c('Low', 'Medium', 'High', 'Very High')
data.ord$EnvironmentSatisfaction <- ordered(data.ord$EnvironmentSatisfaction, 
                                            levels = 1:length(lv.environment), 
                                            labels = lv.environment)

# - Job Involvement -
lv.involvement <- c('Low', 'Medium', 'High', 'Very High')
data.ord$JobInvolvement <- ordered(data.ord$JobInvolvement,
                                   levels = 1:length(lv.involvement),
                                   labels = lv.involvement)

# - Job Satisfaction -
lv.staisfaction <- c('Low', 'Medium', 'High', 'Very High')
data.ord$JobSatisfaction <- ordered(data.ord$JobSatisfaction,
                                    levels = 1:length(lv.staisfaction),
                                    labels = lv.staisfaction)

# - Performance Rating -
lv.performance <- c('Low', 'Good', 'Excellent', 'Outstanding')
data.ord$PerformanceRating <- ordered(data.ord$PerformanceRating,
                                      levels = 1:length(lv.performance),
                                      labels = lv.performance)

# - Relationship Satisfaction -
lv.relationship <- c('Low', 'Medium', 'High', 'Very High')
data.ord$RelationshipSatisfaction <- ordered(data.ord$RelationshipSatisfaction,
                                             levels = 1:length(lv.relationship),
                                             labels = lv.relationship)

# - WorkLife Balance -
lv.balance <- c('Bad', 'Good', 'Better', 'Best')
data.ord$WorkLifeBalance <- ordered(data.ord$WorkLifeBalance,
                                    levels = 1:length(lv.balance),
                                    labels = lv.balance)

# ---- Dumping ---
save(data.ori, data.ord, file = "data_ori.Rdata")
