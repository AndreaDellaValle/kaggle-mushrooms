# Importing packages
require('caret')
library(rpart)
library(rpart.plot)
#install.packages("e1071") installed manually with tar.gz file
require(e1071)

###  Globals&CONSTANTS  ###
pathinput <- "./Mushroom/mushrooms.csv"
dataset <- read.csv2(pathinput , sep = ",", na.string = c("", " ", "?", "NA"))

colnames(dataset) <- c("edibility", "cap_shape", "cap_surface", 
                        "cap_color", "bruises", "odor", 
                        "gill_attachement", "gill_spacing", "gill_size", 
                        "gill_color", "stalk_shape", "stalk_root", 
                        "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
                        "stalk_color_below_ring", "veil_type", "veil_color", 
                        "ring_number", "ring_type", "spore_print_color", 
                        "population", "habitat")

levels(dataset$edibility) <- c("edible", "poisonous")

dataset2 <- subset(dataset, select = -c(veil_type, stalk_root))

# Splitting the data by 80%
set.seed(100)
split_data <- createDataPartition(dataset2$edibility, p=0.8, list = FALSE)
train <- dataset2[ split_data, ]
test <- dataset2[ -split_data, ]

# making a tree without pruning
set.seed(100)
model_tree <- rpart(edibility ~ ., data = train, method = "class")

# making of the tree with rpart
set.seed(100)
# with CP parameter we are pruning (cutting) the tree branches, to avoid overfit
model_tree_cp <- rpart(edibility ~ ., data = train, method = "class", cp = 0.0011)
# with printcp we check the x validation error of our CP
printcp(model_tree_cp)
plotcp(model_tree_cp)
# with confusion Matrix we are checking FALSE POSITIVE and FALSE NEGATIVE
caret::confusionMatrix(data=predict(model_tree_cp, type = "class"),
                       reference = train$edibility, 
                       positive="edible")

# define penalty matrix
penalty_matrix <- matrix(c(0, 1, 10, 0), byrow = TRUE, nrow = 2) # why this matrix works only with 10 and 1? # the tree changes!
# applying a penalty matrix it's another method to improve the tree accuracy
model_tree_penalty <- rpart(edibility ~ ., data = train, method = "class", 
                            parms = list(loss = penalty_matrix))

model_tree_penalty
rpart.plot(model_tree_cp)
rpart.plot(model_tree_penalty)

caret::confusionMatrix(data=predict(model_tree_penalty, type = "class"),
                       reference = train$edibility,
                       positive="edible")

# Pick the best CP automatically from the CPtable
bestcp <- round(model_tree_cp$cptable[which.min(model_tree_cp$cptable[, "xerror"]), "CP"], 5)
model_tree_pruned <- prune(model_tree_cp, cp = bestcp)

# TEST 0
test_tree <- predict(model_tree, newdata = test)
caret::confusionMatrix(data = predict(model_tree, newdata = test, type = "class"), 
                       reference = test$edibility, 
                       positive = "edible")

# TEST 1
test_tree_cp <- predict(model_tree_cp, newdata = test)
caret::confusionMatrix(data = predict(model_tree_cp, newdata = test, type = "class"), 
                       reference = test$edibility, 
                       positive = "edible")
# TEST 2
test_tree_penalty <- predict(model_tree_penalty, newdata = test)
caret::confusionMatrix(data = predict(model_tree_penalty, newdata = test, type = "class"), 
                       reference = test$edibility, 
                       positive = "edible")
# TEST 3
test_tree_pruned <- predict(model_tree_pruned, newdata = test)
caret::confusionMatrix(data = predict(model_tree_pruned, newdata = test, type = "class"), 
                       reference = test$edibility, 
                       positive = "edible")





# Define the function to get the more frequent value
get_mode <- function(x){
  return(names(sort(table(x), decreasing = T, na.last = T)[1]))
}

# Obtain the frequency of the column values
cleanedStalkRootLimit <- round(prop.table(summary(train$stalk.root)[1:4])
                               # and get their quantities in integers
                               *sum(is.na(train$stalk.root)))

# Extract the single quantities for each values 
b_bp <- cleanedStalkRootLimit[[1]]
c_bp <- cleanedStalkRootLimit[[2]]
e_bp <- cleanedStalkRootLimit[[3]]
r_bp <- cleanedStalkRootLimit[[4]]

# Using them as limits to know how many NA values replace in our row
train$stalk.root <- replace(train$stalk.root, which(is.na(train$stalk.root))[1:b_bp], 'b')
train$stalk.root <- replace(train$stalk.root, which(is.na(train$stalk.root))[1:c_bp], 'c')
train$stalk.root <- replace(train$stalk.root, which(is.na(train$stalk.root))[1:e_bp], 'e')
train$stalk.root <- replace(train$stalk.root, which(is.na(train$stalk.root))[1:r_bp], 'r')
### ACTUALLY THIS IS NOT ACCURATE, ASK JULIEN AND OTHER GUYS!!! ###

# Replace all the other NA values by taking the more frequent (mode)
train$bruises <- replace(train$bruises, which(is.na(train$bruises)),get_mode(train$bruises) ) 
train$odor <- replace(train$odor, which(is.na(train$odor)),get_mode(train$odor) ) 
train$gill.attachment <- replace(train$gill.attachment, which(is.na(train$gill.attachment)),get_mode(train$gill.attachment) ) 
train$gill.spacing <- replace(train$gill.spacing, which(is.na(train$gill.spacing)),get_mode(train$gill.spacing) ) 
train$gill.size <- replace(train$gill.size, which(is.na(train$gill.size)),get_mode(train$gill.size) ) 
train$gill.color <- replace(train$gill.color, which(is.na(train$gill.color)),get_mode(train$gill.color) ) 
train$stalk.shape <- replace(train$stalk.shape, which(is.na(train$stalk.shape)),get_mode(train$stalk.shape) ) 
train$stalk.surface.above.ring <- replace(train$stalk.surface.above.ring, which(is.na(train$stalk.surface.above.ring)),get_mode(train$stalk.surface.above.ring) ) 
train$stalk.surface.below.ring <- replace(train$stalk.surface.below.ring, which(is.na(train$stalk.surface.below.ring)),get_mode(train$stalk.surface.below.ring) ) 
train$stalk.color.above.ring <- replace(train$stalk.color.above.ring, which(is.na(train$stalk.color.above.ring)),get_mode(train$stalk.color.above.ring) ) 
train$stalk.color.below.ring <- replace(train$stalk.color.below.ring, which(is.na(train$stalk.color.below.ring)),get_mode(train$stalk.color.below.ring) ) 
train$veil.type <- replace(train$veil.type, which(is.na(train$veil.type)),get_mode(train$veil.type) ) 
train$veil.color <- replace(train$veil.color, which(is.na(train$veil.color)),get_mode(train$veil.color) ) 
train$ring.number <- replace(train$ring.number, which(is.na(train$ring.number)),get_mode(train$ring.number) ) 
train$ring.type <- replace(train$ring.type, which(is.na(train$ring.type)),get_mode(train$ring.type) ) 
train$spore.print.color <- replace(train$spore.print.color, which(is.na(train$spore.print.color)),get_mode(train$spore.print.color) ) 
train$population <- replace(train$population, which(is.na(train$population)),get_mode(train$population) ) 
train$habitat <- replace(train$habitat, which(is.na(train$habitat)),get_mode(train$habitat) )

summary(train$stalk.root)

ggplot(dataset, aes(x = odor, y = cap.color, col = class)) +
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("lightgreen", "purple"))

#corr1 = cor(train$class[which(!is.na(train$class))], train$cap.color[which(!is.na(train$cap.color))])

# plot(dataset , pch=20 , cex=0.5 , col="#69b3a2")

# pairs(dataset)

