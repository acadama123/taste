# For dataIn, assume column 1 contains user IDs, column 2 contains item IDs.
# Ensure that train set contains all userIDs and itemIDs

# If the targetList contains all items in the checkList, returns True
does_contain_all <- function(targetList, checkList)
{
        numElementsLacked <- sum(!(checkList %in% targetList))
        return(numElementsLacked == 0)
}

# Set verbose to TRUE if want to see how many resampling trials the function needed.
# TODO: Need train set size checking to make sure it's possible to get all users and items
sampling_data_small <- function(dataIn, sampleSize, verbose = FALSE)
{
        if (sampleSize > nrow(dataIn)) {
                print("Error: Sample size greater than data size.")
                return(-1)
        }

        uniqueUsersTotal <- unique(dataIn[,1])
        uniqueItemsTotal <- unique(dataIn[,2])
        samplingIdx <- sample(1:nrow(dataIn), sampleSize)
        sampleData <- dataIn[samplingIdx,]
        i <- 1
        while (!does_contain_all(sampleData[,1], uniqueUsersTotal) ||
               !does_contain_all(sampleData[,2], uniqueItemsTotal)) {
                samplingIdx <- sample(1:nrow(dataIn), sampleSize)
                sampleData <- dataIn[samplingIdx,]
                i <- i + 1
        }
        if (verbose) {
                print("Trials needed: ")
                print(i)
        }
        return(sampleData)
}

# TODO: Find a more efficient way to sample; Main bottle neck seems to be in user sampling
sampling_data_big <- function(dataIn, sampleSize, verbose = FALSE)
{
        if (sampleSize > nrow(dataIn)) {
                print("Error: Sample size greater than data size.")
                return(-1)
        }

        uniqueUsers <- unique(dataIn[,1])
        sampleUserIdxs <- vector(mode = "integer", length = length(uniqueUsers))
        for (i in 1:length(uniqueUsers)) {
                userIdxs <- which(dataIn[,1] == uniqueUsers[i])
                sampleUserIdxs[i] <- userIdxs[sample(1:length(userIdxs),1)]
        }
        sampleData <- dataIn[sampleUserIdxs,]
        if (verbose)
                print("Train set: User sample done")

        dataIn2 <- dataIn[-sampleUserIdxs,]
        uniqueItemsTotal <- unique(dataIn[,2])
        uniqueItemsRemaining <- uniqueItemsTotal[!(uniqueItemsTotal %in% sampleData[,2])]
        sampleItemIdxs <- vector(mode = "integer", length = length(uniqueItemsRemaining))
        for (j in 1:length(uniqueItemsRemaining)) {
                itemIdxs <- which(dataIn2[,2] == uniqueItemsRemaining[j])
                sampleItemIdxs[j] <- itemIdxs[sample(1:length(itemIdxs),1)]
        }
        sampleData <- rbind(sampleData, dataIn2[sampleItemIdxs,])
        if (verbose)
                print("Train set: Item sample done")

        if (nrow(sampleData) > sampleSize)
                return(sampleData)

        dataIn3 <- dataIn2[-sampleItemIdxs,]
        remSampleSize <- sampleSize - nrow(sampleData)
        samplingIdxs <- sample(1:nrow(dataIn3), remSampleSize)
        sampleData <- rbind(sampleData, dataIn3[samplingIdxs,])
        return(sampleData)
}

# Train, validation, and test set division restriction:
#       Train set has to contain all userID and itemID in the overal dataset
#       Validation set and test set don't have the same restriction
# ratio is a list of 3 percentages, indicating how to divide dataIn into the 3 sets
#       ratio = [trainSetPercentage, validationSetPercentage, testSetPercentage]
# TODO: Check if actual size of train set is larger than trainSetSize
create_experiment_sets <- function(dataIn, ratio, verbose = FALSE)
{
        if (sum(ratio) != 1) {
                print("Error: Ratios don't sum up to 1.")
                return(-1)
        }

        trainSetSize <- floor(nrow(dataIn) * ratio[1])
        validationSetSize <- floor(nrow(dataIn) * ratio[2])
        testSetSize <- nrow(dataIn) - trainSetSize - validationSetSize

        if (trainSetSize < 100000)
                trainSet <- sampling_data_small(dataIn, trainSetSize, verbose)
        else
                trainSet <- sampling_data_big(dataIn, trainSetSize, verbose)
        if (verbose)
                print("Train set sampling done")

        if (nrow(trainSet) > trainSetSize) {
                print("Warning: Train set ratio modified to fit all users and items.")
                print("Expected ratio:")
                print(ratio[1])
                print("Actual ratio:")
                newTrainRatio <- nrow(trainSet) / nrow(dataIn)
                print(newTrainRatio)

                print("Adjusting ratios of validation set and of test set.")
                validationPortion <- ratio[2] / (ratio[2] + ratio[3])
                testPortion <- 1 - validationPortion
                newSumRatio <- 1 - newTrainRatio
                newValidationRatio <- newSumRatio * validationPortion
                newTestRatio <- newSumRatio * testPortion
                trainSetSize <- nrow(trainSet)
                validationSetSize <- floor(nrow(dataIn) * newValidationRatio)
                testSetSize <- nrow(dataIn) - trainSetSize - validationSetSize

                print("New validation set ratio:")
                print(newValidationRatio)
                print("New test set ratio:")
                print(newTestRatio)
        }

        sampledRows <- as.numeric(rownames(trainSet))
        # We can do this since row index == row name for all rows
        leftovers <- dataIn[-sampledRows,]
        # Rename row names so we can reuse the trick above for getting the test set
        rownames(leftovers) <- 1:nrow(leftovers)

        samplingIdx <- sample(1:nrow(leftovers), validationSetSize)
        validationSet <- leftovers[samplingIdx,]
        if (verbose)
                print("Validation set sampling done")

        sampledRows <- as.numeric(rownames(validationSet))
        testSet <- leftovers[-sampledRows,]
        if (verbose)
                print("Test set sampling done")

        experiment_sets <- list(trainSet = trainSet, validationSet = validationSet, testSet = testSet)
        class(experiment_sets) <- "expSets"
        return(experiment_sets)
}

# Saves the experiment sets.
save_experiment_sets <- function(expSets, setNames)
{
        trainSet <- expSets$trainSet
        validationSet <- expSets$validationSet
        testSet <- expSets$testSet
        write.csv(trainSet, setNames[1], row.names = FALSE)
        write.csv(validationSet, setNames[2], row.names = FALSE)
        write.csv(testSet, setNames[3], row.names = FALSE)
}

#-----------------------How to create the experiment sets-----------------------
#library(lme4)
#ie <- InstEval
#ratio <- c(0.7,0.2,0.1)
#e <- create_experiment_sets(ie, ratio)
#save_experiment_sets(e)
