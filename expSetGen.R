# Assume data has 3 columns: userID, itemID, rating
# Ensure that sample contains all userIDs and itemIDs

# If the targetList contains all items in the checkList, returns True
does_contain_all <- function(targetList, checkList)
{
        numElementsLacked <- sum(!(checkList %in% targetList))
        return(numElementsLacked == 0)
}

# Set verbose to TRUE if want to see how many resampling trials the function needed
sampling_data <- function(dataIn, sampleSize, verbose = FALSE)
{
        if (sampleSize > nrow(dataIn)) {
                print("Error: Sample size greater than data size")
                return(-1)
        }
        uniqueUsersTotal <- unique(dataIn[,1])
        uniqueItemsTotal <- unique(dataIn[,2])
        samplingIdx <- sample(1:nrow(dataIn), sampleSize)
        sampleData <- dataIn[samplingIdx,]
        i <- 1
        while (!does_contain_all(sampleData[,1], uniqueUsersTotal)
            || !does_contain_all(sampleData[,2], uniqueItemsTotal)) {
                samplingIdx <- sample(1:nrow(dataIn), sampleSize)
                sampleData <- dataIn[samplingIdx,]
                i <- i + 1
                if (i %% 1000 == 0)
                        print("10k")
        }
        if (verbose) {
                print("Trials needed: ")
                print(i)
        }
        return(sampleData)
}

# This function is used to sample SongList's training set
sampling_data3 <- function(dataIn, sampleSize, verbose = FALSE)
{
        # Utilize the fact that Song List has 10 samples per user,
        # and the data set is ordered by user ids.
        localIdx <- sample(1:10, 200000, replace = TRUE)
        increment <- c(0:199999) * 10
        samplingIdx <- increment + localIdx
        sampleData <- dataIn[samplingIdx,]
        print("User sample done")

        dataIn2 <- dataIn[-samplingIdx,]
        uniqueItemsTotal <- unique(dataIn[,2])
        uniqueItemsRemaining <- uniqueItemsTotal[!(uniqueItemsTotal %in% sampleData[,2])]
        sampleItemsIdx <- vector(mode = "integer", length = length(uniqueItemsRemaining))
        for (j in 1:length(uniqueItemsRemaining)) {
                if (j %% 1000 == 0)
                        print("Item i")
                itemiIdxs <- which(dataIn2[,2] == uniqueItemsRemaining[j])
                sampleItemsIdx[j] <- itemiIdxs[sample(1:length(itemiIdxs),1)]
        }
        sampleData <- rbind(sampleData, dataIn2[sampleItemsIdx,])
        print("Item sample done")

        dataIn3 <- dataIn2[-sampleItemsIdx,]
        remSampleSize <- sampleSize - nrow(sampleData)
        samplingIdx <- sample(1:nrow(dataIn3), remSampleSize)
        sampleData <- rbind(sampleData, dataIn3[samplingIdx,])
        return(sampleData)
}

# Train, validation, and test set division restriction:
#       Train set has to contain all userID and itemID in the overal dataset
#       Validation set and test set don't have the same restriction
# ratio is a list of 3 percentages, indicating how to divide dataIn into the 3 sets
#       ratio = [trainSetPercentage, validationSetPercentage, testSetPercentage]
create_experiment_sets <- function(dataIn, ratio)
{
        if (sum(ratio) != 1) {
                print("Invalid ratios")
                return(-1)
        }

        trainSetSize <- floor(nrow(dataIn) * ratio[1])
        validationSetSize <- floor(nrow(dataIn) * ratio[2])
        testSetSize <- nrow(dataIn) - trainSetSize - validationSetSize

        trainSet <- sampling_data(dataIn, trainSetSize)
        print("Train set sampling done")

        sampledRows <- as.numeric(rownames(trainSet))
        # We can do this since row index == row name for all rows
        leftovers <- dataIn[-sampledRows,]
        # Rename row names so we can reuse the trick above for getting the test set
        rownames(leftovers) <- 1:nrow(leftovers)

        samplingIdx <- sample(1:nrow(leftovers), validationSetSize)
        validationSet <- leftovers[samplingIdx,]
        print("Validation set sampling done")

        sampledRows <- as.numeric(rownames(validationSet))
        testSet <- leftovers[-sampledRows,]
        print("Test set sampling done")

        experiment_sets <- list(trainSet = trainSet, validationSet = validationSet, testSet = testSet)
        class(experiment_sets) <- "expSets"
        return(experiment_sets)
}

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
#ie <- ie[,c(1,2,7)] # Only care about columns depicting userID, itemID, and rating
#ratio <- c(0.7,0.2,0.1)
#e <- create_experiment_sets(ie, ratio)
#save_experiment_sets(e)
