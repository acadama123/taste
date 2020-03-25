library(data.table)

#-------------------------------kNN utility funcs-------------------------------
# Converts dataIn from a data.frame object to a matrix.
# Rows represent users; columns represent items; entries represent ratings.
# Items that a user hasn't rated will have the value NA.
data_to_matrix <- function(dataIn) {
        dt <- as.data.table(dataIn)

        userID <- names(dataIn)[1]
        itemID <- names(dataIn)[2]
        valueName <- names(dataIn)[3]
        formula <- paste(c(userID, itemID), collapse = '~')
        table <- dcast(dt, formula, fill = NA, value.var=valueName)

        # The first column of table will be the user IDs, which we don't need in our matrix
        mat <- as.matrix(table[,-1])

        # Assign the row names of the new matrix with our user IDs
        rNames <- as.list(table[,1])
        rNames <- rNames[[names(rNames)]]
        row.names(mat) <- rNames
        return(mat)
}

# TODO: Decide whether to remove this func, as general use should allow
# negative ratings, meaning this func will become a single if statement.
is_valid_values <- function(k, maxRating, simFunc, simMode, progressSaving, fileName)
{
        valid <- TRUE
        if (k <= 0) {
                print("Error: k can't be less than or equal to 0.")
                valid <- FALSE
        }
        if (maxRating < 1) {
                print("Error: maxRating can't be less than 1.")
                valid <- FALSE
        }
        if (is.null(simFunc)) {
                possibleVals <- c("rbfk", "correlation", "chebychev", "euclidean", "manhattan", "cosine")
                if (!(simMode %in% possibleVals)) {
                        print("Error: invalid similarity mode.")
                        valid <- FALSE
                }
        }
        if (progressSaving && is.null(fileName)) {
                print("Warning: No file name provided for progress saving process.")
        }
        return(valid)
}

#-----------------------------kNN similarity funcs-----------------------------

# Similarity evaluation rule: bigger value == more similar

get_vlen <- function(v) sqrt(v %*% v)

#----------Radial Basis Function Kernel similarity----------
# https://en.wikipedia.org/wiki/Radial_basis_function_kernel
# TODO: Add sigma as a parameter. Consider how this would affect sim func. choosing as well.
rbfkSim <- function(potentialUser, targetUser, extraArgs = NULL)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        sigma <- 1
        exp(-1 * (get_vlen(xvec - yvec) / (2 * (sigma ^ 2))))
}

#----------Correlation similarity----------
correlSim <- function(potentialUser, targetUser, extraArgs = NULL)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        xMean <- mean(xvec)
        yMean <- mean(yvec)
        xSd <- sd(xvec)
        ySd <- sd(yvec)
        mean((xvec * yvec) - (xMean * yMean)) / (xSd * ySd)
}

#----------Chebychev similarity----------
chebychevSim <- function(potentialUser, targetUser, extraArgs = NULL)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        # Once power by infinity, the max element will overtake all other ones.
        1 / max(abs(xvec - yvec))
}

#----------Euclidean similarity----------
euclideanSim <- function(potentialUser, targetUser, extraArgs = NULL)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        1 / get_vlen(xvec - yvec)
}

#----------Manhattan similarity----------
manhattanSim <- function(potentialUser, targetUser, extraArgs = NULL)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        1 / sum(abs(xvec - yvec))
}

#----------Cosine similarity----------
cosineSim <- function(potentialUser, targetUser, extraArgs = NULL)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        (xvec %*% yvec) / (get_vlen(xvec) * get_vlen(yvec))
}

#-------------------------------kNN calculations-------------------------------

# Find users who have rated the target item.
find_rated_users <- function(dataIn, targetItemIdx)
{
        return(dataIn[which(!is.na(dataIn[,targetItemIdx])),])
}

# Calculate each rated user's similarity to the target user.
# Here is where to choose which similarity function to use.
# NOTE: Make sure to change the similarity function used in both the if and the
#       else statements if you're planning of using only one similarity function.
# TODO: Add option for user to choose which sim func to use.
calc_sims <- function(ratedUsers, targetUser, numRatedUsers, simFunc, extraArgs = NULL)
{
        if (numRatedUsers == 1) {
                simVec <- simFunc(ratedUsers, targetUser, extraArgs)
        } else {
                simVec <- vector(mode = "numeric", length = nrow(ratedUsers))
                for (i in 1:numRatedUsers)
                        simVec[i] <- simFunc(ratedUsers[i,], targetUser, extraArgs)
        }
        return(simVec)
}

# Calculates, for each possible rating, the probability that targetUser will give
# said rating for targetItem.
calc_rating_probs <- function(targetUser, targetItemIdx, ratedUsers,
                              simVec, k, maxRating)
{
        # Each element in simVec corresponds to the same element in ratedUsers
        sortOrder <- sort(simVec, decreasing = TRUE, na.last = TRUE, index.return = TRUE)
        if (length(sortOrder$ix) == 0) {
                # No nearest neighbours found
                # Return probability vector of all zeros.
                return(vector(mode = "numeric", length = maxRating))
        } else {
                if (length(sortOrder$ix) == 1) {
                        # Only 1 neighbor
                        nearestNeighbours <- ratedUsers
                        numNN <- 1
                        kNN <- nearestNeighbours[targetItemIdx]
                } else {
                        # If don't have k nearest neighbours, use as many as there are
                        nearestNeighbours <- ratedUsers[sortOrder$ix,]
                        numNN <- min(k, nrow(nearestNeighbours))
                        kNN <- nearestNeighbours[1:numNN,targetItemIdx]
                }
                # Vector of each rating's frequency among k nearest neighbours
                ratings <- vector(mode = "numeric", length = maxRating)
                for (rating in 1:maxRating)
                        ratings[rating] = sum(kNN == rating) / numNN
                return(ratings)
        }
}

# Saves the current rating prediction matrix.
# TODO: Add option for choosing saving iteration and number of save files to keep
save_mat <- function(ratingPredMat, i, fileName, saveIter, numSaveFiles)
{
        print("Saving matrix")
        name <- paste(fileName, "_", i, ".mat", sep="")
        write.csv(ratingPredMat, name, row.names = FALSE)
        # Only keep latest numSaveFiles iterations
        oldFileName <- paste(fileName, "_", i - numSaveFiles * saveIter, ".mat", sep="")
        if (file.exists(oldFileName))
                unlink(oldFileName)
}

# find_kNN's assumptions:
#       1. newXs doesn't contain any new users or items
#       2. ratings have consecutive integer values, ranging from 1 to maxRating.
# This version of kNN is for cases where dataIn can be entirely converted into a matrix.
#       i.e. (# unique users * # unique items) <= .Machine$integer.max
find_kNN_small <- function(dataIn, k, maxRating, newXs, verbose = FALSE,
                           simFunc = NULL, simMode = "cosine", extraArgs = NULL,
                           progressSaving = FALSE, fileName = NULL,
                           saveIter = 10000, numSaveFiles = 5)
{
        if(is_valid_values(k, maxRating, simFunc, simMode, progressSaving, fileName) == FALSE)
                return(-1)

        dataMat <- data_to_matrix(dataIn)
        ratingPredMat <- matrix(nrow = nrow(newXs), ncol = maxRating)
        for (i in 1:nrow(newXs)) {
                if (verbose)
                        print(i)
                targetUserIdx <- which(rownames(dataMat) == newXs[i,1])
                targetItemIdx <- which(colnames(dataMat) == newXs[i,2])

                if (!is.na(dataMat[targetUserIdx, targetItemIdx])) {
                        # Known rating
                        ratings <- vector(mode = "integer", length = maxRating)
                        ratings[dataMat[targetUserIdx, targetItemIdx]] = 1
                        ratingPredMat[i,] <- ratings
                } else {
                        ratedUsers <- find_rated_users(dataMat, targetItemIdx)
                        targetUser <- dataMat[targetUserIdx,]
                        numRatedUsers <- sum(!is.na(dataMat[,targetItemIdx]))
                        if (!is.null(simFunc)) {
                                simVec <- calc_sims(ratedUsers, targetUser, numRatedUsers, simFunc)
                        } else {
                                simVec <- switch(
                                        simMode,
                                       "rbfk" = calc_sims(ratedUsers, targetUser, numRatedUsers, rbfkSim),
                                       "correlation" = calc_sims(ratedUsers, targetUser, numRatedUsers, correlSim),
                                       "chebychev" = calc_sims(ratedUsers, targetUser, numRatedUsers, chebychevSim),
                                       "euclidean" = calc_sims(ratedUsers, targetUser, numRatedUsers, euclideanSim),
                                       "manhattan" = calc_sims(ratedUsers, targetUser, numRatedUsers, manhattanSim),
                                       "cosine" = calc_sims(ratedUsers, targetUser, numRatedUsers, cosineSim)
                                )
                        }

                        ratingPredMat[i,] <- calc_rating_probs(targetUser,
                                                               targetItemIdx,
                                                               ratedUsers,
                                                               simVec,
                                                               k,
                                                               maxRating)
                }
                if (progressSaving && i %% saveIter == 0)
                        save_mat(ratingPredMat, i, fileName, saveIter, numSaveFiles)
        }
        return(ratingPredMat)
}

# This version of kNN is for cases where dataIn can NOT be entirely converted into a matrix.
#       i.e. (# unique users * # unique items) > .Machine$integer.max
find_kNN_big <- function(dataIn, k, maxRating, newXs, verbose = FALSE,
                         simFunc = NULL, simMode = "cosine", extraArgs = NULL,
                         progressSaving = FALSE, fileName = NULL,
                         saveIter = 10000, numSaveFiles = 5)
{
        if(is_valid_values(k, maxRating, simFunc, simMode, progressSaving, fileName) == FALSE)
                return(-1)

        ratingPredMat <- matrix(nrow = nrow(newXs), ncol = maxRating)
        for (i in 1:nrow(newXs)) {
                if (verbose)
                        print(i)
                foundRating <- dataIn[,1] == newXs[i,1] & dataIn[,2] == newXs[i,2]

                # Assume no repeated rating
                if (sum(foundRating) > 0) {
                        # Known rating
                        ratings <- vector(mode = "integer", length = maxRating)
                        ratings[dataIn[foundRating,3]] = 1
                        ratingPredMat[i,] <- ratings
                } else {
                        # Get users who have rated item newXs[i,2]
                        ratedUsersIDs <- dataIn[dataIn[,2] == newXs[i,2],1]
                        targetUserID <- newXs[i,1]

                        ratedUsersIdx <- which(dataIn[,1] %in% ratedUsersIDs)
                        targetUserIdx <- which(dataIn[,1] %in% targetUserID)

                        dataExtract <- data_to_matrix(dataIn[c(ratedUsersIdx,targetUserIdx),])

                        # Get target user idx of dataExtract
                        targetUserIdx <- which(rownames(dataExtract) == newXs[i,1])
                        targetItemIdx <- which(colnames(dataExtract) == newXs[i,2])

                        ratedUsers <- dataExtract[-targetUserIdx,]
                        targetUser <- dataExtract[targetUserIdx,]
                        numRatedUsers <- length(ratedUsersIDs)
                        if (!is.null(simFunc)) {
                                simVec <- calc_sims(ratedUsers, targetUser, numRatedUsers, simFunc)
                        } else {
                                simVec <- switch(
                                        simMode,
                                       "rbfk" = calc_sims(ratedUsers, targetUser, numRatedUsers, rbfkSim),
                                       "correlation" = calc_sims(ratedUsers, targetUser, numRatedUsers, correlSim),
                                       "chebychev" = calc_sims(ratedUsers, targetUser, numRatedUsers, chebychevSim),
                                       "euclidean" = calc_sims(ratedUsers, targetUser, numRatedUsers, euclideanSim),
                                       "manhattan" = calc_sims(ratedUsers, targetUser, numRatedUsers, manhattanSim),
                                       "cosine" = calc_sims(ratedUsers, targetUser, numRatedUsers, cosineSim)
                                )
                        }
                        ratingPredMat[i,] <- calc_rating_probs(targetUser,
                                                               targetItemIdx,
                                                               ratedUsers,
                                                               simVec,
                                                               k,
                                                               maxRating)
                }
                if (progressSaving && i %% saveIter == 0)
                        save_mat(ratingPredMat, i, fileName, saveIter, numSaveFiles)
        }
        return(ratingPredMat)
}

#-----------------------------kNN testing-----------------------------

# Calculates the percentage of accurate predictions.
# We consider a prediction as the rating (or ratings) with the highest probability
# in the rating distribution.
# Ex:
# ratingPredMat[i,] =           [1] [2] [3] [4] [5]
#                       [i]     0.0 0.2 0.3 0.4 0.1
# => For newXs[i,], kNN predicts the rating to be 4.
get_accuracy <- function(ratingPredMat, maxRating, newXs, correctRatings)
{
        numCorrect <- 0
        for (i in 1:nrow(newXs)) {
                maxProb <- max(ratingPredMat[i,])
                if (ratingPredMat[i,correctRatings[i]] == maxProb)
                        numCorrect <- numCorrect + 1
        }
        return(numCorrect / nrow(newXs))
}

# Runs find_kNN (small or big) and tests the predictions with know ratings.
# TODO: Add option to save results or not.
test_kNN <- function(dataIn, k, maxRating, newXs, correctRatings, verbose = FALSE,
                     simFunc = NULL, simMode = "cosine", extraArgs = NULL,
                     progressSaving = FALSE, fileName = NULL,
                     saveIter = 10000, numSaveFiles = 5)
{
        if (nrow(newXs) != length(correctRatings)) {
                print("Error: Number of prediction entries doesn't match number of correct ratings.")
                return(-1)
        } else {
                if ((.Machine$integer.max / length(unique(dataIn[,1]))) > length(unique(dataIn[,2]))) {
                        ratingPredMat <- find_kNN_small(dataIn, k, maxRating, newXs, verbose = verbose,
                                                        simFunc = simFunc, simMode = simMode, extraArgs = extraArgs,
                                                        progressSaving = progressSaving, fileName = fileName,
                                                        saveIter = saveIter, numSaveFiles = numSaveFiles)
                } else {
                        ratingPredMat <- find_kNN_big(dataIn, k, maxRating, newXs, verbose = verbose,
                                                      simFunc = simFunc, simMode = simMode, extraArgs = extraArgs,
                                                      progressSaving = progressSaving, fileName = fileName,
                                                      saveIter = saveIter, numSaveFiles = numSaveFiles)
                }
                ratingPredMeans <- colMeans(ratingPredMat)
                actualMeans <- vector(mode = "numeric", length = maxRating)
                for (rating in 1:maxRating)
                        actualMeans[rating] <- length(correctRatings[correctRatings == rating]) / length(correctRatings)

                # Print the rating distribution for both the predicted ratings
                # and the actual ratings, as well as the mean absolute
                # prediction error between them.
                print("Rating distributions:")
                print("For predicted ratings:")
                print(ratingPredMeans)
                print("For actual ratings:")
                print(actualMeans)
                print("MAPE value between the above 2 proportions:")
                print(mean(abs(ratingPredMeans - actualMeans)))

                # Print accuracy percentage
                print("Percentage of accurate predictions:")
                print(get_accuracy(ratingPredMat, maxRating, newXs, correctRatings))

                if (!is.null(fileName)) {
                        probResults <- rbind(ratingPredMeans, actualMeans)
                        probName <- paste(fileName, ".result", sep="")
                        matName <- paste(fileName, ".mat", sep="")
                        write.csv(probResults, probName, row.names = FALSE)
                        write.csv(ratingPredMat, matName, row.names = FALSE)
                }
        }
}
