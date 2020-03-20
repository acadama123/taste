require(data.table)

# specialArgs contains k (i.e. number of nearest neighbours)
KNN_setup <- function(dataIn, maxRating, specialArgs)
{
        trainData <- dataIn
        if ((.Machine$integer.max / length(unique(dataIn[,1]))) > length(unique(dataIn[,2]))) {
                dataSetSize <- "small"
        } else {
                dataSetSize <- "big"
        }
        maxRating <- maxRating
        KNNData <- list(trainData = dataIn, mode = dataSetSize,
                        k = specialArgs, maxRating = maxRating)
        class(KNNData) <- "recProbs"
        return(KNNData)
}

KNN_predict <- function(probsFitOut, newXs)
{
        if (probsFitOut$mode == "small")
                find_kNN_small(probsFitOut$trainData,
                               probsFitOut$k,
                               probsFitOut$maxRating,
                               newXs)
        else
                find_kNN_big(probsFitOut$trainData,
                             probsFitOut$k,
                             probsFitOut$maxRating,
                             newXs)
}

#-----------------------------kNN parameters checking-----------------------------

is_valid_values <- function(k, maxRating)
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
        return(valid)
}

#-----------------------------kNN similarity funcs-----------------------------

# Similarity value: the bigger == the more similar

get_vlen <- function(v) sqrt(v %*% v)

#----------In-house similarity----------

inhouseSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        yLen <- get_vlen(yvec)
        dotProd <- (xvec %*% yvec)
        pCos <- dotProd / yLen
        p_tCos <- ((xvec - yvec) %*% yvec) / yLen
        cosine <- dotProd / (get_vlen(xvec) * yLen)
        (pCos - p_tCos) * cosine
}

# Since rbfk similarity assigns higher values for more similar pair of vector
# (value range: [0,1]), to match with our similarity criteria, we subtract said value
# from 1.
#----------Radial Basis Function Kernel similarity----------

rbfkSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        sigma <- 1
        exp(-1 * (get_vlen(xvec - yvec) / (2 * (sigma ^ 2))))
}

#----------Correlation similarity----------

correlSim <- function(potentialUser, targetUser)
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

chebychevSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        # Once power by infinity, the max element will overtake all other ones.
        1 / max(abs(xvec - yvec))
}

#----------Euclidean similarity----------

euclideanSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        1 / get_vlen(xvec - yvec)
}

#----------Manhattan similarity----------

manhattanSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        1 / sum(abs(xvec - yvec))
}

#----------Cosine similarity----------

# Since more similar pair of vectors have higher cosine values (max = 1), to
# match our similarity criteria, we actually calculate the angle between
# the vectors, which explains the acos().
# Problem: c(1) & c(5) has a cosine val of 0 even though they are very different
cosineSim <- function(potentialUser, targetUser)
{
        # 1 & to compare each element of both vectors
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        ((xvec %*% yvec) / (get_vlen(xvec) * get_vlen(yvec)))
}

#-------------------------------kNN calculations-------------------------------

# Find users who have rated the target item.
find_rated_users <- function(dataIn, targetItemIdx)
{
        return(dataIn[which(!is.na(dataIn[,targetItemIdx])),])
}

# Calculate each rated user's similarity to the target user.
# Here is where to choose which similarity function to use.
# NOTE: Make sure to change the similarity function used in both the if and the else
#       statements if you're planning of using only one similarity function
calc_sims <- function(ratedUsers, targetUser, numRatedUsers)
{
        if (numRatedUsers == 1) {
                simVec <- inhouseSim(ratedUsers, targetUser)
        } else {
                simVec <- vector(mode = "numeric", length = nrow(ratedUsers))
                for (i in 1:numRatedUsers)
                        simVec[i] <- inhouseSim(ratedUsers[i,], targetUser)
        }
        return(simVec)
}

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
                        # If don't have k nearest neighbours,
                        #       use as many as there are
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

save_mat <- function(ratingPredMat, i, fileName)
{
        print("Saving matrix")
        name <- paste(fileName,".mat", i, sep="")
        write.csv(ratingPredMat, name, row.names = FALSE)
        # Only keep latest 5 iterations
        oldFileName <- paste(fileName,".mat", i - 5*10000, sep="")
        if (file.exists(oldFileName))
                unlink(oldFileName)
}

# find_kNN's assumptions:
#       1. newXs doesn't contain any new users or items
#       2. ratings have consecutive integer values, ranging from 1 to maxRating.

# This version of kNN is for cases where dataIn can be entirely converted into a matrix.
#       i.e. (# unique users * # unique items) < .Machine$integer.max
find_kNN_small <- function(dataIn, k, maxRating, newXs,
                           fileName = NULL, verbose = FALSE, progressSaving = FALSE)
{
        if(is_valid_values(k, maxRating) == FALSE)
                return(-1)

        dataMat <- dataToMatrix(dataIn)
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
                        simVec <- calc_sims(ratedUsers, targetUser, numRatedUsers)
                        ratingPredMat[i,] <- calc_rating_probs(targetUser,
                                                               targetItemIdx,
                                                               ratedUsers,
                                                               simVec,
                                                               k,
                                                               maxRating)
                }
                if (progressSaving && i %% 10000 == 0)
                        save_mat(ratingPredMat, i, fileName)
        }
        return(ratingPredMat)
}

find_kNN_big <- function(dataIn, k, maxRating, newXs,
                         fileName = NULL, verbose = FALSE, progressSaving = FALSE)
{
        if(is_valid_values(k, maxRating) == FALSE)
                return(-1)
        # byrow = TRUE allows us to replace a matrix's row with a vector
        ratingPredMat <- matrix(nrow = nrow(newXs), ncol = maxRating, byrow = TRUE)
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
                        ratedUsersIDs <- dataIn[which(dataIn[,2] == newXs[i,2]),1]
                        targetUserID <- newXs[i,1]

                        ratedUsersIdx <- which(dataIn[,1] %in% ratedUsersIDs)
                        targetUserIdx <- which(dataIn[,1] %in% targetUserID)

                        dataExtract <- dataToMatrix(dataIn[c(ratedUsersIdx,targetUserIdx),])

                        targetUserIdx <- which(rownames(dataExtract) == newXs[i,1])
                        targetItemIdx <- which(colnames(dataExtract) == newXs[i,2])

                        ratedUsers <- dataExtract[-targetUserIdx,]
                        targetUser <- dataExtract[targetUserIdx,]
                        numRatedUsers <- length(ratedUsersIDs)
                        simVec <- calc_sims(ratedUsers, targetUser, numRatedUsers)
                        ratingPredMat[i,] <- calc_rating_probs(targetUser,
                                                               targetItemIdx,
                                                               ratedUsers,
                                                               simVec,
                                                               k,
                                                               maxRating)
                }
                if (progressSaving && i %% 10000 == 0)
                        save_mat(ratingPredMat, i, fileName)
        }
        return(ratingPredMat)
}

#-----------------------------kNN test-----------------------------

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

test_kNN <- function(dataIn, k, maxRating, newXs, correctRatings, fileName)
{
        if (nrow(newXs) != length(correctRatings)) {
                print("Different lengths of prediction entries and target ratings")
                return(-1)
        } else {
                if ((.Machine$integer.max / length(unique(dataIn[,1]))) > length(unique(dataIn[,2]))) {
                        ratingPredMat <- find_kNN_small(dataIn, k, maxRating, newXs, fileName)
                } else {
                        ratingPredMat <- find_kNN_big(dataIn, k, maxRating, newXs, fileName, TRUE, TRUE)
                }
                ratingPredMeans <- colMeans(ratingPredMat)
                actualMeans <- vector(mode = "numeric", length = maxRating)
                for (rating in 1:maxRating)
                        actualMeans[rating] <- length(correctRatings[correctRatings == rating]) / length(correctRatings)

                # Print average proportion of each rating in both the
                # predicted ratings and the actual ratings, as well as the
                # mean absolute prediction error between them.
                print("Average proportion of ratings:")
                print("     For predicted ratings:")
                print(ratingPredMeans)
                print("     For actual ratings:")
                print(actualMeans)
                print("MAPE value between the above 2 proportions:")
                print(mean(abs(ratingPredMeans - actualMeans)))

                # Print accuracy percentage
                print("Percentage of accurate predictions:")
                print(get_accuracy(ratingPredMat, maxRating, newXs, correctRatings))

                probResults <- rbind(ratingPredMeans, actualMeans)
                probName <- paste(fileName, ".result", sep="")
                matName <- paste(fileName, ".mat", sep="")
                write.csv(probResults, probName, row.names = FALSE)
                write.csv(ratingPredMat, matName, row.names = FALSE)
        }
}

dataToMatrix <- function(dataIn) {
  # turns data frame into data.table which is more enhanced than data.frame
  dt <- as.data.table(dataIn)

  # creates the table entries, and fill empty ones with NAs
  userID <- names(dataIn)[1]
  itemID <- names(dataIn)[2]
  valueName <- names(dataIn)[3]
  formula <- paste(c(userID, itemID), collapse = '~')
  table <- dcast(dt, formula, fill = NA, value.var=valueName)

  # The first column of table will be the userID, which we don't need in our matrix
  mat <- as.matrix(table[,-1])

  # Assign the row names of the new matrix with our userIDs
  rNames <- as.list(table[,1])
  rNames <- rNames[[names(rNames)]]
  row.names(mat) <- rNames
  return(mat)
}
