
#' Format and rename colnames of original biomarker dataframe
#'
#' The UK Biobank biomarker dataset column names are code names. This function recode those names to match those
#' of the look up table (bio.dict), so basically readable names.
#'
#' @param bio.original Original biomarker data frame with the first column being ID and the other columns
#' being biomarker code and some 0 or 1 depending whether the biomarker measurement was the 'at-recruitment' measurement
#' (0) or the one at follow-up (1)
#' @param measurement Measurement time to retrieve. Options: "first"(Default), "second" or "both". Note: if you
#' choose both the second biomarker colnames will be the biomarker names and ".1" appended to them, this will mess
#' up with the BHS calculator formatting and is not currently supported. Get in touch if you want to implement that.
#'
#' @return Biomarker data frame with human readable names
#'
#' @examples
#' library(HDATDS)
#' data("bio.original_example")
#'
#' bio = renameBio(bio.original_example)
#' @export
formatBio = function(bio.original, measurement = "first"){
  ##################################################################
  ##              Changing biomarkers codes by names              ##
  ##################################################################
  #make nicely looking names (programmingly functional)
  colnames(bio.dict) = make.names(colnames(bio.dict), unique=TRUE)

  #get column numbers of columns with name containing pattern *(.)1(.)*
  # use (.) to match the dot as opposed to using . as a wildcard

  if (measurement == "first"){
    bio = bio.original[,c(T, !grepl("*(.)1(.)0", colnames(bio.original)[-1]))]
  }else if (measurement == "second"){
    bio = bio.original[,c(T, !grepl("*(.)0(.)0", colnames(bio.original)[-1]))]
  } else if(measurement == "both"){
    bio = bio.original
  } else {
    stop("Please input a valid argument for measurement (Options: first, second, both")
  }

  # Match code with biomarker name to change column names of b
  # get element 2 to 6 of all string in vector colnames(b)
  # the match() function, match the substring from colnames to
  # the UK.biobank.field in the biomarkers dictionary,
  # effectively ordering the colnames of b
  # Alternative: order UK.bionbank.field entries and match them
  #---- bio.dict = bio.dict %>% arrange(UK.Biobank.Field)

  colnames(bio)[-1] = bio.dict$Biomarker.name[
    match(substring(colnames(bio)[-1],2,6),bio.dict$UK.Biobank.Field)]

  colnames(bio)[-1] = make.names(colnames(bio)[-1], unique=TRUE)
  colnames(bio)[-1] = sub("\\.\\.",".", colnames(bio)[-1])

  # safety-check for all vars being numeric
  stopifnot(all(apply(bio, 2, is.numeric)))

  bio
}


#' Calculate 1st or 3rd quantile for a given biomarker
#'
#' Given a column (biomarker), reference look-up table and dataset (biomarker dataset)
#' this function calculates the 1st quartile if the given biomarker is considered bad in shortage or
#' the 3rd quartile if the given biomarker is considered bad in excess
#'
#' @param column biomarker name ["string"] from biomarker data frame, needs to match one name in the
#' reference look up table
#' @param reference Reference "grading" to be taken from the look-up table (possible values: "Mantej",
#' "Paper", "Barbara")
#' @param dataset Biomarker data frame where quantiles will be calculated, this may be the whole data frame
#' or a subset of it as it is done internally in the BHS calculator when doing stratified BHS calculation
#'
#' @return quartile value for given biomarker, reference and dataset as well as type of quartile nuber (1st or 3rd)
#' @export
quantile_check = function(column, reference, dataset){
  # the "reference" column in this function is the one containing the info on whether or not
  # a given molecule is harmful in excess or in shortage, there are two references:
  # -- One from the biomarkers available in the paper
  # -- Another one created from expert knowledge (by Mantej) including more biomarkers than
  # in the original model

  # if the value of this reference column for a given biomarker is 1, it means that biomarker
  # is harmful in excess, in which case we select the 3rd quartile (75%) which will be our benchmark
  # MORE than this kind of benchmark ==> +1 BHS

  if (bio.dict[bio.dict$`Biomarker name` == column,
               reference] == 1){
    output = stats::quantile(dataset[,column])[4]
    quartile = "3rd"
  }
  # in the case the value of the reference column for a given biomarker is 0, it means the biomarker
  # is harmful in shortage, in which case we select the 1st quartile (25%) which will be our bencmark
  # LESS than this kind of benchmark ==> +1 BHS
  else if (bio.dict[bio.dict$`Biomarker name` == column,
                    reference] == 0){
    output = stats::quantile(dataset[,column])[2]
    quartile = "1st"
  }
  # Store info on biomarker with missing reference value, still will be removed later
  # In the references provided by Mantej, we wrote -1 for those biomarkers we were unsure if they
  # were beneficial or harmful (or irrelevant)
  #
  else{
    output = NA
    quartile = NA
  }
  list(output, quartile)
}


#' Biological Health Score (BHS) calculator
#'
#' This function implements the BHS calculation method presented by Karimi et. al(2018).
#'
#' @details The function has 3 main working parts:
#'
#' 1. Making a quantile look-up table: this part calls the function HDATDS::quantile_check() to calculate
#' the relevant quartiles for the given combination of biomarkers and reference choice.
#'
#' 2. For every biomarker (and each strata if relevant (and if stratified = TRUE)), get the score for each individual
#'
#' 3. Calculate the aggregated score (by systems if bySystems is TRUE)
#'
#' @references Karimi, Maryam, Raphaële Castagné, Cyrille Delpierre, Gaëlle Albertus, Eloïse Berger,
#'  Paolo Vineis, Meena Kumari, Michelle Kelly-Irving, and Marc Chadeau-Hyam. 2019. “Early-Life
#'  Inequalities and Biological Ageing: A Multisystem Biological Health Score Approach in
#'  Understanding Society.” Journal of Epidemiology and Community Health 73(8):693–702.
#'
#' @param bio_df Dataframe containing the biomarkers and the age and gender information if stratified analyis
#' is desired. Additionally the IDs need to be available in the dataframes rownames.
#' Example dataframe provided at: \code{HDATDS::bio.example}
#' @param reference Which reference from the look-up table to use. Options are: "Mantej", "Paper" or "Barbara"
#'  This reference is used for both the "by systems" calculation and the quantiles calculation
#' @param stratified Whether or not the BHS calculation should be performed stratifiying by age group and gender
#'  group or not (at the moment only these stratifications are supported and are not customisable)
#' @param bySystems Whether or not the BHS calculation should be done by first calculating scores by system and
#'  then calculating the mean across systems (bySystems = T) or simply taking the non weighted average of all the
#'  biomarker scores (bySystems = F)
#'  @param lookUpTable (set equal to bio.dict [internal variable] by default). Dataframe containing the reference
#'  values to look up. Can be made custom but will need to have the same biomarker names than bio.dict.
#'  Load bio.dict: \code{bio.dict = HDATDS::bio.dict}
#'
#' @return A vector containing all the biological health scores for all inviduals. The vector is sorted in the same
#'  order than the original data frame (bio_df) so that it can simply be concatenated (cbind) to the original dataframe.
#'  Additionally, the rownames of the output contain the original rownames (IDs)
#'
#'
#' @examples
#'
#'# Original biomarker dataframe
#' data("bio.original_example")
#'
#' bio = formatBio(bio.original, measurement = "first")
#'
#' # Load biomarkers and covariates data frames (bio and cov) and merge them by ID
#' bio = merge(bio, cov[,c("ID","age_cl","gender")], by = "ID")
#' ids = bio$ID # keeping explicit copy of IDs
#' rownames(bio) = bio[,1] # assuming ID is the first column
#' bio = bio[,-1]
#' bio$age_cl = as.factor(bio$age_cl)
#' bio$gender = as.factor(bio$gender)
#'
#' # Run BHS calculation using paper reference
#' scores_paper = BHSCalculator(bio, "Paper", stratified = T, bySystems = T)
#' @export
BHSCalculator = function(bio_df, reference, stratified = F, bySystems = T, lookUpTable = bio.dict){
  # reference can take values:
  # -- "Mantej"
  # or
  # -- "Paper"
  # or
  # -- "Barbara"

  MoreIsBad = paste0("MoreIsBad.",reference)


  # load(file = "data/bio.dict.rda") # apparently do't need to do this
  ##################################################################
  ##        First make a 'dictionary' storing the relevant        ##
  ##         quantiles for each biomarker and each strata         ##
  ##################################################################
  # store relevant quartile info in a dataframe
  if (stratified){
    relevant_quantiles = data.frame(Biomarker = character(0),
                                    Quantile.value = numeric(0),
                                    Quartile = character(0),
                                    AgeClass = character(0),
                                    Gender = character(0))

    for (gender in unique(bio_df$gender)){
      for (age.class in unique(bio_df$age_cl)){

        # getting the entries from the bio dataset where the cov$age_cl values are
        # equal to age.class
        bio.per.class = bio_df[bio_df$age_cl==age.class & bio_df$gender == gender,
                                -c(ncol(bio_df)-1, ncol(bio_df))]
        # -ncol(bio.imp) everywhere to avoid age_cl column

        result = data.frame(Biomarker = colnames(bio.per.class),
                            data.frame(matrix(
                              unlist(
                                lapply(colnames(bio.per.class),
                                       function(x) quantile_check(x, reference = MoreIsBad, bio.per.class))),
                              nrow=length(colnames(bio.per.class)),
                              byrow=T), stringsAsFactors = F),
                            stringsAsFactors = F)

        colnames(result)[2:3] = c("Quantile.value", "Quartile")
        result = cbind(result, AgeClass = age.class, Gender = gender)

        relevant_quantiles = rbind(relevant_quantiles, result)
      }
    }

    # take only complete cases
    relevant_quantiles = relevant_quantiles[complete.cases(relevant_quantiles),]

  } else {
    relevant_quantiles = data.frame(Biomarker = colnames(bio_df[,-c(ncol(bio_df)-1, ncol(bio_df))]),
                                    data.frame(matrix(
                                      unlist(
                                        lapply(colnames(bio_df[,-c(ncol(bio_df)-1, ncol(bio_df))]),
                                               function(x) quantile_check(x, reference = MoreIsBad, bio_df))),
                                      nrow=length(colnames(bio_df[,-c(ncol(bio_df)-1, ncol(bio_df))])),
                                      byrow=T), stringsAsFactors = F),
                                    stringsAsFactors = F)


    colnames(relevant_quantiles)[2:3] = c("Quantile.value", "Quartile")

    # take only complete cases
    relevant_quantiles = relevant_quantiles[complete.cases(relevant_quantiles),]
  }


  #######################################################################
  ##  Second go through every biomarker (and each strata if relevant)  ##
  ##                and get the score for each biomarker               ##
  #######################################################################
  # bio.scores stores for each individual whether its values are over/under the relevant quantiles
  # compare individual values for each biomark to their "benchmark"
  if (stratified){

    # initialising empty dataframe with dims of however many biomarkers are being considered
    # could add two extra columns for each strata
    bio.score = bio_df[F,unique(relevant_quantiles$Biomarker)]

    for (gender in unique(bio_df$gender)){
      for (age in unique(bio_df$age_cl)){
        # select entries of biomarker for which age is equal to iterations
        bio.sub  = bio_df[bio_df$age_cl==age & bio_df$gender == gender,
                           -c(ncol(bio_df)-1, ncol(bio_df))]
        # select quantiles for which age is equal to age iterations
        relevant_quantiles.age.gender = relevant_quantiles[relevant_quantiles$AgeClass==age &
                                                             relevant_quantiles$Gender == gender,]
        attach(relevant_quantiles.age.gender)
        res = lapply(Biomarker, # for each biomarker from relevant_quantiles.age.gender
                     function(x){ # if the biomarker is bad in excess (i.e. boundary is 3rd quartile)
                       # return the element-wise comparison to the relevant quartile value
                       ifelse(Quartile[Biomarker == x] == "3rd",
                              return(bio.sub[,x] >
                                       Quantile.value[Biomarker == x]),
                              return(bio.sub[,x] <
                                       Quantile.value[Biomarker == x]))
                     })
        res = data.frame(matrix(unlist(res),
                                ncol = length(Biomarker), byrow = F), # put in right format
                         stringsAsFactors = F)
        rownames(res) = rownames(bio.sub)
        colnames(res) = Biomarker
        #res$AgeCl = age
        bio.score = rbind(bio.score, res)

        # stopifnot(all(bio.imp$age_cl[bio.imp$age_cl==age]==res$AgeCl))
        # stopifnot(all(rownames(bio.imp$age_cl[bio.imp$age_cl==age])==rownames(res$AgeCl)))

        detach(relevant_quantiles.age.gender)
      }
    }


    # I actually don't need the age class column just need them to keep the original index
    #bio.score = bio.score[,-c(ncol(bio.score)-1, ncol(bio.score))]

  }else{
    attach(relevant_quantiles)
    # bio.score is a list containing for each biomarker (first subset of list)
    # for each individual whether the value of that biomarker is in the healthy/unhealthy range(FALSE, TRUE)
    # respectively
    bio.score = lapply(Biomarker,
                       function(x){
                         ifelse(Quartile[Biomarker == x] == "3rd",
                                return(bio_df[,x] > Quantile.value[Biomarker == x]),
                                return(bio_df[,x] < Quantile.value[Biomarker == x]))
                       })

    # bio.score is then turned into a nice data frame where each biomarker is a column
    # and each row is an individual
    # note: unlist returns a long vector which results from unlist the bio.score list into
    # a vector with a length equal to the number of elements in the original list.
    # byrow in this case must be equal to FALSE
    bio.score = data.frame(matrix(unlist(bio.score),
                                  ncol = length(Biomarker), byrow = F),
                           stringsAsFactors = F)
    rownames(bio.score) = rownames(bio_df)
    detach(relevant_quantiles)
  }

  # This is going on for every row (in code above)
  # bio.imp$Testosterone < relevant_quantiles$Quantile.value[relevant_quantiles$Biomarker == "Testosterone"]
  numbersofBio = ncol(bio.score)
  ##################################################################
  ##    3rd and finally get the mean score for each individual    ##
  ##              and sort in same order as bio.imp               ##
  ##################################################################


  if (bySystems){
    systems.ref = paste0("System.",reference)
    bio.score$ID = rownames(bio.score)
    bio.score = bio.score %>% tidyr::gather(key = "Biomarker", value = "Amount", -ID) %>%
      merge(y = bio.dict[,c(systems.ref, "Biomarker name")],
            all.x = T, by.x = "Biomarker", by.y = "Biomarker name")
    colnames(bio.score)[ncol(bio.score)] = "System"
    # get score for each individual by ID and individual system
    bio.score = bio.score %>% dplyr::group_by(ID, System) %>% dplyr::summarise(BHSsystem = mean(Amount))
    # the line below would give you dataframe with score by system
    # bio.score %>% spread(System, BHS)

    bio.score = bio.score %>% dplyr::group_by(ID) %>% dplyr::summarise(total_score = mean(BHSsystem))
    bio.score = as.data.frame(bio.score) # Setting row names on a tibble is deprecated.
    rownames(bio.score) = bio.score$ID
  }else {
    # get average "grade" over all the biomarkers
    bio.score$total_score = rowMeans(bio.score)
  }

  # sort bio.score in by the row.name as bio.imp
  bio.score = bio.score[rownames(bio_df),]
  stopifnot(all(rownames(bio_df)==rownames(bio.score)))
  print(paste(numbersofBio," biomarkers where assessed for this BHS calculation"))
  bio.score$total_score
}
