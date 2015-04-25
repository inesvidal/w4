rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    cases <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    # Prepare indexes for "string" positions in the rank
    
    ## Check that num and outcome are valid
    if ( num <= 0) {
        stop("invalid number for ranking, <= 0")
    }
    
    if (! outcome %in% names(cases)) {
        stop("invalid outcome")
    }
        
    # filter all hospitals by outcome
    outcome_data <- subset(data, data[[cases[[outcome]]]] != "Not Available")
    
    # keep only columns "State" "Hospital name" and "outcome rate"
    short_outcome_data <- outcome_data[c(7, 2, cases[[outcome]])]
    
    # Rank hospitals based on outcome rate per state
    ## sorted_hospitals <- tapply(short_outcome_data[[cases[[outcome]]]], states, 
    ##                           function(x) x[order(as.numeric(x[[cases[[outcome]]]]), tapply(x[["Hospital.Name"]])] )
    # no vale, estamos usando el indice viejo, que se sale de la matriz recortada
    #sorted_outcomes <- tapply(short_outcome_data[[3]], states, function(x) x[order(as.numeric(x[[3]]))])
    #rank_hospital <- tapply(sorted_outcomes, states, function(y) )
    sorted_hospitals <- short_outcome_data[order(short_outcome_data[[1]], as.numeric(short_outcome_data[[3]]), short_outcome_data[[2]]),]

    # Select hospital in selected position
    hospital_position <- function(data, num){
        position <- list("best" = 1, "worst" = length(data))
        # if "best" or "worst" are used
        rank <- num
        if (! is.numeric(num)) {
            rank <- position[[num]]
        } 
        else {
            # if the position in the ranking is out of the list
            if (num > position[["worst"]] ) {
                return("NA")
            }
        }
        
        # if the position in the ranking is out of the list
        # if a is a vector, and N > length(a), a[N] = NA, so no need to control it
        result <- data[rank]    
    }
    #ranked_hospitals <-tapply(sorted_hospitals, states, rank)
    ranked_hospitals <- tapply(sorted_hospitals[[2]], sorted_hospitals[[1]], hospital_position, num = num)
    
    # format output, shape, column names, and type (data.frame)
    result <- t(rbind(ranked_hospitals, rownames(ranked_hospitals)))
    colnames(result) <- c("hospital", "state")
    result <- data.frame(result)
    return(result)
    
}

# > source("rankall.R")
# > head(rankall("heart attack", 20), 10)
# hospital state
# AK <NA> AK
# AL D W MCMILLAN MEMORIAL HOSPITAL AL
# AR ARKANSAS METHODIST MEDICAL CENTER AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
# CA SHERMAN OAKS HOSPITAL CA
# CO SKY RIDGE MEDICAL CENTER CO
# CT MIDSTATE MEDICAL CENTER CT
# DC <NA> DC
# DE <NA> DE
# FL SOUTH FLORIDA BAPTIST HOSPITAL FL
# > tail(rankall("pneumonia", "worst"), 3)
# hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
# WV PLATEAU MEDICAL CENTER WV
# WY NORTH BIG HORN HOSPITAL DISTRICT WY
# > tail(rankall("heart failure"), 10)
# hospital state
# TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
# TX FORT DUNCAN MEDICAL CENTER TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
# VA SENTARA POTOMAC HOSPITAL VA
# VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
# VT SPRINGFIELD HOSPITAL VT
# WA HARBORVIEW MEDICAL CENTER WA
# WI AURORA ST LUKES MEDICAL CENTER WI
# WV FAIRMONT GENERAL HOSPITAL WV
# WY CHEYENNE VA MEDICAL CENTER WY