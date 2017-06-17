# Functions for reading / working with Applied Biosystems qPCR data
# author: Alex Azzo
# NOTE: requires the 'gdata' package


read.AppliedBiosystemsPCR <- function(path_to_pcr_file, filetype = 'txt') {
    
    if(filetype == 'txt'){
        data <- read.AppliedBiosystemsPCR.txt(path_to_pcr_file)
    } else if (filetype == 'xls'){
        data <- read.AppliedBiosystemsPCR.xls(path_to_pcr_file)
    } else {
        print("Invalid filetype: choose 'txt' or 'xls'.")
        data <- NULL
    }
    
    return(data)
}


read.AppliedBiosystemsPCR.xls <- function(path_to_pcr_file) {
    
    # Requires the gdata package to read xls
    library(gdata)
    
    # Read in data
    whole_file <- read.xls(path_to_pcr_file, header = F)
    whole_file <- as.matrix(whole_file)
    
    # Separate out data lines
    pcr.df <- whole_file[whole_file[,3] != "",]
    colnames(pcr.df) <- pcr.df[1,]
    pcr.df <- pcr.df[-1,]
    pcr.df <- as.data.frame(pcr.df, stringsAsFactors = F)
    
    # Fix weird column names
    colnames(pcr.df) <- gsub(pattern = 'Cт', replacement = 'CT', x = colnames(pcr.df))
    colnames(pcr.df) <- gsub(pattern = 'Δ', replacement = 'd', x = colnames(pcr.df))
    colnames(pcr.df) <- gsub(pattern = ' ', replacement = '.', x = colnames(pcr.df))
    
    # Make large and small datasets
    pcr.df.full <- pcr.df
    pcr.df <- pcr.df.full[,c('Well', 'Sample.Name', 'Target.Name', 'CT')]
    pcr.df$Sample.Name <- as.factor(pcr.df$Sample.Name)
    pcr.df$Target.Name <- as.factor(pcr.df$Target.Name)
    pcr.df$CT <- as.numeric(pcr.df$CT)
    
    # Pull out description values
    endogenous.control <- unname(whole_file[whole_file[,1] == 'Endogenous Control', 2])
    reference.sample <- unname(whole_file[whole_file[,1] == 'Reference Sample', 2])
    chemistry <- unname(whole_file[whole_file[,1] == 'Chemistry', 2])
    instrument.filename <- unname(whole_file[whole_file[,1] == 'Experiment File Name', 2])
    run.endtime <- unname(whole_file[whole_file[,1] == 'Experiment Run End Time', 2])
    
    # Return data and descriptions as list
    return(list(pcr.df = pcr.df, 
                pcr.df.full = pcr.df.full,
                endogenous.control = endogenous.control, 
                reference.sample = reference.sample, 
                chemistry = chemistry, 
                instrument.filename = instrument.filename,
                run.endtime = run.endtime))
}


read.AppliedBiosystemsPCR.txt <- function(path_to_pcr_file) {
    
    # Read in Applied Biosystems csv file
    raw_text <- readLines(path_to_pcr_file, encoding="UTF-8")
    
    # Separate out data from description lines
    to_keep <- 
        unlist(lapply(strsplit(raw_text, '\t'), length)) == 
        max(unlist(lapply(strsplit(raw_text, '\t'), length)))
    
    data_kept <- raw_text[to_keep]
    data_kept <- paste(data_kept, collapse = '\n')
    
    pcr.df <- read.table(text = data_kept, header = T, sep = '\t', stringsAsFactors = F)
    
    # Change weird symbols on df column names
    colnames(pcr.df) <- gsub(pattern = 'Cт', replacement = 'CT', x = colnames(pcr.df))
    colnames(pcr.df) <- gsub(pattern = 'Δ', replacement = 'd', x = colnames(pcr.df))
    
    # Make large and small datasets
    pcr.df.full <- pcr.df
    pcr.df <- pcr.df.full[, c('Well', 'Sample.Name', 'Target.Name', 'CT')]
    pcr.df$Sample.Name <- as.factor(pcr.df$Sample.Name)
    pcr.df$Target.Name <- as.factor(pcr.df$Target.Name)
    pcr.df$CT <- as.numeric(pcr.df$CT)
    
    # Function to pull out description line values
    get_file_parameter <- function(x, pattern) {
        line = x[grepl(pattern = pattern, x = x)]
        if (length(line) == 1) {
            return(unlist(strsplit(x = line, split = paste(pattern, ' = ', sep = '')))[2])
        } else {
            return(NULL)
        }
    }
    
    # Pull out description values
    endogenous.control <- get_file_parameter(raw_text, 'Endogenous Control')
    reference.sample <- get_file_parameter(raw_text, 'Reference Sample')
    chemistry <- get_file_parameter(raw_text, 'Chemistry')
    instrument.filename <- get_file_parameter(raw_text, 'Experiment File Name')
    run.endtime <- get_file_parameter(raw_text, 'Experiment Run End Time')
    
    # Return data and descriptions as list
    return(list(pcr.df = pcr.df, 
                pcr.df.full = pcr.df.full,
                endogenous.control = endogenous.control, 
                reference.sample = reference.sample, 
                chemistry = chemistry, 
                instrument.filename = instrument.filename,
                run.endtime = run.endtime))
}



