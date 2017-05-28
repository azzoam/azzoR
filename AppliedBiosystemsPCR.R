# Functions for reading / working with Applied Biosystems qPCR data
# author: Alex Azzo

read.AppliedBiosystemsPCR <- function(path_to_pcr_file) {
    
    # Read in Applied Biosystems csv file
    raw_text <- readLines(path_to_pcr_file, encoding="UTF-8")
    
    # Separate out data from description lines
    to_keep <- 
        unlist(lapply(strsplit(raw_text, '\t'), length)) == 
        max(unlist(lapply(strsplit(raw_text, '\t'), length)))
    
    data_kept <- raw_text[to_keep]
    data_kept <- paste(data_kept, collapse = '\n')
    
    pcr.df <- read.table(text = data_kept, header = T, sep = '\t')
    
    # Change weird symbols on df column names
    colnames(pcr.df) <- gsub(pattern = 'Cт', replacement = 'CT', x = colnames(pcr.df))
    colnames(pcr.df) <- gsub(pattern = 'Δ', replacement = 'd', x = colnames(pcr.df))
    
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
                endogenous.control = endogenous.control, 
                reference.sample = reference.sample, 
                chemistry = chemistry, 
                instrument.filename = instrument.filename,
                run.endtime = run.endtime))
}



