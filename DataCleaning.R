library(tidyverse)

#get filelist
files <- list.files("Text_inputs/")

#set an empty data frame to hold things
output <- as.data.frame(NULL)

# OK HERE WE GO! Increament the '1' below to go to the next file in the folder.

  file <- paste("Text_inputs/",files[1],sep='')
  data.raw <- readChar(file, file.info(file)$size)
  
  #get varnames for tables
  ## todo: get both the key and the value, store for later (currently just the key)
  m <- regexpr("(?<=((Row: ))).*", data.raw, perl = TRUE)
  x <- regmatches(data.raw,m)
  x.m <- regexpr("(?<=- ).*",x, perl = TRUE)
  x.val <- regmatches(x,x.m)
  
  d.row <- x.val
  
  m <- regexpr("(?<=((Table: ))).*", data.raw, perl = TRUE)
  x <- regmatches(data.raw,m)
  x.m <- regexpr("(?<=- ).*",x, perl = TRUE)
  x.val <- regmatches(x,x.m)
  
  d.table <- x.val
  
  m <- regexpr("(?<=((Column: ))).*", data.raw, perl = TRUE)
  x <- regmatches(data.raw,m)
  x.m <- regexpr("(?<=- ).*",x, perl = TRUE)
  x.val <- regmatches(x,x.m)
  
  d.column <- x.val
  
  #Smart thing to add later : get filters, turn them into columns with values.
  
  # grab column headers
  columnNames <- c("Year", d.table, d.column, d.row,"Value")
  
  # now for some MaGiC and yes I am a wizard
  m <- gregexpr("(?<=\n\n)Trip (.|\n)*?(?=Trip|\\Z)",data.raw, perl = TRUE)
  x.tables <- regmatches(data.raw,m)[[1]]
  x.tableCount <- length(x.tables)
  
  # exploit the csv engine with some lazy code
  for(i in 1:x.tableCount)  {
    x.table.current <- x.tables[i]
    fileConn <- file("temp.txt")
    writeLines(x.table.current, fileConn)
    close(fileConn)
    data.tableChunk <- read.csv("temp.txt",skip=3)
    unlink("temp.txt")
    
    # get the table  and the year
    m <- regexpr("(?<=(Trip ))\\d{4}", x.table.current, perl = TRUE)
    x.year <- regmatches(x.table.current,m)
    m <- regexpr("(?<=\n(Table: )).*", x.table.current, perl = TRUE)
    x.table <- regmatches(x.table.current,m)
    
    #tidy up an output frame and bind 'em together
    data.tableChunk <- data.tableChunk %>%
      gather(!!d.column,value,-X) %>%
      as.data.frame() %>%
      mutate(!!d.table := x.table, year = x.year) %>%
      select(c(5,4,1,2,3))
    output <- rbind(output, data.tableChunk)
    }
  
  #depending on how the previous table treats spaces in the mode_prime column
  #mode2f$mode_prime <- gsub(".", " ", mode2f$mode_prime, fixed = TRUE)
  
  output = left_join(output,
                     mode2f,
                     by = "mode_prime")
  
  
  output = output %>% relocate("mode_sec", .after = "mode_prime")

  colnames(output) <- c("Year",
                        "Mode",
                        "Category",
                        "Origin",
                        "Destination",
                        "Trips")

# note: the next line makes a file that could be way bigger than you want (100X the input file), so probably wiser to export 'output' as a data object.
# and obviously, convert all fields except the values field to factors to aid compresison.

#export to csv for further processing
#  write.csv(output,"Excel_outputs/filename.csv")

  rm(list=setdiff(ls(), "mode2f"))
