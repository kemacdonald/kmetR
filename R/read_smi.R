#' Read in data file created by the SMI tracker's IDF converter utility
#'
#' @param file_name A string with the name of the raw data file.
#' @param file_path A string with the path to the raw data file.
#' @param length_header A number indicating the number of lines in the header of the raw data file.
#' @return A data frame ready for preprocessing
#' @examples
#' read_smi(file_name = "eye_data_idf.txt", file_path = "../data/", length_header = 40)

read_smi <- function (file_name, file_path, length_header = 40) {

  file <- paste0(file_path, file_name)

  ## read the header from the file to paste back into the new file
  tmp.header <- scan(file, what = character(), sep="\n",
                     nlines = length_header, quiet = TRUE)

  ## trim to just header information
  header <- vector()
  for(index in 1:length(tmp.header)) {
    line <- tmp.header[index]
    header <- c(header, line)
    if (line == "## ") {break}
  }

  ## get subject id from the header
  subid <- header[str_detect(header, "Subject")]
  subid <- str_split(subid, pattern = "\t")[[1]][2]

  ## get the length of the header so we know how many rows to skip before reading the data
  header.rows <- length(header)

  ## DATA CLEANING
  # read in data and get rid of header rows
  all.d <- read_tsv(file, skip = header.rows,
                    col_types = cols(Time = "c"))

  ## split data into messages and data
  ## First get data:

  d <- all.d %>% filter(all.d$Type=="SMP")

  # convert to numeric here
  d$rx <- to_n(d$"R POR X [px]")
  d$ly <- to_n(d$"L POR Y [px]")
  d$ry <- to_n(d$"R POR Y [px]")
  d$lx <- to_n(d$"L POR X [px]")

  #clean up data frame
  d %<>%
    select(Time, lx, ly, rx, ry) %>%
    rename(t = Time) %>%
    mutate(t = to_n(t))

  ## Now get "messages" - about the stimulus that's being presented
  all.d$rawx <- all.d$"L Raw X [px]"

  msgs <- all.d %>%
    filter(Type=="MSG") %>%
    select(Time, rawx) %>%
    rename(t = Time,
           msg = rawx) %>%
    mutate(stimulus = gsub("# Message: ", "",msg),
           t = to_n(t))

  ## merge stimulus information back into d frame as a column
  d$stimulus <- sapply(d$t,
                       function(x) {
                         set <- msgs$stimulus[msgs$t < x]
                         set[length(set)]
                       })

  d$stimulus <- as.character(d$stimulus)

  ## drop the times before the first video
  d <- d %>%  filter(stimulus != "character(0)")

  ## add subid
  d$subid <- subid

  return(d)
}
