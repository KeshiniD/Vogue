files <- list.files(pattern = ".csv$")

invisible <- lapply(files, function(file) {
  if (grepl("_root.csv", file)) {
    return()
  }
  message("doing ", file)
  
  ids <- suppressWarnings(read.csv(file, header = FALSE))
  colnames(ids) <- "V1"
  ids$V1 <- as.character(ids$V1)
  filename <- tools::file_path_sans_ext(file)
  
  ids<-ids[!(ids$V1=="1"),]
  
  outfile <- paste0(filename, "_root.csv")
  write.csv(ids, outfile, quote = FALSE, row.names = FALSE)
})

#get freq of file
#lala <- read.csv("Vogue1A_52_DNA_1.viralcol4_ncbi.csv", header = FALSE)
#sort(table(lala$V1)) #can set to variable and write that to file
#a <- taxize::ncbi_get_taxon_summary(c(13097, 13098, 3554))

