if (!requireNamespace("R.utils", quietly = TRUE)) {
  install.packages("R.utils")
}
library(R.utils)

# Short script to check for packages, load them, or install and load them if available.
boomstick <- function (packages) {
  for (package_name in packages) {
    '%!in%' <- function(x,y)!('%in%'(x,y))
    
    if (paste("package:", package_name, sep = '') %in% search()) {
      cat("Package", package_name, "is loaded\n")
      next
    }  else {
      if (!requireNamespace(package_name, quietly = TRUE)) {
        cat("Package", package_name, "not found. Installing...\n")
        
        # Check if the package is available
        available <- tryCatch(
          available.packages()[package_name, ],
          error = function(e) NULL
        )
        
        if (is.null(available)) {
          cat("Package", package_name, "is not available on CRAN.\n")
          next
        }
        
        # Try installing the package with a timeout of 300 seconds (adjust as needed)
        tryCatch(
          withTimeout(
            install.packages(package_name, ask = FALSE, dependencies = TRUE),
            timeout = 300
          ),
          TimeoutException = function(e) {
            cat("Package", package_name, "installation timed out\n")
          },
          error = function(e) {
            cat("Package", package_name, "installation failed\n")
          }
        )
        
        # Try loading the package again after installation
        if (!requireNamespace(package_name, quietly = TRUE)) {
          cat("Package", package_name, "loading failed\n")
          next
        } else if (paste("package:", package_name, sep = '') %!in% search()) {
          attachNamespace(package_name)
          cat("Package", package_name, "loaded and ready\n")
          next
        }
        
      } else if (paste("package:", package_name, sep = '') %!in% search()) {
        attachNamespace(package_name)
        cat("Package", package_name, "loaded and ready\n")
      }
    }
  }
}

libraries <- c("bibliometrix", "tidyverse", "DT", "gt", "rentrez", "easyPubMed", "xml2", "metagear", "clipr", "quanteda.textstats")  # Change/Add more package names as needed
boomstick(libraries)

### ADD YOUR DATA IN HERE ###

wos <- convert2df(file = "data/wos-plaintext-savedrecs.txt", dbsource="wos",format="plaintext")

scopus <- convert2df(file = "data/scopus-bib-crc.bib", dbsource = "scopus", format = "bibtex")

pubmed <- convert2df(file = "data/pubmed-plaintext-colorectal-set.txt", dbsource = "pubmed", format = "plaintext")

# Combine the all your data here
fulldb <- mergeDbSources(wos, scopus, pubmed, remove.duplicated = TRUE)

##############################

results <- biblioAnalysis(fulldb)

mcp <- results[["MostCitedPapers"]] %>%  
  arrange(desc(NTC))                     

histResults <- histNetwork(fulldb)

CR <- citations(fulldb)

net <- histPlot(histResults, n=55, size = 3, labelsize = 4, verbose = TRUE)

mcp_year <- CR %>%
  as.data.frame() %>%
  mutate(DOI = sub(".* DOI ", "", Cited.CR)) %>%
  group_by(Year) %>%
  slice_head(n = 5) %>%
  filter(Cited.Freq >= 3) %>%
  arrange(desc(Cited.Freq), desc(Year))

small_mcp <- mcp %>% 
  slice_head(n=100) %>% 
  mutate(Origin = "results[['MostCited']]")

netpap <- net$graph.data %>%
  select(-KeywordsPlus, -Author_Keywords)

peaks <- rpys(fulldb, graph = F)

outlist <- peaks$rpysTable %>%
  rowwise() %>%
  summarise(sample_list = list(rep(Year, Citations))) %>%
  pull(sample_list) %>%
  unlist()

tfmin <- quantile(outlist, prob=.25, type=1, names = FALSE) - (3 * IQR(outlist))
inlist <- outlist[which(outlist > tfmin)]
quick_peaks <- rpys(fulldb, timespan = c(min(inlist),max(inlist)))

qtile <- quantile(quick_peaks[["rpysTable"]]$diffMedian5, prob=c(.25,.5,.75), type=1, names = FALSE)

qtile.range <- qtile[3] - qtile[1] # or IQR(quick_peaks[["rpysTable"]]$diffMedian5)

lower.fence <- qtile[3] + 1.5 * qtile.range
upper.fence <- qtile[3] + 3.0 * qtile.range

lower.rpys <- subset(quick_peaks[["rpysTable"]], diffMedian5 > lower.fence) %>%
  select(Year)

out_years <- quick_peaks[["df"]] %>%
  inner_join(lower.rpys, by = join_by(citedYears == Year))

peak_art <- out_years %>% 
  group_by(citedYears, Reference) %>%
  summarise(n = n()) %>%
  filter(n >= 4) %>%
  arrange(citedYears, desc(n)) %>%
  ungroup() %>%
  na.omit() %>%
  mutate(DOI = str_extract(.data$Reference, pattern = "DOI.*")) %>%
  mutate(DOI = str_sub(.data$DOI, start = 5)) %>%
  mutate(DOI = str_replace_all(.data$DOI, pattern = ' ', replacement = '.'))

# 100
small_histpap <- histResults[["histData"]] %>%
  select(-KeywordsPlus) %>%
  arrange(desc(GCS)) %>%
  slice_head(n=100) %>% 
  mutate(Origin = 'histResults[["histData"]]')

# 126
labeled_mcp_year <- mcp_year %>% 
  mutate(Origin = "citations()")

# 39
labeled_netpap <- netpap %>% 
  mutate(Origin = "histPlot()")

# 85
labeled_peak_art <- peak_art %>% 
  mutate(Origin = "rpys_outliers()")

biblioCombo <- function(...) {
  if (...length() == 1L)
    ids_lst <- lst(...)
  else
    ids_lst <- lst(...)
  
  df_names <- names(ids_lst)
  
  new <- ids_lst %>%
    reduce(full_join, by = "DOI") %>%
    distinct(DOI, .keep_all = TRUE) %>%
    unite("Origin", starts_with("Origin"), na.rm = TRUE, sep = ", ") 
  
  abs <- fulldb %>%
    mutate(DOI = DI) %>%
    right_join(new, by = "DOI") %>%
    select(DI, AB, TI, DOI, Origin) 
  
  #Function to create an index column indicating which dataframes contain a particular DOI
  create_index <- function(df_list, doi_column) {
    index_column <- sapply(doi_column, function(doi) {
      doi_in_dfs <- sapply(df_list, function(df) {
        any(df$DOI == doi, na.rm = TRUE)
      })
      paste(names(df_list)[doi_in_dfs], collapse = ", ")
    })
    return(index_column)
  }
  
  # Create index column
  abs$Index <- create_index(ids_lst, abs$DOI)
  
  final <- c(ids_lst, list(new = new, abs = abs))
  
  return(final)
}

combo <- biblioCombo(small_mcp, labeled_mcp_year, labeled_netpap, small_histpap, labeled_peak_art)

dois <- combo$abs %>%
  subset(is.na(AB))

get_abstracts <- function(input_df) {
  n <- nrow(input_df)
  pb <- progress::progress_bar$new(
    format = "  Downloading abstracts in :eta : [:bar] :current/:total (:percent) ",
    clear = TRUE, total = n, width = 80)
  
  results_df <- data.frame(DOI = character(), Abstract = character(), Title = character(), stringsAsFactors = FALSE)
  
  for (i in 1:n) {
    doi <- input_df$DOI[i]
    pb$tick()
    #abstracts <- lapply(head(dois$DOI), function(doi) {
    pubmed_abstract <- tryCatch({
      search_query <- paste(doi, "[DOI]", sep = " ")
      pubmed_search <- entrez_search(db = "pubmed", term = search_query)
      
      if (length(pubmed_search$ids) == 0) {
        row <- data.frame(DOI = doi, Abstract = NA, Title = NA, stringsAsFactors = FALSE)  # No IDs found for this DOI
      } else {
        
        pubmed_id <- pubmed_search$ids[1]
        pubmed_summary <- entrez_summary(db = "pubmed", id = pubmed_id)
        pubmed_title <- pubmed_summary$title
        
        if (is.null(pubmed_summary$abstract) || is.na(pubmed_summary$abstract)) {
          pubmed_record <- entrez_fetch(db = "pubmed", id = pubmed_id, rettype = "abstract")
          pubmed_abstract <- paste(strsplit(pubmed_record, "\n")[[1]], collapse = "\n")
        } else {
          pubmed_abstract <- pubmed_summary$abstract
        }
        
        row <- data.frame(DOI = doi, Abstract = pubmed_abstract, Title = pubmed_title, stringsAsFactors = FALSE) 
        
      }
      
      results_df <- rbind(results_df, row)
      
    }, error = function(err) {
      row <- data.frame(DOI = doi, Abstract = NA, Title = NA, stringsAsFactors = FALSE)  # No IDs found for this DOI
      results_df <- rbind(results_df, row)
      return(NA)
    })
  }
  return(results_df)
}

result_df <- get_abstracts(dois)

almost_results <- result_df %>%
  right_join(combo$abs, by = "DOI") %>%
  mutate(TI = coalesce(TI, Title), AB = coalesce(AB, Abstract)) %>%
  select(DOI, TI, AB) %>%
  distinct(DOI, .keep_all = TRUE) %>%
  mutate(AB = ifelse(is.na(AB), TI, AB), TI = ifelse(TI == AB | is.na(TI), DOI, TI))

speedrun <- almost_results %>%
  mutate(AB = str_to_sentence(AB)) %>%
  mutate(TI = str_to_sentence(TI))

#### CHANGE THE `reviewers` name ################

effort_distribute(speedrun, reviewers = 'Colton', initialize = TRUE, save_split = TRUE) 

##############################################

#### CHANGE THE NAME AND KEYWORDS ############

abstract_screener(file = 'effort_Colton.csv', aReviewer = 'Colton', 
                  abstractColumnName = 'AB', titleColumnName = 'TI', windowWidth = 120, 
                  highlightKeywords = c("metabol", "metagenom", "bacteria", "colon", "rectal", "gut", "bile", "fatty", "choline"))

############################################

#### MATCH THE FILE ABOVE TO THE `read_csv()` INPUT BELOW #####

clip <- read_csv("effort_Colton.csv") %>%
  filter(grepl("yes|maybe", INCLUDE, ignore.case = TRUE))

write_clip(content = clip$DOI)

###############################################################