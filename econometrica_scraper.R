# rm(list = ls(all.names = TRUE))

# define options
# which volumes do you want to download?
volumes <- rep(79:80)
# setwd("your working directory for this class")

# load packages
install.packages(rvest)
install.packages(stringr)
install.packages(data.table)
install.packages(lubridate)
install.packages(ggplot2)
install.packages(dplyr)
install.packages(httr)

library(rvest)
library(stringr)
library(data.table)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(httr)

# scrape an econometrica issue to extract article info
# takes as input an issue url

# we'll randomly shuffle through user agents

### NEW NOTES HERE ###

# we need to pick which user-agents to use. This depends on your software
# most of you have Macs. The ideal thing to do is this:
# download the package manager homebrew: https://brew.sh/
# after following the instructions + finishing the download (check with `brew doctor`)
# use this to install gecko: brew install geckodriver
# then use the following user agents: 

# user agents for gecko: 
user_agent_list <- c("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:101.0) Gecko/20100101 Firefox/101.0",
                     "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:99.0) Gecko/20100101 Firefox/99.0"
)

# if you're using a Mac and Chrome, I think this should work even without Gecko:
user_agent_list <- c(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36"
)

# if you're using Linux, one of these two should work even without Gecko:
# (note you may have to comment out one of the lines)
user_agent_list <- c(
  "Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/118.0",
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36"
)

# if you're using Windows and Chrome, I think this should work even without Gecko:
user_agent_list <- c(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/116.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36 Edg/118.0.2088.46",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.3"
)

# if you're using Windows and Chrome, I think this should work even without Gecko:
user_agent_list <- c(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36 Edg/117.0.2045.60",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36 Edg/118.0.2088.46"
)


### END NEW NOTES ###

# scrape an econometrica issue to extract article info
# takes as input an issue url
scrape_econometrica_issue <- function(issue_url){
  publisher_base_url <- "https://onlinelibrary.wiley.com"
  print(sprintf("Loading issue url: %s", issue_url))
  issue_page <- read_html(GET(issue_url, add_headers("user-agent" = sample(user_agent_list,1), "Cache-Control" = "no-cache"))$content)
  # issue_page <- read_html(GET(issue_url, add_headers("Cache-Control" = "no-cache"))$content)
  # issue_page <- read_html(issue_url)
  # get links to papers
  # use abstracts to avoid editorials and other non-papers
  abstract_links <- issue_page %>% html_nodes(".issue-item__links li:nth-child(1) a") %>%
    html_attr("href")
  # remove links that don't start with /doi
  paper_inds <- grepl(fixed("^/doi"), abstract_links)
  abstract_links <- abstract_links[paper_inds]
  paper_links <- paste0(publisher_base_url, abstract_links %>% str_remove_all(fixed("/abs")))
  print(sprintf("Found %s papers in issue", length(paper_links)))
  
  Sys.sleep(sample(1:10, 1))
  
  # scrape individual papers
  issue_data <- data.table()
  for (i in 1:length(paper_links)){
    paper_url <- paper_links[i]
    print(sprintf("Scraping paper url: %s", paper_url))
    skip_paper <- FALSE
    paper_page <- tryCatch(
      read_html(GET(paper_url, add_headers("user-agent" = sample(user_agent_list,1), "Cache-Control" = "no-cache"))$content),
      # read_html(paper_url),
      # if error in url, skip to next
      error = function(e) {
        skip_paper <<- TRUE
        print(sprintf("Error scraping url: %s", paper_url))
        print("Here's the original error message:")
        print(e)
      })
    if(skip_paper){next}
    # paper_page <- read_html(paper_url)
    # wait
    Sys.sleep(sample(1:10, 1))
    
    # gather all data for paper EXCEPT keywords, which requires rselenium
    paper_title <- paper_page %>% html_node(".citation__title") %>% html_text()
    # workaround to get authors
    paper_authors_vec <- c()
    # assume less than 10 authors total
    for (i in 1:10){
      author_i <- paper_page %>% html_node(sprintf("#a%s_Ctrl span",i)) %>% html_text()
      if(is.na(author_i)){
        next
      } else{
        paper_authors_vec <- c(paper_authors_vec, author_i)  
      }
      
    }
    paper_authors <- paste(paper_authors_vec, collapse = ", ")
    
    paper_pages <- paper_page %>% html_node(".page-range span+ span") %>% html_text() %>% str_trim()
    paper_start_page <- str_split(paper_pages, "-")[[1]][1] %>% as.numeric()
    paper_end_page <- str_split(paper_pages, "-")[[1]][2] %>% as.numeric()
    paper_length <- paper_end_page - paper_start_page + 1
    
    paper_issue_month <- paper_page %>% html_node(".volume-issue+ p") %>% html_text()
    paper_issue <- paper_page %>% html_node(".volume-issue") %>% html_text()
    paper_first_published <- paper_page %>% html_node(".epub-date") %>% html_text()
    
    paper_abstract <- paper_page %>% html_node("#section-1-en p") %>% html_text()
    paper_text <- paper_page %>% html_nodes("#article__content p") %>% html_text() %>% paste(collapse = " ")
    
    # add paper_citations here!
    paper_citations <- paper_page %>% html_node(".cited-by-count span") %>% html_text()
    paper_citations <- paper_citations %>% str_remove("Citations: ") %>% as.numeric()
    
    
    paper_data <- data.table(
      link = paper_url,
      title = paper_title,
      authors = paper_authors,
      start_page = paper_start_page,
      end_page = paper_end_page,
      length = paper_length,
      issue_month = paper_issue_month,
      first_published = paper_first_published,
      citations = paper_citations,
      abstract = paper_abstract,
      text = paper_text
    )  
    # bind together
    issue_data <- rbindlist(list(issue_data, paper_data))
  }
  issue_data
}

# create links for scraping
autolink <- expand.grid(issues=1:6, volumes=volumes)
issue_urls <- sprintf("https://onlinelibrary.wiley.com/toc/14680262/%s/%s", autolink$volumes, autolink$issues)
# bind multiple issues together
combined_issue_data <- data.table()
for (issue in issue_urls){
  skip_issue <- FALSE
  issue_data <- tryCatch(scrape_econometrica_issue(issue),
                         # if error in url, skip to next
                         error = function(e) {
                           skip_issue <<- TRUE
                           print(sprintf("Error scraping url: %s", issue))
                           print("Here's the original error message:")
                           print(e)
                         }
  )
  # issue_data <- scrape_econometrica_issue(issue)
  if(skip_issue){next}
  combined_issue_data <- rbindlist(list(combined_issue_data, issue_data))
}

combined_issue_data_copy <- copy(combined_issue_data)
# remove observations with no authors
combined_issue_data <- combined_issue_data[authors!=""]
combined_issue_data <- combined_issue_data[!grepl("Report of the", title, ignore.case = TRUE)]
fwrite(combined_issue_data, "combined_issue_data.csv")
