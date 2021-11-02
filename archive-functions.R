#==============================
# archive-functions
#==============================

# 1.1)
### ==============================================
###' @title read_archive
###' @description read an package into an archive
###' @param x name of the package
###' @return archive of the package
###' =============================================
read_archive <- function(x){
  url <-'http://cran.r-project.org/src/contrib/Archive/'
  link <- paste0(url, x)
  tbl_html <- readHTMLTable(link,sep = '')
  tal_html = tbl_html[[1]]
  return(tbl_html)
}

# 1.2)
### ================================
###' @title clean_archive
###' @description clean an archive
###' @param x an unclean archive
###' @return a clean archive
###' ===============================
clean_archive <- function(x){
  name <- version_names(x)
  number<- version_number(x)
  date <- version_date(x)
  size <- version_size(x)
  df <- data.frame(name,
                   number,
                   date,
                   size,
                   stringsAsFactors = FALSE)
  return(df)
}

### ================================
###' @title version_names
###' @description an unclean archive
###' @param x archive
###' @return the name of the archive
###' ===============================
version_names <- function(x){
  name_version <- str_replace(x$Name, pattern = ".tar.gz", replacement = "")
  name_version <- str_split(name_version,pattern = "_")
  name_version <- lapply(name_version, function(x) x[1])
  name_version <- unlist(name_version)
  return(name_version[3:(length(name_version)-1)])
}

### ================================
###' @title version_number
###' @description an unclean archive
###' @param x archive
###' @return the version number of the archive
###' ===============================
version_number <- function(x){
  name_version <- str_replace(x$Name, pattern = ".tar.gz", replacement = "")
  name_version <- str_split(name_version,pattern = "_")
  name_version <- lapply(name_version, function(x) x[2])
  name_version <- unlist(name_version)
  return(name_version[3:(length(name_version)-1)])
}

### ================================
###' @title version_dates
###' @description an unclean archive
###' @param x archive
###' @return the date of the archive
###' ===============================
version_date <- function(x){
  date_version <- x$"Last modified"
  date_version <- str_sub(date_version,end = -6) 
  return(date_version[3:(length(date_version)-1)])
}

### ================================
###' @title version_size
###' @description an unclean archive
###' @param x archive
###' @return the size of the archive
###' ===============================
version_size <- function(x){
  num_part <- str_sub(x$Size,end = -2)
  num_part <- num_part[3:(length(num_part)-1)]
  num_part
  unit_part<- str_sub(x$Size,start = -1)
  unit_part <- unit_part[3:(length(unit_part)-1)]
  unit_part
  for (i in 1:length(unit_part)) {
    if(unit_part[i]=='M')
      num_part[i] = as.numeric(num_part[i]) *1000
  }
  return(num_part)
}

### ================================
###' @title plot_archive
###' @description a clean archive
###' @param x archive
###' @return the ggplot of archive
###' ===============================
plot_archive<-function(x){
  years <- vector(mode = "numeric")
  for (i in 1:length(x$date)) {
    date_draw <- as.numeric(str_split(x$date,pattern = "-")[[i]])
    years[i] <- date_draw[1] + date_draw[2]/12 + date_draw[3]/360 
  }
  ggplot(x,aes(x = years, y = as.numeric(x$size))) + geom_point(colour = "blue")+geom_step(colour = "red")
}











