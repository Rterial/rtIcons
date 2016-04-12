#' Ensire this helper is loaded
#'
#' \code{dirs.r_home}
#'
#'
#'
dirs.r_home <- function(package = NULL){
  path <- normalizePath(Sys.getenv('R_LIBS_USER'))
  if(!is.null(package)){
    path <- paste0(path,"/",package)
  }
  return(path)
}


#' Create an environment with all path variables
#'
#' \code{dirs.env}
#'
#'
#'This is a helper environment for traversing directories and sub directories
#'quickly and effectively. It will create an index of all rel paths and
#'allow access to file sub structures with a passed variable into a loibrary
#'call. See examples
#'
#'
#'
#'
#'
#'
#'@examples
#'
#'Generate a home directory with my local R libray set to default home
#'
#'> dirs.env(my_home = dirs.r_home())
#'
#'Now find the files located in this directory
#'
#'> dirs$home('whyles/scripts/facebook')
#'
dirs.env <- function(my_home = NULL){

  dirs <<- new.env()

  dirs.roots <- data.frame(dirs = paste0("",list.files('/')))


  fun.call <- function(x = NULL,recursive = FALSE,search_term = NULL){
    if(!is.null(x)){
      path_to = paste0('/',i,"/",x,"/")
    }else {
      path_to = paste0('/',i,"/")
    }
    if(recursive){
      rec = TRUE
    }else{
      rec = FALSE
    }
    do.call('list.files',
            list(path = path_to,
                 full.names = T,
                 recursive = rec,
                 pattern =
                   ifelse(!is.null(search_term),
                          search_term,
                          NULL)
            )
    )
  }

  # Now create all envir varialbles stored as function calls
  # eg dirs$Users('Carl Boneri') will now call that folder


  sapply(dirs.roots[[1]],function(i)
    assign(tolower(i),function(x = NULL,recursive = FALSE){
      if(!is.null(x)){
        path_to = paste0('/',i,"/",x,"/")
      }else {
        path_to = paste0('/',i)
      }
      if(recursive){
        rec = TRUE
      }else{
        rec = FALSE
      }
      do.call('list.files',
              list(path = path_to,
                   full.names = T,
                   recursive = rec))
    },envir = dirs)
  )

  if(!is.null(my_home)){
    assign('home',
           function(x = NULL,recursive = FALSE){
             home_path <-
               ifelse(file.exists(my_home),
                      my_home,
                      paste0('/',my_home,"/")
               )

             if(!is.null(x)){
               path_to = paste0(home_path,"/",x)
             }else {
               path_to = home_path
             }

             if(recursive){
               rec = TRUE
             }else{
               rec = FALSE
             }
             do.call('list.files',
                     list(path = path_to,
                          full.names = T,
                          recursive = rec))
           },envir = dirs)
  }
  # Apply a helper function for finding files anywhere
  #   assign('find',function(x){
  #
  #     a.step <- list.files("/")
  #
  #     b.step <- lapply(paste0("/",a.step),function(i)
  #       file.info(i) %>%
  #         data.frame(file = row.names(.),.)) %>%
  #       rbind.pages %>%
  #       filter(isdir == T) %>%
  #       mutate(updated_time = as.POSIXct(mtime,"%Y-%m-%d %H:%M:%S")) %>%
  #       select(file,updated_time,size,isdir,mode) %>%
  #       filter(stri_sub(mode,-2) == 55 & grepl('(usr|USER|User|users|usr)',file))
  #
  #
  #     c.step <- lapply(list.files(b.step$file,full.names = T),function(i)
  #       data.frame(file = row.names(file.info(i)),file.info(i)))%>%rbind.pages %>%
  #       filter(isdir == T) %>%
  #       mutate(updated_time = as.POSIXct(mtime,"%Y-%m-%d %H:%M:%S")) %>%
  #       select(file,updated_time,size,isdir,mode)
  #
  #     d.step <- grep(paste0("(share|",Sys.info()[['user']],")"),c.step[[1]],value = T)
  #
  #     list.files(grep(paste0("(share|",Sys.info()[['user']],")"),c.step[[1]],value = T),full.names = T)
  #
  #
  #
  #
  #
  #     c.step <- lapply(b.step,function(i)
  #       data.frame(raw = list.files(i,full.names = T,include.dirs = T))%>%
  #         filter(!grepl('\\.',raw))) %>%
  #       unlist %>%
  #       as.character
  #
  #   }
  #     list.files("/",recursive = T,full.names = T,pattern = x),
  #     envir = dirs)
}



#' Locate a file recursively for sourcing and search purposes
#'
#'
#' \code{files.find}
#'
#'
#'This has the functionality to search through all directories and file types
#'either on a home system, local library , cloud based system or other directory.
#'
#'It will locate the files and structure a key table data frame for indexing purposes
#'as well as loop through and read the contents given a specific file type
#'
#'
#'
#'@param name_hint NULL   string or character vectory A hint for the potential
#'     file name or the directory. A term contained in the actual file path etc
#'@param ext_hint NULL   The file extension hint
#'@param source_lib c("R", "docs", "desktop")   The library source to
#'  begin the search in
#'@param sub_lib NULL   The sub directory if known to append to the base library
#'@param show_output FALSE   Should the output be shown on console
#'@param fun_hint NULL   If looking for a specific function,
#'this is a string but will also trigger an interactive selection
#'
#'
#'@return
#'Depends on specified inputs bu can retreiuve and return entire file contents, image or html
#'files copied to local, R files sourced or individual functions sourced.
#'
#'
#'
#'
#'
#'  @returns
#'A data frame object with the full path to file and last updated time

#'.id         file_path                                                                                time_updated
#'----------  ---------------------------------------------------------------------------------------  --------------------
#'  full_path   C:/Users/Carl Boneri/Documents/R/win-library/3.1/wandR/R/apis/window_obs.R               2015-11-11 20:57:34
#'full_path   C:/Users/Carl Boneri/Documents/R/win-library/3.1/testWhyles/scripts/auths/window_obs.R   2015-11-11 10:30:10
#'
#'
#'
#' @examples
#' files.find('whyles',ext_hint = '.css',source_lib = 'docs',sub_lib = 'Visual Studio 2013')
#'
#' files.find(c("obs","window"),ext_hint = '.R',source_lib = 'R')
#'
#' =========  ======================================================================================  ===================
#'.id        file_path                                                                               time_updated
#'=========  ======================================================================================  ===================
#'full_path  C:/Users/Carl Boneri/Documents/R/win-library/3.1/wandR/R/apis/window_obs.R              2015-11-11 20:57:34
#'full_path  C:/Users/Carl Boneri/Documents/R/win-library/3.1/testWhyles/scripts/auths/window_obs.R  2015-11-11 10:30:10
#'=========  ======================================================================================  ===================
#'
#'
#'
#'
files.find <- function(name_hint = NULL,ext_hint = NULL,quick_find = TRUE,
                       source_lib = c('R','docs','desktop'),
                       sub_lib = NULL,show_output = FALSE,fun_hint = NULL){

  if(length(source_lib) == 3){
    source_lib == 'R'
  }

  # If traversing other libraries start at the
  if(source_lib == 'R'){
    com_home <- Sys.getenv('R_LIBS_USER')
  }else if(source_lib == 'docs'){
    com_home <-  Sys.getenv('R_USER')
  }else{
    com_home <- paste0(gsub("\\\\","/",Sys.getenv('USERPROFILE')),"/Desktop")
  }


  if(!is.null(sub_lib)){
    com_home <- grep(sub_lib,list.files(com_home,full.names = T),value = T)
  }


  # Get all the files
  file_tbl <- list.files(com_home,full.names = T)


  # Convert the file list to a key table

  file_key <- data.frame(full = file_tbl,
                         file = basename(file_tbl),
                         exts = stri_extract_last_regex(file_tbl,"[:punct:]\\w+")) %>%
    mutate(type = ifelse(grepl('\\.',exts),'file','folder'))

  # Filter out CRANS
  cran_installs <- row.names(installed.packages())

  non_cran <- file_key[!file_key$file %in% cran_installs,]

  # Now get all sub files
  #setup for list naming
  dir_names <- non_cran %>% filter(type != 'file') %>% `$`(full)


  loops <- lapply(dir_names,function(i)
    data.frame(files = list.files(i,full.names = T,recursive = T)))

  names(loops) <- basename(dir_names)

  file_df <- lapply(1:length(loops),function(i)
    data.frame(folder = rep(names(loops[i]),nrow(loops[[i]])),
               full_path = loops[[i]][[1]])) %>% rbind.pages


  if(!is.null(name_hint)){
    if(length(name_hint) > 1){
      Grep <- paste0('(',paste0(name_hint,collapse = "|"),")")
    }else{
      Grep <- name_hint
    }
    file_df <- file_df %>% filter(grepl(Grep,full_path))
  }

  if(!is.null(ext_hint)){
    nth_ext <- nchar(ext_hint) * -1
    file_df <- file_df %>%
      filter(stri_sub(full_path,nth_ext) == ext_hint)
  }


  data_df <- ldply(file_df,function(i)
    cbind(file_path = i,
          time_updated =
            strftime(file.info(i)[[4]],"%Y-%d-%m %H:%M:%S")
    )) %>% arrange(desc(time_updated)) %>% filter(.id != 'folder') %>%
    mutate(file_ext = stri_extract_last_regex(basename(file_path),"[:punct:]\\w+"))

  if(quick_find){
    dat_out <- data_df[1,2]
    dat_ready <- htmltools::HTML(readLines(dat_out) %>% paste0(collapse = "\n"))
    write(dat_ready,paste0("R/",basename(dat_out)))
    source(paste0("R/",basename(dat_out)))
    invisible()
    return('file ready')
  }else{


    get_out <- sapply(1:nrow(data_df),function(i){

      switch(data_df[i,'file_ext'],
             '.R' = readLines(data_df[i,'file_path']),
             '.rds' = readRDS(data_df[i,'file_path']),
             '.html' = pure.html(paste0(readLines(data_df[i,'file_path']),collapse = "\n")),
             '.css' = pure.html(paste0(readLines(data_df[i,'file_path']),collapse = "\n")),
             '.csv' = read.csv(data_df[i,'file_path'])
      )
    })
  }

  if(ext_hint == '.R'){

    li_of_fun <- sapply(1:length(get_out),function(i)
      sapply(1:length(get_out[[i]]),function(ix)
        fun.name(get_out[[i]][[ix]])
      ) %>% unlist)

    names(li_of_fun) <- data_df$file_path

    if(!is.null(fun_hint)){

      get_out <- list.drop_zero(Map(function(x)
        x[grepl(fun_hint,x)],
        li_of_fun))

      selection <- sapply(get_out,function(i)readline(cat(i)))

      get_out <- names(li_of_fun[grep('(y|yes|Y|YES|true|T)',selection)])

      suppressWarnings(
        sapply(get_out,function(i)i %>% readLines %>% paste0(collapse = "\n") %>% HTML %>%
                 write(paste0('R/',basename(i)))))
      invisible()

    }else{


      get_out <- lapply(1:length(li_of_fun),function(i)
        list(file_name = names(li_of_fun[i]),
             funs_in_file = li_of_fun[[i]],
             raw_file = HTML(paste0(get_out[[i]],collapse = "\n"))
        ))
    }
  }else{
    get_out <- data_df
  }

  return(get_out)
}


#' Helper to find and extract function names from files
#'
#' \code{fun.name}
#'
fun.name <- function(x){
  stri_extract_first_words(grep("<- function",x ,value = T))
}


#' Helper to prepare an output for searching through files
#'
#'
#'
file.find_prep <- function(x){
  lapply(1:length(x),function(i)
    sprintf('Get from the following?\n filename: %s \n functions: \n %s ',
            names(x[i]),
            paste0("-f() ",
                   fun.name(x[[i]]),
                   collapse = " \n "))
  )
}
