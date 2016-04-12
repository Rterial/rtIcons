#' Parse and return the Repositories for the user
#'
#'
#' \code{git.user_repo}
#'
#' The start page for github users accounts listing out the repositories
#' contributed to and forked.
#'
#' @param user_name string The username of the account. See examples
#'
#' @return
#' A data frame of all repositories and descriptions with links to each
#'
#' @family API's,githubAPI
#'
#' @examples
#'
#' git_me <- 'CarlBoneri'
#' git_rep <- 'rtillery/tree/master/R'
#' go_get <- paste0(git_me,git_rep,collapse = "/")
#' git.user_repo(go_get)
#'
#' git.user_repo('angular/material/tree/v1.1.0-rc1/docs/app/img/icons')


git.user_repo <- function(qry_path, files_only = T,get_ext = NULL){


  pre_src <- list(
    ang_mat_icons = "angular/material/tree/v1.1.0-rc1/docs/app/img/icons")

  git.public = "https://github.com/"
  git.raw = 'https://raw.githubusercontent.com'

  git.call <- paste0(git.public,qry_path)

  git.parse <- llply(html(git.call) %>%
                       html_nodes('.content a'),function(i)
                         data.frame(link = i %>% html_attr('href'),
                                    name = i %>% html_text,
                                    stringsAsFactors = F)
  ) %>% rbind.pages %>%
    mutate(href = paste(git.raw, gsub("blob/", "", link), sep="")
    ) %>% select(name, href) %>% mutate(file_type = parse.file_ext(name)$ext)

  if(files_only){
    git.parse <- git.parse %>% filter(!grepl('folder',file_type))
  }

  if(!is.null(get_ext)){
    git.parse <- git.parse %>% filter(grepl(get_ext,ext))
  }else{
    git.parse

  }

}





git.recursive <- function(...){

  a <- rbind.pages(Map(git.get, ...))


}



#' Helper for parsing and reattaching relative paths
#'
#' @examples
#' > a <- glue_path('https://raw.githubusercontent.com',"/ajaxorg/ace/tree/master/lib/ace/mode")
#' # a
#' [1]"https://raw.githubusercontent.com/ajaxorg/ace/tree/master/lib/ace/mode"
#'
#' # check the url
#'
#'
glue_path <- function(url,path,...){

  #make sure no double /

  if(stri_sub(url_in,-1) == "/"){
    url_prep <- stri_sub(url_in,-1)
  }

  if(stri_sub(path_in, 1, 1) == "/"){
    path_prep <- stri_sub(path_in,2,nchar(path_in))
  }

  paste0(url_prep,path_prep)


}


#' Assertion the number of chars returned are positive and the file exists
#'
#'
git.assert <- function(url_check){
  if(length(url_check) == 1){
    data.frame(url_check = url_check,
               LOG = url_ok(url_check),
               stringsAsFactors = FALSE)
  }else{
    cbind(url_check,ldply(url_check,url_ok)) %>%
      mutate(LOG = V1) %>% select(-V1)
  }
}


git.assert_code <- function(...){

  a <- git.assert(...)

  lapply(a,parse.file_ext)

}






git.r_file <- function(...,save_dir = NULL,file_name = NULL){
  rf <- readLines(...)

  if(is.dir(save_dir)){
    tmp.fi <- paste0(save_dir,"/",file_name,".R")
  }else{
    dir.create(save_dir)
    tmp.fi <- paste0(save_dir,"/",file_name,".R")
  }
  write(rf,file = tmp.fi)
}
