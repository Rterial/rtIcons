#' All dependencies for the material design packages
#'
#' \code{rt.setup}
#'
#' Read all of the dependency index tables and externals into the package
#' data folder for package build and function dependency requirements.
#'
#'
#'
#' @aliases \code{mtrl.setup}
#'
#'
rt.setup <- function(add_res = NULL){
  base <- 'https://github.com/Rterial/Rtbase/blob/master/inst/data/'

  tail <- '?raw=true'

  file_list <- list(mtrl_icons = 'mtrl_icons.rds',
                    mtrl_icons_svg = 'mtrl_icons_svg.rds',
                    mtrl_colors = 'mtrl_colors.rds',
                    html_tags = 'tag_refs.rds')

  if(!is.null(add_res)){
    file_list <- append(file_list,add_res)
  }

  if(!file.exists('inst')){
    dir.create('inst')
    dir.create('inst/data')
  }

  rel_paths <- paste0(base,file_list,tail)

  to_paths <- paste0('inst/data/',file_list)

  get_df <- data.frame(froms = rel_paths, tos = to_paths)

  lapply(1:nrow(get_df),function(i)
    curl::curl_download(get_df[[1]][[i]],
                        get_df[[2]][[i]])
  )

  if(file.exists(get_df[nrow(get_df),2])){
    sapply(1:nrow(get_df),function(i)
      assign(names(file_list[i]),readRDS(get_df[i,2]),envir = .GlobalEnv)
    )
    invisible()
  }


  if(!file.exists('revdep')){
    dir.create('revdep')

    if(!exists('pure.html')){
      assign('pure.html',
             function (text, ...) {
               htmlText <- c(text, as.character(list(...)))
               htmlText <- paste(htmlText, collapse = " ")
               attr(htmlText, "html") <- TRUE
               class(htmlText) <- c("html", "character")
               htmlText
             },envir = .GlobalEnv)
    }
    fi_src <- "https://raw.githubusercontent.com/CarlBoneri/mtace/master/R/MTRL_DEPS.R"
    readLines(fi_src) %>%
      paste0(collapse = "\n") %>% pure.html %>%
      write(paste0('revdep/',basename(fi_src)))
  }

  invisible()


}
