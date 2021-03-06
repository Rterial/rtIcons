#' Icon Parsing from github
#'
#'
#' \code{rt.icon_parse}
#'
#' \href{https://github.com/google/material-design-icons}{Material Icons Github}
#' \href{https://design.google.com/icons/}{Material Design Icon Site}
#'
#'
rt.icon_parse <- function(){

  # Helper strings for loop functions
  ghub_base <- 'https://github.com'
  ghub_raw <- 'https://raw.githubusercontent.com'
  icon_repo <- '/google/material-design-icons'
  repo_node <- '.js-navigation-open'
  drill_node <- '.css-truncate-target .js-navigation-open'
  filter_grep <- '\\.|LICENSE|woff|www|gh-pages|master|sprites|iconfont'
  filter_png <- 'ic_|_white|_black|_12dp.png|_18dp.png|_24dp.png|_36dp.png|_48dp|.png'


  # The base repo directory

  git_home <- html(paste(ghub_base,icon_repo,sep="")) %>%
    html_nodes(repo_node) %>% html_attr('href')


  idx <- ldply(git_home,function(i)
    data.frame(links = i,
               link_ext = basename(i))
    ) %>%
    filter(!grepl(filter_grep,link_ext)) %>%
    mutate(link = paste(ghub_base,links,sep="")) %>%
    select(-links)



  git_drill <- lapply(1:nrow(idx),function(i)
    html(idx[i,'link']) %>% html_nodes(drill_node) %>%
      html_attr('href') %>% chain.df('links') %>%
      mutate(icon_group = idx[i,'link_ext']) %>%
      filter(grepl('(2x_web|svg)',links)) %>%
      mutate(link = paste(ghub_base,links,sep="")) %>%
      mutate(link = ifelse(stri_sub(link,-3) == 'svg',
                           paste(link,"/production",sep=""),
                           link)
             ) %>%
      mutate(sub_folder = basename(link)) %>%
      select(-links) %>%
      select(icon_group,sub_folder,link)
  ) %>% rbind.pages



  git_all <-
    lapply(1:nrow(git_drill),function(xn)
      ldply(html(git_drill[xn,'link']) %>% html_nodes(drill_node),function(i)
        data.frame(icon_name = html_text(i),
                   icon_source = html_attr(i,'href')) %>%
          mutate(icon_name = gsub(filter_png,'',icon_name)) %>%
          mutate(icon_source = paste(ghub_raw, gsub('blob/','',icon_source),sep=""),
                 icon_size = stri_sub(basename(icon_source),-8,-7)
          )
      )
    )

  git_idx <- rbind.pages(git_all) %>%
    mutate(icon_type = ifelse(grepl('.png',icon_source),'png','svg')) %>%
    mutate(icon_color =
             ifelse(grepl('_black_',icon_source),'black',
                    ifelse(grepl('_white_',icon_source),'white','none'))
           )

  git_idx <- git_idx %>%
    mutate(icon_group =
             stri_extract_first_words(
               gsub('https://raw.githubusercontent.com/google/material-design-icons/master/',"",icon_source))
           )

  saveRDS(git_idx,'inst/data/rt_icons_idx.rds')

  assign('rt_icons_idx', git_idx, envir = .GlobalEnv)

}



#' Parsing the external custom and svg icon sets
#'
#' \code{rt.icon_parse_extra}
#'
#'
rt.icon_parse_extra <- function(){

  AA <- readLines('https://raw.githubusercontent.com/klarsys/angular-material-icons/master/angular-material-icons.js')
  AA <- stri_trim_both(AA)
  AA <- list.drop(AA,'empty')
  split.list <- melt(list(social = list(starts = "'amazon'",
                                        ends = "'windows'"),
                          custom = list(starts = "'signal_wifi_0_bar'",
                                        ends = "'account_child'")
  )
  )

  split_eo <- chain.split_even_odd <- function(x){
    es <- cbind(x[c(T,F)] %>% as.numeric)
    os <- cbind(x[c(F,T)] %>% as.numeric)
    data.frame(evens = es,odds = os)
  }

  aa.idx <- ldply(split.list[[1]],function(i)
    data.frame(term = i,idx = grep(i,AA)))

  aa.sp <- split_eo(aa.idx[[2]])

  aa.parse <- lapply(1:nrow(aa.sp),function(i)
    lapply(as.numeric(aa.sp[i,1]:aa.sp[i,2]),function(ii)
      data.frame(icon_name = stri_extract_first_words(AA[[ii]]),
                 icon_svg = strsplit(AA[[ii]],":") %>%
                   chain.df('raw') %>% slice(2) %>%
                   unlist %>% as.character %>%
                   stri_replace_all_regex("'","") %>%
                   stri_trim_both)
    ) %>% rbind.pages)

  aa.parse[[1]]$icon_group <- 'social'
  aa.parse[[2]]$icon_group <- 'custom'

  aa.done <- rbind.pages(aa.parse) %>%
    mutate(icon_svg = stri_sub(icon_svg,1,-2)) %>%
    select(icon_group,icon_name,icon_svg)

  assign('svg_extra',aa.done, envir = .GlobalEnv)
  saveRDS(aa.done,'inst/data/svg_extra.rds')
}
