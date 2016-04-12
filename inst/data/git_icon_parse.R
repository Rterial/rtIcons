mtrl.icons <- function(){

  git_rel <- function(var = c('tree','blob'),var_lib = NULL){
    sprintf('https://github.com/google/material-design-icons/%s/master/%s',var,var_lib)
  }

  git_node <- '.js-navigation-open'

  git_sub_node <- '.js-directory-link'

  git_branch <- '/tree/master/'

  git_tail <- '/svg/production'

  git_bash <- '/res/2x_web/'

  icon_folders <- c("action","alert","av","communication","content","device",
                    "editor","file","hardware","image","maps","navigation",
                    "notification","social","toggle")

  icon_groups <- html(git_rep) %>% html_nodes(git_sub_node) %>% html_text

  icon_groups <- icon_groups[c(1:3,5:12,14:17)]

  git_calls <- paste0(git_rep,icon_groups,git_bash)

  return(git_calls)
  icon_repos <- llply(1:length(git_calls),function(x)
    data.frame(icon_src = html(git_calls[[x]]) %>%
                 html_nodes('.js-directory-link') %>% html_text,
               icon_group = icon_groups[[x]],
               stringsAsFactors = FALSE))

  icon_data <- llply(1:length(icon_repos),function(i)
    data.frame(icon_repos[[i]],
               icon_link = paste0(git_src,icon_groups[[i]],
                                  git_bash,icon_repos[[i]]$icon_src),
               stringsAsFactors = FALSE)) %>% rbind.pages %>%
    mutate(icon_render_png =
             paste0('<div class="col-sm-4"><a><img style="" src="',
                    (icon_link),'"/></a></div>'),
           icon_name = stri_sub(icon_src,4,-10)) %>%
    mutate(icon_name = ifelse(stri_sub(icon_name,-4) == "_wht",
                              stri_sub(icon_name,1,-5),icon_name)) %>%
    mutate(icon_size = stri_sub(icon_src,-8,-7) %>%
             stri_trim_both %>% as.numeric) %>%
    mutate(icon_render_css =
             paste0('<i class="material-icons">',
                    icon_name,
                    "</i>")
    )


  return(icon_data)

}



#' This gits the svg file icons from the angular material library
#'
#' \url{"https://github.com/angular/material/tree/v1.1.0-rc1"}
rt.git_ang_icons <- function(){
  B <- git.user_repo('angular/material/tree/v1.1.0-rc1/docs/app/img/icons') %>%
    filter(file_type == '.svg')

  lapply(1:nrow(B),function(i)
    write(HTML(paste(readLines(B[i,'href']),
                     sep = "\n")),
          paste('inst/www/svg_icons/',B[i,'name'],sep = "")
    )
  )

}

rt.git_svg <- function(){
  ul.ac <- function(x)as.character(unlist(x))
  git.Uri <- "https://github.com/google/material-design-icons/tree/master/"
  git.RawUri <- 'https://raw.githubusercontent.com/google/material-design-icons/master/'

  svg_hash <- lapply(append(unique(mtrl_icons$icon_group),"places") %>%
                       chain.df %>%
                       arrange(df_col) %>%
                       ul.ac,function(i)
                         lapply(
                           html(
                             paste0(git.Uri,i,"/svg/production")
                           ) %>%
                             html_nodes('.js-directory-link') %>%
                             html_text,function(xx)
                               data.frame(icon_group = i,
                                          icon_name = gsub("(ic_|.svg|_24px|_48px)","",xx),
                                          icon_size = stri_sub(gsub('.svg','',xx),-4,-1),
                                          icon_src = paste0(git.RawUri, i, '/svg/production/',xx),
                                          icon_home = 'google_icons',
                                          stringsAsFactors = FALSE)
                         ) %>%
                       rbind.pages) %>% rbind.pages

  AA <- readLines('https://raw.githubusercontent.com/klarsys/angular-material-icons/master/angular-material-icons.js')
  AA <- stri_trim_both(AA)
  AA <- list.drop(AA,'empty')
  split.list <- melt(list(social = list(starts = "'amazon'",
                                        ends = "'windows'"),
                          custom = list(starts = "'signal_wifi_0_bar'",
                                        ends = "'account_child'")
  )
  )

  BB <- split.list %>% mutate(Locate = llply(value,function(i)
    grep(i,AA)))%>%select(Locate) %>%
    ul.ac %>% chain.df %>%
    split.df_even_odd %>%
    mutate(evens_x = as.numeric(evens_x),
           odds_x = as.numeric(odds_x))

  AB <- llply(1:nrow(BB),function(i)c(BB[i,1]:BB[i,2]))
  names(AB) <- c('social_icons','custom_icons')

  klarsys.df <-
    lapply(names(AB),function(ni)
      llply(strsplit(AA[AB[[ni]]],":"),function(i)
        data.frame(
          name = gsub("'",'',i[1]),
          svg = gsub("('|',)",'',i[2]) %>%
            stri_trim_both,
          stringsAsFactors = FALSE)
      ) %>%
        rbind.pages) %>%
    rbind.pages %>%
    mutate(SVG =
             paste('<svg xmlns="http://www.w3.org/2000/svg"',
                   'width="48" height="48" viewBox="0 0 48 48"',
                   'stroke="#0089ec" fill="rgba(0,0,0,.2)" fill-opacity:"0.2">',
                   svg,"</svg>",sep="")
    ) %>%
    mutate(icon_group =
             c(rep('social',BB[[2]][[1]]-BB[[1]][[1]]+1),
               rep('custom',BB[[2]][[2]]-BB[[1]][[2]]+1))
    ) %>%
    mutate(icon_size = 48,
           icon_home = 'klarsys_github') %>%
    select(icon_group, icon_name = name,
           icon_size,
           icon_src = SVG,
           icon_home)

  svg_hash <- rbind(svg_hash, klarsys.df)

  assign('mtrl_svg_hash',svg_hash,envir = .GlobalEnv)

  saveRDS(svg_hash,'inst/data/rt_icon_hash_svg.rds')

}
