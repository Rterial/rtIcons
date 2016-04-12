mtrl.colors <- function(){
  git.src <- 'https://github.com/Rterial/Rtbase/blob/master/inst/data/mtrl_colors.rds?raw=true'
  clr_file <- grep('mtr_colors.rds',
                   list.files(recursive = T,full.names = T),
                   value = T)

  if(length(clr_file) == 1){
    mtrl_colors <- readRDS(clr_file)
  }else{
    mtrl_colors <- readRDS(curl::curl_download(git.src,'mtrl_colors.rds'))
  }

  mtrl_colors <- apply(mtrl_colors,2,stri_trim_both) %>%
    as.data.frame(stringsAsFactors = FALSE)

  google_ind <- unique(mtrl_colors$color_mod) %>%
    chain.df('ref') %>%
    mutate(levels =
             c(50,100,200,300,400,600,700,800,900,
               'A100','A200','A400','A700',
               500,50,100,200,300,400,600,700,800,900,
               'A100','A200','A400','A700',
               'black','white','transparent',
               'black','white','transparent')
    )

  mtrl_colors <-
    mtrl_colors %>%
    mutate(
      google_colors =
        google_ind[
          match(mtrl_colors$color_mod,google_ind$ref),
          'levels']
    )

  assign("mtrl_colors",mtrl_colors,envir = .GlobalEnv)
}

mtrl.colors_theme <- function(theme_set = 'light',
                              prime_set = 'blue',
                              accent_set = 'pink',
                              warn_set = 'orange'){

  if(!exists('mtrl_colors')){
    mtrl.colors()
  }

  var_lvl <- list(accent = c("A200","A100","A400","A700"),
                  primary = c('500','300','800','A100'),
                  warn = c('500','300','800','A100'))


  theme_var <- list(
    dark = c("#000000","#212121","#303030","#424242"),
    light =  c('#E0E0E0','#F5F5F5','#FAFAFA','#FFFFFF'))


  prime_stroke <-
    mtrl_colors[mtrl_colors$color_name == prime_set,]  %>%
    filter(google_colors == '500' |
             google_colors == '300' |
             google_colors == '800' |
             google_colors == 'A100') %>%
    select(hex) %>% mutate(pal_name = 'primary',hue_lvl = c(500,300,800,'A100'),color_name = prime_set) %>%
    select(color_name,
           selector = pal_name,
           level = hue_lvl,
           color = hex)

  warn_stroke <-
    mtrl_colors[mtrl_colors$color_name == warn_set,] %>%
    filter(google_colors == '500' |
             google_colors == '300' |
             google_colors == '800' |
             google_colors == 'A100') %>%
    select(hex) %>% mutate(pal_name = 'warn',hue_lvl = c(500,300,800,'A100'),color_name = warn_set) %>%
    select(color_name,
           selector = pal_name,
           level = hue_lvl,
           color = hex)

  accent_stroke <-
    mtrl_colors[mtrl_colors$color_name == accent_set,] %>%
    filter(google_colors == 'A200' |
             google_colors == 'A100' |
             google_colors == 'A400' |
             google_colors == 'A700') %>%
    select(hex) %>% mutate(pal_name = 'accent',hue_lvl = c('A200','A100','A400','A700'),color_name = accent_set) %>%
    select(color_name,
           selector = pal_name,
           level = hue_lvl,
           color = hex)

  pals <- rbind(prime_stroke,warn_stroke)

  pals <- rbind(pals,accent_stroke)

  var_base <- theme_var[[theme_set]] %>%
    chain.df('hex') %>%
    mutate(pal_name = 'theme_master',
           hue_lvl = 1:4,
           color_name = theme_set) %>%
    select(color_name,
           selector = pal_name,
           level = hue_lvl,
           color = hex)

  pal_out <- rbind(pals,var_base)

  fonts_pal <- function(theme_set = NULL){
    if(theme_set == 'light'){
      text_base <- '#000000'
      theme_scale <- c(0.87, 0.54, 0.38, 0.12)
    }else{
      text_base <- "#FFFFFF"
      theme_scale <- c(1, 0.70, 0.5, 0.12)
    }

    fonts_names <- c('primary','secondary','disabled|hint|icons','dividers')

    llply(1:4,function(i)
      data.frame(color_name = fonts_names[[i]],
                 selector = 'fonts',
                 level = i,
                 color = mtrl.hex_to_opace(text_base,theme_scale[[i]])
      )
    ) %>% rbind.pages
  }

  var_font <- fonts_pal(theme_set = theme_set)

  pal_out <- rbind(pal_out,var_font)

  return(pal_out)
}




mtrl.scss_libs <- function(){
  git.base_raw <- 'https://raw.githubusercontent.com'
  mtrlz_git <- 'https://github.com/Dogfalo/materialize/tree/master/sass/components'
  node_parse <- '.css-truncate-target .js-navigation-open'

  raw_tbl <- html(mtrlz_git) %>% html_nodes(node_parse)



  home_tree <-
    llply(raw_tbl,function(i)
      data.frame(link = html_attr(i,'href'),
                 name = html_text(i))
    ) %>% rbind.pages %>%
    mutate(types = ifelse(grepl("\\.",name),'file','folder')) %>%
    mutate(sets_up = gsub('(_|.scss)',"",name)) %>%
    mutate(link = ifelse(types != 'folder',
                         paste0(git.base_raw,
                                gsub('blob/','',
                                     link)
                         ),
                         paste0('https://github.com',link)
    )
    )

  sub_trees <- home_tree %>% filter(types == 'folder') %>% `$`(link)

  sub_tree <- lapply(lapply(sub_trees,function(i)
    html(i) %>% html_nodes(node_parse)) %>% unlist,function(xi)
      data.frame(link = html_attr(xi,'href'),
                 name = html_text(xi))) %>%
    rbind.pages %>%
    mutate(sets_up = gsub('(_|.scss)',"",name)) %>%
    mutate(link = paste0(git.base_raw, gsub('blob/','', link)))


  file_idx <- rbind(home_tree %>% filter(types != 'folder') %>% select(-types) ,sub_tree)


  # If we need to copy to a local directory this is the loop function
  #
  #   lapply(1:nrow(file_idx),function(i)
  #     readLines(file_idx[[1]][[i]]) %>%
  #       paste0(collapse = "\n") %>%
  #       HTML %>%
  #       write(paste0('inst/rtbuild/scss/',
  #                    file_idx[[2]][[i]])
  #             )
  #     )

  globs <- subset(file_idx,sets_up == 'global','link')[[1]]


  # Every css variable

  vars_ <- subset(file_idx,sets_up == 'variables','link')[[1]]

  Va  <- readLines(vars_)

  comps <-  stri_sub(
    gsub("([*]+[[:space:]]|[/]+[[:punct:]])","",
         grep('[/]+[*]+[[:space:]]+[[:digit:]]',
              Va, value = T)),5,50) %>%
    stri_trim_both

  var_index <- lapply(comps,function(i)
    data.frame(names = i,
               locs =
                 grep(i,Va)[
                   length(grep(i,Va))
                   ])) %>%
    rbind.pages


  lapply(strsplit(Va[grep('\\$',Va)],": "),function(i)
    data.frame(var_name = i[[1]],var_value = i[[2]])) %>%
    rbind.pages


}






#' Inline css builder function
#'
#'
#' \code{mtrl.build_css}
#'
#' @examples
#' > mtrl.build_css(list(background_color = "blue",width=100))
#' [1] "background-color:blue;width:100"
#'
#'
mtrl.build_css <- function(...){

  options('useFancyQuotes' = FALSE)

  raw <- HTML(paste0(unlist(lapply(list(...),function(i)
    paste(gsub("_","-",names(i)),i,sep=":",collapse = ";")))))

  HTML(paste0("style=",dQuote(raw)))
}


#' Apply a list of named attribs to a tag
#'
#'\code{mtrl.build_attrs}
#'
#'@examples
#' > mtrl.tag_paste(c(class = "container",id = "new_id"))
#'
#' [1] class="container" id="new_id"
mtrl.build_attrs <- function(...){

  options('useFancyQuotes' = FALSE)

  HTML(sapply(list(...),function(i)
    HTML(paste0(names(i),"=",dQuote(i),sep = " ") %>%
           stri_trim_both))
  )

}


#' Generate waves css from R
#'
#' \code{mtrl.css_waves}
#'
mtrl.css_waves <- function(color_name,color_hex){
  HTML(paste0(".waves-effect.waves-",
              color_name," ",".waves-ripple {\n\t\tbackground-color:",
              lapply(color_hex %>% toupper %>% as.character,function(i)
                paste0("rgba(",paste0(col2rgb(i),collapse = ","),
                       ",0.67);\n}")
              ),
              collapse = "\n")
  )
}


#' Set opacity on a color and return either a usable color or pure css
#'
#' \code{mtrl.css_opacity}
#'
#'
#'
#' @examples
#'

mtrl.css_opacity <- function(color_name = NULL,color_value = NULL,opacity = NULL,
                             css_var = c('background-color'),
                             write_css = FALSE){

  measure.between <- function(var_obj,small,big){
    all(c(is.smaller = var_obj < big,
          is.bigger = var_obj > small) == TRUE)

  }

  if(measure.between(opacity,small = 0, big = 1)){
    opace <- opacity
  }else{
    t.opace <- opacity/10
    if(t.opace > 1){
      opace <- opacity/100
    }else{
      opace <- t.opace
    }
  }

  if(!is.null(color_value)){
    color_line <-
      lapply(color_value,function(i)
        paste0("rgba(",paste0(col2rgb(i),collapse = ","),
               ",",opace,
               ")")
      )
  }

  lum.color <- c(color_line)

  if(!is.null(color_name)){
    names(lum.color) <- color_name
  }

  if(write_css){
    # To see if we're creating a border or alternative rule or
    # if it's just the color syntax
    # eg. css_var = "border-bottom:1px solid" - vs - "background-color"

    css_inner <- ifelse(grepl(":",css_var),css_var,":")
    lum.color <-
      pure.html(
        paste0(".", color_name, " ", "{\n",
               "\t\t",css_inner, color_line,";\n",
               "}\n")
      )
  }

  return(lum.color)

}



mtrl.hex_to_opace <- function(hex = NULL,opacity = 0.5){
  paste0("rgba(",paste0(col2rgb(hex),collapse = ","),",",opacity,")")
}




#' Create a css block-rule
#'
#'  \code{mtrl.css_block}
#'
#'   @examples
#'   mtrl.css_block(css_selector = '.md-input-container','    display: inline-block;
#'    position: relative;
#'    padding: 2px;
#'    margin: 18px 0;
#'    vertical-align: middle;',to_file = 'test.css')
#'> mtrl.css_block(css_selector = '.md-input-container','    display: inline-block;
# +     position: relative;
# +     padding: 2px;
# +     margin: 18px 0;
# +     vertical-align: middle;',for_head = T)
# <style>
# .md-input-container{
# display: inline-block;
# position: relative;
# padding: 2px;
# margin: 18px 0;
# vertical-align: middle;
# }
# </style>
#'
#'
#'
mtrl.css_block <- function(css_selector = NULL,...,
                           for_head = F,to_file = NULL){


  css_inner <- paste0(lapply(strsplit(...,'\n')[[1]],function(i)
    strwrap(stri_trim_both(i),indent = 3)),collapse = "\n")

  new_block <- paste0(css_selector,"{\n",css_inner,"\n}") %>% pure.html

  if(for_head){
    sprintf('<style>\n %s \n </style>',new_block)%>%pure.html

  }else if(!is.null(to_file)){

    if(file.exists(to_file)){
      write(new_block,'try.css')

      file.append(to_file,'try.css')
    }else{
      write(new_block,to_file)
    }

  }
}



mtrl.color_block <- function(color_val = NULL,color_name = NULL){

  text_block <- function(color_val = NULL,pos = c('left','right'),color_name = NULL){
    p.block <- c("vertical-align:middle",
                 "padding:5px 15px",
                 "float:%s !important",
                 "color:rgba(0,0,0,.53)",
                 "margin-left:5px",
                 "font-weight:600",
                 "border:1px solid transparent !important")

    HTML(
      paste('<td style="',
            sprintf(paste0(p.block,collapse = ";"),pos),
            '"><h3>',
            ifelse(!is.null(color_val) & pos == 'right',color_val,''),
            " ",
            ifelse(!is.null(color_name) & pos == 'right',color_name,""),
            "</h3></td>",
            sep="")
    )
  }

  color_box <- function(x){
    blocks <- c("height:25px",
                "width:25px",
                "margin-top:5px",
                "padding:10px 5px",
                "margin:auto",
                "background-color:%s",
                "border:0px solid #eee")

    HTML(
      paste0("<td><div style=\"",
             sprintf(paste0(blocks,collapse = ";"),x),
             "\"></div></td>"
      )
    )
  }



  parent <- function(x){
    s.block <- c("background:#ccc",
                 "border-top:1px solid rgba(0,0,0,.13)",
                 "padding-top:2px",
                 "list-style:box")


    HTML(paste0("<tr style=\"",paste0(s.block,collapse = ";"),'">',
                paste0(x,collapse = "\n"),
                "</tr>")
    )


  }

  paste(parent(paste0(
    text_block(pos = "left",color_name),
    color_box(color_val),
    text_block(color_val = color_val,pos = 'right',color_name = color_name),collapse = "\n")
  ))
}
