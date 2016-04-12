
rt.template <- function(html_opt = 'materialize',...){


  withTags(
    tagList(
      head(
        link(
          href="https://fonts.googleapis.com/icon?family=Material+Icons",
          rel="stylesheet"),
        link(
          rel="stylesheet",
          href = "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/css/materialize.min.css"),
        script(
          type="text/javascript",
          src="https://code.jquery.com/jquery-2.1.1.min.js"),
        script(src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/js/materialize.min.js")),
      ...)
  )
}




# HEAD DEPS
#' @examples
#' rt.get_head('angular_material')
#'
#'
#' > rt.get_head('angular_material')
#' <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet"/>
#'   <link href="http://ajax.googleapis.com/ajax/libs/angular_material/1.0.0/angular-material.min.css" rel="stylesheet"/>
#'  <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.4.8/angular.min.js"></script>
# ' <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.4.8/angular-animate.min.js"></script>
#'   <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.4.8/angular-aria.min.js"></script>
#'   <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.4.8/angular-messages.min.js"></script>
#'   <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.3.15/angular-route.min.js"></script>
#'   <script src="http://ajax.googleapis.com/ajax/libs/angular_material/1.0.0/angular-material.min.js"></script>
#'   <script src="https://s3-us-west-2.amazonaws.com/s.cdpn.io/t-114/assets-cache.js"></script>
#'   <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.3.15/angular-route.min.js"></script>
#'
#'
#'> rt.get_head('morris')
#'<link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet"/>
#'   <link href="https://cdnjs.cloudflare.com/ajax/libs/morris.js/0.5.1/morris.css" rel="stylesheet"/>
#'   <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"></script>
#'   <script src="https://cdnjs.cloudflare.com/ajax/libs/raphael/2.1.0/raphael-min.js"></script>
#'   <script src="https://cdnjs.cloudflare.com/ajax/libs/morris.js/0.5.1/morris.min.js"></script>
#'   > rt.get_head('owl')
#' <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet"/>
#'   <link href="https://cdnjs.cloudflare.com/ajax/libs/owl-carousel/1.3.3/owl.carousel.css" rel="stylesheet"/>
#'   <link href="https://cdnjs.cloudflare.com/ajax/libs/owl-carousel/1.3.3/owl.theme.css" rel="stylesheet"/>
#'   <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/1.9.1/jquery.js"></script>
#'   <script src="https://cdnjs.cloudflare.com/ajax/libs/owl-carousel/1.3.3/owl.carousel.js"></script>
#'   <script src="https://cdnjs.cloudflare.com/ajax/libs/modernizr/2.8.3/modernizr.js"></script>
#'
#'
#' > rt.get_head('materializ')
#' <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet"/>
#'
#'
#'  > rt.get_head('materialize')
#' <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet"/>
# '  <link href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/css/materialize.min.css" rel="stylesheet"/>
# '  <script src="https://code.jquery.com/jquery-2.1.1.min.js"></script>
# '  <script src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/js/materialize.min.js"></script>

rt.get_head <- function(lib_name = NULL){
  mtrl.deps()
  mtrl_headers %>%
    filter(lib == 'roboto'|
             lib == 'material_icons'|
             lib == lib_name) %>%
    mutate(head_out =
             ifelse(class == 'css',
                    paste0("<link href=",
                           dQuote(src_link),
                           ' rel="stylesheet" />'),
                    paste0('<script src=',
                           dQuote(src_link),
                           "></script>")
             )
    ) %>% `$`(head_out) %>% paste0(collapse = "\n") %>% HTML
}



mtrl.template <- function(...){
  tagList(
    tags$head(rt.head()),
    tags$body(...)
  )
}


mtrl.init <- function(...){

  mtrl.icons_css <-
    ".material-icons.md-18 { font-size: 18px; }
  .material-icons.md-24 { font-size: 24px; }
  .material-icons.md-36 { font-size: 36px; }
  .material-icons.md-48 { font-size: 48px; }"

  tagList(
    tags$script(
      HTML(
        "$(document).ready(function() {\n
        $('.button-collapse').sideNav();
        $('.collapsible').collapsible({
        accordion : false
        });
        $('ul.tabs').tabs();
        $('select.rt-select').material_select();
});")
    ),
    tags$style(HTML(".caret{border:none !important}
                    .material-icons.md-48 { font-size: 48px; }
                    .select-dropdown li.left-circle{float:left}")
    )
    )

  }





ext.libs_folders <- function(dir_name = NULL, lib_name = NULL){

  if(!is.null(dir_name)){

    # Check if the directory exists
    if(!grepl(dir_name,dir())){
      dir.create(dir_name)

      top_level <- dir_name
    }else{
      top_level <- dir_name
    }
  }

  folder_top <- "inst/www"

  src_libs <- list('d3','nvd3','morris','owl','mtrl',
                   'mtrl_dt','mtrl_image','md_todo')


  sapply(paste0(folder_top,src_libs),dir.create)



}


#'Codepens in HTML from personal dev account
#'
#'
#'\code{mtrl.go_to_codepen}
#'
#'Browse to a specific pen or add a new ref to the list
#'
mtrl.go_to_cp <- function(){

  dash_sandbox <- "http://codepen.io/CarlBoneri/pen/rxWmKO"
  data_table <- "http://codepen.io/CarlBoneri/pen/zrwMNY"
  to_do <- "http://codepen.io/CarlBoneri/pen/mVMbEY"
  owl <- "http://codepen.io/CarlBoneri/pen/adWabJ"
  sidebar <- "http://codepen.io/CarlBoneri/pen/PZjopO"
  image_upload <- "http://codepen.io/CarlBoneri/pen/jWewxz"


}
mtrl.res_from_codepen <- function(get_template = c('dash','dt','to_do','owl'),
                                  view_raw = c('html','js','css')){

  dash_sandbox <- "http://codepen.io/CarlBoneri/pen/rxWmKO"
  data_table <- "http://codepen.io/CarlBoneri/pen/zrwMNY"
  to_do <- "http://codepen.io/CarlBoneri/pen/mVMbEY"
  owl <- "http://codepen.io/CarlBoneri/pen/adWabJ"

  get_from <- switch(get_template,
                     'dash' = dash_sandbox,
                     'dt' = data_table,
                     'to_do' = to_do,
                     'owl' = 'owl')

  if(length(view_raw) == 3){

    raw_html <- paste0(get_from,'.html')
    raw_js <- paste0(get_from,'.js')
    raw_css <- paste0(get_from,'.css')

    cp_out <- list(html_page = html(raw_html),
                   js_page = html(raw_js),
                   css_page = html(raw_css))
  }else{
    var_get <- paste0(get_from,'.',view_raw)
    cp_out <- html(var_get)
  }

  return(cp_out)
}



mtrl.widget_js <- function(widget_name = c('owl','dt','todo')){

  var_ctrl <-
    switch(widget_name,
           'owl' = 'www/js/ang_owl.js',
           'dt' = 'www/js/dt_js.js',
           'todo' = 'www/js/angular_todo.js')

  if(!file.exists(var_ctrl)){
    drop_get(paste0("/rterial/",var_ctrl),local_file = 'tmp_js.js',
             overwrite = T)

    raw_js <- ldply(readLines('tmp_js.js'),function(i)
      stri_trim_both(i))%>%filter(nchar(V1)>1) %>%
      select(V1) %>%
      unlist %>%
      as.character %>%
      HTML

  }else{
    raw_js <- ldply(readLines(var_ctrl),function(i)
      stri_trim_both(i))%>%filter(nchar(V1)>1) %>%
      select(V1) %>%
      unlist %>%
      as.character %>%
      HTML
  }
  return(raw_js)
}



mtrl.deps <- function(add_library = NULL){
  res_libs <-
    list(
      roboto = list(
        stylesheet = "http://fonts.googleapis.com/css?family=Roboto"),
      material_icons = list(
        stylesheet = "https://fonts.googleapis.com/icon?family=Material+Icons"
      ),
      font_awesome = list(
        stylesheet = "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
      ),
      materialize = list(
        stylesheet = "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/css/materialize.min.css",
        script = "https://code.jquery.com/jquery-2.1.1.min.js",
        script = "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/js/materialize.min.js"
      ),
      angular_material = list(
        stylesheet = "http://ajax.googleapis.com/ajax/libs/angular_material/1.0.0/angular-material.min.css",
        script = "http://ajax.googleapis.com/ajax/libs/angularjs/1.4.8/angular.min.js",
        script = "http://ajax.googleapis.com/ajax/libs/angularjs/1.4.8/angular-animate.min.js",
        script = "http://ajax.googleapis.com/ajax/libs/angularjs/1.4.8/angular-aria.min.js",
        script = "http://ajax.googleapis.com/ajax/libs/angularjs/1.4.8/angular-messages.min.js",
        script = "https://ajax.googleapis.com/ajax/libs/angularjs/1.3.15/angular-route.min.js",
        script = "http://ajax.googleapis.com/ajax/libs/angular_material/1.0.0/angular-material.min.js",
        script = "https://s3-us-west-2.amazonaws.com/s.cdpn.io/t-114/assets-cache.js",
        script = "https://ajax.googleapis.com/ajax/libs/angularjs/1.3.15/angular-route.min.js"
      ),
      todo_app = list(
        stylesheet = "http://clnhll.com/todo/css/angular-material.css",
        script = "http://clnhll.com/todo/js/angular-drag-and-drop-lists.js",
        script = "http://clnhll.com/todo/js/assets-cache.js"
      ),
      material_data_table = list(
        stylesheet = "http://rawgit.com/daniel-nagy/md-data-table/master/dist/md-data-table.css",
        script = "http://rawgit.com/daniel-nagy/md-data-table/master/dist/md-data-table.js"
      ),
      bootstrap = list(
        stylesheet = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css",
        script = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
      ),
      owl = list(
        stylesheet = "https://cdnjs.cloudflare.com/ajax/libs/owl-carousel/1.3.3/owl.carousel.css",
        stylesheet = "https://cdnjs.cloudflare.com/ajax/libs/owl-carousel/1.3.3/owl.theme.css",
        script = "https://cdnjs.cloudflare.com/ajax/libs/jquery/1.9.1/jquery.js",
        script = "https://cdnjs.cloudflare.com/ajax/libs/owl-carousel/1.3.3/owl.carousel.js",
        script = "https://cdnjs.cloudflare.com/ajax/libs/modernizr/2.8.3/modernizr.js"
      ),
      d3 = list(
        script = "http://d3js.org/d3.v3.min.js"
      ),
      nvd3 = list(
        stylesheet = "https://cdnjs.cloudflare.com/ajax/libs/nvd3/1.8.1/nv.d3.css",
        script = "https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.2/d3.min.js",
        script = "https://cdnjs.cloudflare.com/ajax/libs/nvd3/1.8.1/nv.d3.js"
      ),
      morris = list(
        stylesheet = "https://cdnjs.cloudflare.com/ajax/libs/morris.js/0.5.1/morris.css",
        script = "https://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js",
        script = "https://cdnjs.cloudflare.com/ajax/libs/raphael/2.1.0/raphael-min.js",
        script = "https://cdnjs.cloudflare.com/ajax/libs/morris.js/0.5.1/morris.min.js"
      )
    )
  # IF NEW LIB
  if(!is.null(add_library)){
    res <- append(res_libs,add_library)
  }else{
    res <- res_libs
  }

  # LIST TO REF TABLE
  res_df <- melt(res_libs) %>%
    mutate(src_link = as.character(value),
           res = L2,
           lib = L1) %>%
    mutate(class = ifelse(res == 'stylesheet','css','js'))

  res_df <- res_df[,c('src_link','res','lib','class')]

  assign("mtrl_headers",res_df,envir = .GlobalEnv)

}


mtrl.make_head <- function(lib_name){
  hd.grep <- paste0(c('roboto','material_icons',lib_name),collapse = "|")
  mtrl_headers %>% filter(grepl(hd.grep,lib)) %>%
    mutate(outs =
             ifelse(class == 'css',
                    sprintf('<link href="%s" rel="stylesheet">',src_link),
                    sprintf('<script src="%s"></script>',src_link))) %>%
    `$`(outs) %>%
    paste0(collapse = "\n") %>% pure.html



}

mtrl.normalize_deps <- function(dep_name = NULL){

  if(!is.null(dep_name)){
    dep_list <- sapply(dep_name,function(i)c(dep_list[[i]]))
  }
  if(!is.null(dep_name)){
    dep_list <- dep_list
  }


  ldply(1:length(dep_name),function(i)
    ifelse(names(...) == "script",
           make_heads <- list(
             script =
               function(...)
                 HTML(
                   paste("<script src=\"",(...),"\"></script>",
                         sep="")
                 ),
             link =
               function(...)
                 HTML(paste('<link href=\"',(...),
                            '\" rel="stylesheet" type="text/css" />',
                            sep="")),
             qualify = function(...)
               if(names(...) == "stylesheet"){
                 make_head$link(...)
               }else{
                 make_heads$script(...)
               }
           )
    ))
}



mtrl.heads <- function(load_libs = c('ang_material','materialize','bootstrap'),
                       add_in = c('md_dt','md_todo','md_owl'),
                       font_var = NULL){

  ul.ac <- function(...)unlist(as.character(...))

  ref_df <- mtrl.deps() %>% arrange(class)

  if(!is.null(font_var)){
    font_in <- font_var
  }else{
    font_in <- ref_df %>%
      filter(lib == 'roboto') %>%
      select(src_link) %>% ul.ac
  }

  icon_in <- ifelse(load_lib == 'bootstrap',
                    ref_df %>%
                      filter(lib == 'font_awesome') %>%
                      select(src_link) %>% ul.ac,
                    ref_df %>%
                      filter(lib == 'material_icons') %>%
                      select(src_link) %>% ul.ac
  )

  lib_deps <-




    if(load_libs == 'bootstrap' & length(load_libs)!=3){
      icon_rel <- ref_df %>% filter(res == 'fa_icon')
    }else{
      icon_rel <- ref_df %>% filter(res == 'material_icons')
    }

  if(!is.null(font_var)){
    font_rel <- ifelse(stri_sub(font_var,1,5) == '<link',
                       font_var,
                       tags$link(font_var)
    )
  }else {
    font_rel <- ref_df%>%filter(res == 'roboto')
  }

  lib_refs <- switch(load_libs,
                     'ang_material' = ref_df %>%
                       filter(lib == 'ang_mtr'),

                     'materialize' =  ref_df %>%
                       filter(lib == 'materialize'),

                     'bootstrap' = ref_df %>%
                       filter(lib == 'boot'))

  if(length(add_in) != 3){

    lib_refs <- switch(add_in,
                       'md_dt' = ref_df %>%
                         filter(lib == 'ang_mtr' | lib == 'md_dt'),

                       'md_todo' = ref_df %>%
                         filter(grepl('ang_mtr',lib) | grepl('md_todo',lib)),

                       'md_owl' = ref_df %>%
                         filter(lib == 'ang_mtr' | lib == 'md_owl')
    )

  }

  head_out <- rbind(font_rel,icon_rel,lib_refs) %>% select(src_link) %>%
    unlist %>% as.character %>% paste0(collapse = "\n") %>%
    HTML

  return(head_out)
}
