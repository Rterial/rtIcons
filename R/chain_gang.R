#' Pass an object through a dataframe converting chain
#'
#' \code{chain.df} returns an object as a Data Frame
#' Most of the time when manipulating data in a 'pipe'
#'  converting an object or array into a data frame is
#'  simplest, but comes with varying drawbacks and I
#'  don't normally set opitions(stringsAsFactors=F)
#'  as a default. This allows passing a melted or converted object
#'  in a chain and setting the class on each column as well as
#'  renaming the defaults or including the row names as a column
#'
#'
chain.df <- function(x,col_name = NULL,with_row = NULL){

  df <- as.data.frame(x,stringsAsFactors=F)

  if(length(df)>1 & is.null(with_row)){
    row.names(df) <- NULL
    return(df)
  }
  if(length(df)>1 & !is.null(with_row)){
    rw_nm <- row.names(df)
    df <- data.frame(with_row = rw_nm,df,stringsAsFactors = FALSE)
    return(df)
  }

  if(is.null(col_name)){
    colnames(df) <- 'df_name'
  }else{
    colnames(df) <- col_name
  }
  if(!is.null(with_row)){
    rw_nm <- row.names(df)
    df <- data.frame(row_names = rw_nm,df,stringsAsFactors = FALSE)
  }

  return(df)
}




#making a function in order to handle list objects as well
col_key <- function(x){
  mapply(class,x) %>% melt %>% chain.rename_col('class','columns') %>% filter(duplicated(columns)==F)
}

chain.key_ind <- function(x){


  #first data frame is a key used to group the hierarchy
  step_one <-
    lapply(colnames(x),
         function(i)
           data.frame(col_names = i,
                      c_class = class(x[[i]]),c_len = length(x[[i]]),
                      c_sub_names = ifelse(length(names(x[[i]])) < 1,
                                           0,
                                           paste0(names(x[[i]]),collapse=" | ")
                                           ),stringsAsFactors=F)
         ) %>% rbind.pages

  p1_name_chr <- step_one %>% filter(c_class == 'character' & c_sub_names == 0) %>% select(col_names) %>% unlist %>% as.character

  p1_name_num <- step_one %>% filter(c_class == 'integer' & c_sub_names == 0) %>% select(col_names) %>% unlist %>% as.character

  one_sub <- a[,c(a_one %>% filter(c_sub_names != 0) %>% select(col_names) %>% unlist %>% as.character)]


  b <- mapply(length,x)

  c <- mapply(class,x)

  d <- colnames(x)

  data.frame(slot = d,name_in = a,n_length = b,classed = c,stringsAsFactors=F)
}

#'Isolate columns in a Data Frame by class
#'
#'\code{chain.df_key} returns either the name and class of each segment in a df or
#'



chain.df_key <- function(x){

  tmp_df <- x


  key <- data.frame(slot_class = mapply(class,tmp_df),slot_len = mapply(length,tmp_df),stringsAsFactors=F) %>%
    chain.df(with_row = 'slots') %>% rename(col_name = with_row)

  sub_key <- key %>% filter(slot_class == 'data.frame'| slot_class == 'list')


  key <- key %>% filter(slot_class != 'data.frame',slot_class != 'list')

  top_df <- tmp_df[,key$col_name]

  bottom_df <- tmp_df[,sub_key$col_name]



  sub_cols <- sub_key %>% select(col_name) %>% unlist %>% as.character

  sub_df <- lapply(1:length(sub_cols),function(i)tmp_df[,colnames(tmp_df) == sub_cols[i]])

  names(sub_df) <- sub_cols




  sub_out <-
    lapply(1:length(sub_df),function(i)
    data.frame(slot_class = mapply(class,sub_df[[i]]),
             slot_len = mapply(length,sub_df[[i]]),
             stringsAsFactors=F) %>% chain.df(with_row = 'slots') %>%
    rename(col_name = with_row))





  keys <- list(parent = top_df,sub_out,bottom_df)

  return(keys)


}



chain.top_key <- function(x){

  tmp_df <- x


  key <- data.frame(slot_class = mapply(class,tmp_df),slot_len = mapply(length,tmp_df),stringsAsFactors=F) %>%
    chain.df(with_row = 'slots') %>% rename(col_name = with_row)

  sub_key <- key %>% filter(slot_class == 'data.frame'| slot_class == 'list')


  key <- key %>% filter(slot_class != 'data.frame',slot_class != 'list')

  top_df <- tmp_df[,key$col_name]



  sub_cols <- sub_key %>% select(col_name) %>% unlist %>% as.character

  sub_df <- lapply(1:length(sub_cols),function(i)tmp_df[,colnames(tmp_df) == sub_cols[i]])

  names(sub_df) <- sub_cols




  sub_out <-
    lapply(1:length(sub_df),function(i)
      data.frame(slot_class = mapply(class,sub_df[[i]]),
                 slot_len = mapply(length,sub_df[[i]]),
                 stringsAsFactors=F) %>% chain.df(with_row = 'slots') %>%
        mutate(col_name = with_row)%>%select(-with_row))





  keys <- list(parent = top_df,sub_out)

  return(keys)


}


chain.key_loop <- function(orig_data,df_key){

  new_key <-
    sapply(names(df_key$child),
         function(i)df_key$child[[i]] %>%
           filter(slot_class == 'data.frame'|slot_class=='list') %>%
           select(col_name) %>%
           unlist %>% as.character) %>%
    unlist %>% chain.df

  key_three <-lapply(colnames(),function(i)chain.df_key(a[,i]))


  sapply(names(a3[[2]]$child),
         function(i)a3[[2]]$child[[i]]%>%
           filter(slot_class == 'data.frame'|slot_class=='list') %>%
           select(col_name) %>%
           unlist %>% as.character)

  sub_key <- new_key %>% filter(slot_class == 'data.frame'| slot_class == 'list')


  key <- new_key %>% filter(slot_class != 'data.frame',slot_class != 'list')



  sub_cols <- sub_key %>% select(col_name) %>% unlist %>% as.character

  sub_df <- lapply(1:length(sub_cols),function(i)tmp_df[,colnames(tmp_df) == sub_cols[i]])

  names(sub_df) <- sub_cols

}


chain.df_key_2 <- function(x,filter_out = NULL,filter_by = NULL,all_flat = NULL,parse_df = NULL){

  df_in <- x

  col_key <- mapply(class,df_in) %>% melt %>%
    chain.rename_col('class','columns') %>%
    filter(duplicated(columns)==F)

  if(!is.null(all_flat)){

    pass_one <- col_key %>% filter(class != 'list' & class != 'data.frame') %>% select(columns) %>% unlist %>% as.character

    pass_two <-  col_key %>% filter(class == 'list' | class == 'data.frame') %>% select(columns) %>% unlist %>% as.character

    nest_pass <- ldply(df_in,
                       function(i)
                         data.frame(class = class(i),
                                    nl = length((i)),
                                    stringsAsFactors = FALSE)) %>%
      filter(class =='data.frame'|class=='list') %>%
      mutate(n_names = lapply(mapply(names,df_in[.id]),function(i)paste(i,collapse="|")) %>% unlist %>% as.character)

    nest_df <- lapply(lapply(nest_pass$n_names,
                             function(i)lapply(stri_split_fixed(i,"|") %>% unlist,
                                               function(x)df_in[unlist(nest_pass$.id)][[x]] %>% as.data.frame(stringsAsFactors=F)
                             ) %>% rbind.pages),
                      function(xj)xj %>% cbind)


    #first df
    df_one <- df_in[,pass_one]

    df_two <- lapply(df_in[,pass_two],function(i)unlist(i) %>% paste(collapse="|")) %>% unlist

    if(length(nest_df)==1){
      flattened <- data.frame(df_one,df_two,stringsAsFactors=F)
    }else{
      flattened <- list(flat = data.frame(df_one,df_two,stringsAsFactors=F),nest = nest_df)
    }
    return(flattened)
  }

  if(!is.null(filter_by) & !is.null(filter_out)){

    filter_df_out <- lapply(filter_out,function(i)col_key[col_key$class != i,])
    filter_df <- lapply(filter_by,function(ioi)col_key[col_key$class == ioi,])

    if(length(filter_df)>1){

      filter_df <- rbind.pages(filter_df)

    }else{
      filter_df <- filter_df
    }
    if(!is.null(parse_df)){
      return(df_in[,filter_df$columns])
    }
    return(filter_df)
  }
  if(is.null(filter_out) & is.null(filter_by)){
    return(col_key)
  }else{
    if(!is.null(filter_by)){
      filter_df <- lapply(filter_by,function(i)col_key[col_key$class == i,])
    }
    if(!is.null(filter_out)){
      filter_df <- lapply(filter_out,function(i)col_key[col_key$class != i,])
    }else{
      filter_df = col_key
    }
    if(length(filter_df)==2){
      names(filter_df) <- c('orig_key','filter_key')
    }else{
      names(filter_df) <- 'filter_key'
    }
    if(!is.null(parse_df)){
      return(df_in[,filter_df$filter_key$columns])
    }
    return(filter_df)
  }
}

chain.stats <- function(x,calcs = list(...)){
  df_in <- x

  if(is.data.frame(df_in) & length(df_in) == 1){
    raw <- as.numeric(df_in[[1]])
  }
  if(is.data.frame(df_in) & length(df_in) > 1){

    col_key <- mapply(class,df_in) %>% melt %>%
      chain.rename_all_col(c('class','columns')) %>%
      filter(duplicated(columns)==F) %>%
      filter(class == 'numeric'| class=='integer') %>%
      select(columns) %>% unlist %>% as.character

    raw <- lapply(df_in[,col_key],function(i)naConvert(as.numeric(i),0)) %>% chain.df

    computed <- sapply(colnames(raw),
                       function(ni)
                         sapply(calcs,function(i)do.call(i,list(raw[,ni])))) %>%
      chain.df %>% mutate(calculation = calcs)

    return(computed)

    }else{
      raw <- as.numeric(x)
    }
  calculated <-
    sapply(calcs,function(i)
      data.frame(do.call(i,list(raw)),stringsAsFactors=F)) %>%
        as.data.frame(stringsAsFactors=F)

  colnames(calculated) <- calcs


  return(calculated)
}

#' All the tans
chain.sick_tan <- function(x, y = NULL){

  #key set up

  stat_df <- data.frame(val_x = as.numeric(x),stringsAsFactors=FALSE)

  if(!is.null(y)){
    stat_df$val_y <- as.numeric(y)

    sign_out <- atan2(y = stat_df[[2]],x = stat_df[[1]])
  }else{
    sign_out <-
      lapply(1:nrow(stat_df),
            function(i)
              data.frame(constant = stat_df[i,1],
                         cos_val = cos(pi * as.numeric(stat_df[i,1])),
                         acos_val = acos(stat_df[i,1]),
                         cos_pi_val = cospi(stat_df[i,1]),
                         sin_val = sin(pi * stat_df[i,1]),
                         asin_val = asin(stat_df[i,1]),
                         sin_pi_val = sinpi(stat_df[i,1]),
                         tan_val = tan(pi * stat_df[i,1]),
                         atan_val = atan(pi * stat_df[i,1]),
                         tan_pi_val = tanpi(stat_df[i,1]),
                         log_val = log(1+stat_df[i,1]),
                         log_one = log1p(stat_df[i,1]),
                         expn_val = exp(stat_df[i,1])-1,
                         expn_one = expm1(stat_df[i,1]),
                         stringsAsFactors = FALSE)
      ) %>% rbind.pages

    n.cor <- grep("NaN",sign_out[1,])

    sign_out <- sign_out[-c(n.cor)]


  }
  return(sign_out)
}



#chainers-------------------
chain.cbind_orig <- function(x,orig_df){

  d_in <- x

  if(is.data.frame(d_in)==TRUE)if(length(x) != length(orig_df)){
    NULL
  }else{
    cbind(orig_df,x)
  }
}
chain.rename_col <- function(x,col_name = NULL,row_name = NULL){

  colnames(x) <- 'col_um'
  if(length(row.names(x))>1 &&!is.null(row_name)){
    x$from_row = rownames(x)
  }
  if(!is.null(col_name)){
    colnames(x)[[1]] <- as.character(col_name)
  }
  if(!is.null(row_name)){
    colnames(x)[[2]]<- as.character(row_name)
  }
  return(x)
}

chain.rename_all_col <- function(x,new_names = c(...)){
  raw_df <- x

  if(length(raw_df) != length(new_names)){
    n_short <- as.numeric(length(raw_df))-length(new_names)

    xtra_col <- lapply(1:n_short,function(i)paste0('col_extra_',i))%>%unlist
    colnames(raw_df)[1:length(new_names)] <- new_names

    colnames(raw_df)[(length(new_names)+1):as.numeric(length(raw_df))] <- (xtra_col)
  }else{
    colnames(raw_df) <- new_names
  }
  return(raw_df)
}


chain.map_bound <- function(x){

  a2$result$geometry$viewport %>% unlist %>% as.list%>% melt %>%
    mutate(cord = stri_sub(L1,-3),place = c(1,1,2,2))%>%
    mutate(cord_plot = paste0(cord,place))%>%
    select(cord_plot,value)%>%slice(c(2,1,4,3))


}


#x is a data frame
chain.df_to_matrix <- function(x,row_name_pos = 1){
  #build the temporary df
  df_tmp <- x
  row.names(df_tmp) <- df_tmp[,row_name_pos]
  df_tmp[[row_name_pos]] <- NULL
  return(as.matrix(df_tmp))
}
# }
#
#   if(!is.null(row_name_col)){
#     name_str <- df_tmp[[row_name_col]]
#
#     if(length(unique(name_str)) != nrow(df_tmp)){
#       #append the row.names with a numerical key for unique factoring and allowance
#       name_str <- lapply(1:nrow(df_tmp),function(i)paste0(name_str,"_",i))
#       }else{
#       name_str <- name_str
#       }
#     #attach rownames
#     row.names(df_tmp) <- name_str
#     #remove the original column
#     df_tmp[[name_str]] <- NULL
#   }
#   #convert to matrix
#   mx_tmp <- as.matrix(df_tmp)
#
#   return(mx_tmp)
# }
#

chain.peek_in <- function(x,extract_class = NULL,df_split = NULL){

  df_in <- x
  a <- rbind(mapply(class,df_in),mapply(length,df_in)) %>% as.data.frame(stringsAsFactors=F)
  df_outs <- c(a[c(grep('data.frame',a[1,]))] %>% names)

  list_of_df <- sapply(1:length(df_outs),
                       function(i)
                         df_in[[df_outs[i]]]%>%
                         as.data.frame(stringsAsFactors=F))
  names(list_of_df) <- df_outs

    return(list_of_df)
  }



chain.build_web_keys <- function(url_in){

  #parse html from provided site handle
  base_html <- html(url_in)

  #refrence of all possible html tags
  ref_tags <- names(htmltools::tags)

  #create a recursive list of all potential html nodes
  raw_list <-
    sapply(ref_tags,
         function(i)base_html %>% html_nodes(i) %>% unlist) %>% list_drop_null


  #doing a lot here..basically pass through recursivly the raw html lists created
  #extract the html attributes and nest and loop through and extract the html text
  # and apply a cleansing token to validate formatting and remove dangerous chars
  # and split on returned spaces so we can create a list or string variable by first removing
  # spaces and NULL areas and then parsing backtogether and collapsing with a space
  clean_list <-
    sapply(names(raw_list),
           function(i)lapply(1:length(raw_list[[i]]),
                             function(x)
                               list(attrs = raw_list[[i]][[x]] %>%
                                      html_attrs %>% list_drop_null,
                                    text = raw_list[[i]][[x]] %>%
                                      html_text %>% txt.safe_text %>% splitSpaceToken %>% paste0(collapse=" ") %>%
                                      list_drop_null) %>%
                               list_drop_null)
           ) %>% list_drop_null

  #now pass back and create nested data frames for attributes and then the text

  #attr levels----------
  nest_df_attr <- sapply(names(clean_list),
                    function(i)list_drop_null(lapply(1:length(clean_list[[i]]),
                                                     function(x)list_drop_null(clean_list[[i]][[x]][['attrs']] %>% unlist))) %>%
                      unlist %>% as.data.frame(stringsAsFactors=F)
                    )

  #build a ref table to filter
  df_attr_key <- data.frame(li_name = names(nest_df_attr),
                            df_row = sapply(1:length(nest_df_attr),
                                            function(i)nrow(nest_df_attr[[i]]) %>% as.numeric),
                            stringsAsFactors=F) %>%
    mutate(key_ref = 1:length(df_row))

  pop_slot_attr <- df_attr_key %>% filter(df_row != 0) %>% select(key_ref) %>% unlist %>% as.numeric


  nest_df_attr <- nest_df_attr[pop_slot_attr]

  attr_out <-
    lapply(1:length(nest_df_attr),
           function(i)data.frame(attrs = nest_df_attr[[i]][[1]],
                                 ref = names(nest_df_attr[i]),
                                 stringsAsFactors=F)) %>% rbind.pages %>% filter(nchar(attrs)>1)


  #text levels------------
  nest_df_text <- sapply(names(clean_list),
                         function(i)list_drop_null(lapply(1:length(clean_list[[i]]),
                                                          function(x)list_drop_null(clean_list[[i]][[x]][['text']] %>% unlist))) %>%
                           unlist %>% as.data.frame(stringsAsFactors=F)
                         )
  text_out <-
    lapply(names(nest_df_text),
         function(i)data.frame(text = nest_df_text[[i]],
                               ref = i %>% removePunctuation,
                               stringsAsFactors=F)) %>%
    rbind.pages %>% filter(nchar(text) > 0)

  #compile into a list output
  out <- list(attrs = attr_out,text = text_out)
  return(out)


}


chain.web_list_to_df <- function(x){


  li_dat <- x

  if(is.null(names(li_dat))){
    key <- lapply(1:length(li_dat),
                  function(i)sapply(1:length(li_dat[[i]]),function(x)li_dat[[i]][[x]]%>%
                                      html_attrs%>%list_drop_null)%>%list_drop_null%>%unlist%>%as.data.frame(stringsAsFactors=F))
  }else{
    key <- lapply(names(li_dat),
                  function(i)sapply(1:length(li_dat[[i]]),function(x)li_dat[[i]][[x]]%>%
                                      html_attrs%>%list_drop_null)%>%list_drop_null%>%unlist%>%as.data.frame(stringsAsFactors=F))
  }

  return(key)

}
chain.drop_from_list <- function(x,rm_name = c('NA','NULL','all')){

  clean_list <- switch(rm_name,
                       'NA' = x[!vapply(x, is.na, FUN.VALUE=logical(1))],
                       'NULL' = x[!vapply(x, is.null, FUN.VALUE=logical(1))],
                       'all' = x[!vapply(x, is.na, FUN.VALUE=logical(1))] %>%
                         x[!vapply(x, is.null, FUN.VALUE=logical(1))]
                       )
  return(clean_list)

}

chain.grep_term <- function(grep_term = NULL,fallback = NULL,data_input = NULL){


  #token
  gr_token <- function(x){
    term_init <- as.character(x)
    list(orig = term_init,
         low = tolower(term_init),
         up = toupper(term_init),
         stem = wordStem(tolower(term_init))
         )
  }


  #check for number of terms
  if(length(grep_term) != 1){
    #apply token
    t_in <- sapply(grep_term,gr_token)
  }else{
    t_in <- gr_token(grep_term)
  }





  term_parts <- c(,tolower(grep_term),wordStem(grep_term),toupper(grep_term))


  if(is.data.frame(data_input) & length(data_input) > 1){
    #convert to individual arrays
    ref_col <- colnames(data_input)


  }






}
#' Return all date formatting in R
chain.all_date_vars <- function(x,use_today = NULL){

  ref_url <- html("https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html")

  ref_df <- ref_url %>%html_nodes('dd p , dt')%>%html_text%>%chain.df('raw') %>% chain.cut_to_df

  date_format_key <- ref_df %>% slice(c(1:34,36:41)) %>% mutate(key_char = c(odd[1:20],even[21:34],odd[36:41])) %>%
    mutate(info = c(even[1:20],odd[21:34],even[36:41]) %>% txt.safe_text) %>% select(key_char,info) %>% slice(-40)

  return(date_format_key)

}
#to clean the names that are created using the jsonlite package
#when it creates coloumn names with a period as a name sep
#in the event that flatten is the parsing tatctic used
#rather than the parsing style used in the rjson package
#example -> fromJSON(xxx) -> colnames -> 'data.picture.user' -> picture_user
chain.c_name_token <- function(x,do_what = NULL){

  df_tmp <- x

  orig_col <- colnames(x)

  new_names <- do.call(do_what,list(orig_col))
  final_names <- sapply(
    seq_along(new_names),function(i)
      ifelse(grepl("[[:punct:]]",stri_sub(new_names[[i]],1,1)),
             stri_sub(new_names[[i]],2,nchar(new_names[[i]])),
             new_names[[i]])
    )

  colnames(df_tmp) <- final_names

  return(df_tmp)

}

#will replace any colyumn name that contains puncutation with alternative
#mostly for use with jsonlite issues


chain.rm_col_punc <- function(x,drop_dup_names = NULL){
  df_raw <- x
  old_names <- colnames(x)
  new_names <- str_replace(old_names,'[[:punct:]]',"_")
  colnames(df_raw) <- new_names
  return(df_raw)
}

#

chain.null_list_df <- function(df_in){
  df_key <-data.frame(lengths = mapply(ncol,df_in),stringsAsFactors=F)
  df_key$n_row <- 1:nrow(df_key)

}


chain.col_name_cut <- function(x){
  raw_data <- x
  freq_name <- stri_split_regex(colnames(raw_data),'[.]')%>%
    melt(stringsAsFactors=F)%>%
    as.data.frame(stringsAsFactors=F)%>%
    count(value)%>%
    arrange(desc(n))%>%
    data.frame(stringsAsFactors=F)%>%
    mutate(col_name = as.character(value))%>%
    select(col_name)%>%slice(1)%>%as.character

  n_in <- nchar(freq_name)+2
  n_out <- max(nchar(colnames(raw_data)))

  new_cols <- stri_sub(colnames(raw_data),n_in,n_out)

  colnames(raw_data) <- new_cols

  return(raw_data)




}


chain.swap_class <- function(x,new_class){

  if(is.null(ncol(x))){

    x <- switch(new_class,
                'numeric' = as.numeric(x),
                'character' = as.character(x),
                'integer' = as.integer(x),
                'factor' = as.factor(x),
                'date' = as.Date(x),
                'time' = time.all_splits(x,filter_by = c('Hour','Minute','Second')
                )
    )
  }else{
    x <- adply(x,1,function(i)
      switch(new_class,
             'numeric' = as.numeric(x),
             'character' = as.character(x),
             'integer' = as.integer(x),
             'factor' = as.factor(x),
             'date' = as.Date(x),
             'time' = time.all_splits(x,filter_by = c('Hour','Minute','Second')))
    )
  }
  return(x)
}


chain.col_classer <- function(x){

  ldply(Map(class,x))%>%rename(key_name = .id,class = V1)


}

chain.add_li_name<-function(x,names_in){
  li <- as.list(x)
  names(x) <- rep(names_in,length(li))
  li
}


#working version
chain.to_numeric <- function(x){
  tmp_df <- x

  int_key <-
    mapply(class,tmp_df) %>%
    chain.df(with_row=T) %>%
    filter(df_name == 'integer') %>%
    select(row_names) %>%
    unlist %>% as.character

  tmp_df[,c(int_key)] <- sapply(int_key,function(i)tmp_df[[i]] %>% as.numeric)

  chr_key <-
    mapply(class,tmp_df) %>%
    chain.df(with_row=T) %>%
    filter(df_name == 'char') %>%
    select(row_names) %>%
    unlist %>% as.character

  tmp_df[,c(chr_key)] <- sapply(chr_key,function(i)tmp_df[[i]] %>% as.numeric)

  return(tmp_df)
}

chain.fact_to_char <- function(x){
  n_th <- ncol(x)


}

chain.class_it_up <- function(x){

  dat_in <- x
  orig_names <-
    ldply(colnames(dat_in),
          function(i)
            data.frame(col = i,class = class(dat_in[[i]]))) %>%
    filter(class=='integer') %>% select(col) %>%unlist %>% as.character

  new_df <- sapply(ldply(colnames(dat_in),
                         function(i)
                           data.frame(col = i,class = class(dat_in[[i]]))) %>%
                     filter(class=='integer') %>% select(col) %>% unlist,
                   function(ii)as.numeric(dat_in[[ii]])) %>% as.data.frame(stringsAsFactors=F)

  colnames(new_df) <- orig_names

  dat_in <- dat_in[,-match(orig_names,colnames(dat_in))]

  dat_out <- cbind(dat_in,new_df)

  return(dat_out)
}
#chain function to convert integer to numeric
chain.int_to_num <- function(x){

  df_in <- x
  converted <- sapply(
    colnames(df_in),
    function(i)
      ifelse(class(df_in[[i]])=='integer',
             as.numeric(df_in[[i]]),
                               df_in[[i]])) %>%
    as.list %>% as.data.frame(stringsAsFactors=F)

  return(converted)

}



#apply a token to a column---------
chain.col_token <- function(x,token_with = NULL) {

  data <- as.list(x)

  call_fun <- util.func_in_file(file_name = 'text',cloud_base='t')

  call_this <- call_fun$functions[grep(token_with,call_fun$functions)]

  x_out <- do.call(call_fun,lapply(data,as.data.frame(stringsAsFactors=F)))
  return(x_out)
}





#twitter chains--------------

#chain_user
#this will remove the info not needed in a chain and rename accordingly
#
# data_prep is to determine is we are chaining in a raw call or now...NULL would mean we already have a parsed df




chain.twitter_user <- function(x,data_prep = NULL,pick = NULL){

  if(!is.null(data_prep)){
  new_frame <-
    nullConvert(lapply(x$entities$url$urls,function(i)i[['expanded_url']]),'none') %>%
    unlist %>% data.frame(stringsAsFactors=F) %>% chain.rename_col(col_name='website') %>%
    cbind(x %>% select(-entities,-status))

  new_frame$name <- cleanTwit(new_frame$name)
  new_frame$description <- cleanTwit(new_frame$description)
  new_frame$screen_name <- cleanTwit(new_frame$screen_name)
  new_frame$created_at <- twTimeCt(new_frame$created_at)
  find_canada <- as.numeric(grep('fav',colnames(new_frame)))
  names(new_frame)[[find_canada]] <- 'favorites_count'
  }else{
    new_frame <- x
  }

  if("favourites_count" %in% colnames(new_frame)){
    new_frame$favorites_count <- new_frame$favourites_count
  }
  convert_nums <- adply(new_frame %>% select(contains('followers'),
                                             friends_count,
                                             favorites_count,
                                             statuses_count),1,as.numeric)

  new_frame$followers_count <- convert_nums$V1
  new_frame$friends_count <- convert_nums$V2
  new_frame$favorites_count <- convert_nums$V3
  new_frame$statuses_count <- convert_nums$V4

  if(!is.null(pick)){

    send_out = switch(
      pick,
      'user_box' = new_frame %>%
        select(website,
               id,
               name,
               profile_image_url,
               location,
               description
        )%>%rename(pro_img  = profile_image_url),
      'stats' = new_frame %>%
        select(website,
               id,
               name,
               followers_count,
               friends_count,
               listed_count,
               favorites_count,
               statuses_count) %>%
        arrange(desc(followers_count))
      )


    return(send_out)
  }else{
    return(new_frame)
  }
}

chain.insta_user <- function(x){

  ig_raw <- x

  ig_df <- data.frame(ig_raw$data,ig_raw$data$counts,stringsAsFactors=F)%>%select(-contains('counts'))

  colnames(ig_df) <- c('name','about','website','pro_img','full_name','id','posts','followers','follows')

  return(ig_df)

}

chain.ga_user <- function(x){

  if(is.list(x) & 'ga_user' %in% names(x)){
    ga_raw <- x$ga_user
  }else{
    ga_raw <- x
  }
  key_df <- data.frame(g_name = colnames(ga_raw),stringsAsFactors=F)%>%mutate(nth = 1:length(g_name))
  key_df$corrected <- 1
  key_df[6,'corrected'] <- 'name'
  key_df[9,'corrected'] <- 'link'
  key_df[10,'corrected'] <- 'pro_img'
  key_df[13,'corrected'] <- 'circled_by'
  key_df[16,'corrected'] <- 'cover'

  ga_df <- ldply(key_df %>% filter(corrected!=1) %>% select(g_name),function(i)ga_raw[i])

  ga_df <- ga_df %>% select(-.id)

  name_key <- key_df %>% filter(corrected!=1) %>% select(corrected)

  colnames(ga_df) <- name_key$corrected

  ga_df$pro_img <- gsub("sz=50","sz=150",ga_df$pro_img)

  return(ga_df)






}


#apply a function in line to the names of a list object
chain.list_names <- function(x,do_what = NULL,from_rds = FALSE){


  #this should return a flat list of items read from rds files
  #some that may have sub lists of data framse and others that are strictly char/df class already
  #so read in the list as needed
  if(from_rds){
  list_in <- sapply(list.files(pattern='rds')[c(1:3,6:11)],function(i)readRDS(i))
  }
  sub_df <- lapply(1:length(list_in),
                   function(i)
                     mapply(length,list_in[[i]]) %>% as.list %>% melt %>%
                     as.data.frame(stringsAsFactors=F) %>%
                     rename(lengths = value,name = L1) %>%
                     mutate(file_in = names(list_in[i]),
                            classes = class(list_in[[i]]))) %>% rbind.pages %>%
    select(file_in,classes,name,lengths) %>% arrange(desc(lengths))

  if(!is.list(x)){
    NULL
  }else{
    list_in <- x
    l_names <- names(list_in)
    new_names <- do.call(do_what,list(l_names))
    names(list_in) <- new_names
  }


  return(list_in)


}



chain.list_builder <- function(x){

  li_data <- x

  df_key <-
    lapply(1:length(li_data),function(i)
      mapply(length,li_data[[i]]) %>%
        as.list %>% melt %>%
        as.data.frame(stringsAsFactors=F) %>%
      mutate(lengths = value,name = L1) %>%
      select(-value,-L1) %>%
      mutate(file_in = names(li_data[i]),
             classes = class(li_data[[i]]))) %>%
    rbind.pages %>%
    select(file_in,classes,name,lengths) %>%
    arrange(desc(lengths)) %>%
    mutate(slot_down = paste0(file_in,"$",name))

  out <- list(
    key = df_key,
    names = list(
      parents = names(li_data),
      parent_child = lapply(names(li_data),function(i)df_key[df_key$file_in == i,'name']),
      parent_child_child = lapply(names(li_data),function(i)
        lapply(df_key[df_key$file_in == i,'name'],function(x)
          list(name = as.list(names(li_data[[i]][[x]]))))),
      children = df_key %>% select(slot_down) %>% unlist %>% as.character)
    )
  out
}

chain.unique_child <- function(x){
  unlist(x) %>% as.character %>% unique %>% as.list
}




chain.vis_build <- function(x,y_cord=NULL,x_cord=NULL,group_in=NULL,type_in=NULL){

  dat_in <- x
  y_in <- (y_cord)
  x_in <- (x_cord)
  group_in <- (group_in)
  chrt_type <- (type_in)

  plot <- nPlot(y = y_in,x = x_in,group = group_in,data=dat_in,type=chrt_type)

  return(plot)

}

chain.build_web_links <- function(x){
  link_refs <-
    txt.split_in_bind_out('about us about-us blog news press press-release info information clients our-work latest-news products learn-more')
  if(length(x)>1){
    src_list <- lapply(1:length(x),function(j)check_raw_enc(i))
    names(src_list) <-  gsub("(www.|.com)",'',parse_url(x)$hostname)
    }else{
      src_list <- x
    }
  if(is.null(check_raw_enc)){
    object_name <- gsub("(www.|.com)",'',parse_url(url_raw)$hostname)
  }
  check_list <- getHTMLLinks(url_raw)
  html %>% html_nodes('li a')%>%html_attr('href') %>% unique

}

chain.fb_user <- function(srch_for = NULL,data_raw = NULL){

  if(!is.null(data_raw)){
    df_prepped <- ifelse(is.list(data_raw),data_raw[[names(data_raw)!='paging']],data_raw)
  }else{

  }

  df_in[names(df_in)!='page']%>%class

  if(is.list(df_in)){
    names(df_in)
  }
}

chain.ig_build <- function(qry_in=NULL){

  instaAll('andrew smith')[[1]]%>%fromJSON%>%chain.insta_user%>%mutate(abt = URLdecode(about))%>%select(-about)

}


#to get even or odd indexes from data frame objects
chain.even_odd <- function(x,slice_by = 'even',position = 'row'){

  df_in <- x

  if(position == 'row'){
    data_cut <- switch(slice_by,
                     'even' = df_in[!c(T,F),],
                     'odd' = df_in[c(T,F),])
  }else{
    data_cut <- switch(slice_by,
                       'even' = df_in[,!c(T,F)],
                       'odd' = df_in[,c(T,F)])
  }
  return(data_cut)

}

#slice a data frame in one column into two by even and odd index


chain.cut_to_df <- function(x,c_name_left = NULL,c_name_right = NULL){

  df.e <- chain.df(x,'even')%>%chain.even_odd('even')

  df.o <- chain.df(x,'odd')%>%chain.even_odd('odd')

  df <- data.frame(even = df.e,odd = df.o,stringsAsFactors=F)

  if(!is.null(c_name_left)){
    colnames(df)[[1]] <- c_name_left
  }

  if(!is.null(c_name_right)){
    colnames(df)[[2]] <- c_name_right
  }
  df
}
