#' Pattern matching
#'
#' \code{txt.best_match}
#'
#'
#'
#' @param term_in string or vector The term to be found in an index list or
#' vector.
#' @param terms_idx vector The vector of strings to match against.
#'
#'
#' @return
#' A string or character vector of best matched results
#'
#'
#' @examples
#' > txt.best_match('Dino',row.names(mtcars))
#'
#'
#'
txt.best_match <- function(term_in, terms_idx){

  lapply(term_in,function(x)
    data.frame(var = unique(terms_idx),
               nth = stringsim(x, unique(terms_idx)) * 100 %>% as.numeric) %>%
      arrange(desc(nth)) %>% mutate(tile = dense_rank(nth)) %>%
      arrange(desc(tile)) %>% slice(1) %>% `$`('var')) %>%
    unlist

}
