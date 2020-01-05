
# I've pseudo-built this so that it can stand-alone in its own package, but not completely.

.clean_fbref_tm_page_table <- function(table, total_row = FALSE, meta_header = FALSE) {
  stopifnot(is.data.frame(table))
  df_raw <- table %>% janitor::clean_names() %>% as_tibble()
  row_1 <- df_raw %>% slice(1)
  row_1_nms <- row_1 %>% names()
  row_1_vals <- row_1 %>% unlist() %>% unname()
  nms_df <-
    tibble(
      prefix = row_1_vals,
      suffix = row_1_nms
    )
  
  nms_new <-
    nms_df %>%
    mutate_at(
      vars(suffix),
      ~case_when(
        . == 'x' ~ NA_character_,
        str_detect(., '^x_') ~ NA_character_,
        TRUE ~ str_remove(., '_[0-9]+$')
        # str_detect(., '^per_') ~ str_replace_all(., 'per_', 'p') %>% str_remove('_minutes.*'),
        # TRUE ~ NA_character_
      )
    )
  
  if(meta_header) {
    nms_new <-
      nms_new %>% 
      mutate(name = if_else(!is.na(suffix), sprintf('%s_%s', prefix, suffix), prefix))
  } else {
    nms_new <-
      nms_new %>% 
      mutate(name = prefix)
  }
  
  nms_new <-
    nms_new %>% 
    mutate_at(vars(name), janitor::make_clean_names) %>% 
    pull(name)
  
  col_1 <- nms_new[1]
  col_1_sym <- rlang::sym(col_1)
  
  df <-
    df_raw %>% 
    slice(c(2:n())) %>% 
    set_names(nms_new)
  
  if(!total_row) {
    df <-
      df %>% 
      filter(str_detect(!!col_1_sym, 'Total$', negate = TRUE))
  }
  
  file_temp <- tempfile()
  opt_1 <- getOption('readr.num_columns')
  opt_2 <- getOption('readr.show_progress')
  options(readr.num_columns = 0)
  options(readr.show_progress = FALSE)
  on.exit(options(readr.num_columns = opt_1))
  on.exit(options(readr.show_progress = opt_2))
  write_csv(df, file_temp)
  res <- read_csv(file_temp)
  res
}

# TODO: Coerce season like `2019L` or '2019' to '2019-20' and use in `url`.
.validate_fbref_season <- function(x) {
  stopifnot(length(x) == 1L)
  if (is.character(x)) {
    if(str_detect(x, '-')) {
      rgx <- '(^.*)-(.*$)'
      x1 <- str_replace(x, rgx, '\\1')
      x2 <- str_replace(x, rgx, '\\2')
    } else {
      v <- safely(~as.numeric(x))
    }
  } else if(!is.numeric(x)) {
    stop('`x` is not a valid season.', call. = FALSE)
  }
  stopifnot(x >= 1992L)
  stopifnot(x <= as.numeric(format(Sys.Date(), '%Y')))
  # TODO: ...
  invisible(x)
}

# TODO: Take `team` as an input instead of link.
scrape_fbref_tm_page <- function(link, season = 2019L, idx = NULL, unnest = ifelse(!is.null(idx), TRUE, FALSE), ...) {
  # link <-  '/en/squads/18bb7c10/Arsenal'
  # idx <- 1
  # browser()
  season <- .validate_fbref_season(season)
  if(!is.null(idx)) {
    stopifnot(is.numeric(idx))
  }
  # TODO: Add season to this url
  url <- sprintf('https://fbref.com%s', link)
  # See https://github.com/abresler/nbastatR/blob/master/R/bref.R and https://github.com/rtelmore/ballr/blob/master/R for insight.
  page <- url %>% xml2::read_html()
  tables <- page %>% rvest::html_table()
  tables_df <- tibble(.idx = seq_along(tables), table = tables)
  if(!is.null(idx)) {
    stopifnot(idx <= length(tables))
    tables_df <- tables_df %>% filter(.idx %in% idx)
  }
  # browser()
  res_nested <-
    tables_df %>%
    mutate(
      res = map(table, .clean_fbref_tm_page_table, ...)
    ) %>% 
    select(-table)
  
  if(!unnest) {
    return(res_nested)
  }
  
  res <-
    res_nested %>% 
    select(res) %>% 
    unnest(res)
  res
}