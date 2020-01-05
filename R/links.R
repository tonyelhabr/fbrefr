
.retrieve_team_links <- function() {
  url_base <- 'https://fbref.com'
  html_raw <- url_base %>% xml2::read_html()
  nodes_script <- html_raw %>% rvest::html_nodes('script')
  idx_allowed <- nodes_script %>% rvest::html_attr('class') %>% str_which('allowed')
  # The `eval()` part doesn't seem to work when there is more than one node, so take the last node.
  script_raw <- nodes_script[idx_allowed[-1]])
  text_raw <- script_raw %>% rvest::html_text()
  engine <- V8::v8()
  invisible(engine$eval(text_raw))
  teams_json_raw <- engine$get('sr_goto_json["team_json"]')
  teams_json <- teams_json_raw %>% purrr::map(names)
  
  teams_links_df <-
    teams_json %>% 
    tibble::enframe('.idx_league', 'link') %>% 
    tidyr::unnest(link) %>% 
    dplyr::filter(link != '') %>% 
    dplyr::mutate(
      team = link %>% str_replace_all('(^.*\\/)(.*$)', '\\2')
    )
  
  nodes_body <- html_raw %>% rvest::html_node('body')
  nodes_filt <- nodes_body %>% rvest::html_nodes(xpath = '//*[@name="league_val"]')
  text_leagues <- nodes_filt %>% rvest::html_children() %>% rvest::html_text()
  idx_leagues <- nodes_filt %>% rvest::html_children() %>% rvest::html_attr('value')
  
  leagues_df <-
    tibble::tibble(
      .idx_league = idx_leagues,
      league = text_leagues
    ) %>% 
    dplyr::filter(.idx_league != '')

  teams_leagues_links_df <-
    leagues_df %>% 
    dplyr::full_join(teams_links_df, by = '.idx_league') %>% 
    dplyr::select(.idx_league, league, team, link) %>% 
    dplyr::mutate_at(dplyr::vars(.idx_league), as.integer) %>% 
    dplyr::mutate_at(dplyr::vars(link), ~sprintf('%s%s', url_base, .))
  teams_leagues_links_df
}