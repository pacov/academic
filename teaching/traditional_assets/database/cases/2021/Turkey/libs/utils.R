
format.dt.f = function(
  df, 
  page_length = 10,
  perc_vars,
  ron_vars,
  ron_digits=2
){
  if( is.null(df) | purrr::is_empty(df) ){return()}
  
  double.two.int.f = function( df ){
    get_no_digits = function(x){
      if( ! is.numeric(x) ){return(NULL)}
      x = x %% 1
      x = as.character(x)
      no_digits = nchar(x) - 2
      no_digits = ifelse( no_digits == -1, 0, no_digits )
      return(no_digits)
    } 
    suppressWarnings({
      new_df = df %>%
        as_tibble() %>%
        mutate_if( function(x) max( get_no_digits(x), na.rm = T ) == 0, as.integer )
    })
    return(new_df)
  }
  df = double.two.int.f( df )
  max_length = nrow(df)
  page_length_menu = c(10,25,50,100, max_length, page_length) %>% unique()
  page_length_menu = page_length_menu[ !page_length_menu > max_length]
  
  dt = DT::datatable(
    df, 
    extensions = c('Buttons', 'ColReorder', 'KeyTable', 'FixedColumns'), 
    rownames = FALSE, 
    options = list(
      dom = 'Bflrtip', 
      buttons = I( c('colvis','copy', 'excel') ), 
      colReorder = TRUE, 
      keys = TRUE, 
      pageLength = page_length, 
      lengthMenu = page_length_menu,
      scrollX = TRUE,
      scrollCollapse = TRUE
    )
  )
  
  if (!is.na(ron_vars)[1]) dt=dt %>% DT::formatRound( ron_vars, ron_digits )
  if (!is.na(perc_vars)[1]) dt=dt %>% DT::formatPercentage( perc_vars, 2 )
  
  return(dt)
}
