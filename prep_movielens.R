library(data.table)
library(stringr)
get_ml_ratings = function(file_location){
  ratings = fread(file_location)
  
  setorder(ratings,userId,movieId,-timestamp)
  
  #in case a user has multiple ratings on different dates
  ratings = ratings[,.SD[1],.(userId,movieId)]
  
  ratings_wide = dcast(ratings, userId ~ movieId, value.var = 'rating')
  
  ratings_wide
}


get_movies = function(file_location){
  movies = fread(file_location)
  
  #Parse year out of title, remove year from title
  movies[,year := stringr::str_extract(title, '\\s(\\([0-9]+\\))$')]
  movies[,year := as.numeric(gsub('\\(|\\)','', year))]
  movies[,movie_title := sub('\\s(\\([0-9]+\\))$', '', title)]
  movies[, title := NULL]
  
  #Replace Genres str with one-hot-encode of genre
  genres_split = lapply(unique(movies$genres), function(genres_str){
    
    dt = data.table(genres = genres_str, genre = strsplit(x = genres_str, split = '\\|')[[1]])
    dcast(dt, genres ~genre, fun = length)
  })
  
  genre_dt = rbindlist(genres_split, fill = T)
  genre_dt[is.na(genre_dt)] = 0
  movies = merge(movies, genre_dt, by = 'genres')
  movies[,genres := NULL]
  
  movies
}




