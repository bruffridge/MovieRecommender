## server.R

# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

# define functions
get_user_ratings <- function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = paste("m",MovieID, sep=""), Rating = as.numeric(Rating))]
  #dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

# use colClasses = 'NULL' to skip columns
tmp = fread('data/genre-movies.csv')


shinyServer(function(input, output, session) {

  ### SYSTEM 1 ###

  # Calculate recommendations when the sbumbutton is clicked
  df1 <- eventReactive(input$btn1, {
    withBusyIndicatorServer("btn1", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('#shiny-tab-system1 [data-widget=collapse]').click();"
        runjs(jsCode)

        moviesInGenre = subset(tmp, get(input$genre) == 1)

        moviesInGenre = moviesInGenre[order(moviesInGenre$MRS2, decreasing = TRUE), ]
        moviesInGenre = moviesInGenre[1:10,]
        user_results = (1:10)/10

        recom_results = data.table(MovieID = moviesInGenre$MovieID)
        
    }) # still busy
    
  }) # clicked on button

  # display the recommendations
  output$results1 <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df1()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        mID = recom_result$MovieID[(i - 1) * num_movies + j]
        mov = subset(movies, MovieID == mID)
        rnk = paste0("Rank ", (i - 1) * num_movies + j)
        iURL = mov$image_url
        tit = mov$Title
        box(width = 2, status = "success", solidHeader = TRUE, title = rnk,
            
          div(style = "text-align:center", 
              a(img(src = iURL, height = 150))
             ),
          div(style="text-align:center; font-size: 100%", 
              strong(tit)
             )
          
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function

  ### SYSTEM 2 ###
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })

  # Calculate recommendations when the submit button is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('#shiny-tab-system2 [data-widget=collapse]').click();"
        runjs(jsCode)
        
        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list) #m x 2 $MovieID and $Rating [1:5]
        
        if(length(user_ratings$Rating) <= 1) {
          #random sample if zero or 1 movie rated.
          samp = movies$MovieID
          if(length(user_ratings$Rating) == 1) {
            # if one movie is rated remove it from the potential sample pool.
            movID = as.integer(substr(user_ratings$MovieID, 2, nchar(user_ratings$MovieID)))
            samp = samp[! samp == movID]
          }
          
          user_predicted_ids = sample(samp, 10)
        } else {
          # handle the case where all the ratings are the same.
          if (length(unique(user_ratings$Rating)) == 1) {
            # add 1 to the first rating if the ratings are all 1 or 2.
            if(user_ratings$Rating[1] %in% 1:2) {
              user_ratings$Rating[1] = user_ratings$Rating[1] + 1
            } else { # minus 1 from the first rating if the ratings are all 3,4, or 5.
              user_ratings$Rating[1] = user_ratings$Rating[1] - 1
            }
          }
        
          # prepare the training data as a realRatingMatrix, say Rmat.
          myurl = "https://liangfgithub.github.io/MovieData/"
          ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                             sep = ':',
                             colClasses = c('integer', 'NULL'), 
                             header = FALSE)
          colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
          
          i = paste0('u', ratings$UserID)
          j = paste0('m', ratings$MovieID)
          x = ratings$Rating
          tmp = data.frame(i, j, x, stringsAsFactors = T)
          Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
          rownames(Rmat) = levels(tmp$i)
          colnames(Rmat) = levels(tmp$j)
          Rmat = new('realRatingMatrix', data = Rmat)
          Rmat = Rmat[1:500, ] # let's use a subset
          
          #Fit a UBCF model
          rec_UBCF = Recommender(Rmat, method = 'UBCF',
                                 parameter = list(normalize = 'Z-score', 
                                                  method = 'Cosine', 
                                                  nn = 25))
          
          # Prepare a new user
          movieIDs = colnames(Rmat)
          n.item = ncol(Rmat)  
          new.ratings = rep(NA, n.item)
          #new.ratings[user_ratings$MovieID] = user_ratings$Rating
          for(i in 1:length(user_ratings$MovieID)) {
            new.ratings[which(movieIDs == user_ratings$MovieID[i])] = user_ratings$Rating[i]
          }
          #new.ratings[which(movieIDs %in% user_ratings$MovieID)] = user_ratings$Rating
          
          new.user = matrix(new.ratings, 
                            nrow=1, ncol=n.item,
                            dimnames = list(
                              user=paste('user'),
                              item=movieIDs
                            ))
          new.Rmat = as(new.user, 'realRatingMatrix')
          
          recom1 = predict(rec_UBCF, new.Rmat, type = 'topN')
          #recom1@items
                  
          user_predicted_ids = recom1@items[[1]] #1:10
          user_predicted_ids = movieIDs[user_predicted_ids]
          user_predicted_ids = sapply(user_predicted_ids, function(x) {
            as.integer(substr(x, 2, nchar(x)))
          })
        }
        user_results = (1:10)/10
        recom_results <- data.table(Rank = 1:10, 
                                    MovieID = user_predicted_ids,
                                    Predicted_rating =  user_results)
        
    }) # still busy
    
  }) # clicked on button
  

  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
          div(style = "text-align:center", 
              a(img(src = movies$image_url[which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j])], height = 150))
             ),
          div(style="text-align:center; font-size: 100%", 
              strong(movies$Title[which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j])])
             )
          
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function
