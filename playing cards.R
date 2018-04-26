write.csv(deck, file="cards.csv", row.names=FALSE)
setup <- function(deck){
  DECK<-deck
  DEAL<-function(){
    card<-deck[1, ]
    assign("deck",deck[-1, ], envir=globalenv())
    card
  }
  SHUFFLE<-function(){
    random<-sample(1:52, size=52)
    assign("deck",DECK[random, ], envir=globalenv())
  }
  list(deal=DEAL, shuffle=SHUFFLE)
}
cards<-setup(deck)
deal<-cards$deal
shuffle<-deal$shuffle
