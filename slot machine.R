get_symbols<-function(){
  wheel<-c("DD","7","BBB","BB","B","C","0")
  sample(wheel, size=3, replace=TRUE, prob=c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}
play<-function(){
  symbols<-get_symbols()
  structure(score(symbols),symbols=symbols,class="slots")}


score <- function(symbols) {
  #Different Cases
  same<-symbols[1]==symbols[2] && symbols[2]==symbols[3]
  bars<-symbols %in% c("B","BB","BBB")
  #Prize according to them
  if(same){
    payouts<-c("DD"=100, "7"=80,"BBB"=40,"BB"=25,"B"=10,"C"=10,"0"=0)
    prize<-unname(payouts[symbols[1]])
  }else if(all(bars)){
    prize<- 5
  }else{
    cherries<-sum(symbols=="C")
    prize<-c(0,2,5)[cherries+1]
  }
  #adjusting for diamonds
  diamonds<-sum(symbols=="DD")
  prize*2^diamonds
}

one_play<-play()
attributes(one_play)
attr(one_play,"symbols")<-c("B","0","B")

slot_display<-function(prize){
  
  symbols<-attr(prize, "symbols")
  
  symbols<-paste(symbols, collapse = "")
  
  string<-paste(symbols, prize, sep="\n$")
  
  cat(string)
}

class(one_play)<-"slots"
print.slots<-function(x,...){
  slot_display(x)
}
#---------------------------
prob=c("DD"=0.03, "7"=0.03, "BBB"=0.06, "BB"=0.1, "B"=0.25, "C"=0.01, "0"=0.52)
combos<-expand.grid(wheel,wheel,wheel,stringsAsFactors = FALSE)
combos$prob1<-prob[combos$Var1]
combos$prob2<-prob[combos$Var2]
combos$prob3<-prob[combos$Var3]
combos$prob<-combos$prob1*combos$prob2*combos$prob3
combos$prize<-NA
for(i in 1:nrow(combos)){
  symbols<-c(combos[i, 1], combos[i,2], combos[i,3])
  combos$prize[i]<-score(symbols)
}

change_symbols<-function(vec){
  x<-c("DD"="joker","C"="ace","7"="king","B"="queen","BB"="jack"
0}