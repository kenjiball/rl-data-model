# Script for mapping team id to name and code

team_name_df <-
data.frame(rbind(
      c(55003,"WAR","Warriors","New Zealand Warriors",""),
      c(55004,"CBR","Canberra","Canberra Raiders",""),
      c(55005,"CBY","Canterbury","Canterbury Bulldogs","Canterbury-Bankstown Bulldogs"),
      c(55006,"CRO","Cronulla","Cronulla Sharks","Cronulla-Sutherland Sharks"),
      c(55007,"NEW","Newcastle","Newcastle Knights",""),
      c(55008,"NQL","North Queensland","North Queensland Cowboys","North QLD Cowboys"),
      c(55009,"MAN","Manly","Manly Sea Eagles","Manly-Warringah Sea Eagles"),
      c(55010,"PAR","Parramatta","Parramatta Eels",""),
      c(55011,"PEN","Penrith","Penrith Panthers",""),
      c(55012,"STI","St George Illawarra","St. George Illawarra Dragons","St George Dragons"),
      c(55013,"SYD","Sydney Roosters","Sydney Roosters",""),
      c(55014,"WST","Wests Tigers","Wests Tigers",""),
      c(55015,"MEL","Melbourne","Melbourne Storm",""),
      c(55016,"BRI","Brisbane","Brisbane Broncos",""),
      c(55017,"SOU","South Sydney","South Sydney Rabbitohs",""),
      c(55044,"GLD","Gold Coast Titans","Gold Coast Titans","")
  
))

names(team_name_df) <- c("team_id", "team_code", "team_name", "team_name_long", "team_name_alternate")

team_name_df
