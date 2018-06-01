# Script for mapping venue id to name, code and classification
# venue-name.R
# To use this venue and team id need to be matched



venue_name_df <-
  data.frame(rbind(
    c(25,"Suncorp Stadium","Brisbane",55016,"Brisbane","BRI",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(2,"GIO Stadium","Canberra",55004,"Canberra","CBR",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(13,"McDonald Jones Stadium","Newcastle",55007,"Newcastle","NEW",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(4,"1300SMILES Stadium","Townsville",55008,"North Queensland","NQL",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(23,"ANZ Stadium","Sydney",55005,"Canterbury","CBY",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(16,"Southern Cross Group Stadium","Sydney",55006,"Cronulla","CRO",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(83,"Cbus Super Stadium","Gold Coast",55044,"Gold Coast Titans","GLD",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(12,"Lottoland","Sydney",55009,"Manly","MAN",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(161,"AAMI Park","Melbourne",55015,"Melbourne","MEL",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(7,"Pirtek Stadium","Sydney",55010,"Parramatta","PAR",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(8,"Panthers Stadium","Sydney",55011,"Penrith","PEN",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(23,"ANZ Stadium","Sydney",55017,"South Sydney","SOU",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(24,"UOW Jubilee Oval","Sydney",55012,"St George Illawarra","STI",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(10,"WIN Stadium","Wollongong",55012,"St George Illawarra","STI",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(9,"Allianz Stadium","Sydney",55013,"Sydney Roosters","SYD",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(5,"Mt Smart Stadium","Auckland",55003,"Warriors","WAR",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(11,"Leichhardt Oval","Sydney",55014,"Wests Tigers","WST",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(3,"Campbelltown Sports Stadium","Sydney",55014,"Wests Tigers","WST",TRUE,FALSE,FALSE,FALSE,"Home"),
    c(45,"Belmore Sports Ground","Sydney",55005,"Canterbury","CBY",FALSE,TRUE,FALSE,FALSE,"Alternate_Home"),
    c(14,"Central Coast Stadium","Gosford",55005,"Canterbury","CBY",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(14,"Central Coast Stadium","Gosford",55006,"Cronulla","CRO",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(14,"Central Coast Stadium","Gosford",55009,"Manly","MAN",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(14,"Central Coast Stadium","Gosford",55017,"South Sydney","SOU",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(14,"Central Coast Stadium","Gosford",55013,"Sydney Roosters","SYD",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(25,"Suncorp Stadium","Brisbane",55005,"Canterbury","CBY",FALSE,FALSE,FALSE,TRUE,"Opp_Home"),
    c(25,"Suncorp Stadium","Brisbane",55009,"Manly","MAN",FALSE,FALSE,FALSE,TRUE,"Opp_Home"),
    c(25,"Suncorp Stadium","Brisbane",55015,"Melbourne","MEL",FALSE,FALSE,FALSE,TRUE,"Opp_Home"),
    c(23,"ANZ Stadium","Sydney",55010,"Parramatta","PAR",FALSE,TRUE,FALSE,FALSE,"Alternate_Home"),
    c(23,"ANZ Stadium","Sydney",55012,"St George Illawarra","STI",FALSE,TRUE,FALSE,FALSE,"Alternate_Home"),
    c(23,"ANZ Stadium","Sydney",55014,"Wests Tigers","WST",FALSE,TRUE,FALSE,FALSE,"Alternate_Home"),
    c(61,"nib Stadium","Perth",55005,"Canterbury","CBY",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(320,"Optus Stadium","Perth",55005,"Canterbury","CBY",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(205,"Patersons Stadium","Perth",55009,"Manly","MAN",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(61,"nib Stadium","Perth",55009,"Manly","MAN",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(61,"nib Stadium","Perth",55017,"South Sydney","SOU",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(320,"Optus Stadium","Perth",55017,"South Sydney","SOU",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(202,"TIO Stadium","Darwin",55044,"Gold Coast Titans","GLD",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(234,"Clive Berghofer Stadium","Toowoomba",55044,"Gold Coast Titans","GLD",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(242,"Marley Brown Oval","Gladstone",55044,"Gold Coast Titans","GLD",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(197,"Lavington Sports Ground","Albury",55009,"Manly","MAN",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(203,"Glen Willow Regional Sports Stadium","Mudgee",55010,"Parramatta","PAR",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(202,"TIO Stadium","Darwin",55010,"Parramatta","PAR",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(203,"Glen Willow Regional Sports Stadium","Mudgee",55012,"St George Illawarra","STI",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(202,"TIO Stadium","Darwin",55013,"Sydney Roosters","SYD",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(277,"Scully Park","Tamworth",55014,"Wests Tigers","WST",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(21,"Sydney Cricket Ground","Sydney",55017,"South Sydney","SOU",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(9,"Allianz Stadium","Sydney",55017,"South Sydney","SOU",FALSE,TRUE,FALSE,FALSE,"Alternate_Home"),
    c(21,"Sydney Cricket Ground","Sydney",55012,"St George Illawarra","STI",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(9,"Allianz Stadium","Sydney",55012,"St George Illawarra","STI",FALSE,TRUE,FALSE,FALSE,"Alternate_Home"),
    c(9,"Allianz Stadium","Sydney",55014,"Wests Tigers","WST",FALSE,TRUE,FALSE,FALSE,"Alternate_Home"),
    c(21,"Sydney Cricket Ground","Sydney",55014,"Wests Tigers","WST",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(17,"Carrington Park","Bathurst",55011,"Penrith","PEN",FALSE,TRUE,FALSE,FALSE,"Alternate_Home"),
    c(298,"AMI Stadium","Christchurch",55011,"Penrith","PEN",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(99,"Eden Park","Auckland",55003,"Warriors","WAR",FALSE,TRUE,FALSE,FALSE,"Alternate_Home"),
    c(22,"Westpac Stadium","Wellington",55003,"Warriors","WAR",FALSE,TRUE,FALSE,FALSE,"Alternate_Home"),
    c(288,"Yarrow Stadium","New Plymouth",55003,"Warriors","WAR",FALSE,TRUE,FALSE,FALSE,"Alternate_Home"),
    c(79,"Waikato Stadium","Hamilton",55003,"Warriors","WAR",FALSE,TRUE,FALSE,FALSE,"Alternate_Home"),
    c(55,"Adelaide Oval","Adelaide",55005,"Canterbury","CBY",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(204,"Stadium Mackay","Mackay",55005,"Canterbury","CBY",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(79,"Waikato Stadium","Hamilton",55005,"Canterbury","CBY",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(22,"Westpac Stadium","Wellington",55005,"Canterbury","CBY",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(217,"Forsyth Barr Stadium","Dunedin",55005,"Canterbury","CBY",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(196,"Owen Delany Park","Taupo",55006,"Cronulla","CRO",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(271,"McLean Park","Napier",55015,"Melbourne","MEL",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(172,"Barlow Park","Cairns",55017,"South Sydney","SOU",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(55,"Adelaide Oval","Adelaide",55013,"Sydney Roosters","SYD",FALSE,FALSE,TRUE,FALSE,"Neutral"),
    c(5,"Mt Smart Stadium","Auckland",55014,"Wests Tigers","WST",FALSE,FALSE,FALSE,TRUE,"Opp_Home")
))


names(venue_name_df) <- c("venue_id","venue_name","venue_city","for_team_id","for_team_name","for_team_code"
                          ,"Is_Home","Is_Alternate_Home","Is_Neutral","Is_Opp_Home","Venue_Classification")

										


venue_name_df

