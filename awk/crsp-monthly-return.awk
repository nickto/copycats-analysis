{
#    line = $1 "," $2
#    if ( $3 == "N" )
#	line = line ",";
#    else 
#	line = line "," $3;
    
    line = $1 "," $2

    if ( $3 == "T" )
	line = line ","; 
    else 
        line = line "," $3;

    if ( $4 == "R" )
	line = line ","; 
    else 
        line = line "," $4;
    
    if ( $5 == "N" )
	line = line ","; 
    else 
        line = line "," $5;

    print line
}
