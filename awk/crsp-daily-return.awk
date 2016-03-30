{
    line = $1 "," $2
    if ( $3 == "N" )
	line = line ",";
    else 
	line = line "," $3;

    if ( $4 == "R" )
	line = line ","; 
    else 
        line = line "," $4;

    print line
}
