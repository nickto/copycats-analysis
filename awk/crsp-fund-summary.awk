{
    line = ""
    for(i=1;i<=3;++i)
	line = line $i ",";

    # nav_latest
    if ( $4 == "N")
	line = line ",";
    else
	line = line $4 ",";

    for(i=5;i<=5;++i)
	line = line $i ",";

    # tna_latest
    if ( $6 == "T")
	line = line ",";
    else
	line = line $6 ","

    for (i=7;i<=12;++i)
	line = line $i ","
    
    # nav_52w_l
    if ( $13 == "N" )
	line = line ",";
    else 
	line = line $13 ",";

    for(i=14;i<=72;++i)
	line = line $i ",";

    line = line $73

    print line
}
