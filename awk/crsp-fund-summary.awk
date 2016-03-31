{
    line = ""
    for(i=1;i<=12;++i)
	line = line $i ",";

    if ( $13 == "N" )
	line = line ",";
    else 
	line = line $13 ",";

    for(i=14;i<=72;++i)
	line = line $i ",";

    line = line $73

    print line
}
