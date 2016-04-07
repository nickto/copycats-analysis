{
    line = ""
    for(i=1;i<=37;++i)
	line = line $i ",";

    # dlretx
    if ($38 == "S")
	line = line "-55" ",";
    else if ($38 == "T")
	line = line "-66" ",";
    else if ($38 == "A")
	line = line "-88" ",";
    else if ($38 == "P")
	line = line "-99" ",";
    else
	line = line $38 ",";

    for(i=39;i<=39;++i)
	line = line $i ",";

    # dlret
    if ($40 == "S")
	line = line "-55" ",";
    else if ($40 == "T")
	line = line "-66" ",";
    else if ($40 == "A")
	line = line "-88" ",";
    else if ($40 == "P")
	line = line "-99" ",";
    else
	line = line $40 ",";

    for(i=41;i<=48;++i)
	line = line $i ",";

    # ret
    if ($49 == "E")
	line = line "-44" ",";
    else if ($49 == "D")
	line = line "-55" ",";
    else if ($49 == "C")
	line = line "-66" ",";
    else if ($49 == "B")
	line = line "-77" ",";
    else if ($49 == "A")
	line = line "-88" ",";
    else if ($49 == ".")
	line = line "-99" ",";
    else
	line = line $49 ",";

    for(i=50;i<=56;++i)
	line = line $i ",";

    # retx
    if ($57 == "E")
	line = line "-44";
    else if ($57 == "D")
	line = line "-55";
    else if ($57 == "C")
	line = line "-66";
    else if ($57 == "B")
	line = line "-77";
    else if ($57 == "A")
	line = line "-88";
    else if ($57 == ".")
	line = line "-99";
    else
	line = line $57;





    print line
}
