module ExifIO where

-- This submodule contains all the direct Input / Output functions for the Exif library so the later one contains only the important code, letting the garbage of putStrLn's and getLines to the first one.

import Control.Monad

menu x = do
	putStrLn ""
	putStrLn "+============ Haskell Exif Library ======================+"
	putStrLn "|                                                        |"
	if x == 0  then putStrLn "|    Choose an option:                                   |" else putStrLn "|    Invalid option. Please choose a correct one:        |"
	putStrLn "|        1 -> Parse only Musics                          |"
	putStrLn "|        2 -> Parse only Videos                          |"
	putStrLn "|        3 -> Parse Both                                 |"
	putStrLn "|                                                        |"
	putStrLn "+========================================================+"
	putStr "Option: "
	c <- getLine
	if (c == "1") || (c == "2") || (c == "3") then return ((read c) :: Int) else menu 1

	
do_ask_sorting x = do
	putStrLn ""
	putStrLn "+=============== Sorting Options ===============+"
	putStrLn "|                                               |"
	putStrLn "|    Choose an option:                          |"
	putStrLn "|        1 -> Sort by filename                  |"
	putStrLn "|        2 -> Sort by filesize                  |"
	if x == 1 then putStrLn "|        3 -> Sort by artist                    |" else putStr ""
	if x == 1 then putStrLn "|        4 -> Sort by title                     |" else putStr ""
	if x == 1 then putStrLn "|        5 -> Sort by year                      |" else putStr ""
	putStrLn "|        6 -> Sort by parsing (pure output)     |"
	putStrLn "|                                               |"
	putStrLn "+===============================================+"
	putStr "Option: "
	c <- getLine
	if (c == "1") then return 1 else if (c == "2") then return 2 else if (c == "3") then return 3 else if (c == "4") then return 4 else if (c == "5") then return 5 else return 0
	
do_ask_select = do
	putStrLn ""
	putStrLn "+============= Query Select Options ============+"
	putStrLn "|                                               |"
	putStrLn "| Please insert (in the fields below) the query |"
	putStrLn "| method you would like to use to the media you |"
	putStrLn "| are selecting. You can select items that have |"
	putStrLn "| a size less '<' or greater '>' than x.xx MB.  |"
	putStrLn "|                                               |"
	putStrLn "| ie.: To select items greater than 10 Megabits |"
	putStrLn "|      enter the following:                     |"
	putStrLn "|            Signal: >                          |"
	putStrLn "|            Size: 10 MB                        |"
	putStrLn "|                                               |"
	putStrLn "| If you don't want to make any selection, just |"
	putStrLn "| insert '0' in the 'Signal: ' field.           |"
	putStrLn "|                                               |"
	putStrLn "+===============================================+"
	putStr "Signal: "
	s <- getLine
	if s == "0" then putStrLn "Assuming no selection method! Press Enter to continue" else putStrLn ""
	if s /= ">" && s /= "<" && s /= "0" then putStrLn "Invalid argument. Assuming no selection method! Press Enter to continue." else putStrLn ""
	when (s == "<" || s == ">") $ putStr "Size: "
	si <- getLine
	return (s, si)
	
do_ask_path = do
	putStrLn ""
	putStrLn "+============ Media path selection =============+"
	putStrLn "|                                               |"
	putStrLn "| Please insert the pathname where you want to  |"
	putStrLn "| search for media files. If you want to search |"
	putStrLn "| in the current directory where this file is   |"
	putStrLn "| present, just press Enter.                    |"
	putStrLn "|                                               |"
	putStrLn "+===============================================+"
	putStr "Path: "
	c <- getLine
	if (c == "") then return "." else return c
	
do_ask_pdf_output s t = if s == 3 && t /= "All" then do {
	putStrLn "";
	putStrLn "+============ Media path selection =============+";
	putStrLn "|                                               |";
	putStrLn "| You ordered the software to sort music on     |";
	putStrLn "| artist. Do you wish you pdf to be outputed    |";
	putStrLn "| normally (1), or do you prefer separated      |";
	putStrLn "| lists of music by each artist, with size info |";
	putStrLn "| and title only? (2)                           |";
	putStrLn "|                                               |";
	putStrLn "+===============================================+";
	putStr "Option: ";
	c <- getLine;
	if (c == "2") then return 2 else return 1 } else return 1
