module Main where

import Control.Monad
import ExifIO
import Data.List
import System.Cmd
import Text.Printf
import Char
import Text.Regex
import Data.Maybe
import TeX

-- The "new" Media datatype. It can be Audio or Video.
data Media = Music { name :: String, size :: Int, ftype :: String, copyright :: String, title :: String, artist :: String, year :: Int }
	| Video { name :: String, size :: Int, ftype :: String, copyright :: String, width :: String, height :: String }

-- This Eq instance is not accurate on purpose, instancing Eq this way helps to correctly generate the alternative output pdf. Comparing media files is not needed in any part of the code, except in the "difm" function, where it needs to be compared this way in order to work correctly.
instance Eq Media where
	(Music fn s ft c t a y) == (Music fn2 s2 ft2 c2 t2 a2 y2) = (a == a2) -- Only comparing artist here...
	(Video fn s ft c w h) == (Video fn2 s2 ft2 c2 w2 h2) = (fn == fn2) -- If filename is the same, the file is the same!
-- 
{--
Show instance declaration for Media, for pretty-presenting on the terminal! It presents Audio and Video in nice table view.
ie.
	+------------+-------------------------------------------+
	|  Filename  |  8bp038-03-vim-when_a_child_is_bored.mp3  |
	+------------+-------------------------------------------+
	|  Size      |  1.79 MB                                  |
	+------------+-------------------------------------------+
	|  Ftype     |  MP3                                      |
	+------------+-------------------------------------------+
	|  Copyright |  False                                    |
	+------------+-------------------------------------------+
	|  Title     |  when a child is bored                    |
	+------------+-------------------------------------------+
	|  Artist    |  vim                                      |
	+------------+-------------------------------------------+
	|  Year      |  2003                                     |
	+------------+-------------------------------------------+
--}
instance Show Media where
	show (Music fn s ft c t a y) = let score = (putt (larger + 17) 0) ; larger = length (largest [fn, (tomegabit s), ft, c, t, a, (show y)])
					in 
					"\n" ++
					"+" ++ score ++ "+" ++ "\n" ++ "|  Filename  |  " ++ fn ++ puts (larger - length fn) ++ "  |" ++ "\n" ++ 
					"+" ++ score ++ "+" ++ "\n" ++ "|  Size      |  " ++ tomegabit s ++ puts (larger - length (tomegabit s)) ++ "  |" ++ "\n" ++ 
					"+" ++ score ++ "+" ++ "\n" ++ "|  Ftype     |  " ++ ft ++ puts (larger - length ft) ++ "  |" ++ "\n" ++ 
					"+" ++ score ++ "+" ++ "\n" ++ "|  Copyright |  " ++ c ++ puts (larger - length c) ++ "  |" ++ "\n" ++ 
					"+" ++ score ++ "+" ++ "\n" ++ "|  Title     |  " ++ t ++ puts (larger - length t) ++ "  |" ++ "\n" ++ 
					"+" ++ score ++ "+" ++ "\n" ++ "|  Artist    |  " ++ a ++ puts (larger - length a) ++ "  |" ++ "\n" ++ 
					"+" ++ score ++ "+" ++ "\n" ++ "|  Year      |  " ++ showy y ++ puts (larger - length (show y)) ++ "  |" ++ "\n" ++ 
					"+" ++ score ++ "+"
					++ "\n"
	
	show (Video n s ft c w h) = let score = (putt (larger + 17) 0) ; larger = length (largest [n, show s, ft, c, w, h])
					in 
					"\n" ++
					"+" ++ score ++ "+" ++ "\n" ++ "|  Filename  |  " ++ n ++ puts (larger - length n) ++ "  |" ++ "\n" ++ 
					"+" ++ score ++ "+" ++ "\n" ++ "|  Size      |  " ++ tomegabit s ++ puts (larger - length (tomegabit s)) ++ "  |" ++ "\n" ++ 
					"+" ++ score ++ "+" ++ "\n" ++ "|  Ftype     |  " ++ ft ++ puts (larger - length ft) ++ "  |" ++ "\n" ++ 
					"+" ++ score ++ "+" ++ "\n" ++ "|  Copyright |  " ++ c ++ puts (larger - length c) ++ "  |" ++ "\n" ++ 
					"+" ++ score ++ "+" ++ "\n" ++ "|  Width     |  " ++ w ++ puts (larger - length w) ++ "  |" ++ "\n" ++ 
					"+" ++ score ++ "+" ++ "\n" ++ "|  Height    |  " ++ h ++ puts (larger - length h) ++ "  |" ++ "\n" ++ 
					"+" ++ score ++ "+"
					++ "\n"else
					
-- The "new" Media Collection datatype. It supports collections of multiple filetypes, namely audio and video at the same time! 
data MC = MC { 
	--total :: Int ,
	mc :: [Media] } deriving Show


-- This and the below functions aim to simplify some of the select options by combining them with sortOn to a nice and easy way to Sort and Select media at the same time.	
-- Rules for this function:
	-- x == 1 means to sort the media by name;
	-- x == 2 means to sort the media by size;
	-- Anything else means no sorting ;)
sorting_options x media | x == 1 = (sortOn name media)
			| x == 2 = (sortOn size media)
			| x == 3 && and (map is_music media) = (sortOn artist media)
			| x == 4 && and (map is_music media) = (sortOn title media)
			| x == 5 && and (map is_music media) = (sortOn year media)
			| otherwise = (media)

-- This fand the above functions to simplify some of the select options by combining them with sortOn to a nice and easy way to Sort and Select media at the same time.
-- Rules for this function:
	-- x -> 1 = <; 2 = >; anything = Nothing
	-- y means the size in megabytes string (ie. "xx.xx MB") (not necessary if x argument equals anything but 1 or 2)
	-- f is the "sorting_options" function ;)
select_options x y f | x == "<" = (select ((<(tobytes y)).size) f)
		     | x == ">" = (select ((>(tobytes y)).size) f)
		     | otherwise = f


				
showit t = do
	p <- do_ask_path
	when (t == "Musics" || t == "All") $ do_exif_music_recursive p
	when (t == "Videos" || t == "All") $ do_exif_video_recursive p
	outputm <- if (t == "Musics" || t == "All") then readFile "music.tmp" else readFile "dummy"
	outputv <- if (t == "Videos" || t == "All") then readFile "video.tmp" else readFile "dummy"
	if outputv == "" && outputm == "" then error "There is no media files available for parsing! Exiting... " else putStr ""
	s <- if (t == "Musics" || t == "All") then do_ask_sorting 1 else do_ask_sorting 2
	s2 <- do_ask_select
	o <- do_ask_pdf_output s t
 	let parsem = map (splitRegex (mkRegex "\t")) (lines outputm); parsev = map (splitRegex (mkRegex "\t")) (lines outputv); mediam = map seqtomusic (parsem); mediav = map seqtovideo (parsev); fm = select_options (fst s2) (snd s2) (sorting_options s mediam) ; fv = select_options (fst s2) (snd s2) (sorting_options s mediav) ; fm' = (fm ++ fv) ; total = length fm'; mc = MC fm' ; dat = prettyprint (read (show o)) mc
	print fm'
	system "rm *.tmp"
	putStr "There is a total of "
	putStr (show total)
	putStrLn " media files in your query."
	putStrLn "Would you like to print this information to a .PDF? (y or n)"
	c <- getLine
	if c == "y" then do do_exif_covers p ; writeFile "out.tex" dat; system "pdflatex --interaction=batchmode out"; system "rm *.aux *.tex *.log"; putStrLn "\nDone!. \nA pdf was generated with the media information!\n Thanks for using the Exif library!"
		else main
		
-- Recursively Exiftool's all the .mp3 files present on the folder and subfolders of the given path and outputs to a temporary file (out.tmp) for post-processing.
-- WARNING: Experimental function. Needs the "find" shell command present in most Linux distributions and MacOS as well as the "sed, and works for any kind of .mp3 file and extracts only the needed metadata. Files with bad behaved metadata don't crash the script.
do_exif_music_recursive p =
	do
		putStrLn "" ;
		putStrLn "+-----------------------------------------------------------+" ;
		putStrLn "|      Extracting information for all .mp3 files            |" ;
		putStrLn "| Please wait... this can take a while for big libraries... |" ;
		putStrLn "+-----------------------------------------------------------+" ;
		let cmd = "find " ++ p ++ " -name *.mp3 -exec exiftool -f -T -FileName -FileSize# -FileType -Copyright -Title -Artist -Year {} ';' > music.tmp"
		system cmd ;
		system "sed -i 's/#/ /g' music.tmp"
		--system "sed -i 's/\\/ /g' music.tmp"
		return ();
		
do_exif_video_recursive p =
	do
		putStrLn "" ;
		putStrLn "+-----------------------------------------------------------+" ;
		putStrLn "|    Extracting information for all .mp4 and .ogv files     |" ;
		putStrLn "| Please wait... this can take a while for big libraries... |" ;
		putStrLn "+-----------------------------------------------------------+" ;
		let cmd = "find " ++ p ++ " \\( -name *.mp4 -o -name *.ogv \\) -exec exiftool -f -T -FileName -FileSize# -FileType -Copyright -ImageWidth -ImageHeight {} ';' > video.tmp"
		system cmd ;
		system "sed -i 's/#/ /g' video.tmp"
		--system "sed -i 's/\\//g' video.tmp"
		return ();
		
		
-- Helper Function for converting bulk information to the Music type.
--seqtomusic s = Music (s !! 0) (read(s !! 1)) (s !! 2) (s !! 3) (s !! 4) (s !! 5) (forceyear(s !! 6))
seqtomusic (fn:s:ft:c:t:a:y) = Music fn (read s) ft c t a (forceyear (unlines y))

-- Helper Function for converting bulk information to the Video type.
seqtovideo s = Video (s !! 0) (read(s !! 1)) (s !! 2) (s !! 3) (s !! 4) (s !! 5)


-- Converts an Integer with bytes information to a string with the converted megabits information and the "MB" prefix.
tomegabit :: Int -> String
tomegabit x = (printf "%.2f" y) ++ " MB" where y = (((fromIntegral x) / 1024) / 1024) :: Float

-- Converts a String with the type "xx.xx MB" to an integer with the repective number of bytes

tobytes :: String -> Int
tobytes x = round (y * 1024) * 1024
	where y = read(head(splitRegex(mkRegex " ") x)) :: Float
	
-- ==== Micro-LaTeX formatting auxiliary functions below ==== --

-- This function composes the three necessary functions to succesfully output a pdf by using the TeX library. The "o" argument is what decides the output type. Lists by artist (2) or normal output (1).
prettyprint :: Int -> MC -> [Char]
prettyprint o = texTeX2txt . (latex "article") . (if o == 1 then mcMc2TeX else onlytitles2TeX)
     
-- Media Collection Converting Function
mcMc2TeX :: MC -> TeX
mcMc2TeX (MC fs) =
	string ("\\begin{figure}[h] \\begin{center} \\includegraphics{Covers/coverfinale.jpg} \\end{center} \\end{figure} \\newpage") $
	quote $
	description (map mcMedia2TeX fs)
 
     
-- Media Converting Function
mcMedia2TeX :: Media -> TeX
-- ... for music
mcMedia2TeX (Music fn s ft c t a y) =
     descitem (STR "Music: ") $
     string fn $
     quote $
     tabular "|l|l|" $
       header $
       wrap nlhline [
          strTabRow ["File Name", fn]
        , strTabRow ["Size", tomegabit s]
        , strTabRow ["File Type", ft]
        , strTabRow ["Copyright", c]
        , strTabRow ["Title", show t] -- The show function is what puts the "" in the title. Remove it if you want!
        , strTabRow ["Artist", a]
        , strTabRow ["Year", showy y]
     ]
     where header = ([hline,strTabRow ["\\bf Attribute", "\\bf Data"]]++)

-- ... For video
mcMedia2TeX (Video fn s ft c w h) =
     descitem (STR "Video: ") $
     string fn $ 
     quote $
     tabular "|l|l|" $
       header $
       wrap nlhline [
          strTabRow ["File Name", fn]
        , strTabRow ["Size", tomegabit s]
        , strTabRow ["File Type", ft]
        , strTabRow ["Copyright", c]
        , strTabRow ["Width", w] 
        , strTabRow ["Height", h]
     ]
     where header = ([hline,strTabRow ["\\bf Attribute", "\\bf Data"]]++);
     
--
-- ALTERNATIVE OUTPUT (LISTS BY EACH ARTIST PARSED) WITH ONLY TITLE AND FILESIZE INFO.
--

-- Media Collection Converting function (alternative output)
onlytitles2TeX :: MC -> TeX
onlytitles2TeX (MC fs) =
	string ("\\begin{figure}[h] \\begin{center} \\includegraphics{Covers/coverfinale.jpg} \\end{center} \\end{figure} \\newpage") $
	--description ([mcTitle2TeX fs])
	description (map mcTitle2TeX (difm fs))


-- Media converting function (alternative output) (for musics only)
mcTitle2TeX ((Music fn s ft c t a y):xs) = let k = if a == "" then "unknown artists" else a in
	quote $
	string ("Musics by " ++ k ++ ": \\n \\n") $
	quote $
	tabular "|l|l|" $ header $ wrap nlhline (map torows ((Music fn s ft c t a y):xs))
	where header = ([hline, strTabRow ["\\bf Title", "\\bf Size"]]++);

-- === CONTENT SELECTION AND SORTING FUNCTIONS ===

-- provides the decorate-sort-undecorate idiom, aka the "Schwartzian transform"
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f  = map snd . sortOn' fst .  map (\x -> (f x, x))

-- This variant of 'sortOn' recomputes the function to sort on every comparison.  This is better for functions that are cheap to compute, including projections. Used as axuiliary function to the above "sortOn"

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f = sortBy (\x y -> compare (f x) (f y))

-- The select function is just filter that is already included in the prelude! 

select = filter

-- Difm outputs a list of lists of Music. Each sublist contains the all the music of a given artist. Uses difm' below to help calculate and group the music that have the same artist
difm :: [Media] -> [[Media]]
difm x = ((group . concat) (difm' x))

difm' :: [Media] -> [[Media]]
difm' ((x@(Music fn s ft c t a y1)):(y@(Music fn2 s2 ft2 c2 t2 a2 y2)):[]) = if a == a2 then ((x:[y]):[]) else ([x]:[y]:[])
difm' ((x@(Music fn s ft c t a y1)):(y@(Music fn2 s2 ft2 c2 t2 a2 y2)):xs) = if a == a2 then ((x:[y]):(difm (xs))) else ([x]:difm (y:xs))
	
-- ======= SHOW INSTANCING AUXILIARY FUNCTIONS ======== 

-- Inserts the number of "-" given in the first argument. The second argument is an acumulator for inserting the "+" when the string has exactly 12 chars length.
putt :: Int -> Int -> String
putt 0 _ = ""
putt x y = if y == 12 then "+" ++ (putt (x-1) (y+1)) else "-" ++ (putt (x-1) (y+1))

-- Inserts the number of spaces given in the first argument.
puts :: Int -> String
puts 0 = ""
puts x = if x > 0 then " " ++ (puts (x-1)) else puts 0

-- Calculates the string with the largest length and returns it
largest :: [String] -> String
largest [x] = x
largest (x:y:xs) = if length x < length y then largest (y:xs) else largest (x:xs)

-- ==== OTHER AUXILIARY FUNCTIONS ======

is_music :: Media -> Bool
is_music (Music fn s ft c t a y) = True
is_music (Video fn s ft c w h) = False

-- Regex function that returns True if the first string is contained in the second and False otherwise
is_on_string :: String -> String -> Bool
is_on_string a b = isJust (matchRegex (mkRegex a) b)

-- This function forces a correct parsing by introducing the 0 number when the year field is not a valid number before being parsed, avoiding parsing errors. Only for parsing pruposes, output goes correctly if using "showy" function.
forceyear :: String -> Int
forceyear x = if and (map isDigit x) == False && x /= "" then 0 else read x :: Int

-- This function replaces the year field with "-" when the it is equal to 0. Used for output.
showy :: Int -> String
showy x = if x == 0 then "-" else show x

-- Forces an empty tag to be "-"
forcetag :: String -> String
forcetag x = if x == "" then "-" else x

-- This function adds pelicans to the beggining and the end of the string in order to be used on the shell
shellspaces :: String -> String
shellspaces x = "'" ++ x ++ "'"

-- This functions composes various strings into a unique string separated by spaces
composeargs :: [String] -> String
composeargs [] = ""
composeargs (x:xs) = x ++ " " ++ (composeargs xs)

-- This function (used with map), dinamically creates rows in a table, depending on the number of music files passed as argument
torows :: Media -> TeX
torows (Music _ s _ _ t a _) = strTabRow [t, tomegabit s]

-- This function recieves a path to a .mp3 file and outputs a string that can be given to the system function so it can extract the cover art of that music to a file with the same name + .cover.jpg
exif_path_cover :: String -> String
exif_path_cover y = "exiftool -b -Picture " ++ y ++ " > " ++ y ++ ".cover.jpg"
-- NOTICE: If the given file does not have a cover, this line will create an empty file, that must be deleted later because it is just junk.

-- This function recursively extracts all the cover art for every music present in the folder and sub-folders of a given pathname. 
do_exif_covers p = 
    do
    	let cmd = "find " ++ p ++ " -name *.mp3 > filepaths.tmp"
        system cmd
        r <- readFile "filepaths.tmp"
        let x = map shellspaces (lines r) ; y = map exif_path_cover x;
        --For debugging pruposes: writeFile "covers-new.tmp" (unlines x)
        putStrLn ""
        putStrLn "Extracting cover art of every .mp3 file... please wait a little bit! ;)"
        putStrLn ""
        mapM_ system y
        let delete_junk = "find " ++ p ++ " -name '*.cover.jpg' -empty -delete"
        let cleanup = "find " ++ p ++ " -name '*.cover.jpg' -exec mv {} ./Covers/ ';'"
        system delete_junk -- Finds and deletes all junk files created in the process
        system cleanup -- Finds and moves all the generated cover art to a temporary folder
        system "fdupes -d -N Covers/" -- Needs the 'fdupes'. sudo apt-get install fdupes.
        --system "mkdir Covers/uniques; find ./Covers -type f -name \"*.png\" -exec md5sum {} + | awk '{if(!a[$1]++){$1=\"\";printf(\"%s\\0\",$0)}}' | while read -r -d '' file; do mv \"$file\" Covers/uniques/; done" -- Without dependencies ;)
        let cmdc = "find Covers/ -name '*.cover.jpg' > coverpaths.tmp"
        system cmdc
        r <- readFile "coverpaths.tmp"
        let cpaths = map shellspaces (lines r) ; tilesjpg = "montage " ++ (composeargs cpaths) ++ "-shadow -geometry 150x150+4+4 Covers/coverfinale.jpg"
        system tilesjpg -- Generate cover image for pdf with a tile of all cover art
        system "rm Covers/*.cover.jpg" -- Deletes the covers so it wont appear in the next pdf generation
        return ()
        
-- The main menu. Depending on what the menu function returns, it calls the appropriate function. Subject to change.
main :: IO ()
main = do
		s <- menu 0
		case s of
			1 -> do showit "Musics"
			2 -> do showit "Videos"
			3 -> do showit "All"
			0 -> return ()
