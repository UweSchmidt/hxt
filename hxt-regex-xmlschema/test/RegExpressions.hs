{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
-- module W3W.RegExpressions
where

import		Data.Char					( toLower )
import		Data.List					( isPrefixOf )
import		Data.Maybe
import		Text.Regex.XMLSchema.String hiding ( tokenize )
import      Data.Char.Properties.XMLCharProps
import		Holumbus.Crawler
import		Text.XML.HXT.Core

-- -------------------------------------------
data MarkableRE	= REFunc (Bool -> String)
				| REString String

reMarked :: String -> MarkableRE -> MarkableRE
reMarked marker (REString s) = REFunc $ \ b -> if b then "({" ++ marker ++ "}" ++ s ++ ")" else s

reSimple :: String -> MarkableRE
reSimple s = REString s

eval :: MarkableRE -> Bool -> String
eval (REString s) _ = s
eval (REFunc f) b = f b

----------------------------------------------
opt :: MarkableRE -> MarkableRE
opt (REFunc f) = REFunc $ \ b -> "(" ++ (f b) ++ ")?"
opt (REString s) = REString $ "(" ++ s ++ ")?"

atLeastOne :: MarkableRE -> MarkableRE -> MarkableRE
atLeastOne (REFunc f1) (REFunc f2) = REFunc $ \ b -> "((" ++ (f1 b) ++ (f2 b) ++ ")|(" ++ (f1 b) ++ ")|(" ++ (f2 b) ++ "))"
atLeastOne (REString f1) (REString f2) = REString $ "((" ++ f1 ++ f2 ++ ")|(" ++ f1 ++ ")|(" ++ f2 ++ "))" -- !

exactlyOne :: MarkableRE -> MarkableRE -> MarkableRE
exactlyOne (REFunc f1) (REFunc f2) = REFunc $ \ b -> "((" ++ (f1 b) ++ ")|(" ++ (f2 b) ++ "))"
exactlyOne (REString f1) (REString f2) = REString $ "((" ++ f1 ++ ")|(" ++ f2 ++ "))"

atMostOne :: MarkableRE -> MarkableRE -> MarkableRE
atMostOne (REFunc f1) (REFunc f2) = REFunc $ \ b -> "((" ++ (f1 b) ++ ")|(" ++ (f2 b) ++ "))?"
atMostOne (REString f1) (REString f2) = REString $ "((" ++ f1 ++ ")|(" ++ f2 ++ "))?"

plus :: MarkableRE -> MarkableRE -> MarkableRE
plus (REFunc f1) 	(REFunc f2) 	= REFunc $ \ b -> (f1 b) ++ (f2 b)
plus (REFunc f1) 	(REString s2) 	= REFunc $ \ b -> (f1 b) ++ s2
plus (REString s1) 	(REFunc f2) 	= REFunc $ \ b -> s1 ++ (f2 b)
plus (REString s1) 	(REString s2) 	= REString $ s1 ++ s2

-- -------------------------------------------
dayOfMonth :: MarkableRE
dayOfMonth = 
	reMarked "dayOfMonth" ((reSimple "[1-3][0-9]") `exactlyOne` (reSimple "0?[1-9]"))

normDayOfMonth :: String -> String
normDayOfMonth (x:[]) = '0':x:[]
normDayOfMonth s = s

-- -------------------------------------------
dayOfWeek :: MarkableRE
dayOfWeek = reMarked "dayOfWeek" $
				(reSimple "[Mm]ontag|[Dd]ienstag|[Mm]ittwoch|[Dd]onnerstag|[Ff]reitag|[Ss]amstag|[Ss]onnabend|[Ss]onntag")
				`exactlyOne`
				(reSimple "Mo[.]|Di[.]|Mi[.]|Do[.]|Fr[.]|Sa[.]|So[.]")


normDayOfWeek :: String -> String
normDayOfWeek s 
	| (s == "Montag")		|| (s == "montag") 		|| (s == "Mo.") = "1"
	| (s == "Dienstag") 	|| (s == "dienstag") 	|| (s == "Di.") = "2"
	| (s == "Mittwoch") 	|| (s == "mittwoch") 	|| (s == "Mi.") = "3"
	| (s == "Donnerstag") 	|| (s == "donnerstag") 	|| (s == "Do.") = "4"
	| (s == "Freitag") 		|| (s == "freitag") 	|| (s == "Fr.") = "5"
	| (s == "Samstag") 		|| (s == "samstag") 	|| (s == "Sonnabend") || (s == "sonnabend") || (s == "Sa.") = "6"
	| (s == "Sonntag") 		|| (s == "sonntag") 	|| (s == "So.") = "7"
	| otherwise = "Failing at: " ++ s

-- -------------------------------------------
monthNumeric :: MarkableRE
monthNumeric = reMarked "monthNumeric" $ 
				(reSimple "1[0-2]") `exactlyOne` (reSimple "0?[1-9]")

normMonthNumeric :: String -> String
normMonthNumeric (x:[]) = '0':[x]
normMonthNumeric s = s

-- -------------------------------------------
monthWritten :: MarkableRE
monthWritten = reMarked "monthWritten" $
				(reSimple "[Jj]anuar|[Ff]ebruar|[Mm]ärz|[Aa]pril|[Mm]ai|[Jj]uni|[Jj]uli|[Aa]ugust|[Ss]eptember|[Oo]ktober|[Nn]ovember|[Dd]ezember")
				`exactlyOne`
				(reSimple "[Jj]an[.]|[Ff]eb[.]|[Mm]är[.]|[Aa]pr[.]|[Jj]un[.]|[Jj]ul[.]|[Aa]ug[.]|[Ss]ep[.]|[Ss]ept[.]|[Oo]kt[.]|[Nn]ov[.]|[Dd]ez[.]")

normMonthWritten :: String -> String
normMonthWritten s
	| (s == "Januar")		|| (s == "januar") 		|| (s == "Jan.") || (s == "jan.") = "01"
	| (s == "Februar")		|| (s == "februar")		|| (s == "Feb.") || (s == "feb.") = "02"
	| (s == "März")			|| (s == "märz")		|| (s == "Mär.") || (s == "mär.") = "03"
	| (s == "April")		|| (s == "april")		|| (s == "Apr.") || (s == "apr.") = "04"
	| (s == "Mai")			|| (s == "mai")							 				  = "05"
	| (s == "Juni")			|| (s == "juni")		|| (s == "Jun.") || (s == "jun.") = "06"
	| (s == "Juli")			|| (s == "juli")		|| (s == "Jul.") || (s == "jul.") = "07"
	| (s == "August")		|| (s == "august")		|| (s == "Aug.") || (s == "aug.") = "08"
	| (s == "September")	|| (s == "September")	|| (s == "Sep.") || (s == "sep.") || (s == "Sept.") || (s == "sept.") = "09"
	| (s == "Oktober")		|| (s == "oktober")		|| (s == "Okt.") || (s == "okt.") = "10"
	| (s == "November")		|| (s == "november")	|| (s == "Nov.") || (s == "nov.") = "11"
	| (s == "Dezember")		|| (s == "dezember")	|| (s == "Dez.") || (s == "dez.") = "12"
	| otherwise = "Failing at: " ++ s
-- -------------------------------------------
year :: MarkableRE
year = reMarked "year" $ 
			reSimple "(20)?[0-9][0-9]"

normYear :: String -> String
normYear (a:b:[]) = "20" ++ (a:b:[])
normYear s = s

-- -------------------------------------------
singleYear :: MarkableRE
singleYear = 	(
					(reSimple "('|[Ww][Ss]|[Ss][Ss]|[Ww]intersemester|[Ss]ommersemester|[Ss][Oo][Ss][Ee])[ ]?[ ]?[ ]?")
					`plus`
				   	(reMarked "singleYear" $ reSimple "(20)?[0-9][0-9](/(20)?[0-9][0-9])?")
				) 
				`exactlyOne`
				(
					(reMarked "singleYear" $ reSimple "20[0-9][0-9]")
				)

normSingleYear :: String -> [String]
normSingleYear (a:b:[]) = ["****20" ++ (a:b:[])]
normSingleYear (a:b:c:d:[]) = ["****" ++ (a:b:c:d:[])]
normSingleYear (a:b:'/':c:d:[]) = ["20"++(a:b:[]), "20"++(c:d:[])]
normSingleYear (a:b:'/':c:d:e:f:[]) = ["20"++(a:b:[]), (c:d:e:f:[])]
normSingleYear (a:b:c:d:'/':e:f:[]) = [(a:b:c:d:[]), "20"++(e:f:[])]
normSingleYear (a:b:c:d:'/':e:f:g:h:[]) = [(a:b:c:d:[]), (e:f:g:h:[])]
normSingleYear s = ["normSingleYear failing on :" ++ s]

-- -------------------------------------------
timeHM :: MarkableRE
timeHM = reMarked "timeHM" $ 
			reSimple "(([0-1]?[0-9])|(2[0-4]))[:]((0?[0-9])|([1-5][0-9]))"

normTimeHM :: String -> String
normTimeHM t = (normHM hours) ++ ":" ++ (normHM minutes)
				where
					matches = matchSubex "({h}(([0-1]?[0-9])|(2[0-4])))[:.]({m}(0?[0-9])|([1-5][0-9]))" t
					hours = snd $ matches!!0
					minutes = snd $ matches!!1
					normHM (a:[]) = '0':[a]
					normHM l = l
					
-- -------------------------------------------
timeH :: MarkableRE
timeH =	(reMarked "timeH" $ reSimple "(([0-1]?[0-9])|(2[0-4]))")
		`plus`
		(reSimple "[ ]?[Uu]hr")

normTimeH :: String -> String
normTimeH t = (normHM t) ++ ":00"
				where
					normHM (a:[]) = '0':[a]
					normHM l = l
					
-- -------------------------------------------

time :: MarkableRE
time = timeHM `exactlyOne` timeH

-- -------------------------------------------
noNumber :: Int -> MarkableRE
noNumber 0 = reSimple ""
noNumber n = (reSimple "[^0-9]?") `plus` (noNumber (n-1))

noAlphaNum :: Int -> MarkableRE
noAlphaNum 0 = reSimple ""
noAlphaNum n = (reSimple "[^0-9a-zA-Z]?") `plus` (noAlphaNum (n-1))

dateDelimiterGerm :: MarkableRE
dateDelimiterGerm = reSimple "[.]"

dateDelimiterUSA1 :: MarkableRE
dateDelimiterUSA1 = reSimple "[/]"

dateDelimiterUSA2 :: MarkableRE
dateDelimiterUSA2 = reSimple "[-]"


-- ------------------------------------

date1 :: MarkableRE
date1 = opt (dayOfWeek `plus` (reSimple "[^.0-9]*"))
		`plus`
		singleYear

date2 :: MarkableRE
date2 = dayOfWeek
		`plus`
		(opt (reSimple "[^.0-9]*" `plus` singleYear))
-- ! tokenize_
date3 :: MarkableRE
date3 = ( 
			(	
				(opt (dayOfWeek `plus` reSimple "[^.0-9]*")) -- !
				`plus`
				(
					(
--						opt 
						(dayOfMonth `plus` dateDelimiterGerm `plus` noNumber 1) -- z.B. 7._Juni...
						`plus` 
						(monthWritten `plus` (opt $ reSimple "[^.0-9]*"))  -- Monat: nicht optional!
					)
					`exactlyOne`
					(
						(dayOfMonth `plus` dateDelimiterGerm) `atLeastOne` (monthNumeric `plus` dateDelimiterGerm)
					)
				)
			) 
			`plus` year 
		)

date4 :: MarkableRE
date4 = year `plus`   -- Jahr: nicht optional!
		(
			(
				(
					(dateDelimiterUSA2 `exactlyOne` dateDelimiterUSA1) `plus` monthNumeric
				) 
				`atLeastOne` 
				(
					(dateDelimiterUSA2 `exactlyOne` dateDelimiterUSA1) `plus` dayOfMonth
				)
			)
		)


-- ------------------------------------
perm :: [a] -> [[a]]
perm [] = [[]]
perm (x:xs) = [ps ++ [x] ++ qs | rs <- perm xs, (ps, qs) <- splits rs]

splits :: [a] -> [([a],[a])]
splits [] = [([], [])]
splits (y:ys) = ([],y:ys):[(y:ps,qs) | (ps,qs) <- splits ys]

-- ------------------------------------
findInTupleList :: [(String, String)] -> String -> String -> String
findInTupleList [] _ elseStr = elseStr
findInTupleList (x:xs) s elseStr
	| fst(x) == s = snd(x)
	| otherwise   =	findInTupleList xs s elseStr

normList :: [[(String, String)]] -> [[(String, String)]]
normList l = map normListEntry l

transform :: [(String, String)] -> String
transform l =  "year:" ++ (findInTupleList l "year" "****")
			++ " singleYear:" ++ (findInTupleList l "singleYear" "********")
			++ " monthNumeric:" ++ (findInTupleList l "monthNumeric" "**")
			++ " monthWritten: " ++ findInTupleList l "monthWritten" "**"
			++ " dayOfMonth: " ++ (findInTupleList l "dayOfMonth" "**")
			++ " dayOfWeek: " ++ (findInTupleList l "dayOfWeek" "*")
			++ " timeHM: " ++ (findInTupleList l "timeHM" "**:**")
			++ " timeH: " ++ (findInTupleList l "timeH" "**:**")

normListEntry :: [(String, String)] -> [(String, String)]
normListEntry l = map normListElem l

normListElem :: (String, String) -> (String, String)
normListElem (key, value)
	| key == "dayOfMonth"	= (key, normDayOfMonth value)
	| key == "dayOfWeek"	= (key, normDayOfWeek value)
	| key == "monthNumeric"	= (key, normMonthNumeric value)
	| key == "monthWritten"	= (key, normMonthWritten value)
	| key == "year"			= (key, normYear value)
	| key == "singleYear"	= (key, concat (normSingleYear value))
	| key == "timeHM"		= (key, normTimeHM value)
	| key == "timeH"		= (key, normTimeH value)
	| otherwise = ("fail on key " ++ key , "with value " ++ value)

-- ------------------------------------
theDateExpression :: MarkableRE
theDateExpression = ((date4 `exactlyOne` date3 `exactlyOne` date2 `exactlyOne` date1) `plus` opt (reSimple "[^.0-9]*" `plus` time)) 
					`exactlyOne` 
					(opt (time `plus` reSimple "[^.0-9]*") `plus` (date4 `exactlyOne` date3 `exactlyOne` date2 `exactlyOne` date1))

test :: String
test = "Am Sonntag, dem 17. Februar '03 findet um 9 Uhr ein wichtiger Termin statt. "
	++ "Dieser wird allerdings auf Montag verschoben. Und zwar auf den ersten Montag im Wintersemester 11/12, 12:30. "
	++ "Ein wichtiger Termin findet im SoSe 2011 statt. Im Jahr '12 gibt es Termine, aber auch in WS 2010/11. "
	++ "Ein weiterer Termin ist  am 2.4.11 um 12 Uhr. Oder war es doch Di. der 3.4.? Egal. "
	++ "Ein weiterer wichtiger Termin findet am 2001-3-4 statt bzw. generell zwischen 01/3/4 - 01/6/4 um 13 Uhr. "
	++ "Am kommenden Mittwoch findet Changemanagement in HS5 statt. Dies gilt dann auch für den 7. Juni "
	++ "des Jahres 2011. Noch ein wichtiger Termin findet um 16:15 Uhr am Do., 1.2.03 statt. "

main :: IO ()
main = 	mapM_ putStrLn $ 
		map transform $
		normList $
		map (matchSubex $ eval markableRE True) $
		tokenize (eval markableRE False) test
		where markableRE =  theDateExpression

{-			reSimple "[^0-9]" 
			`plus`
			(
				reMarked "day" (reSimple "[0-9]+")
				`plus`
				reSimple "[.]"
					`atLeastOne`
				reMarked "month" (reSimple "[0-9]+")
				`plus`
				reSimple "[.]"
			)
			`plus`
			reMarked "year" (reSimple "[0-9]+")
			`plus`
			reSimple "[^0-9]"
-}
-----------



