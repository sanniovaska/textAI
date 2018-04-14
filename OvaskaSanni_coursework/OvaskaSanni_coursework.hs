--Advanced Functional Programming coursework, spring 2017
--Sanni Ovaska, programmed in collaboration with Arttu Nevalainen and Joni Sarikoski

module OvaskaSanni_coursework where
    import LexGram
    import ParGram
    import AbsGram
    import Interpreter

    import System.IO     
    import System.Environment   
    import Data.List
    import System.Directory
    import Data.List.Split
  
    import ErrM
    
    --Read statements from first file and questions from second. Print answers to questions.
    main = do
        (file1:file2:_) <- getArgs
        file <- readFile file1
        let statements = lines file
        let state = reverse (map handle statements)

        file <- readFile file2
        let questions = lines file
        let answer = question questions state

        putStr (unlines answer)

    --Statement handler
    handle :: String -> String
    handle entry = do
        let Ok e = pEntry (myLexer entry)
        case e of
            EState s -> interpState s

    --Handle questions and return answers in a list
    question :: [String] -> [String] -> [String]
    question [q] state = [questionHandler q state]
    question (q1:questions) state = [questionHandler q1 state] ++ question questions state

    --Question handler
    questionHandler :: String -> [String] -> String
    questionHandler entry statements = do
        let Ok e = pEntry (myLexer entry)
        case e of
            EQuestion s -> interpQuestion s statements

    --Answer question according to its type
    interpQuestion :: Question -> [String] -> String
    interpQuestion x statements = case x of
        PerLoc person place -> answerPerLoc (interpPerson person) (interpPlace place) statements
        ObjLoc object -> answerObjLoc (interpObject object) statements statements
        HowMany person -> if personMentioned (interpPerson person) statements
            then show (answerHowMany (interpPerson person) 0 statements)
            else "don't know"
        Before person place -> answerBefore (interpPerson person) (interpPlace place) statements
        After person place -> answerAfter (interpPerson person) (interpPlace place) statements []
        Path place dest -> answerDirection (interpPlace place) (interpPlace dest) (bothWays [] statements)


    --Has given person been mentioned? Helper for interpQuestion in "HowMany" case
    personMentioned :: String -> [String] -> Bool
    personMentioned _ [] = False
    personMentioned person (row:s) = 
        --Person is always the second word in a statement (if it has a person), except for "Hand" statements, which have two people
        if (findFirst row == "Hand") && (((findI row 2) == person))
            then True
            else if  (findI row 1) == person
                then True
                else personMentioned person s

    --Shorthand for interperting statements
    findFirst :: String -> String
    findFirst row = head (splitOn " " row)
    findLast :: String -> String
    findLast row = last (splitOn " " row)
    findI :: String -> Int -> String
    findI row index = (splitOn " " row) !! index

    --Is given person in given place? Return: yes/no/maybe
    answerPerLoc :: String -> String -> [String] -> String
    answerPerLoc _ _ [] = "maybe"
    answerPerLoc person place (row:s) = 
        if (findFirst row == "Either") && ((findI row 1) == person) && ((findI row 2) == place || (findI row 3) == place)
            then "maybe"
            else if (findFirst row == "NotLoc") && ((findI row 1) == person)
                then if ((findI row 2) == place)
                    then "no"
                    else "maybe"
                else if (findFirst row == "Loc") && ((findI row 1) == person)
                    then if (findI row 2) == place
                        then "yes"
                        else "no"
                    else answerPerLoc person place s

    --Where is given object? Return: place's name/don't know
    answerObjLoc :: String -> [String] -> [String] -> String
    answerObjLoc _ [] _ = "don't know"
    answerObjLoc object (row:s) statements = 
        --Find the latest statement about given object
        if (findLast row == object)
            --Object was picked up
            then if findFirst row == "Take"
                then findPerson (findI row 1) statements
                --Object was dropped
                else if findFirst row == "Drop"
                    then findPerson (findI row 1) s
                    --Object was handed to the person
                    else if (findPerson (findI row 2) statements) == "don't know"
                        then findPerson (findI row 1) s
                        else findPerson (findI row 2) statements
            else answerObjLoc object s statements

    --Where is given person? Return: place's name/don't know
    findPerson :: String -> [String] -> String
    findPerson _ [] = "don't know"
    findPerson person (row:s) = 
        --"Either" and "not in" statements mean location can't be determined
        if (findFirst row == "Either") && ((findI row 1) == person)
            then "don't know"
            else if (findFirst row == "NotLoc") && ((findI row 1) == person)
                then "don't know"
                --Return location if found, otherwise keep going
                else if (findFirst row == "Loc") && ((findI row 1) == person)
                    then (findI row 2)
                    else findPerson person s

    --How many objects is given person holding? Return number of objects, >= 0
    answerHowMany :: String -> Int -> [String] -> Int
    --Return final count
    answerHowMany _ count [] = count
    --Count starts at 0
    answerHowMany person count (row:s) = 
        --Person picks up an object
        if (findFirst row == "Take") && ((findI row 1) == person)
            then answerHowMany person (count + 1) s
            --Person drops an object
            else if (findFirst row == "Drop") && ((findI row 1) == person)
                then answerHowMany person (count - 1) s
                --Person gives an object to another person
                else if (findFirst row == "Hand") && ((findI row 1) == person)
                    then answerHowMany person (count - 1) s
                    --Person gets handed an object
                    else if (findFirst row == "Hand") && ((findI row 2) == person)
                        then answerHowMany person (count + 1) s
                        else answerHowMany person count s

    --Where was given person before given place?
    answerBefore :: String -> String -> [String] -> String
    answerBefore _ _ [] = "don't know"
    answerBefore person place (row:s) =
        --If statement about given place found, find where person was before (from the remaining statements)
        if (findFirst row == "Loc") && ((findI row 1) == person) && ((findI row 2) == place)
            then findPerson person s
            else answerBefore person place s

    --Where was given person after given place?
    answerAfter :: String -> String -> [String] -> [String] -> String
    --Store handled statements in newS
    answerAfter _ _ [] _ = "don't know"
    answerAfter person place (row:s) newS =
        if (findFirst row == "Loc") && ((findI row 1) == person) && ((findI row 2) == place)
            then if newS == []
                then "don't know"
                --If statement about given place found, find where person was after (from newS)
                else findPerson person (reverse newS)
            else answerAfter person place s (newS ++ [row])

    --Create a list of "Dir" statements with both ways included. E.g. if have statement about "A is south of B", add statement about "B is north of A"
    bothWays :: [String] -> [String] -> [String]
    bothWays directions [] = directions
    bothWays directions (row:s) = 
        --If "Dir" statement found, add both directions
        if (findFirst row == "Dir")
            then bothWays (directions ++ [findFirst row ++ " " ++ (findI row 2) ++ " " ++ (findI row 1) ++ " " ++ (flipDirection (findI row 3))] ++ [row]) s
            else bothWays directions s

    --Flip given direction to opposite
    flipDirection :: String -> String
    flipDirection dir
        | dir == "north" = "south"
        | dir == "south" = "north"
        | dir == "east" = "west"
        | dir == "west" = "east"
        | otherwise = dir

    --Get path from one given place (current) to another given place (dest)
    answerDirection :: String -> String -> [String] -> String
    answerDirection current dest directions = printPath (answerDir current dest directions ["start"] [])

    --Convert found path to printable form
    printPath :: [String] -> String
    printPath [] = ""
    printPath [d] = (flipDirection d)
    printPath (d:path) = if d == "start"
        then printPath path
        else (flipDirection d) ++ ", " ++ printPath path

    --Recursively explore every possible route from starting place, until a branch's end or the destination is reached. Return: correct path

    --Check if at destination and if not, keep going. Otherwise return found path
    answerDir :: String -> String -> [String] -> [String] -> [String] -> [String]
    answerDir current dest directions path corrPath =
        if current == dest
            then path
            --Call function to check all possible directions except previous one
            else checkDirections current dest directions path corrPath (findPossible current directions (flipDirection (last path)) [])

    --Check all found directions from current place
    checkDirections :: String -> String -> [String] -> [String] -> [String] -> [String] -> [String]
    checkDirections _ _ _ _ corrPath [] = corrPath
    checkDirections current dest directions path corrPath possible = 
        checkDirections current dest directions path (answerDir (findPlace (head possible) current directions) dest directions (path ++ [head possible]) corrPath) (tail possible)

    --Find possible directions from current. Return: array of directions, exclude previous
    findPossible :: String -> [String] -> String -> [String] -> [String]
    findPossible current [row] previous possible = 
        --If statement with current as starting point is found and it is not the direction of the previous visited place, add direction
        if ((findI row 1) == current) && (not ((findI row 3) == previous))
            then possible ++ [(findI row 3)]
            else possible
    findPossible current (row:d) previous possible = 
        if ((findI row 1) == current) && (not ((findI row 3) == previous))
            then findPossible current d previous (possible ++ [(findI row 3)])
            else findPossible current d previous possible

    --Find the place in given direction (dir) from current. Return: place's name
    findPlace :: String -> String -> [String] -> String
    findPlace dir current [row] =
        if ((findI row 1) == current) && ((findI row 3) == dir)
            then findI row 2
            else "don't know"
    findPlace dir current (row:d) =
        if ((findI row 1) == current) && ((findI row 3) == dir)
            then findI row 2
            else findPlace dir current d