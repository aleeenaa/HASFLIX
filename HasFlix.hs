--
-- MATHFUN - Discrete Mathematics and Functional Programming
-- Functional Programming Assignment 2014/15
--

import Data.List hiding (delete)
import Data.Set hiding (foldr, map, filter)
import Text.Printf

--
-- Types
type Title   = String
type Actor   = String
type Year    = Int
type Fan     = String

--
-- Define Film type here
data Film    = Film Title [Actor] Year [Fan]
                deriving (Eq,Ord,Show,Read)

testDatabase :: [Film]
testDatabase = [Film "Casino Royale" ["Daniel Craig", "Eva Green", "Judi Dench"] 2006 ["Garry", "Dave", "Zoe", "Kevin", "Emma"],
                Film "Cowboys & Aliens" ["Harrison Ford", "Daniel Craig", "Olivia Wilde"] 2011 ["Bill", "Jo", "Garry", "Kevin", "Olga", "Liz"],
                Film "Catch Me If You Can" ["Leonardo DiCaprio", "Tom Hanks"] 2002 ["Zoe", "Heidi", "Jo", "Emma", "Liz", "Sam", "Olga", "Kevin", "Tim"],
                Film "Mamma Mia!" ["Meryl Streep", "Pierce Brosnan","Colin Firth"] 2008 ["Kevin", "Jo", "Liz", "Amy", "Sam", "Zoe"],
                Film "Titanic" ["Leonardo DiCaprio", "Kate Winslet"] 1997 ["Zoe", "Amy", "Heidi", "Jo", "Megan", "Olga"],
                Film "Quantum of Solace" ["Daniel Craig", "Judi Dench"] 2008 ["Bill", "Olga", "Tim", "Zoe", "Paula"],
                Film "You've Got Mail" ["Meg Ryan", "Tom Hanks"] 1998 ["Dave", "Amy"],
                Film "Collateral" ["Tom Cruise", "Jamie Foxx"] 2004 ["Dave", "Garry", "Megan", "Sam", "Wally"],
                Film "The Departed" ["Leonardo DiCaprio", "Matt Damon", "Jack Nicholson"] 2006 ["Zoe", "Emma", "Paula", "Olga", "Dave"],
                Film "Up in the Air" ["George Clooney", "Vera Farmiga"] 2009 ["Wally", "Liz", "Kevin", "Tim", "Emma"],
                Film "Gravity" ["George Clooney", "Sandra Bullock"] 2013 ["Zoe", "Emma", "Garry", "Ian", "Neal", "Wally", "Olga", "Dave"],
                Film "The King's Speech" ["Colin Firth", "Geoffrey Rush"] 2010 ["Garry", "Megan", "Sam", "Ian", "Bill", "Emma", "Chris"],
                Film "Ocean's Twelve" ["George Clooney", "Matt Damon", "Catherine Zeta-Jones", "Julia Roberts"] 2004 ["Jo", "Wally", "Emma"],
                Film "The Adjustment Bureau" ["Matt Damon", "Emily Blunt"] 2011 ["Kevin", "Tim", "Emma", "Emma", "Garry", "Ian", "Neal"],
                Film "Cloud Atlas" ["Tom Hanks", "Halle Berry"] 2012 ["Dave", "Amy", "Garry", "Ian", "Neal"],
                Film "The Reader" ["Kate Winslet", "Ralph Fiennes"] 2008 ["Emma", "Bill", "Dave", "Liz"],
                Film "Begin Again" ["Keira Knightley", "Mark Ruffalo", "James Corden"] 2013 ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma"],
                Film "Revolutionary Road" ["Leonardo DiCaprio", "Kate Winslet"] 2008 ["Wally", "Sam", "Dave", "Jo"],
                Film "Into the Woods" ["Meryl Streep", "Emily Blunt", "James Corden"] 2014 ["Dave", "Jo", "Wally", "Emma"],
                Film "Now You See Me" ["Jesse Eisenberg", "Mark Ruffalo"] 2013 ["Bill", "Sam", "Zoe", "Jo"],
                Film "Larry Crowne" ["Tom Hanks", "Julia Roberts"] 2011 ["Liz", "Wally"],
                Film "The Terminal" ["Tom Hanks", "Catherine Zeta Jones"] 2004 ["Olga", "Heidi", "Bill", "Sam", "Zoe"],
                Film "Edge of Tomorrow" ["Tom Cruise", "Emily Blunt"] 2014 ["Jo", "Chris", "Wally", "Ian", "Garry", "Bill", "Olga", "Megan", "Sam"],
                Film "Django Unchained" ["Jamie Foxx", "Leonardo DiCaprio", "Christoph Waltz"] 2012 ["Kevin", "Tim", "Emma", "Olga"],
                Film "Skyfall" ["Daniel Craig", "Judi Dench", "Ralph Fiennes"] 2012 ["Bill", "Olga", "Zoe", "Paula", "Megan", "Sam", "Wally"]]

--
--
-- HELPER FUNCTIONS
--
--

-- sums up the numbers in a list
-- used in command 6 and actorNumFans (helper function)
addUp :: [Int] -> Int
addUp = (foldr (+) 0)

-- converts a list of strings to string through recursion and pattern matching
-- used in movieAsSring for the cast list, fansOfMovies for the list of fans,
-- coStarsAsString to display the list of co stars of an actor and addCast which
-- is an IO helper function to display the accumulated list of actors entered by user.
displayListAsString :: [String] -> String
displayListAsString [] = ""
displayListAsString (x:[]) = x ++ displayListAsString []
displayListAsString (x:xs) = x ++ ", " ++ displayListAsString xs

-- displays a movie as well formatted string
movieAsString :: Film -> String
movieAsString (Film title cast year fans) = "\nTitle: " ++ title ++ "\n Cast: " ++ (displayListAsString cast) ++ "\n Year: " ++ show year ++ "\n Fans: " ++ show (length fans)

-- checks if fan exists through helper function isFanOf to search through the database given and returns a string.
fanExists :: Fan -> [Film] -> String
fanExists fanName movieDB
    | isFanOf fanName movieDB == []     = "You don't seem to be a fan of any films on HasFlix.\nYou can mend this by entering 4 in main menu."
    | otherwise                         = ""

-- filters through the database to find the films which a particular user is a fan of.
isFanOf :: Fan -> [Film] -> [Film]
isFanOf fanName = filter (\(Film _ _ _ fans) -> elem fanName fans)

-- returns the fans of a particular film.
getFans :: [Film] -> [Fan]
getFans [(Film _ _ _ fans)] = fans

-- filters through a databse of type [Film] to find a particular film
findMovie :: Title -> [Film] -> [Film]
findMovie movieName = filter (\(Film titles _ _ fans) -> titles == movieName)

-- filters through a given database to find films within a time constraint
moviesByPeriod :: Year -> Year -> [Film] -> [Film]
moviesByPeriod startY endY = filter(\(Film t c y f) -> startY <= y && y <= endY)

-- eliminates the error of user inputting the years the wrong way around
moviePeriodErrorCatch :: Year -> Year -> [Film] -> [Film]
moviePeriodErrorCatch y1 y2 movieDB
    | y1 < 0 || y2 < 0  =   []
    | y2 >= y1          =   moviesByPeriod y1 y2 movieDB
    | y1 > y2           =   moviesByPeriod y2 y1 movieDB

-- adds a particular fan to the list of fans of a particular film
addFan :: Fan -> Title -> [Film] -> [Film]
addFan fanName movieName = map (\(Film t c y f) -> if movieName == t && notElem fanName f then (Film t c y (f ++ [fanName])) else (Film t c y f))

-- checks if an actor exists in a given database of films. returns a string
actorExists :: Actor -> [Film] -> String
actorExists actorName movieDB
    | actorMovies actorName movieDB == []   = "\nThis actor is not on HasFlix.\nKnow any movies with "++ actorName++"?\nWhy not add them. Simply enter 1 in the main menu."
    | otherwise                             = ""

-- filters through a given database to find all the films a particular actor acts in
actorMovies :: Actor -> [Film] -> [Film]
actorMovies actor = filter(\(Film _ cast _ fans) -> elem actor cast)

-- finds the total number of films an actor has in a given database
actorNumMovies :: Actor -> [Film] -> Int
actorNumMovies actor movieDB = length $ actorMovies actor movieDB

-- finds the number of fans for each film in a given database
getFanNum :: [Film] -> [Int]
getFanNum = map (\(Film _ _ _ fans) -> length fans)

-- counts the number of fans a particular actor has (includes duplicates)
actorNumFans :: Actor -> [Film] -> Int
actorNumFans actor movieDB = addUp $ getFanNum $ actorMovies actor movieDB

-- takes a film and returns its cast as list which is turned into a set
getCastAsSet :: Film -> Set Actor
getCastAsSet (Film _ cast _ _) = fromList $ cast

-- finds all the actors a particular actor has co starred in and returns a list of sets
getCoActors :: Actor -> [Film] -> [Set Actor]
getCoActors actor movieDB = map getCastAsSet (actorMovies actor movieDB)

-- the union of a list of sets which represent the cast for each film that a particular actor 
-- has cosstarred in and removes said particular actor
unionOfCast :: Actor -> [Film] -> Set Actor
unionOfCast actor movieDB = delete actor $ unions $ getCoActors actor movieDB

--
--
--  Your functional code goes here
--
--

-- i. Add a new film to the database.

addMovie :: Title -> [Actor] -> Year -> [Film] -> [Film]
addMovie title cast year movieDB = movieDB ++ [Film title cast year []]

-- ii. Give all films in the database (if database passed through directly)

moviesAsString :: [Film] -> String
moviesAsString movieDB = (unlines.map movieAsString) movieDB

-- iii. Give all films that a particular user is a fan of.

isFanOfMovies :: Fan -> [Film] -> String
isFanOfMovies fanName [] = "No Database provided."
isFanOfMovies fanName movieDB = moviesAsString $ isFanOf fanName movieDB

-- iv. Give all fans of a particular film.

fansOfMovies :: Title -> [Film] -> String
fansOfMovies movieName movieDB = displayListAsString $ getFans $ findMovie movieName movieDB

-- v. Give all the films that were released during a particular period (i.e. between a given start year and end year).

moviesByPeriodAsString :: Year -> Year -> [Film] -> String
moviesByPeriodAsString startY endY movieDB = moviesAsString $ moviePeriodErrorCatch startY endY movieDB

-- vi. Allow a user to say they are a fan of a particular film.

becomeFan :: Fan -> Title -> [Film] -> [Film]
becomeFan _ _ []    =   []
becomeFan fanName movieName movieDB = addFan fanName movieName movieDB


-- vii. Give the average number of fans for the films starring a particular actor.

actorFanAvg :: String -> [Film] -> Float
actorFanAvg actor movieDB = fromIntegral(actorNumFans actor movieDB) / fromIntegral(actorNumMovies actor movieDB)

-- viii. Give (without duplicates) the names of actors who have co-starred in at least one film with a particular actor.

coStarsAsString :: Actor -> [Film] -> String
coStarsAsString actor movieDB = displayListAsString $ toList $ unionOfCast actor movieDB


-- Demo function to test basic functionality (without persistence - i.e.
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

demo :: Int -> IO ()
demo 1   = putStrLn $ moviesAsString $ addMovie "The Monuments Men" ["George Clooney", "Matt Damon", "Bill Murray"] 2014 testDatabase
        -- putStrLn all films after adding 2014 film "The Monuments Men"
        -- starring "George Clooney", "Matt Damon" and "Bill Murray"
        -- to testDatabase
demo 2   = putStrLn $ moviesAsString testDatabase                                    -- displaying the database as strings
demo 3   = putStrLn $ isFanOfMovies "Zoe" testDatabase                               -- displaying all films that Zoe is fan of
demo 4   = putStrLn $ fansOfMovies "Titanic" testDatabase                             -- displaying all fans of Titanic
demo 5   = putStrLn $ moviesByPeriodAsString 2010 2013 testDatabase                  -- displaying all films between 2010 and 2013
demo 6   = putStrLn $ moviesAsString $ becomeFan "Zoe" "The Reader" testDatabase     -- displaying all films after "Zoe" says she is a fan of "The Reader"
demo 66  = putStrLn $ moviesAsString $ becomeFan "Zoe" "Skyfall" testDatabase        -- displaying all films after "Zoe" says she is a fan of "Skyfall"
demo 7   = putStrLn $ printf "%3.2f" $ actorFanAvg "Tom Hanks" testDatabase         -- displaying average number of fans for films starring "Tom Hanks"
demo 8   = putStrLn $ coStarsAsString "Tom Hanks" testDatabase                      -- displaying all co-stars of "Tom Hanks"

--
--
-- Your user interface code goes here
--
--


-- Main IO functions

-- main which loads the database from a file called films.txt and processes it as a list of type Film
-- asks the user for their to be able to implement become fan command 4 and find all films user is fan of command 5
-- calls userInterface to allow user to interact with the loaded database and saves it back to films.txt once user decides to quit.
-- databse saved as string but in format of list of films
main :: IO ()
main = do films <- readFile "films.txt"
          let movieDB = read films :: [Film]
          putStrLn "\nCharging Jet Engines..."
          putStrLn "                   3\n                    2\n                     1 ..."
          putStrLn "\nMission control launch ready.\n"
          putStrLn "Enter your name: "
          username <- getLine
          movieDB <- userInterface (username, movieDB)
          writeFile "films.txt" (show movieDB)
          putStrLn "\nYou are now leaving HasFlix."
          putStrLn "Your changes have been successfully saved."
          putStrLn "GOODBYE."

-- userInterface that utilises a case switch to allow user to select specific commands to interact with the database.
userInterface :: (Fan, [Film]) -> IO [Film]
userInterface (username, movieDB) = do
                                let info = (username, movieDB)
                                let note = "\nNOTE: Press any key to go back to the Main Menu.\n      Entering no input inside each command will return you to main menu.\n"
                                putStrLn $ "\nWelcome to HasFlix " ++ username
                                putStrLn "Where you can search through our database of 25 films."
                                putStrLn "Following are the actions that you can perform:\n"
                                putStrLn "1 - Add a Movie to HasFlix.\n"
                                putStrLn "2 - Display a single Movie on HasFlix.\n"
                                putStrLn "3 - Display all films on HasFlix.\n"
                                putStrLn "4 - Become a fan for a single Movie on HasFlix.\n"
                                putStrLn "5 - Display all the films you're a fan of.\n"
                                putStrLn "6 - Display the fans of a single Movie.\n"
                                putStrLn "7 - Display all films on HasFlix using between certain years.\n"
                                putStrLn "8 - Display the average number of fans for the films starring a particular actor.\n"
                                putStrLn "9 - Display all the names of actors who have co-starred in at least one movie with a particular actor.\n"
                                putStrLn "0 - Exit and Update database\n"
                                putStrLn "Enter a number to perform an action or 0 to exit and save the database.\n"
                                putStr "Command: "
                                input <- getLine
                                if input /= "0"
                                    then case input of
                                        "1" -> do
                                            info <- command 1 info
                                            putStr note
                                            entry <- getLine
                                            userInterface info
                                        "2" -> do
                                            info <- command 2 info
                                            putStr note
                                            entry <- getLine
                                            userInterface info
                                        "3" -> do
                                            info <- command 3 info
                                            putStr note
                                            entry <- getLine
                                            userInterface info
                                        "4" -> do
                                            info <- command 4 info
                                            putStr note
                                            entry <- getLine
                                            userInterface info
                                        "5" -> do
                                            info <- command 5 info
                                            putStr note
                                            entry <- getLine
                                            userInterface info
                                        "6" -> do
                                            info <- command 6 info
                                            putStr note
                                            entry <- getLine
                                            userInterface info
                                        "7" -> do
                                            info <- command 7 info
                                            putStr note 
                                            entry <- getLine
                                            userInterface info
                                        "8" -> do
                                            info <- command 8 info
                                            putStr note
                                            entry <- getLine
                                            userInterface info
                                        "9" -> do
                                            info <- command 9 info
                                            putStr note
                                            entry <- getLine
                                            userInterface info
                                        _ -> do
                                            putStrLn "You entered an invalid command."
                                            userInterface info
                                        else return (snd info)

-- Add a Movie to the database passed through the userInterface with the username and database parameter
command :: Int -> (String, [Film]) -> IO (String, [Film])
command 1 (username, database) = do
    putStrLn "\nAdd a Movie to HasFlix.\n"
    putStr "Movie Title: "
    title <- getLine
    if title == ""
        then do
            return (username, database)
        else do
            let filmCheck = findMovie title database
            if filmCheck /= []
                then do
                    putStrLn "That movie already exists."
                    command 1 (username, database)
                else do
                    putStrLn "\nEnter the Cast.\nEach Actor on a separate line.\nPress Enter twice after the last actor's name has been entered.\n"
                    cast <- addCast []
                    putStr "\nEnter the Year the movie was released: "
                    y <- getLine
                    case reads y :: [(Integer, String)] of
                        [(n,"")] -> do
                            let year = read y :: Year
                            let newMovieDB = addMovie title cast year database
                            putStrLn "Displaying new database below:"
                            putStrLn $ moviesAsString newMovieDB
                            return (username, newMovieDB)
                        _ -> do
                            putStrLn "Please enter a valid year."
                            command 1 (username, database)

-- Displays a single movie by asking for a title and searching through
-- the database passed through userInterface.
-- Utilises the helper function findMovie
-- Displays the movie as string using moviesAsString
command 2 (username, database) = do
    putStrLn "\nDisplay a single Movie on HasFlix.\n"
    putStr "Movie Title: "
    title <- getLine
    if title == ""
        then do
            return (username, database) -- exits command and takes user back to main menu
            else do                
                let filmCheck = findMovie title database
                if filmCheck /= []
                    then do
                        putStrLn $ moviesAsString $ findMovie title database
                        return (username, database)
                    else do
                        putStrLn "\nPlease choose a film which is currently in the HasFlix database."
                        command 2 (username, database)

-- Displays all the films in the database passed to it through the userInterface.
-- utilises moviesAsString to display films as well formatted string.
command 3 (username, database) = do
    putStrLn "\nDisplaying all films on HasFlix below:"
    putStrLn $ moviesAsString database
    return (username, database)


-- Allows the user to become a fan of a particular film
-- fanName is passed through userInterface as username
-- movie title is searched through database to check if it is already in the database
-- displays the whole database as well formatted string through moviesAsString
command 4 (username, database) = do
    putStrLn "\nBecome a fan for a single Movie on HasFlix.\n"
    putStr "Movie Title: "
    title <- getLine
    if title == ""
        then do
            return (username, database) ---- exits command and takes user back to main menu
        else do
            let filmCheck = findMovie title database
            if filmCheck /= []
                then do
                    let newMovieDB = becomeFan username title database
                    putStrLn $ moviesAsString $ becomeFan username title database
                    return (username, newMovieDB)
                else do
                    putStrLn "Please choose a film which is currently in the HasFlix database."
                    command 4 (username, database)

command 5 (username, database) = do
    let fan = fanExists username database
    if fan == ""
        then do
            putStrLn "\nDisplaying all the films you're a fan of below:"
            putStrLn $ isFanOfMovies username database
            return (username, database)
        else do
            putStrLn "\nSearching through HasFlix ..."
            putStrLn "No Results Found.\n"
            putStrLn $ fanExists username database
            return (username, database)

command 6 (username, database) = do
    putStrLn "\nDisplay the fans of a single Movie.\n"
    putStr "Movie Title: "
    title <- getLine
    if title == ""
        then do
            return (username, database) -- exits command and takes user back to main menu
        else do
            let filmCheck = findMovie title database
            if filmCheck /= []
                then do
                    let fans = addUp $ getFanNum filmCheck
                    if fans /= 0
                        then do
                            putStrLn $ "\nDisplaying all fans of " ++ title ++ " below:\n"
                            putStrLn $ fansOfMovies title database
                            return (username, database)
                        else do
                            putStrLn $ "\n" ++ title ++ " doesn't seem to have any fans at the moment.\nHave you seen " ++ title ++ "? Did you like it?\nYou could be the first fan for this Movie.\nSimply enter 4 in the main menu."
                            return (username, database)
                else do
                    putStrLn "Sorry that movie was not found on HasFlix.\nGrow our database by adding this movie to HasFlix.\nSimply enter 1 in the main menu and follow the instructions."
                    return (username, database)

command 7 (username, database) = do
    putStrLn "\nDisplay all films on HasFlix using between certain years.\n"
    putStr "Enter Start Year: "
    startY <- getLine
    case reads startY :: [(Integer, String)] of
        [(n,"")] -> do
            let startYear = read startY :: Year
            putStr "Enter End Year:"
            endY <- getLine
            case reads endY :: [(Integer, String)] of
                [(n,"")] -> do
                    let endYear = read endY :: Year
                    putStrLn $ "\nDisplaying all Movies between " ++ (show startYear) ++ " & " ++ (show endYear) ++ ":"
                    putStrLn $ moviesByPeriodAsString startYear endYear database
                    return (username, database)
                _ -> do
                    putStrLn "Please enter a valid year."
                    command 7 (username, database)
        _ -> do
            putStrLn "Please enter a valid year."
            command 7 (username, database)

command 8 (username, database) = do
    putStrLn "\nDisplay the average number of fans for the films starring a particular actor.\n"
    putStr "Enter Actor's name: "
    actorName <- getLine
    if actorName == ""
        then do
            return (username, database) -- exits command and takes user back to main menu
        else do
            let actor = actorMovies actorName database
            if actor /= []
                then do 
                    putStrLn $ "\nDisplaying the Average Fans per Movie (AFPM) for " ++ actorName ++ ":"
                    putStrLn $ printf "%3.2f" $ actorFanAvg actorName database
                    return (username, database)
                else do
                    putStrLn $ actorExists actorName database
                    return (username, database)

command 9 (username, database) = do
    putStrLn "\nDisplay all the names of actors who have co-starred in at least one movie with a particular actor.\n"
    putStr "Enter Actor's name: "
    actorName <- getLine
    if actorName == ""
        then do
            return (username, database) -- exits command and takes user back to main menu
        else do
            let actor = length $ actorMovies actorName database
            if actor /= 0
                then do 
                    putStrLn $ "\nDisplaying the co-stars of " ++ actorName ++ " below:\n"
                    putStrLn $ coStarsAsString actorName database
                    return (username, database)
                else do
                    putStrLn $ actorExists actorName database
                    return (username, database)


-- IO Helper Functions

-- User interface IO for adding actors when called from the addMovie IO - command 1
-- takes each actor on a separate line, one after the other.
addCast :: [Actor] -> IO [Actor]
addCast list = do
                putStr "Actor: "
                actor <- getLine
                if actor == ""
                then do
                    putStrLn ("\nCast Entered: " ++ (displayListAsString list))
                    return list
                else do if (elem actor list)
                        then do
                            putStrLn "\nThat actor has already been entered, try again.\n"
                            putStr "Current Cast: "
                            putStrLn ((displayListAsString list) ++ "\n")
                            addCast list
                        else do
                            let castList = (list ++ [actor])
                            putStr "\nCurrent Cast: "
                            putStrLn ((displayListAsString castList) ++ "\n")
                            addCast castList