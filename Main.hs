import Data.List
import HaskManager.Commands
import HaskManager.FileStore
import HaskManager.Task
import HaskManager.TaskList
import System.IO

{-
STOP #11 Guards
Sometimes you want to do different things depending on the argument(s) to a
function. For this purpose, Haskell has guards. After the function's name and
parameters are listed (in this case, "interpret cmd") you can use a vertical
bar (guard) to separate out each possibility. When the function is executed, the
conditions after each guard are evaluated until one is found that is true.
The matching function is then executed.

For example, if the user has typed a line beginning in "add", then the cmdAdd
function is called with the line passed as an argument.

More: https://en.wikibooks.org/wiki/Haskell/Control_structures#if_and_guards_revisited
-}
interpret :: String -> IO String
interpret cmd
    | "todo"  `isPrefixOf` cmd = cmdQueryMany getPending
    | "all" `isPrefixOf` cmd = cmdQueryMany (\x -> x)
    | "today" `isPrefixOf` cmd = cmdQueryManyIO todaysTasks
    | "add" `isPrefixOf` cmd = cmdAdd cmd
    | "tick" `isPrefixOf` cmd = cmdUpdate cmd tick
    | "untick" `isPrefixOf` cmd = cmdUpdate cmd untick
    | "show" `isPrefixOf` cmd = cmdQuery cmd
    | otherwise = return ("Invalid command")

{-
STOP #10 Program entry point
Now that we've seen the two main parts of the program (Task and TaskList)
let's see how we construct a program using them.

The entry point of a Haskell program is the main function. This behaves like
the previous functions we've seen that use do-notation. That is, you can
think of it as a miniature bit of imperative code.

Basically, this function keeps accepting a line from the user and passing it
to the interpret function.
-}
main :: IO ()
main = do
    putStr "> "
    hFlush stdout
    line <- getLine
    -- Here's our first if expression. It works like if from other languages,
    -- except that it must have both a "then" and an "else".
    if line `elem` ["q", "quit", "exit"]
        -- The empty parentheses denote an empty tuple which is what the
        -- main function always returns. Returning () is the way to exit
        -- the main function.
        then return ()
        else do
            result <- interpret line
            putStrLn result
            main
