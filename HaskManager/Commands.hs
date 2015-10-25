{-
STOP #12 The functional backend.
You can think of this as the backend. Control is passed here from the main
module so commands can be executed. The functional style allows us to build
a kind of query API.

It's a bit like SQL. The types of operations are relatively few (adding,
selecting, updating), but you can use each operation in many different ways
depending on what you want it to do.

Most operations accept a function that specifies the missing details of how
that operation should be carried out - you need to provide that function.
For example, see cmdUpdate...
-}

module HaskManager.Commands
(
    cmdAdd,
    cmdQuery,
    cmdQueryMany,
    cmdQueryManyIO,
    cmdUpdate
) where

import Data.String.Utils
import Data.Time
import HaskManager.FileStore
import HaskManager.Globals
import HaskManager.Task
import HaskManager.TaskList
import System.Locale

cmdQuery :: String -> IO String
cmdQuery input = do
    task <- findTaskById input
    case task of
        Left err -> return err
        Right t  -> return $ show t

cmdQueryMany :: (TaskList -> TaskList) -> IO String
cmdQueryMany f = do
    tasks <- getTasksFromFile
    return $ showTaskList(f(tasks))

cmdQueryManyIO :: (TaskList -> IO TaskList) -> IO String
cmdQueryManyIO f = do
    tasks <- getTasksFromFile
    tasksFiltered <- f(tasks)
    return $ showTaskList(tasksFiltered)

cmdAdd :: String -> IO String
cmdAdd cmd = do
    case parseTaskFromInput cmd of
        Nothing -> return ("Error in input.")
        Just t  -> do
            oldTasks <- getTasksFromFile
            let newTasks = addTask t oldTasks
            saveTaskList newTasks
            return ("Done.")

{-
cmdUpdate takes a string (the line the user entered) but also a function, f.
All cmdUpdate knows is that the function takes a task and returns task;
cmdUpdate trusts that when it executes f, the resultant task is updated.

For example, calling cmdUpdate with f=tick passes the tick function as an
argument. When the desired task has been found, the tick function is
eventually called on that task (which is updated in the task list).
-}
cmdUpdate :: String -> (Task -> Task) -> IO String
cmdUpdate cmd f = do
    let taskNum = parseTaskNumFromInput cmd
    tasks <- getTasksFromFile
    case taskNum of
        Nothing -> return ("Error reading input.")
        Just i  ->
            case getTaskById i tasks of
                Nothing -> return ("Task num not recognised.")
                Just _  -> do
                    let newTasks = updateTask f i tasks
                    saveTaskList newTasks
                    return ("Done.")

-- These functions are helpers for the commands and are not exported.
getDayFromInput :: String -> Day
getDayFromInput s = case (parseTime defaultTimeLocale dateFormat s) of
    -- TODO use today's date
    Nothing -> fromGregorian 2015 10 10
    Just t  -> utctDay t

{-
STOP #13 Maybe
When you saw cmdUpdate, you might have noticed a part of the code like this:

case taskNum of
    Nothing -> return ("Error reading input.")
    Just i  -> -- etc.

The case statement is similar to that from other languages: it matches a value
to one from a list of specific cases and executes the code from the corresponding
case. But what are "Nothing" and "Just"?

The value of taskNum comes from the function parseTaskFromInput. What does it
return? This funny thing called "Maybe Task". Maybe is like a box that may or
may not contain a value. It's a way of telling the using function that a value
might not be returned and will need to deal with that eventuality.

When you "unwrap" a Maybe, there is either one of two things to be found: 1)
a Nothing (the box is empty), or 2) Just x, where x is a single value. It's
similar to dealing with null values in other languages.

Take parseTaskFromInput as an example. It will try to parse a task from the
user input... but the user might have made a mistake when typing it in, thus
the function would be unable to construct a new task. In this case it returns
Nothing. Otherwise, it creates a task and wraps in a Just.

More: https://en.wikibooks.org/wiki/Haskell/Understanding_monads/Maybe
-}
parseTaskFromInput :: String -> Maybe Task
parseTaskFromInput input
    | length taskParts /= 3 = Nothing
    | otherwise = Just Task {
        description = taskParts!!1,
        done = False,
        due = getDayFromInput (taskParts!!2)
    }
    where taskParts = split "\t" input

parseTaskNumFromInput :: String -> Maybe Int
parseTaskNumFromInput s
    | length cmdParts /= 2 = Nothing
    | otherwise = maybeRead (cmdParts!!1)
    where cmdParts = split "\t" s

{-
STOP #14 Either
Either is somewhat like Maybe. It's a way to deal with functions that might
not work out as desired. Whereas Maybe helps with cases where there's no
value to return, Either helps deal with errors that crop up in the middle of
a function.

Either returns one of two values: Left or Right. Similarly to Just, Left and
Right each wrap up a value. By convention, you write a function to return a
Left if an error occurs. Otherwise, if everything works out fine, you return
a Right.

Let's look at findTaskById. This is a function that takes a string (the user's
input), and does some IO that returns an Either. The Either returns a String or
a Task. If something goes wrong, we return a String (Left) with an error
message. If it works fine, we return the Task (Right).

There are two potential errors that could crop up:

1) The task ID cannot be found in the user's input (so we return Left "Error
reading input.")
2) The user has entered an ID but it's not in the TaskList (so we return
Left "Task num not recognised.").
-}
findTaskById :: String -> IO (Either String Task)
findTaskById cmd = do
    let taskNum = parseTaskNumFromInput cmd
    tasks <- getTasksFromFile
    case taskNum of
        Nothing -> return (Left "Error reading input.")
        Just i  ->
            case getTaskById i tasks of
                Nothing -> return (Left "Task num not recognised.")
                Just t  -> return (Right t)
