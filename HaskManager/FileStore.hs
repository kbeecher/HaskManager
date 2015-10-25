module HaskManager.FileStore (
    getTasksFromFile,
    saveTaskList
) where

import qualified Data.ByteString as ByteStr
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map as Map
import Data.String.Utils
import Data.Time
import HaskManager.Globals
import HaskManager.Task
import HaskManager.TaskList
import System.Directory
import System.IO
import System.Locale

saveTaskList :: TaskList -> IO ()
saveTaskList ts = do
    renameFile taskFileName taskBackupFileName
    withFile taskFileName WriteMode (\handle -> do
        let output = taskListToFileFormat ts
        hPutStr handle output
        hFlush handle)
    return ()

taskListToFileFormat :: TaskList -> String
taskListToFileFormat m = Map.foldr
    (\t ts ->
        description t ++ "\t" ++
        (if done t then "1" else "0") ++ "\t" ++
        (formatTime defaultTimeLocale dateFormat (due t)) ++ "\n" ++ ts)
    "" m

getTasksFromFile :: IO TaskList
getTasksFromFile = do
    fileExists <- doesFileExist taskFileName
    case fileExists of
        False -> do
            writeFile taskFileName ""
            return Map.empty
        True  -> do
            contents <- ByteStr.readFile taskFileName
            let fileLines = lines (Char8.unpack contents)
            let tasks = taskListToMap $ (parseLinesToTasks fileLines)
            return (tasks)

taskListToMap :: [(Int, Task)] -> TaskList
taskListToMap ts = Map.fromList ts

parseLinesToTasks :: [String] -> [(Int, Task)]
parseLinesToTasks [] = []
parseLinesToTasks (l:ls) =
    let idx = length ls
    in  parseLinesToTasks ls ++ [(idx, parseLineToTask l)]

parseLineToTask :: String -> Task
parseLineToTask l =
    let taskParts = split "\t" l
    in  Task {
        description = taskParts!!0,
        done = if taskParts!!1 == "0" then False else True,
        due = utctDay (readTime defaultTimeLocale dateFormat (taskParts!!2) :: UTCTime)
    }
