{-
STOP #5 Building on our earlier work
We're now going to build on our Task stuff to create another type called
a TaskList which will conveniently group together all our tasks. It will
just be a simple map that maps unique integer IDs to tasks.
-}
module HaskManager.TaskList
(
    TaskList,
    getTaskById,
    addTask,
    updateTask,
    getPending,
    todaysTasks,
    showTaskList,
) where

-- Here we import the Data.Map module, but rename it to Map for convenience
import qualified Data.Map as Map
import qualified Data.Map.Lazy as Map.Lazy
import Data.Time
import HaskManager.Task

{-
STOP #6 Using in-built types
Maps are part of the in-built Data.Map library. Also, Int is the in-built
integer type. Thus, "Map.Map Int Task" defines a type that maps integers to
tasks. We alias it here to "TaskList" for convenience.
-}
type TaskList = Map.Map Int Task

{-
STOP #7 Higher-order functions
Higher order functions are a core part of functional programming. This is
demonstrated nicely when you work with collections of things.

Consider that in Haskell, there's no looping construct... no for, while,
repeat or anything like that. So how do you iterate over something like
a map and do stuff with its elements?

One way, often seen, is to use a higher-order function - that is a function
that takes another function as an argument or returns a function. There are
three in particular that are highly useful in functional programming:
* map
* filter
* reduce (a.k.a. fold)

Briefly put, here's what each one does:
* map: apply a function to each element in a list and return a new list
  containing changed elements
  (e.g. map (*2) [1,2,3] == [2,3,6])
* filter: apply a boolean function to each element in a list and return a new
  list containing only elements that returned true
  (e.g. filter isEven [1,2,3] == [2])
* reduce/fold: apply a binary function to each pair of values from one end of
  a list to the other, keeping a running value (accumulator) as it goes, and
  return a single value
  (e.g. fold add [1,2,3,4,5] == 15)
TODO
More: http://learnyouahaskell.com/higher-order-functions

In the getPending function, we want to remove all tasks that are not pending,
hence we filter for tasks that are not done.

The first argument is the function to test whether to keep an element or not.
It's a lambda function (you can tell because it starts with a backslash) which
means we're not bothering to give it a name - it's just a function
that takes one parameter, t, and executes "done (t) == False". It gets applied
to all the items in ts.
-}
getPending :: TaskList -> TaskList
getPending ts = Map.filter (\t -> done (t) == False) ts

{-
The insert function takes three arguments: key and value of new map
entry, and the map that the entry is inserted into. For simplicity, the
Tasks in a TaskList each get an integer ID that is equal to the number of
entries in the list before insertion. You can't delete a task; the list just
keeps growing as you add new ones.
-}
addTask :: Task -> TaskList -> TaskList
addTask t ts = Map.insert (length (Map.keys ts)) t ts

getTaskById :: Int -> TaskList -> Maybe Task
getTaskById i ts = Map.lookup i ts

{-
Here we've created our own higher-order function. The in-built function
for updating a map entry (Data.Map.adjust) takes a function describing the
update and applies it to the chosen entry.

A function parameter appears in a function declaration as two or more
parameters surrounding by parentheses... like (a -> a) or (a -> b -> b)

TODO variables names
-}
updateTask :: (Task -> Task) -> Int -> TaskList -> TaskList
updateTask f i ts = Map.adjust f i ts

showTaskList :: TaskList -> String
showTaskList m = Map.Lazy.foldrWithKey
    (\k t tstring -> show k ++ ": " ++ show t ++ "\n" ++ tstring)
    "" m

{-
STOP #8 Impure code
TODO Explain procedures
So far we've only looked at "pure" functions. They're called pure because
they behave just like the mathematical idea of a function (as opposed to
'functions' in many other languages, which are more like procedures). They
have no state, do not access the outside world and have no side effects.
Every time you execute a pure function with the same set of arguments, the
result is always the same.

Writing pure code is good for several reasons. Not least of which is side
effects and unanticipated state changes are a major source of bugs and cause
problems when you want to reason about your programs or chain functions
together.

But at some point in any program, you have to deal with at least a bit of
impure code. For any program to be useful, it must manipulate state, even if
it's writing to the screen or a file.

The function todaysTasks analyses a TaskList and returns a new list without
the tasks which don't have today's date. Immediately you should see that this
is not quite pure -- the behaviour of the function is going to be different
depending on when you execute it.

So how do we deal with it?
-}



{-
STOP #9 IO
A pure language like Haskell tries to keep the real world out of a
program as much as possible. But when a function has to deal with the outside
world, it risks letting the real world seep into the rest of the program.

Haskell's solution is to explicitly mark parts of the code that contain
side effects so they can be treated appropriately and in a way that retains
the language's purity.

This is where the IO type comes in. Look at the declaration of todaysTasks.
You can read it as "this function takes a TaskList and does some I/O that
returns a TaskList".

In other words, at some point in its execution, this function has to deal
with the real world. When you do that, state and the sequence of
operations becomes important. Hence, functions which include IO can look like
mini imperative programs via do-notation.

Using do-notation you can write a function as a sequence of steps.

More: http://learnyouahaskell.com/input-and-output
-}
todaysTasks :: TaskList -> IO TaskList
todaysTasks ts = do
    {- getCurrentTime is the reason todaysTasks returns IO TaskList:
    getCurrentTime does some IO and returns a time value. It does IO because
    it has to check with the outside world what the current time is, which
    means executing the function results in different results every time.

    Remember, Haskell requires that you explicitly mark code that contains side
    effects. Therefore, any function you write that uses getCurrentTime must be
    marked as doing IO, because those side effects "seep into" your function.
    -}
    now <- getCurrentTime
    let tasks = Map.filter (\t -> due (t) == (utctDay now)) ts
    return (tasks)
