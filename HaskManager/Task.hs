{-
STOP #1 Modules
Let's start with some simple stuff. The most fundamental part
of our task manager is the definition of the task itself.

In Haskell, we can divide up our code into modules which contain related things.
This module has task-related stuff, so we call it Task.

Inside the parentheses, you see a list of names. These are all the
things that this module exports (i.e. makes publicly visible to other
modules).

We'll also import the in-built module for handing dates and times
(Data.Time) because our tasks will have due dates attached.

More: http://learnyouahaskell.com/modules
-}
module HaskManager.Task (
    Task(Task),
    tick,
    untick,
    description,
    done,
    due
) where

import Data.Time

{-
STOP #2 Record syntax
Here we define what a task is. There are actually a couple of different
concepts here, but for now we'll just focus on one: record syntax. This is
similar to structures in other languages (e.g. structs) and ties together
several fields into one custom type. The record is defined on the right-hand
side of the '='.

More: http://learnyouahaskell.com/making-our-own-types-and-typeclasses#type-parameters
-}
data Task = Task {
    description :: String, -- the double-colon means "has the type"
    done :: Bool,
    due :: Day -- Day is a type from the Data.Time package.
}

{-
STOP #3 Simple functions
Here's a nice simple function to start off with. The first line is the
function declaration. You can read it something like this...
"hasDescription has the type function (which takes a Task and returns a Bool)"

You can tell it's a function because of the right-arrow (a -> b).
-}
hasDescription :: Task -> Bool
{-
The second line defines the function. Just like you would define, say, an
integer by saying "n = 3", in Haskell you can say something like
"sum x y = x + y".

On the left-hand side of the equals sign, the name of the function (sum) is
followed by a list of parameters the function takes (separated by spaces if
there are many of them).

On the right-hand side comes the expression that deals with those parameters.

In this case, the hasDescription function takes one parameter (a Task). It
examines the length of the task's description and returns whether it is
non-zero in length (for record types, Haskell automatically generates getter
functions for accessing field values).
-}
hasDescription t = length (description t) > 0

{-
A few things to note:
1) The declaration isn't strictly necessary. Haskell is clever enough to deduce
the function signature. Still, it's good practice to include it because it
makes clear what you intend the function to do... because you may
inadvertantly write the function so it behaves differently than intended!

2) Parentheses for containing function arguments can often be omitted. Notice
we didn't write "length (description (t))" which is still perfectly valid.
For example, we could write "length myList" and that would be enough.
In this case, we have to make it clear that the length function is being
applied to a string - the result of "description t". If we had written
"length description t" we would end up trying to find out the length of the
description function and that's not valid.

More: https://www.fpcomplete.com/blog/2012/09/ten-things-you-should-know-about-haskell-syntax#2--function-call-syntax-is-terse
-}

{-
STOP #4 More simple functions
tick and untick are a couple more simple functions. Each one takes an
existing task and returns a new Task with all the fields retaining the same
value, except for the done field.

Notice that we're not managing state - rather, a new Task is returned and
the old one disposed. That's because Haskell is *purely* functional by
default, meaning that all data is immutable. As we'll see later, there is a
way to do impure things in Haskell.
-}
tick :: Task -> Task
tick t = t { done = True }

untick :: Task -> Task
untick t = t { done = False}

instance Show Task where
    show (Task desc done due) =
        "'" ++ desc ++
        "' due: " ++ show due ++
        if done then " (DONE)" else ""
