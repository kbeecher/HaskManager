# Task Manager Language Comparison

**NOTE**: This repo is one of a series of repos, including:

* https://github.com/kbeecher/gogtd
* https://github.com/kbeecher/HaskManager

## Intro

This repo developed out of some playing around I did with **Haskell** in order to brush up on my functional programming. In order to re-learn my Haskell skills (acquired ten years previously and rarely used since!) I decided to choose a non-trivial program somewhere between "Hello World" and a full-blown operating system in complexity. I chose a simple -- no, wait -- a *very* simple task manager.

Once I had written the program, I wondered what would happen if I rewrote the program in a modern, *non*-functional language to compare the style and approach of both languages. For that I chose **Go**.

## Disclaimers

This is not your normal type of repo.

* The code is not intended to be particularly efficient -- it was all written as a learning exercise for myself, so it is optimised for clarity and learnability.
* The executable is not intended to be a real, useable program (although it does run). Where I've faced choices between simplifying code and simplifying UX, I decided in favour of the code.
* I don't claim to be a great expert in either Haskell or Go. If you find bugs or you think part of the code can be rewritten in a way more fitting to the language, please give me feedback.
* The "comparison" is not greatly methodical or scientific. It's just an account of what I noticed as I went which I hope is interesting and/or helpful for others.


## What does the program do?

It's really simple. You run the program and start typing commands to maintain your task list. Commands include:

* `all`
* `add <description> <date>`
    * e.g. `add get milk 2015-11-12`
* `todo`
* `today`
* `show <id>`
    * e.g. `show 3`
* `tick`|`untick <id>`
* `quit`

Please note, for commands with multiple words in them, words are separated by **tabs** not spaces. Ugly I know, but it simplifies the code a bit (see Disclaimers).

The task list is stored in a text file called tasks.txt in the same directory as the executable.

## What are these STOPs in the code?

As I was familiarising myself with Haskell, and later when I was comparing the two versions, I wrote comments in the code. I have a bad memory, so I find writing stuff down is the best way for me learn and have it stick.

In case anyone else is interested in learning the basics of either language, or just looking through the code comparison, I put the phrase "STOP" in each comment and a number denoting the order in which I recommend you follow them. So, if you search for the phrase STOP in your editor, you should get a nice convenient list of stops to go through.

## Facts and figures

### Haskell version

* **Name**: Hask Manager
* **Source lines of code**: 227
* **Executable size**: 1.2 MB

Built using [GHC](https://www.haskell.org/ghc/) 7.6.3 (and optionally Cabal 1.20).

### Go version

* **Name**: Go Get Things Done
* **Source lines of code**: 325
* **Executable size**: 2.2 MB

* Built using go 1.4.2 linux/amd64.

## Interesting notes

I wrote my thoughts in a more detailed blog article, but here's the summary of things I found.

### Learnability

I believe functional programming is harder and takes more investment to learn than the imperative style.

**Experienced** programmers who know imperative but not functional might have a lot of assumptions and deeply embedded habits to undo.

But I suspect a **novice** would fare little better if they learned the functional style first. Functional programming involves some quite abstract mathematical thinking that is not highly intuitive for a beginner. Conversely, imperative programming is more about small concrete steps. Consequently, one can achieve something using an imperative language with less investment and grasp it quicker.

### Finding mistakes

One thing that makes Haskell harder to learn is the abstract error messages which I found difficult to decipher when learning. However, once I got the hang of things, I found that the time spent understanding and correcting errors in my programs became much shorter.

I even got the feeling that, with experience, correcting errors in the Haskell code took less time than the Go code. In Haskell, once an error was understood, the fix was usually clear, whereas in Go more time was spent running through a piece of faulty code procedurally trying to work out the problem and trying multiple changes to fix it.

In fact, the rather frightening "works after first attempt" (that unsettling occurrence when the first draft of your code works without problem) happened a few times after writing a Haskell function, not something that seemed to happen with Go.

### Functional as a prototype

I've read online a few examples of people developing a Haskell version of a program as a prototype before then re-implementing it in a more 'mainstream' language. (I've also read examples of people finding that their Haskell prototypes worked just fine, thank you, and subsequently junking plans to re-implement.)

If that were your desire (although I think Haskell is a perfectly suitable implementation language), I see the advantages of the approach. I noticed a few places where the Go version took inspiration from the Haskell version and was an improvement over the version that I probably would have written.

One example is the `commands.go` file, which is a sort of backend for updating task data. I can imagine that I would have written the functions in there as less generic versions (e.g. GetTodaysTasks or GetUndoneTasks) had I not first written a Haskell version which, by the language's nature, encouraged me to write the functions generically. That backend turned out more like a database API, with a small number generic functions (AddTask, QueryTask, UpdateTask etc.). This meant that the backend could stay concise and reuseable, and still allow the programmer to easily come up with an endless variety of different 'queries' by filling in the blanks of the generic functions. However, the Go syntax makes this same functionality less readable than its Haskell counterpart.

### Making Go more functional

Just because support for the functional style is limited in Go, doesn't mean you can't add a litle bit yourself. For example, the standard functional primitives (map, filter and reduce/fold) may be missing, but with first-class functions in Go you can easily roll your own (see Filter in `task_list.go`).

### Conciseness

The Haskell version is more concise, but this is hardly surprising. Functional programming is generally thought of as being more expressive.
