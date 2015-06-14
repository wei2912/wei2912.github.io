---
title: Introduction to Haskell
date: 2014-11-27
---

This post assumes that the reader has at least an intermediate knowledge of an imperative programming language, but does not assume knowledge of functional programming.

You'll need the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) installed (you can get it as part of [Haskell Platform](https://www.haskell.org/platform/)). Then, you'll be able to evaluate stuff and see what you get.

## Basics

Before we cover the more interesting stuff, we'll need to cover the basics of Haskell.

### Interactive Haskell

We can use Haskell as a calculator. Let's start up `ghci` first:

```bash
$ ghci
```

and we can now type stuff into the prompt:

```haskell
Prelude> 3 + 7
10
Prelude> 20 - 10
10
Prelude> 9 / 0
Infinity
```

Or check out the types of expressions:

```haskell
Prelude> :type 2 + 2 == 5
2 + 2 == 5 :: Bool
```

What goes after the double colon is our type. As you can see here, `2 + 2 == 5` yields a boolean. We'll see the double colon notation being used later on.

**Protip**: `:t` is an alias for `:type`.

### Comments

Comments can start with `--`, which lasts till the end of a line, or enclosed within `{-` and `-}`. The latter can span multiple lines.

### Functions

Functions are the building block of Haskell. Everything we do in Haskell relies on stringing together functions.

Every function is comprised of 2 parts:

1) Type signature
2) Equations

### Type Signature

The type signature tells us what type the function has. Let's look at an example:

```haskell
addByThree :: Integer -> Integer
```

The first line is a type signature. In this case, we take in a single Integer and return an Integer.

Let's look at a function to add two integers:

```haskell
add :: Integer -> Integer -> Integer
```

This time round, we take in 2 integers and return a single integer.

In Haskell, we usually go by type signatures to describe what our function does.

### Equations

Let's take a look at what the whole of `addByThree` looks like.

```haskell
addByThree :: Integer -> Integer
addByThree x = x + 3
```

In Haskell, we write functions similar to equations. The left hand side is `addByThree x`, which is our function name and the integer `x`. The right hand side is `x + 3` which adds 3 to our integer `x`.

Hence, by calling `addByThree 10` we'll get back `13`.

Likewise, our `add` function is the following:

```haskell
add :: Integer -> Integer -> Integer
add x y = x + y
```

You can give these functions a try by pasting this into `main.hs`:

```haskell
addByThree :: Integer -> Integer
addByThree x = x + 3

add :: Integer -> Integer -> Integer
add x y = x + y
```

and then running GHCi:

```bash
$ ghci
```

Then, typing into the prompt:

```haskell
Prelude> :load main.hs
[1 of 1] Compiling Main             ( main.hs, interpreted )
Ok, modules loaded: Main.
*Main> addByThree 10
13
*Main> add 10 20
30
```

**Protip**: You can also run the command `ghci main.hs` on your shell. Another nifty thing is that you can reload a file by `:reload main.hs`.

**Protip 2**: `:l` is `:load` and `:r` is `:reload`.

### Infix functions

Operators like `+` and `*` are infix functions. We can use it as a "normal" function by enclosing it in brackets. For example, we can rewrite `3 + 7` as `(+) 3 7` (give this a try in GHCi!).

### Lists

Lists in Haskell are [linked lists](https://en.wikipedia.org/wiki/Linked_list).

We define lists this way:

```haskell
Prelude> [1, 2, 3]
[1, 2, 3]
Prelude> :type "abc"
"abc" :: [Char]
```

Lists in Haskell are implemented as a series of "cons" operations, which join up elements of the list. We use the `:` operator to do this.

The `:` operator is known as the *cons* operator. It prepends an element to a list of the *same type*.

```haskell
Prelude> 3 : [1, 2]
[3, 1, 2]
```

The interesting part about the `:` operator is that it is right-associative. This means that everything on the right is evaluated first.

```haskell
Prelude> 3 : (2 : (1 : []))
[3, 2, 1]
Prelude> 3 : 2 : 1 : []
[3, 2, 1]
```

### Polymorphic types

This sounds complex but it's actually quite a simple concept.

Polymorphic types allow for any type. Let's take a look at a function, `id`.

```haskell
id :: a -> a
id x = x
```

The function `id` can take in any type.

As you can see from the use of `a` as both input and output, the two types have to be the same (interesting fact: polymorphic types are lowercase while "normal" types are uppercase).

In our equations, we specified that `id x = x`. This function just returns the value it was passed into, hence the reason why it's called the *id*entity function.

### Higher Order functions

Higher order functions are functions that can take in or return other functions (isn't this mind blowing?).

A function which takes in one parameter and returns something of the same type is denoted by `a -> a`. Likewise, a function that takes in a function and returns something of the same type is `(a -> a) -> a`.

A very commonly used function is `map`. It applies a function to every value in a list. Let's look at the type:

```haskell
map :: (a -> b) -> [a] -> [b]
```

`map` takes in a function that changes the type of an element from `a` to `b`. It then takes in a list of type `a` and returns a list of type `b`.

We can test this out with `map addByThree [1, 2, 3, 4, 5]` etc.

That's it for the basics. Let's get down to some interesting code.

## Powers of a number

Haskell lets us do quite a lot of elegant stuff. Here's one that I find particularly elegant (there're many other ways to write this function too, but I believe that this is one of the clearest).

```haskell
pows :: Integer -> [Integer]
pows base = iterate (* base) 1
```

In here, we define a function that takes in an *integer* and returns a *list of integers*.

Our equation then defines `pows base` as `iterate (* base) 1`. What does that do?

### `(* base)`?

If you were paying close attention, you may realize that I appear to have made a typo in `* base`. This is not the case. Haskell allows the use of [partially applied functions](https://en.wikipedia.org/wiki/Partial_application).

Let's look at the difference between multiplying 2 integers and multiplying an integer by 3:

```haskell
multiply :: Integer -> Integer -> Integer
multiply x y = x * y
multiplyByThree :: Integer -> Integer
multiplyByThree x = x * 3
```

We notice that the two functions appear similar. Both take in `x` and multiply it by a value.

Now, what if we had a way to specify not all arguments to `(*)` such that we'd get another function?

This is where the interesting part comes in. We can rewrite the above functions as:

```haskell
multiply :: Integer -> Integer -> Integer
multiply x y = (x *) y
multiplyByThree :: Integer -> Integer
multiplyByThree x = (x *) 3
```

We provide the left value, `x`, to `(*)`. `(x *)` gives us a function that takes in a single number and multiples that by `x`. Finally, we get a value.

Haskell allows us to supply only a few parameters required, so as to get back a function that takes in the rest of the parameters. This is known as partial application.

How does that help us? This is where higher order functions come in.

### The `iterate` function

Here's the definition of `iterate`:

```haskell
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
```

By now, you should be familiar with looking at type signatures.

What `iterate` does is that it prepends a value to a list, applies a function to the value and pass the new value into `iterate` again. This is what we call a recursive function -- a function that calls itself.

### Trace of the `pows` function

It's hard to see what this function does if you're not familiar with recursion, so let's look at a trace.

```haskell
  pows 3
= iterate (3 *) 1
= 1 : iterate (3 *) 3           -- 3 * 1
= 1 : 3 : iterate (3 *) 9       -- 3 * 3
= 1 : 3 : 9 : iterate (3 *) 27  -- 3 * 9
= ...
```

We can see that we'll get a list of powers of 3. How this comes about is that we go through the following steps:

1) Prepend the current value to the list
2) Apply `(3 *)` to the current value to get a new value.
3) Apply `iterate (3 *)` to the new value.

Partially applied functions allow us to write this very elegantly, without having to define a new function `multiplyByThree`. This is one of the lovely things about functional programming; we get to reuse all sorts of functions.

Running that will give us a list of the powers of 3.

But wait! Wouldn't that go on infinitely? In Haskell, executing `pows 3` would have given us an infinite list of the powers of 3 (try it and see what happens!). Notice that the list will keep on growing. That looks like a horrible thing.

But we can still do stuff with an infinite list, thanks to laziness. We need not evaluate the whole list. We only need to evaluate the items that we need to. That is the crux of laziness.

That means that from our infinite list:

```
[1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, ...]
```

We can do stuff like get the first 5 powers of 3:

```haskell
take 5 (pows 3)
```

and get:

```
[1, 3, 9, 27, 81]
```

or maybe get all powers of 2 below 100:

```haskell
takeWhile (< 100) (pows 2)
```

and get:

```
[1, 2, 4, 8, 16, 32, 64]
```

The elegance of this solution lies in the fact that we easily did this with recursion, in a rather terse, clear and "mathematical" way.

The ability to reuse functions like this grants us a lot of power in Haskell and allows us to reason about code in a very powerful way.

## Fibonacci Numbers

Fibonacci numbers start with 0 and 1. Each number is the *sum of the previous two numbers*. In this example, we'll handle only positive integers.

Here's how the sequence looks like:

```
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 ...
```

As you can see, the third number, 1, is the sum of 0 and 1. The fourth number, 2 is the sum of 1 and 1. This sequence goes on forever.

On to the code:

```haskell
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

This time round, we define `fibs` as a *list of integers*. We then prepend `0` and `1` to `zipWith (+) fibs (tail fibs)`.

What does `zipWith` do? Before we cover it, we need to take a look at some list terminology and pattern matching.

### Lists

Take a look at this graphical representation:

```
    +--+--+--+--+--+--+--+--+
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
 ^
```

The top line denotes the elements that make up the *tail* while the bottom carat denotes the *head*. To put it in words, the *head* is the first element of the list, while the *tail* is everything after the *head*.

Here's another graphical representation:

```
 +--+--+--+--+--+--+--+--+
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
                            ^
```

This time round, the top line denotes the **init** and the carat denotes the **last**.

All these 4 terminology are functions. You can try them out in GHCi.

```haskell
> head [1, 2, 3]
1
> tail [1, 2, 3]
[2, 3]
> init [1, 2, 3]
[1, 2]
> last [1, 2, 3]
3
```

### `zipWith`

Now that we've conquered list terminology, let's look at `zipWith`.

```haskell
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (a : as) (b : bs) = f a b : zipWith f as bs
zipWith _ _ _ = []
```

Hmm. We have 2 equations, with some weird looking stuff. How does this work?

Haskell allows for pattern matching. In our case, `(a : as)` seperates a list of values into `a` (the head) and `as` (the tail). We do the same with `(b : bs)`.

Afterwards, we apply `f` to `a` and `b`, then prepend our value to `zipWith f as bs`.

However, there's still a second equation. Underscores are a way for us to dispose unneeded values. The last clause catches everything that doesn't fit the first equation and returns an empty list.

But why do we need a catchall? As it turns out, the head of an empty list is undefined. Hence, `(a : as)` will only match lists with at least one element. Our catchall ensures that the function doesn't error out when we exhaust both lists. Instead, it returns an empty list.

This is what we call the `base case` of a recursive function, as all other calls to the recursive function gets reduced to this. It's also the reason why `zipWith` terminates while `iterate` doesn't.

While we now know what the function is doing, we still have no idea how it works. Looking at the trace should gain some inspiration as to how this works:

```haskell
zipWith (+) [1, 2, 3] [4, 5, 6]   -- 1 + 4
= 5 : zipWith (+) [2, 3] [5, 6]   -- 2 + 5
= 5 : 7 : zipWith (+) [3] [6]     -- 3 + 6
= 5 : 7 : 9 : zipwith (+) [] []   -- returns []
= 5 : 7 : 9 : []
= [5, 7, 9]
```

The comment shows how the `(+)` function is being applied to the values of the list and how the values turn into a list.

### Back to the `fibs` function.

`zipWith (+) fibs (tail fibs)` is still a mystery to us. What could it possibly mean? Let's look at `fibs` and `tail fibs` first (with the values that we already know):

```
  0 1 ...
+ 1 ...
---------
  1
```

The first line is `fibs` and the second line is `tail fibs`. Adding up the numbers on the first column, 0 and 1, gives us 1, our third number. We append this to the list.

When we evaluate the fourth number, here's what our list looks like:

```
  0 1 1 ...
+ 1 1 ...
---------
  1 2
```

We get the fourth number, 2, and it's appended to the list. Now, for the fifth number:

```
  0 1 1 2 ...
+ 1 1 2 ...
---------
  1 2 3
```

We get 3. This continues, eventually building up a list of Fibonacci numbers.

This algorithm, while a bit hard to comprehend, is inherently elegant and simple.

## Conclusion

I hope this has showed you how elegant Haskell could be and why many programmers like working in it.

There's a lot more to Haskell than this (such as functors, monads, etc.) which make solving other real-life problems quite elegant as well.

You can take a look at the following readings:

* [Why Functional Programming matters](http://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf)

You may also wish to follow [bitemyapp's guide](https://github.com/bitemyapp/learnhaskell) to learn more Haskell.

If you have any comments or suggestions, go to [this post's Reddit post](https://www.reddit.com/r/haskell/comments/2oo7dy/revisied_introduction_to_haskell_looking_for/). Thanks for reading!
