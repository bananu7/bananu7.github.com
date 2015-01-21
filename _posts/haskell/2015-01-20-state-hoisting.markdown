---
layout: post
title: State hoisting
categories: haskell
---

# State hoisting

...is useful. And I'll show you why and how. Multiple persons have told me that this topic is too basic to demand its own post,
but I personally was struggling to find a good reference on it. So, without further ado...

*Note: I use a word "context" a lot. Sometimes it means a Monad. Refer to my previous article about monads.*

## The original problem

Assume you have a data type for state:

    import Control.Monad.State

    data MyData = MyData { str :: String } deriving (Show)

and a context:

    type Context a = State MyData a

    myFunction :: Context Int
    myFunction = do
        a <- fmap length $ gets str
        return $ a + 1

Someone gives you a nice funky computation you want to run in your context:

    super :: Int -> State Int Int

You can add an `Int` to your context, turning it into:

    data MyData = MyData { str :: String, val :: Int } deriving (Show)

What do you do?

Well, actually it's not totally obvious for everyone. I'll give you a solution right now and then we'll
talk about possible improvements to it. Here's a function you need:

    hoistVal :: State Int a -> State MyData a

(Make sure you understand the signature, and why it works that way).

    hoistVal fn = do
        s <- get
        let a = val s
        let (r, a') = runState fn a
        put $ s { val = a' }
        return r

Now we can use it!

    myFunction = do
        a <- gets str
        b <- hoistVal $ super 5  
        return $ a + b

## What's going on?

Okay, I promised you an explanation, so here it is. We can't use the `super` function directly, because it expects a context
that's precisely of type `Int`. A record won't do. A pair of `Int`s won't do. You have to tell it exactly on what part of your state
it's supposed to operate on. And that's what `hoist` is doing; it takes a "state narrower" (`acc`, for which we substitute `val`), and
an actual combinator (`fn` -> `super 5`). Note that we had to apply `super` with `5` to make it compatible - this should be obvious for you.

## So, that was it?

Technically, yes, because that's the entire mechanism. I've noticed a few additional tricks you can use in your code, though.

### Be more generic!

`super` shouldn't have the signature it has. Don't write your functions like that! It makes it much harder for people to use it afterwards.
Consider an example, in which you might need `IO` to print from your state combinator for whatever bad reason, but the type incidentally matches
the one needed by the function:

    type ContextIntIO a = StateT Int IO a

Can you run `super` in this context? No, because there's a mismatch between `StateT Int Id a` (that's what `State Int a` boils down into if you use transformers) and `StateT Int IO a`. But since you don't care about
that `Id` (you really only want any `State` that has `Int` inside), it should use `MonadState`.

It's a really useful little thing that resides in `Control.Monad.State.Class`:

    class (Monad m) => MonadState s m | m -> s where
        -- | Return the state from the internals of the monad.
        get :: m s
        -- | Replace the state inside the monad.
        put :: s -> m ()

What it does is basically defining an interface that every stateful context can implement.

    super :: MonadState Int m => Int -> m Int

*Note:* you need `FlexibleContexts` for that, despite the fact that if you don't enable it and omit the signature, GHC will infer it correctly.

This means *this function will work for every context `m` that's `Int`*. Now we could use it in both our hoisted state, raw `State Int`, or that `StateT` transformer.

If you look closer, you'll realize that `hoist` actually has the same problem!

    hoistVal :: MonadState MyData m => State Int a -> m a

Cool, it can now hoist inside of both of regular and transformed variants.

### But not too much.

Why not make the first `State` another type parameter? After all, we might expect that someone might give us an `StateT Int IO ...` action, and then...

**Wrong.** Haskell doesn't allow you to mix pure and impure code for a reason, and for the exact same reason that there's no possibility of `IO a -> a` ever working
(leave `unsafePerformIO` out of that; For all I care, it might not exist), there's no way to get `StateT s IO a -> State s a` to ever compile.

That being said, you can hoist `StateT IO sa x` into `StateT IO sb x` (or whatever instead of IO);
the only caveat is that you have to replace one line:

    let (r, a') = runState fn a
    -- to
    (r, a') <- runStateT fn a

### I'm tired of typing `Val` every time

So am I. I hope you've heard about `Lens`. Before I introduce it, let's see what would happen if we tried to parametrize over `val`:

    b <- hoist val $ super 5 

`hoist` would need to look more or less like:

    hoist acc fn = do
        s <- get
        let a = acc s
        (r, a') = runState fn a
        put $ s { acc = a' }
        return r

But sans the fact Haskell doesn't allow us to use `acc` with `put` (that's why there's no `puts`, which is kind of unfortunate), `acc`'s signature itself makes
it "read-only". What we need is a way to extract the part of the state and then put it back together.

So a getter and setter pair.

That's a Lens.

In our case, even `Simple Lens` will do:

    hoist :: ( MonadState outerState m
             , Functor m ) => 
             Simple Lens outerState innerState -> 
             State innerState a ->
             m a
    hoist acc fn = do
        sp <- fmap (^. acc) get 
        let (res, sp') = runState fn sp
        acc .= sp'
        return res

And now our desired syntax works perfectly. We can freely nest records, and the lenses will take care of wrapping and unwrapping.

### Full examples

## Example 1

    {-# LANGUAGE FlexibleContexts #-}

    import Control.Monad.State

    data MyData = MyData { str :: String, val :: Int } deriving Show

    type Context a = State MyData a

    hoistVal :: (MonadState MyData m) => State Int a -> m a
    hoistVal fn = do
        s <- get
        let a = val s
        let (r, a') = runState fn a
        put $ s { val = a' }
        return r

    super :: Int -> State Int Int 
    super a = do
        x <- get
        return $ x * a

    myFunction :: Context Int
    myFunction = do
        a <- fmap length $ gets str
        b <- hoistVal $ super 5
        return $ a + b

    main = print $ runState myFunction (MyData "" 4)

## Example 2

    {-# LANGUAGE TemplateHaskell #-}
    {-# LANGUAGE RankNTypes #-}

    import Control.Lens
    import Control.Monad.State

    data MyData = MyData { _str :: String, _val :: Int } deriving (Show)
    makeLenses ''MyData 

    hoist :: ( MonadState outerState m
             , Functor m ) => 
             Simple Lens outerState innerState -> 
             State innerState a ->
             m a
    hoist acc fn = do
        sp <- fmap (^. acc) get 
        let (res, sp') = runState fn sp
        acc .= sp'
        return res

    super :: Int -> State Int Int 
    super a = do
        x <- get
        return $ x * a

    myFunction :: State MyData Int
    myFunction = do
        a <- fmap length $ use str
        b <- hoist val $ super 5
        return $ a + b

    main = print $ runState myFunction (MyData "" 4)