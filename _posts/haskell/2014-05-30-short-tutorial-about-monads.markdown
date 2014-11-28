# This is going to be a short tutorial about monads.

## Context in imperative programming

I'd like to start by looking at a simple computer program:

    doThingA();
    doThingB();

You might recognize that it's written in your favorite programming language. It represents the series of operations you instruct your computer to perform. However, it's implied that if the above program was to work, it needs something else. It needs *context*.

There are three most popular methods of adding context to programs.

### Global variables

First would be global variables.

    int a;
    doThingA() { a = 0; }
    doThingB() { a = a - 1; }
    
    main() {
        doThingA();
        doThingB();
    }

We can now see what the methods are doing. Every time `doThingA` appears in our requested sequence of operations, the global counter `a` will be reset to 0. Easy enough.

Global variables exist mostly because first “computers” without that thing called operating system had pretty much only registers and basic operations that allowed to do operations of them. In every point of code every operation was allowed to modify every register, and we soon learned that this approach doesn’t scale well. You’d be hard pressed to find a beginner programmer’s resource that doesn’t discourage using global variables.

### OOP

And so, the second way appears; Object-Oriented Programming.

    class C {
        int a;
    
        doThingA() { a = 0; }
        doThingB() { a = a - 1; }
    }
    
    main() {
        C c = new C();
        c.doThingA();
        c.doThingB();
    }
    
The bodies of the functions stay the same (some languages with weird or or simply badly designed scoping rules require explicit `this` annotations), but we isolate the `a` context. We can now create as many of those as we want (each time instantiating the `C` class). What’s important to note is that we now precede each instruction with the name of the instance that denotes the particular context that has to be used with that particular call.

This has actually proven to be a much better idea than global variables. It was easier to test, and since functions could no longer modify each and every part of the program, the call for real design appeared. Over the last years this has evolved tremendously, giving class-based designs (prototype-based designs are similar enough with regard to the topic that it’s not worth mentioning them separately here) much more power: interfaces and polymorphism, generics, reflection, all of those and many more made OOP the dominant paradigm of programming today.

### C-style OOP

However, I’ve mentioned a third way. It’s actually closer to the one that we’re going to get to, eventually. Before the rise of Java and C++, we got an intermediate way to supply context to our operations. Languages like C and Pascal offered the “structural” way of programming.

    struct S {
        int a;
    }
    doThingA(S s) { s.a = 0; }
    doThingB(S s) { s.a = s.a - 1; }
    
    main () {
        S s = S();
        doThingA(s);
        doThingB(s);
    }

The main observable (in such a simple scenario) difference between #2 and #3 is that the functions reside outside the data definition, and require some explicitness when operating on the context inside of them. Otherwise, looking at the part of the program that actually does something, it’s extremely similar. We pass the context to the functions as a parameter, not precede the calls with it, but otherwise it’s quite similar.

This style appeared before a way to link data definition and functionality appeared. Even now, internal implementations of languages use it to provide the CPU with low-level code, and high-level languages like Lua use clever tricks to allow combining both approaches (In Lua the “OOP” syntax is just sugar over the third approach).

Okey, that was supposed to be a short tutorial, and I’ve written a considerable amount of text already. However, if you have programmed before in your life, you probably know all that. Hence, the previous part shouldn’t be anything more than a refreshment. I’ve mentioned the three ways for completeness sake, because, well…

## Monad is a context

Monads are essentially another, fourth way of expressing context.

### Rationale

Why do we even bother with inventing new ways? OOP style seems to be working well enough. Well, as it seems, there are two important properties of computer programs that are valuable, but hard to achieve with that style.

They are functional purity and referential transparency.

For those who know what they are, feel free to skip this paragraph. (insert short readable explanation).

### Monadic example

So, Monads are a way of expressing context, but keeping the aforementioned properties. However, at the same time we want to keep the code short and readable. Let’s take a look at yet another example of the same functionality, but now expressed with monads.

    doThingA = put 0
    doThingB = do
        a <- get
        put (a-1)
    
    main = do 
        let a = 0
        let a' = evalState (do doThingA; doThingB) a
        return ()


It might seem a little more complicated, because it’s actually a valid Haskell program, but otherwise, if you try a bit, you can see how it’s similar to the previous ones. The most important difference is that the value of `a` stays the same! We take `a` as the initialized context, do some operations on it, and produce a modified copy of it. That way we are able to keep the computation functionally pure. We also provide all the dependences in a transparent way. The only downside is having to write `evalState` explicitely, but as we will see later, this isn’t actually that bad, because we are being explicit about *the way we are going to work with the context*. We also don’t have to repeat the context name explicitely, as in approach #2.

### Larger example

That out of the way, let’s see how we might express a larger set of common operations using Monads.

First the OOP way:

    class TestClass {
        int x;
        const int c = 10;
        string log;
    
        test() { x = 5 + c; log = log + “Set x to sum of constant c and 5”; print(log); }
    }

    main() {
        TestClass instance = new TestClass();
        instance.test();
    }

and now the monadic way:

    test = do 
        c <- ask
        put (5 + c)
        tell "Set the state to sum of constant c and 5"
        
    main = do 
        let c = 10
        let (s', w') = execRWS test c 0
        print w'

I hope you’re grasping it a bit more. We are making the “execution in context” explicit again by saying `execRWS test c 0`, that is, “execute the operation `test` with context of type RWS, given initial state of constant set to `c`, and initial state of variable set to 0”.

We don’t have to write the type of the operation `test` (if we did, it could be `RWS Int String Int ()`), because the compiler is able to infer that for itself. As we can see, it’s not really that important to write that explicitely. However, the compiler requires a way more important thing from us; *how are we going to use the context*.

You see, when we created our `TestClass`, we also did that; we explicitely marked `c` as `const`, stating that it can only be read, not modified. We could do anything we want with `log`, though, and, more importantly, we could `print` from inside of our function.

Monads are more powerful in a way that they offer you the ability to express the usage of contexts with finer granularity. In imperative code, you have essentially two operations that you can do on a variable: get and set (read and write). Anything else revolves around it. Sometimes we state that the variable becomes a constant, and leave only the "get" (read) part. In Haskell, we also get a context that's write-only, it's called the `Writer`. Usual mutable variables reside in `State`, whereas constant parameters belong in `Reader`. There's a context that allows us to use all three simultaneously, called `RWS` (Reader-Writer-State).

In our example, `x` resides in the `State` part of RWS monad, meaning that we can freely modify it with `get` and `put`. However, `log` has a “writeonly” property, because it resides in the `Writer` part. That means we can only append new messages to the log, using `tell`.

And our `print` resides outside of the `test` function. I’ve mentioned that one goal of monads is to keep functional purity. Because IO is essentially impure, Haskell requires all IO operations to be invoked in the special IO context. As such, we cannot use print inside of `test`; we process it separately (and it remains pure) and only then use `print` from `main` (which is in IO, otherwise the programs would be rather… limited).

## Further possibilities

But the use of context restriction doesn’t end here. We can use those contexts to, for example, restrict some functions to operate only when the user is logged in, and make creation of the initial state for “logged in” context possible in only one way - user logging in.

Monads are even more powerful. The contexts control not only what is available to the code that works using them; they can *alter its behavior*. This can be extremely handy when, for example, you want to stop execution of the program, if one part of it fails:

    test = do 
        a <- obtainValue
        b <- obtainValueBasedOn a
        return b

If this was in `Maybe` Monad, if any of the operations that obtain values failed, resulting in `Nothing`, the whole function would automatically result in `Nothing`. The context, being responsible for chaining operations, is able to “look inside” their returned values and change behaviour according to them. There are some laws that dictate what operations are possible, to keep the program’s behavior reasonable, but nevertheless it still offers a lot of possibilities.

Now, this was tutorial about Monads, not Haskell specifically. The examples aren’t really idiomatic Haskell; they were moved further to imperative style to make them more readable for an imperative programmer. The code would be much shorter and probably even more transparent if written using idiomatic style.

Even in this “overblown” form, I think it’s enough to show what Monads mean in practice. Without any mathematical background, you should be able to sum them in a few sentences. I hope the article pushes you further towards understanding the behind-the-scenes mechanisms. For that, numerous tutorials exist.
