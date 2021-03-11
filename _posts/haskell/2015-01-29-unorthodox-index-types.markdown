---
title: Unorthodox index types for spatial containers
date: 2015-01-29 00:00:00 Z
layout: post
---

# Unorthodox index types for spatial containers

In this article I'd like to share some of my thoughts about using index types different
from the common ones. We'll explore the possibilities this change brings to the table

## Status quo

The most basic and ubiquituous data structure in the computing world is most certainly an array:

    int arr[5];

It's available in every programming language, and keeps elements in sequential order. What's
hidden in this definition is the access to it:

    int value = arr[ix];

In the C programming language, we index arrays with unsigned integers starting at 0 and ending
at array size - 1. Thus, `ix` could simply be of type `unsigned` (`unsigned` and `unsigned int` are
exactly the same). It's actually expressed as `signed int` for various (bad) reasons, and you can kind
of get the idea about where we're going with the remainder of this article.

In languages like Lua, it gets closer to something like a "natural" number, because indices, by
convention, start at 1; however Lua has just one number type, and doesn't enforce it either.

## The case of dimensionality

Many beginner game programmers thus choose this straightforward data structure to implement
their game states, most notably boards. They exploit the ability that arrays can actually contain
arrays inside, and so we get:

    int board[size][size];

Bam! `board[x][y]` gives us access to one particular field. Great, isn't it?

Actually, it's pretty terrible.

Let's write a simple game.

    enum Field {
        Empty,
        Player,
        Monster,
        Rock
    };

    Entity board [size][size];

    int playerX, playerY;

    void movePlayer(int keyPressed) {
        switch (keyPressed) {
            case LEFT_ARROW:
                playerX -= 1; break;
            // ...
        }
    }

Okey, so what's wrong with that? Well, for one `playerX` and `playerY` look like something we could
refactor into a "point" (or perhaps "position" class):

    struct Point {
        int x, y;
    }
    Point playerPosition;

Instead of writing `playerX` we now write `playerPosition.x`, which is arguably much cooler. We can also
make our movement function pure:

    Point getMovement(int keyPressed) {
        switch (keyPressed) {
            case LEFT_ARROW:
                return Point (-1, 0);
            // ...
        }
    }

Moving the player is now a bit more explicit:

    playerPosition.add(getMovement(key));

And we can reuse this function for other things.
So far, so good. However, one problem remains: how do we place a player onto the board?

    placeOnTheBoard(Unit u) {
        // act on u polymorphically
        board[u.position.x][u.position.y] = u.type;
    }

Most of the people will stop there, thinking it's fine. And, for most of the time, it will.
But it's not going to be enough for us.

Point being, I always hated people writing code like so:

    pX += dX;
    pY += dY;

Every time I saw something like that I immediately had the proper solution in mind:

    p += d;

If your language doesn't support operator overloading, `p.add(d);` is perfectly fine as well. So is `add(p,d);`.
Both of them treat the X and Y coordinates equally, without exposing the dimensionality and details to the user.
The only problem with this approach is that arrays can't take the point parameter, and people don't notice how
their nice abstraction of space breaks there.

What if we could change this?

## Practical change
I'm going to show you two examples from my real-life codebases where I've managed to get around that issue. I hope
they serve as an inspiration in the future endeavours:

### Case study 1 - Potato Empires (Haskell)

Potato empires is a Haskell game that takes place on a regular 2D grid. I use an Array to store that.
Typically, you'll see `Array` used with `(Int, Int)` index type in that scenario. That was what I begun with,
and that was what I ultimately deemed much less useful than:

    data Point = Point Int Int deriving (Show, Eq, Ord)

The cool thing about Haskell is that Index types in it get their own special typeclass - `Ix`.

    instance Ix Point where
        range (Point minX minY, Point maxX maxY) = [Point x y | x <- [minX .. maxX], y <- [minY .. maxY]] 
        inRange (Point minX minY, Point maxX maxY) (Point x y) = and [x >= minX, y >= minY, x <= maxX, y <= maxY] 
       
        -- implemented the same as default (a,b) Ix instance
        index r@(Point l1 l2, Point u1 u2) p@(Point i1 i2) | inRange r p = index (l1,u1) i1 * rangeSize (l2,u2) + index (l2,u2) i2 
                                                           | otherwise = error "Out of range"

My game map becomes simply

    type GameMap = Array Point MapField

Because the second type parameter to `Array` is anything that has an instance for `Ix`, I can use my own custom type, 
and I never have to think about conversions, despite using my own index type.

    getUnitAt :: GameMap -> Point -> Maybe Unit
    getUnitAt gmap p = (gmap ! p) ^. unit

I can place arbitrary restrictions on `Point` type, and I'm not constrained by anything that my data structure imposes on me.

### Case study 2 - Minicraft (C++)

The minicraft's case is arguably even more interesting and needs a bit more of explanation. This game used much more complicated
coordinate system - it was based on voxels, and to avoid loading the whole map at once, I did it in `n`x`n`x`n` chunks.

This required establishing two coordinate systems; one that described chunks in the "chunk space", and one that described voxels
inside of the chunk. Actually, that made 3, because I wanted to be able to know the absolute coordinate of one voxel. That prompted code 
reuse that used a funny technique called *tagging*:

    static const int size = 24;

    struct WorldCoordTag;
    struct OuterChunkCoordTag;
    struct InnerChunkCoordTag;

    template<typename Tag>
    class Coord;

    typedef Coord<WorldCoordTag> WorldCoord;
    typedef Coord<OuterChunkCoordTag> OuterChunkCoord;
    typedef Coord<InnerChunkCoordTag> InnerChunkCoord;

    template<typename Tag>
    class Coord {
    public:
        int x, y, z;
        
        Coord() 
            : x(0), y(0), z(0) { }
        Coord(int _x, int _y, int _z)
            : x(_x), y(_y), z(_z) {
        }

        void operator+= (Coord const& other) {
            x += other.x;
            y += other.y;
            z += other.z;
        }
    };

What followed were conversion routines that I'll skip implementations of (they're [available here](https://github.com/bananu7/MiniCraft/blob/master/src/Minefield.h#L40-L68) 
and are worth another article dedicated just to the maths describing them; it took me quite some time to perfect
their behaviour so that the world can grow in both positive and negative dimensions without issues).

    static OuterChunkCoord convertToOuter (WorldCoord const& wc);
    static InnerChunkCoord convertToInner (WorldCoord const& wc);
    static WorldCoord convertToWorld (InnerChunkCoord const& ic, OuterChunkCoord const& oc);

This is just the baseline, and the really interesting stuff is what you can build with them. Here's the `Chunk`:

    class Chunk {
        std::array<BlockType, size*size*size> data;
    };

And finally, the main data structure:

    std::unordered_map<OuterChunkCoord, Chunk, CoordHash> data;

I've decided to use the `unordered` variant because there's no obvious way to order voxels in 3D space. (Hell, there's 
no obvious ways to order fields of a grid; both row-major and column-major formats are present and widely used.) This
required a simple hashing routine:

    class CoordHash {
        template<typename Tag>
        std::size_t operator()(Coord<Tag> const& p)
        {
            std::size_t seed = 0;
            boost::hash_combine(seed, p.x);
            boost::hash_combine(seed, p.y);
            boost::hash_combine(seed, p.z);

            return seed;
        }
    };

`hash_combine` sadly isn't a part of C++ standard library. It's a pity, considering how useful this little helper is.

What this whole complication buys us might not be obvious at first, so let's take a quick look at the list:

 1. The world can now grow indefinitely. That was my primary requirement. A player should be able to walk as long as he
 wants in one direction without ever encountering a border.
 2. The chunks can be loaded an unloaded on demand.
 3. Access to any voxel in the world is amortized `O(1)` - `unordered_map` with such a hash should be able to avoid collisions
 most of the time, and once it finds a chunk, accesing an array element is just addition and multiplication.

 

