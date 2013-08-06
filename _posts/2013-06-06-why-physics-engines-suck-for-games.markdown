---
layout: default
title: Why physics engines suck for game development?
---

At first, don't get me wrong. I love physics engines. They are best thing to happen to
game development since Twitter and flushed toilets. However, there are some points to them
that I just can't stand, and which are, in my opinion, spoiling game developers.

So, let's start at the beginning. You're a young, ambitious game developer. You'd like to create
a game; doesn't matter if it's 2D or 3D. You start doodling ideas in your head, and soon you
know more or less what the game is gonna be about. You subconciously start to determine technology
you want to use, graphics engine, networking library, physics...

And there lies the problem. In the era of bouncing objects everywhere, creating a game without
physics engine seems really... inappropriate? Let's look first at what they contribute to the game:

##Benefits

 * The objects behave more realistically - you can push them, and they will move, start to 
   roll and eventually hit other objects and interact with them in some way.
 * You don't have to be concerned with the things such as speed
 * As the laws of physics apply to everything equally, you dont't have to create a different set
   of rules for your modeled environment - you just need to provide data describing what the entity
   is, instead of how it should behave. That way actual behavior of an entity is more bound to data
   than to code.

So far, so good. These seem as really strong points towards using physics engine in your project.
Many developers quickly settle at that point and don't ask themselves what are they losing, when
they decide to use them. Right from the beginning, you have to add yet another dependency to your
project. It's certainly less of a problem when using dynamic languages, but still it's a chunk of
code or library that you have to maintain alongside your code. It's especially visible in small
projects, dominated by the large graphics and physics engine. The technology that was designed to
help the developer is actually taking more time to make it work properly. Which takes us to another point,

##Physics engines complicate things

That's a thing that might sound odd at first (especially when compared to data-driven environment behavior). The point is, the more sophisticated and realisitic your physics software is, the more you will have to feed to it, to make it work satisfactory. It's like a large green monster behind your workstation, demanding constant attention from you. And so, you can't just put a tree right there, because it has too rough collision model, and is standing in the way of your character. You also shouldn't expect the table to flip properly before you take your time to calculate its angular inertia along the Z-axis.

###And the results aren't neither realistic...

Supposing you somehow managed to estimate density and thus, weight of every object of your game, you launch it and... zonk! Nothing happens as expected, heavy objects bounce around and light ones hardly move. You go back go tweak the little screws, adding 0.001 to your restitution factor everywhere, and takes exorbitant amount of time to make the world believable, even if it's just cartoonish game. And then you add a weapon that shoots projectiles with a speed that exceeds your calculated maximum for just a tiny bit. Guess what? Yeah, you're screwed.

###...nor worth it.

The point is, your game was meant to be arcade shootout or platformer from the beginning. Adding falling cubes sounded like fun idea, but they weren't really the heart of your project. And to avoid writing a few lines of code that would make your character stand on the platform and not fall through, you spent three days making the capsule shape for your character load properly.

On the other hand, implementing the basic features you need usually isn't that hard. Warning, theory spoiler upcoming

Typical physics engine consists of those parts:

 * Advancer or Updater (moves objects based on their speed, advances animations)
 * Broadphase collision detection (AABBs (also Circles in 2D) or similar)
 * Narrowphase collision detection (based on actual vertices of the collision model)
 * The (Collision) Solver (pushes objects apart if they collide)

Most of the games really require only 2. The solver can be simplified from around 15k LOC with integrals to really simple, high school maths. And given you code it especially for the game, it usually requires only minimal tweaking. There are just fewer parameters needed and thus fewer of them have to be set up properly.

##To sum it all up

Ask yourself how much your game will gain and if the gains are concentrated around the essence and the main idea of it. 