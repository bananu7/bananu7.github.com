---
layout: post
title: So, I've started learning Haskell
categories: haskell
---

## Haskell!

Well, I've heard about haskell for quite a long time already. People are using it. People *love* it.

{% highlight haskell %}
-- Exercise 10
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack list = let (left,right) = span (== head list) list in
            left : pack right
{% endhighlight %}

{% highlight haskell %}
-- Exercise 11
encode :: (Eq a) => [a] -> [(Int, a)]
encode list = map (\x -> (length x, head x)) (pack list)
{% endhighlight %}