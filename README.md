Ratamarkup
----------

Ratamarkup is a very simple markup language, like Markdown and WikiWiki. It's main feature is that you can define a section and specify a parser for it, so you could actually use CommonMark, Markdown, Wiki, plain HTML or whatever markup language you want. Most importantly, you can define a custom parser for your block, so you could fill a block with audio file urls and get an embeded media player.

It's made in Racket. A standalone stdin filter exists but it's not maintained. There's also a legacy php implementation on the repo, the core of it is pretty much functional but has no new modules.

It's currently meant to be used for web apps, I use it in my other projects.
