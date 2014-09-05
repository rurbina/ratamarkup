Ratamarkup
----------

Ratamarkup is yet another simple markup language. This was originally developed for a Wiki software back in 2002 and has been evolving because I'm not yet satisfied with it.

It actually has two layers: you may use the core language as it is, which is inspired by MediaWiki, Ward's wiki and Emacs' Org mode. Or you can insert what I call "sections", which basically are formatted with a callback that you provide. If you write the proper callback then you could be using anything: plain text, bare HTML, Markdown...

It is so because then you can write a section handler (plugin) for, say, embedding a YouTube video, a SoundCloud track list, Facebook blocks and whatnot.

The main application is geeklog, but I've used it in several other web apps.
