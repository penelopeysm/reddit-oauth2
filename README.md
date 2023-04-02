## reddit-oauth2

A collection of functions for interacting with the Reddit OAuth2 API.

It's currently rather underdeveloped, but there's enough in here to write a simple bot with.
See [src/Reddit/Example.hs](https://github.com/penelopeysm/reddit-oauth2/blob/main/src/Reddit/Example.hs) for an example of one.

I only plan to release this on Hackage when this is at least semi-complete.
Documentation for this package can be found at https://penelopeysm.github.io/reddit-oauth2/Reddit.html.

### Other links

Reddit's own documentation of the API is not particularly complete or up-to-date.
There are two main sources of info:

 - [On Reddit](https://www.reddit.com/dev/api/) (supposedly live-updated)
 - [On GitHub](https://github.com/reddit-archive/reddit/wiki) (old but contains lots of details)
 - It's also worth searching through posts on [/r/redditdev](https://www.reddit.com/r/redditdev).

There is an existing Haskell [reddit library](https://hackage.haskell.org/package/reddit), but it's not been actively maintained since at least 2018 and doesn't support OAuth2 (which, [according to Reddit](https://github.com/reddit-archive/reddit/wiki/API), is compulsory).
