# TODO

This is a place to collect things left to do for GabDecker.

* Allow configuration of the columns.
* Post / reply / quote / upvote / downvote / repost
* Add Notifications feed to `Gab` module, then add feed here.
* Font size and column width preferences.
* Increase inter-row spacing in paragraphs.
  I tried to do this with `spacing`, as documented, and that works in Chrome,
  but not in Brave, Safari, or Firefox.
* Lookup user name for column header.
* if `post.is_reply`, display `post.parent` & `post.conversation_parent`
  Those fields need to be added to `Gab.Types.Post`.
  Showing comments at all and showing parents should both be preferences.
* Pop up small profile page on hover over name, just like Gab.com.
* Verified, pro, and donor identification for users.
* Open clicked image in overlay pane.
* Link user image to profile page.
* HTML elements, "`&amp;`" -> "`&`".
* `<strong>foo</strong>`, `<blockquote>foo</blockquote>`, `<em>foo</em>`, `<u>foo</u>`.
* Single `<br />` -> multiple `row`s.
* Auto-load more near end of scrolling a column.
* Periodic update of feed. Mark it somehow if not scrolled to top of column.
* Visual signal that loading is in progress.
* Dark mode
* Create an elm-ui styles mechanism, and a library of common idioms that uses it.

## Bugs

There is still no API for getting comments or group or topic feeds, and posting still gets an error 429 (too many requests).
