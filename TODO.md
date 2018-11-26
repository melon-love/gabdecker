# TODO

This is a place to collect things left to do for GabDecker.

* Allow configuration of the columns.
* Increase line and paragraph spacing.
* Post / reply / quote / upvote / downvote / repost
* Font size and column width preferences.
* Add Notifications feed to `Gab` module, then add feed here.
* Lookup user name for column header.
* if `post.is_reply`, display `post.parent` & `post.conversation_parent`
  Those fields need to be added to `Gab.Types.Post`.
  Showing comments at all and showing parents should both be preferences.
* Pop up small profile page on hover over name, just like Gab.com.
* Verified, pro, and donor identification for users.
* Open clicked image in overlay pane.
* Link user image to profile page.
* HTML elements, "`&amp;`" -> "`&`".
* `<strong>foo</strong>`, `<blockquote>foo</blockquote>`, italics.
* Single `<br />` -> multiple `row`s.
* Auto-load more near end of scrolling a column.
* Periodic update of feed. Mark it somehow if not scrolled to top of column.
* Visual signal that loading is in progress.
* Dark mode

## Bugs

There is still no API for getting comments or group or topic feeds, and posting still gets an error 429 (too many requests).
