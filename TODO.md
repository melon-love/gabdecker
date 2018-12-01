# TODO

This is a place to collect things left to do for GabDecker.

* Post / reply / quote / repost
* Add Notifications feed to `Gab` module, then add feed here.
* Icons insted of "User:", to the right of the name.
  For Home and Popular, too.
* Feed icons in left column. Click on one to scroll it horizontally into view.
  Highlight the clicked feed somehow.
* Font size and column width preferences.
* Encode the preferences as a string, so you can paste them into another browser.
* Handle `body.attachment` of `YoutubeAttachment` and `GiphyAttachment`.
* if `post.is_reply`, display `post.parent` & `post.conversation_parent`
  Those fields need to be added to `Gab.Types.Post`.
  Showing comments at all and showing parents should both be preferences.
* Pop up small profile page on hover over name, just like Gab.com.
* Verified, pro, and donor identification for users.
* Open clicked image in overlay pane.
* Link user image to profile page.
* HTML elements, "`&amp;`" -> "`&`".
* `<strong>foo</strong>`, `<blockquote>foo</blockquote>`, `<em>foo</em>`,
  `<u>foo</u>`.
* Mark the boundary between just loaded posts and older ones.
* Single `<br />` -> multiple `row`s.
* Auto-load more near end of scrolling a column.
* Periodic update of feed. Mark it somehow if not scrolled to top of column.
* Visual signal that loading is in progress.
* Multiple named pages of feeds, selectable on the left column.
* Escape to dismiss dialog.
  Focus first input field in dialog.
  Enter to execute selected input field in dialog
* Colored versions of icons instead of rectangular background color highlight.
* If you get a post in a feed, and then it is deleted, when you update the
  feed, it will still be there.
  This happens also when you repost and then unrepost.
  The easy fix is to NOT merge, but always initialize on refresh.
* Help, About
* One ad per column, unless paid to eliminate them.
  Gotta make money off of this somehow.
  Save only Gab username and expiration date.
  Warn on the screen when expiration date approaches.
* Dark mode
* Create an elm-ui styles mechanism, and a library of common idioms that uses it.

## Bugs

There is still no API for getting comments or group or topic feeds, and posting still gets an error 429 (too many requests).

(un)Follow and (un)Mute do not work.
