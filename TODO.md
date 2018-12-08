# TODO

This is a place to collect things left to do for GabDecker.

* Post / reply / quote / repost
* After a reload, of one or all feeds, a new post count should be shown
  in the sidebar.
  Also mark the boundary between new and old somehow, likely a colored
  line.
* Profile at top of newly-added user feed.
  Ability to toggle it off and back on.
  Choice is persistent.
* Bug in URL parsing. Sometimes gets cruft at the end.
  E.g. https://gab.com/billstclair/posts/42716809
  Looked at this in `elm repl`. Couldn't reproduce. Try again.
* When somebody "mentioned you in a comment", include an interaction row.
* Figure out the proper way to compute `repostString`. It's wrong now.
  repost, repost a comment, quote, quote a comment, comment, comment on a comment.
* If a comment has an empty body and only a photo, display the photo, or
  the date gets covered by the parent below.
  Likewise for the parent post. If it has no body, but does have an attachment,
  show the attachment (usually an image).
* It looks like the notification API doesn't return comments on comments.
  Looks like it doesn't return commenst either, unless they get liked.
  It should. Verify this, then report it.
* Clicking on a name should give option of adding feed or opening profile.
  Same with group and topic, once those feeds are in the API.
  Should also be able to mute/unmute user.
* Clicking on a post time should give the option of opening in Gab
  or performing an action, all five actions displayed in a feed.
* Font size and column width preferences.
  "Show comments in feeds" preference, or maybe per-feed.
* Encode the preferences as a string, so you can paste them into another browser.
* Handle `body.attachment` of `YoutubeAttachment` and `GiphyAttachment`.
* if `post.is_reply`, display `post.parent` & `post.conversation_parent`
  Those fields need to be added to `Gab.Types.Post`.
  post.parent was there, and I used it. Check if I really need
  post.conversation_parent.
* Make the sidebar scroll vertically, probably just limit the size of the
  feed portion and scroll that, ensuring that the "+" to add a feed, and
  the settings and whatever else at the bottom are always visible.
* Incremental search. Show only posts containing the string the user types.
* Pop up small profile page on hover over name, just like Gab.com.
  Or maybe this should be another choice on click.
* Verified, pro, and donor identification for users.
* Link user image to profile page.
* HTML elements, "`&amp;`" -> "`&`".
* `<strong>foo</strong>`, `<blockquote>foo</blockquote>`, `<em>foo</em>`,
  `<u>foo</u>`.
* Single `<br />` -> multiple `row`s.
* A faster way than move-left-or-right clicks to move a column.
  Dragging in the control column would be great, but just typing a number
  in a popup would be good enough.
* Auto-load more near end of scrolling a column.
* Periodic update of feed. Mark it somehow if not scrolled to top of column.
* Multiple named pages of feeds, selectable on the left column.
* Escape to dismiss dialog.
  Focus first input field in dialog.
  Enter to execute selected input field in dialog
* Colored versions of icons instead of rectangular background color highlight.
* If a comment is reposted it should say:
  "foo reposted bar's comment"
* If lots of people like a post, line-wrap the icon list in the notification entry.
* When a post has multiple images, the pop-up viewer should have a way
  to navigate through them.
* Reduce the size of the JSON for saved feeds.
  ["notification", {"user: "billstclair"}, {"group": "groupid"}, ...]
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
