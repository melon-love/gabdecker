# TODO

This is a place to collect things left to do for GabDecker.

* Post / reply / quote / repost
* User profile dialog.
  Need to resize if screen too narrow for full 850 pixel cover image width.
  Put a little black box below the cover image, like Gab does,
  So text won't collide with image.
  Clicking on "Folowers" or "Following" count should show icons somewhere.
* Multiple named pages of feeds, selectable on the left column.
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
  "Disable YouTube & Giphy embeds" (they track you).
  Or use the convenient Gab link to a non-tracking option.
  And the Post.Embed.html for YouTube image.
  YouTube may be replaceable with https://invidio.us/
  <iframe id='ivplayer'
          type='text/html'
          width='640' height='360'
          src='https://invidio.us/embed/_eLddzLagsg?'
          frameborder='0'>
  </iframe>
* Encode the preferences as a string, so you can paste them into another browser.
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
* A faster way than move-left-or-right clicks to move a column.
  Dragging in the control column would be great, but just typing a number
  in a popup would be good enough.
* Auto-load more near end of scrolling a column.
* Periodic update of feed. Mark it somehow if not scrolled to top of column.
* Colored versions of icons instead of rectangular background color highlight.
* If a comment is reposted it should say:
  "foo reposted bar's comment"
* When a post has multiple images, the pop-up viewer should have a way
  to navigate through them.
* Display polls:
  Vote on them once I get that API.
  Need to add them to a `Post`:
  ```
    "poll": {
      "option_1_title": "Yes",
      "option_1_votes": 276,
      "option_2_title": "No",
      "option_2_votes": 4,
      "option_3_title": null,
      "option_3_votes": 0,
      "option_4_title": null,
      "option_4_votes": 0,
      "option_5_title": null,
      "option_5_votes": 0,
      "voted_option": 1,
      "total_votes": 280
     }
   ```
* Persistent feed list should be different for each logged-in username.
* Restore the "Last Closed" feed to its former position, not the end.
* Dark mode for login page.
* Help, About
* Premium features.
  Gotta make money off of this somehow.
  Save only Gab username and expiration date.
  1) groups & topics
  2) multiple accounts
  3) automatic loading
  Warn on the screen when expiration date approaches.
* Create an elm-ui styles mechanism, and a library of common idioms that uses it.

## Gab API Bugs

* Posting still gets an error 429 (too many requests).
* (un)Follow and (un)Mute do not work.
* The Popular Users feed (GET https://api.gab.com/v1.0/popular/users)
  always returns nothing.

## Gap API Features Wanted

* Fetch the comments for a post.
* Group and Topic Feeds.
* Comments in user feeds.
* Logout of authentication server (clear cookie), so can have multiple accounts.
* One call to find new feed count for each of a list of feeds.
* Ability to vote on polls.
