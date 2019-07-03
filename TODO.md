# TODO

This is a place to collect things left to do for GabDecker.

* Load more doesn't work on the Notifications feed.
* Adding a feed to a saved page should make it persistent.
  Or something like that. Need to think about how this should work.
* Email addresses parse as @nick link.
  Should be <a href='mailto:foo@bar'>foo@bar</a>.
* If username case mismatch, image doesn't appear.
  Downcase for table key.
* UI to resample screen size. Workaround for the bug when switching
  from landscape to portrait.
* Post / reply / quote
  * Auto-complete @username from a list of known usernames.
    These are already gathered for user columns. Gather them also from
    feeds, if it doesn't take too long, which I doubt it will.
* Magic numbers for scrolling horizontally depend on font size.
* URLs:
  /feed/<feed> opens and scrolls to feed.
  /feed opens the feed dialog
  /user/<username> opens the user profile dialog
  /post/<postid> opens the post dialog
* Quoting a comment doesn't display correctly.
  https://gab.com/DeplorableBuilder/posts/44491314
  It's not "Satoshi commented". It's "StarXO commented on Satoshi's post"
* Figure out the proper way to compute `repostString`. It's wrong now.
  repost, repost a comment, quote, quote a comment, comment, comment on a comment.
* If a comment has an empty body and only a photo, display the photo, or
  the date gets covered by the parent below.
  Likewise for the parent post. If it has no body, but does have an attachment,
  show the attachment (usually an image).
* Errors are confusing on liking or reposting a Gab from a private
  account.  https://gab.com/chadbigly/posts/47431352
* Make errors on fetching a feed easier to understand.
  Maybe show x-ratelimit-limit and x-ratelimit-remaining return headers.
* It looks like the notification API doesn't return comments on comments.
  Looks like it doesn't return comments either, unless they get liked.
  It should. Verify this, then report it.
* Clicking on a group and topic should open a dialog,
  once those feeds are in the API.
  Should also be able to mute/unmute user in user profile dilaog.
  User profile dialog should offer "Scroll to user feed".
* Clicking on a post time should give the option of opening in Gab
  or performing an action, all five actions displayed in a feed.
  Or, open post dialog with comment navigation (once I get tha API).
* "Show comments in feeds" preference, or maybe per-feed.
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
* Open new feed right next to the one you clicked on to get the user profile.
  On closing a feed, scroll the one to its left into view.
* Mute lists. Easily sharable. Encodable in a post, so you can request to execute.
* Dark mode for login page.
* User profile dialog.
  Need to resize if screen too narrow for full 850 pixel cover image width.
  Put a little black box below the cover image, like Gab does,
  So text won't collide with image.
  Clicking on "Followers" or "Following" count should show icons somewhere.
* Fix rotate on mobile. Rotate back doesn’t update window height.
  Works in Safari. Flaky in Firefox. A bug workaround would be good.
* Help, About
* Premium features.
  Gotta make money off of this somehow.
  Save only Gab username and expiration date.
  1) groups & topics
  2) multiple accounts
  3) automatic loading
  4) RSS feeds
  Warn on the screen when expiration date approaches.
* Create an elm-ui styles mechanism, and a library of common idioms that uses it.

## Gab API Bugs

* (un)Follow and (un)Mute do not work.
  always returns nothing.
* The rate limiting should be per user, not per app.

## Gap API Features Wanted

* Fetch the comments for a post.
* Search for user IDs matching a prefix.
  Need this while typing a post.
* Delete post.
* HTML posts.
* Group and Topic Feeds.
* Comments in user feeds.
* One call to find new post count for each of a list of feeds.
* Logout of authentication server (clear cookie), so can have multiple accounts.
* Ability to vote on polls.
