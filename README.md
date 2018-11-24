# GabDecker

[GabDecker.com](https://gabdecker.com/) is a [TweetDeck](https://tweetdeck.twitter.com/)-like interface to [Gab.com](https://gab.com/).

Other than persistence and file uploading, the code will run in `elm reactor`. But you need to copy `src/GabDecker/Authorization.elm.template` to `src/GabDecker/Authorization.elm` and fill in your middleware server information.

```
git clone git@github.com:melon-love/gabdecker.git
cd gabdecker
cp src/GabDecker/Authorization.elm.template \
   src/GabDecker/Authorization.elm
# Edit src/GabDecker/Authorization.elm
# for your middleware server
elm reactor
```

Then aim your browser at http://localhost:8000/src/Main.elm

You won't be able to authenticate with Gab, unless you set up a [billstclair/elm-oauth-middleware](https://package.elm-lang.org/packages/billstclair/elm-oauth-middleware/latest/) server, and use a fake host, listed as a `redirectUri` in that server's configuration. Without an authorization server, you can use cloned data, from the `site/json` directory.

Bill St. Clair - 14 September, 2018
