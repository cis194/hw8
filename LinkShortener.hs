{-# LANGUAGE OverloadedStrings #-}

module LinkShortener where


{- 8.0 -- A Functional Link Shortener Web App -}

{-
Did you think that Haskell is only good for writing compilers/intepreters?
Guess again! Haskell provides rich libraries for all sorts of domains, one
of which is backend web app development. In this assignment, we'll explore
this domain in Haskell by building a link shortener through a web framework
called Scotty.

TinyUrl (https://tinyurl.com/) is an example of a link shortener. We will
essentially build the same thing! The app works by taking in a long URL
(e.g. http://www.google.com/some/other/long/stuff/here) and assigning a
random alphanumeric ID to it, e.g. 3gx5a, which are stored together in
a database. When a user navigates to ourapp.com/3gx5a the app captures the
ID from the URL, lookups the "long URL" associated with that ID in the
database, and redirects the user to the long URL.

If you've worked with a framework like Node.JS or Flask before, you'll feel
right at home with Scotty's similar interfaces. If you haven't done much web
development, that's fine too! We have stubbed out enough functions to guide you
along while also providing glue code to fit everything together. We recommend
this two-minute read that explains the fundamentals of HTTP and defines a lot
of the terminology we will be using in this assignment:
https://learn.onemonth.com/understanding-http-basics/.

The key Haskell ingredient that will make our app work is the Monad.

You know by now that monads model imperative side effects in the functional
setting of a language like Haskell. Then it'll be no surprise that most Scotty
code is monadic, and we'll see below how that manifests itself in using the
ScottyM monad for our route handlers. A `route` in web programming is just an
abstraction for a resource, and web frameworks typically let you assign
functions (handlers) to run when a user requests a particular route, specifying
how the server should react to a request for that particular resource.

In this assignment we'll also explore reading from/writing to a local SQLite
database using Haskell (through a package called `sqlite-simple`). A database
is just a way of storing data in a `persistent` way, which means that data
sticks around even when our application is not running. Practically, this
means you could restart your server and still see the same data in the
database. Not surprisingly, interacting with a database in Haskell will be
heavily monadic - most computation will be wrapped in the IO monad. For our
app, we'll use the database to store the mapping between IDs and long URLs,
as well as click data about URLs for analytics.

We can interact with the database using SQL code, a domain-specific language
for managing relational databases. We have provided you with all of the SQL
code you will need for this assignment at the end of the file.

Take a few minutes to read through the entire file before beginning; seeing how
the assignment fits together will help you figure out the individual parts.
-}

{-`
You'll want to run the following commands from your terminal before
starting the assignment. Each of these commands installs a dependency that
we need for our web server. Please try doing this sooner rather than later
so that if you run into trouble, we will have time to help you.

`cabal install scotty`
`cabal install wai-middleware-static`
`cabal install sqlite-simple`
`cabal install random-strings`

This may take a while...
-}

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Database.SQLite.Simple
  (Only(..), execute, execute_, query, withConnection)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import qualified Data.ByteString.Lazy.Char8 as C (unpack)
import qualified Data.Text.Lazy as T (pack)
import System.Environment (lookupEnv)
import Test.RandomStrings (onlyAlphaNum, randomASCII, randomString)
import Text.Read (readMaybe)
import Web.Scotty
  ( ActionM, ScottyM, body, file
  , get, html, middleware, notFound
  , param, post, redirect, scotty, text
  )



{- 8.1 -- Basic Routing with Scotty -}

{-
Like we mentioned, the idea of a "route handler" in Scotty is to map
server code to different "paths". This mapping will specify what the
server should do when a user sends an HTTP request to a particular route;
for example, if a user navigated to ourapp.com/ Scotty would route the
web request to the route handler for "/", which determines what content
to give back to the user. You don't need to know how Scotty or similar
libaries do this (see CIS 555), but it _is_ important that your route
handler does a few things:

  1. It should specify what kind of requests are being handled on this route,
     e.g. GET or POST.
  2. It should specify the path on which it executes, e.g. "/".
  3. It should specify what to return to the client upon a request,
     e.g. an HTML page with content "<h1>OK</h1>". This part is important;
     routes which don't return anything to the client cause client requests
     to timeout, giving a bad user experience.

We heavily encourage you to look through the Scotty documentation as you
complete the assignment. It can be found here:

http://hackage.haskell.org/package/scotty-0.11.2/docs/Web-Scotty.html
-}

{-
We have given you one route handler for free. The `home` handler takes
requests to the path '/' and simply gives back to the client the main
index.html page (the conventional filename for a site's homepage).

index.html is actually a compiled Elm program! If you are interested in
checking it out or extending it, look at the code in client/src/Index.elm.
-}

home :: ScottyM ()
home = get "/" $
  file "./client/index.html"

{-
Now fill in the following handler which should direct all GET requests to
the path /analytics.js to the ./client/analytics.js file. The solution
should look similar to the `home` handler above.
-}

analyticsJs :: ScottyM ()
analyticsJs = notFound undefined



{- 8.2 -- Shortening Links -}

{-
Implement a route handler for POST requests on the path "/shorten" which takes
the long link given by the client from the _request body_, assigns it a
random ID, and inserts it into the database. You should send back a
_text response_ to the client indicating what the ID for their URL is.

Since you're expected to read from / write to the database in this route and
those that follow, you'll want to look at Appendix A (the code towards the
end of this file) to make sure you understand the interface we have given
you for interacting with the database. Just to be clear, you do not need
to modify (or even read) the SQL code that we gave you, but you should
know how each function that uses SQL allows you to interact with the database.

To help you, we have provided a definition `randomId` that can generate
a random identifier of 10 alphanumeric characters. Let's take a look at
the type of randomId for a minute: `IO Identifier`. Generating random
values clearly cannot be done with a pure function. The idea of purity
is that it always returns the same output given the same input, while
the purpose of a random generator is to return a different output each
time it is called even if it is called on the same input. Therefore,
it makes sense that we need to use the IO monad here. That means that
randomId is essentially a _description_ for _how_ to generate a random
`Identifier`.
-}

randomId :: IO Identifier
randomId = randomString (onlyAlphaNum randomASCII) 10

{-
But the story is not over yet. We want to use `randomId` in our
implementation for `shorten` but the types don't work out. `randomId`
is a computation in the IO monad while shorten is a computation in the
`ScottyM` monad (or more accurately, `ActionM` monad). This is a problem
because `>>=` only allows us to sequence monads of the same type. To get
around this restriction we are going to use a trick called
`liftIO :: Monad m => IO a -> m a` (well, actually the type is a little more
complicated but that should suffice for our purposes). `liftIO` allows
us to embed computations in the IO monad within other arbitrary monads
(note that the `m` in the type of `liftIO` is an unbound type variable that
satisfies the Monad constraint). All this means is that if we pass `randomId`
to `liftIO`, we can then use it within `shorten`'s `ScottyM` monad.

NOTE: Keep the `liftIO` trick in mind-- it will come in handy elsewhere
in this assignment.
-}

shorten :: ScottyM ()
shorten = notFound undefined



{- 8.3 -- Redirecting to the Original Links -}

{-
Implement a route handler for GET requests on the path "/:id" which finds
the link associated with the given ID in the database and redirects the user
to it. `:id` is an example of a _capture_. These are named wildcards that
we can be look up with `param :: String -> ActionM a` (again, this is a
slight simplification of the actual type but fine for our purposes).

The recommended way of implementing this handler would be:

  1. Retrieve the ID being requested from the URL using `param`.
  2. Retrieve the long URL and current number of clicks from the database
     using the ID from the capture.
  3. Increment the click count of this URL in the database (see
     the `Analytics` section for more context).
  4. Send a _redirect response_ back to the client with the long URL.

If the ID does not map to a valid ID in the database, you can send a
response to the client using `invalidIdResponse` which just displays an
error message to the user. Feel free to spruce it up if that's your thing.
-}

redirectTo :: ScottyM ()
redirectTo = notFound undefined

invalidIdResponse :: Identifier -> ActionM ()
invalidIdResponse id_ = html . T.pack . concat $
  [ "<h2>There are no links associated with id: "
  , id_
  , "!</h2>"
  ]


{- 8.4 -- Click Analytics -}

{-
Implement a route handler for GET requests on the path "/analytics/:id"
which finds the link associated with the given ID in the database, gets
its long URL and click count, and returns this information to the client
using the `analyticsResponse` function that we made for you. This response
initializes a small Elm app with the information about the link so that
it can display the information as a webpage.

To be honest, this is probably a bit excessive for our use-case (we could
get away with an approach similar to invalidIdResponse where we just
embed the link information directly into HTML tags). The advantage here is
that if we decide to scale our analytics page to show fancier information--
perhaps a chart showing the time of day when a link is clicked or a map
showing the location of users who clicked on the link-- Elm would give us
much greater flexibility to do so.
-}

analytics :: ScottyM ()
analytics = notFound undefined

analyticsResponse :: ByteString -> Int -> ActionM ()
analyticsResponse longUrl clicks = html . T.pack . concat $
  [ "<!DOCTYPE HTML>"
  , "<html>"
  , "<head>"
  ,   "<meta charset=\"UTF-8\">"
  ,   "<title>Analytics</title>"
  , "</head>"
  , "<body>"
  , "<div id=\"main\"></div>"
  , "<script src=\"/analytics.js\"></script>"
  , "<script>"
  , "var app = Elm.Analytics.init({"
  ,   "node: document.getElementById(\"main\"),"
  ,   "flags: { \"longUrl\": " ++ show (C.unpack longUrl)
  ,           ", \"clicks\": " ++ show clicks
  ,          "}"
  , "});"
  , "</script>"
  , "</body>"
  , "</html>"
  ]



{- Appendix A: Interfacing with the Database -}

{-
First, we have provided you with functions for interacting with the database,
specifically `insertLink` to put a link into the database, `queryForLink` to
pull a link out of the database based on ID, and `updateClicks` to update
the number of clicks for a given link based on ID.

`LinkData` is comprised of the URL id, the long URL itself, and the number
of clicks it has in that order.
-}

type Identifier = String

type LinkData = ( Identifier, ByteString, Int )

linkShortenerDB :: String
linkShortenerDB = "linkShortener.db"

createTable :: IO ()
createTable = withConnection linkShortenerDB $ \conn ->
  execute_ conn "CREATE TABLE IF NOT EXISTS links (id TEXT, link TEXT, clicks INTEGER, PRIMARY KEY (id))"

insertLink :: Identifier -> ByteString -> IO ()
insertLink id_ longUrl = withConnection linkShortenerDB $ \conn ->
  execute conn "INSERT INTO links (id, link, clicks) VALUES ((?),(?),(?))" (id_, longUrl, 0 :: Int)

queryForLink :: Identifier -> IO (Maybe LinkData)
queryForLink id_ = withConnection linkShortenerDB $ \conn ->
  listToMaybe <$> query conn "SELECT * from links WHERE id=(?)" (Only id_)

updateClicks :: Identifier -> Int -> IO ()
updateClicks id_ newClicks = withConnection linkShortenerDB $ \conn ->
  execute conn "UPDATE links SET clicks=(?) WHERE id=(?)" (newClicks, id_)



{- Appendix B: Starting the Server -}

{-
Finally, we've provided you with some code to setup the database,
initialize the server, enable debug logging, and put together all
the routes from above. You're not required to understand all of it,
but you may read through it to get a better picture of how the
different Scotty components fit together.
-}

main :: IO ()
main = do
  liftIO createTable
  port <- fromMaybe 3000 . join . fmap readMaybe <$> lookupEnv "PORT"
  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "static/images")
    middleware logStdoutDev
    home >> analyticsJs >> shorten >> redirectTo >> analytics
