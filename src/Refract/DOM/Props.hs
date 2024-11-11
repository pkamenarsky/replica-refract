{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Refract.DOM.Props where

import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as ST
import qualified Control.Monad.Trans.Writer.CPS as W

import qualified Data.Text                 as T

import           Replica.VDOM              (DOMEvent)
import           Replica.VDOM.Types        (DOMEvent(getDOMEvent), EventOptions(EventOptions))

type ModState st = (st -> IO st) -> IO ()
type Ctx st = (ModState st, st)

newtype Mod st a = Mod (R.ReaderT (ModState st) (ST.StateT st IO) a)
  deriving (Functor, Applicative, Monad)

modify :: (st -> IO (a, st)) -> Mod st a
modify f = Mod $ lift $ ST.StateT f

modify' :: (st -> IO st) -> Mod st ()
modify' f = modify $ \st -> f st >>= \st' -> pure ((), st')

get :: Mod st st
get = modify $ \st -> pure (st, st)

set :: st -> Mod st ()
set st = modify $ \_ -> pure ((), st)

nonBlockingIO :: IO a -> Mod st a
nonBlockingIO io = Mod $ liftIO io

blockingIO :: IO (Mod st ()) -> Mod st ()
blockingIO io = Mod $ do
  modState <- R.ask

  liftIO $ void $ forkIO $ do
    Mod f <- io
    modState $ ST.execStateT $ R.runReaderT f modState

data Prop st
  = PropText T.Text
  | PropBool Bool
  | PropEvent EventOptions (DOMEvent -> Mod st ())
  | PropMap (Props st)

newtype Props' st a = Props (W.Writer [(T.Text, Prop st)] a)
  deriving (Functor, Applicative, Monad)

type Props st = Props' st ()

props :: T.Text -> Prop st -> Props st
props k v = Props $ W.tell [(k, v)]

boolProp :: T.Text -> Bool -> Props st
boolProp k v = props k (PropBool v)

textProp :: T.Text -> T.Text -> Props st
textProp k v = props k (PropText v)

key :: T.Text -> Props st
key v = textProp "key" v

style :: [(T.Text, T.Text)] -> Props st
style m = props "style" (PropMap $ sequence_ [ props k (PropText v) | (k, v) <- m ])

-- | Define multiple classes conditionally
--
-- > div [ classList [ ("empty", null items) ] [ ]
--
classList ::  [(T.Text, Bool)] -> Props st
classList xs = textProp "class" $ T.intercalate " " [ t | (t, True) <- xs ]

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/title>
title ::  T.Text -> Props st
title = textProp "title"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/selected>
selected ::  Bool -> Props st
selected = boolProp "selected"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/hidden>
hidden ::  Bool -> Props st
hidden = boolProp "hidden"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/value>
value ::  T.Text -> Props st
value = textProp "value"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/defaultValue>
defaultValue ::  T.Text -> Props st
defaultValue = textProp "defaultValue"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/accept>
accept ::  T.Text -> Props st
accept = textProp "accept"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/acceptCharset>
acceptCharset ::  T.Text -> Props st
acceptCharset = textProp "acceptCharset"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/a>
a_ ::  T.Text -> Props st
a_ = textProp "a"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/autocomplete>
autocomplete ::  Bool -> Props st
autocomplete b = textProp "autocomplete" (if b then "on" else "off")

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/autosave>
autosave ::  T.Text -> Props st
autosave = textProp "autosave"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/disabled>
disabled ::  Bool -> Props st
disabled = boolProp "disabled"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/enctype>
enctype ::  T.Text -> Props st
enctype = textProp "enctype"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/formation>
formation ::  T.Text -> Props st
formation = textProp "formation"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/list>
list ::  T.Text -> Props st
list = textProp "list"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/maxlength>
maxlength ::  T.Text -> Props st
maxlength = textProp "maxlength"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/minlength>
minlength ::  T.Text -> Props st
minlength = textProp "minlength"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/method>
method ::  T.Text -> Props st
method = textProp "method"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/multiple>
multiple ::  Bool -> Props st
multiple = boolProp "multiple"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/novalidate>
novalidate ::  Bool -> Props st
novalidate = boolProp "noValidate"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/pattern>
pattern ::  T.Text -> Props st
pattern = textProp "pattern"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/readonly>
readonly ::  Bool -> Props st
readonly = boolProp "readOnly"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/required>
required ::  Bool -> Props st
required = boolProp "required"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/size>
size ::  T.Text -> Props st
size = textProp "size"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/for>
for ::  T.Text -> Props st
for = textProp "for"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/form>
form_ ::  T.Text -> Props st
form_= textProp "form"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/max>
max ::  T.Text -> Props st
max = textProp "max"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/min>
min ::  T.Text -> Props st
min = textProp "min"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/step>
step ::  T.Text -> Props st
step = textProp "step"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/cols>
cols ::  T.Text -> Props st
cols = textProp "cols"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/rows>
rows ::  T.Text -> Props st
rows = textProp "rows"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/wrap>
wrap ::  T.Text -> Props st
wrap = textProp "wrap"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/target>
target_ ::  T.Text -> Props st
target_= textProp "target"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/download>
download ::  T.Text -> Props st
download = textProp "download"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/downloadAs>
downloadAs ::  T.Text -> Props st
downloadAs = textProp "downloadAs"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/hreflang>
hreflang ::  T.Text -> Props st
hreflang = textProp "hreflang"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/media>
media ::  T.Text -> Props st
media = textProp "media"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/ping>
ping ::  T.Text -> Props st
ping = textProp "ping"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/rel>
rel ::  T.Text -> Props st
rel = textProp "rel"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/ismap>
ismap ::  T.Text -> Props st
ismap = textProp "ismap"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/usemap>
usemap ::  T.Text -> Props st
usemap = textProp "usemap"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/shape>
shape ::  T.Text -> Props st
shape = textProp "shape"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/coords>
coords ::  T.Text -> Props st
coords = textProp "coords"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/src>
src ::  T.Text -> Props st
src = textProp "src"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/height>
height ::  T.Text -> Props st
height = textProp "height"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/width>
width ::  T.Text -> Props st
width = textProp "width"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/alt>
alt ::  T.Text -> Props st
alt = textProp "alt"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/autoplay>
autoplay ::  Bool -> Props st
autoplay = boolProp "autoplay"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/controls>
controls ::  Bool -> Props st
controls = boolProp "controls"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/loop>
loop ::  Bool -> Props st
loop = boolProp "loop"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/preload>
preload ::  T.Text -> Props st
preload = textProp "preload"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/poster>
poster ::  T.Text -> Props st
poster = textProp "poster"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/default>
default_ ::  Bool -> Props st
default_= boolProp "default"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/kind>
kind ::  T.Text -> Props st
kind = textProp "kind"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/srclang>
srclang ::  T.Text -> Props st
srclang = textProp "srclang"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/sandbox>
sandbox ::  T.Text -> Props st
sandbox = textProp "sandbox"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/seamless>
seamless ::  T.Text -> Props st
seamless = textProp "seamless"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/srcdoc>
srcdoc ::  T.Text -> Props st
srcdoc = textProp "srcdoc"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/reversed>
reversed ::  T.Text -> Props st
reversed = textProp "reversed"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/start>
start ::  T.Text -> Props st
start = textProp "start"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/align>
align ::  T.Text -> Props st
align = textProp "align"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/colspan>
colspan ::  T.Text -> Props st
colspan = textProp "colspan"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/rowspan>
rowspan ::  T.Text -> Props st
rowspan = textProp "rowspan"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/headers>
headers ::  T.Text -> Props st
headers = textProp "headers"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/scope>
scope ::  T.Text -> Props st
scope = textProp "scope"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/async>
async ::  T.Text -> Props st
async = textProp "async"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/charset>
charset ::  T.Text -> Props st
charset = textProp "charset"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/content>
content ::  T.Text -> Props st
content = textProp "content"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/defer>
defer ::  T.Text -> Props st
defer = textProp "defer"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/httpEquiv>
httpEquiv ::  T.Text -> Props st
httpEquiv = textProp "httpEquiv"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/language>
language ::  T.Text -> Props st
language = textProp "language"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/scoped>
scoped ::  T.Text -> Props st
scoped = textProp "scoped"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/type>
type_ ::  T.Text -> Props st
type_ = textProp "type"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/name>
name ::  T.Text -> Props st
name = textProp "name"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/href>
href ::  T.Text -> Props st
href = textProp "href"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/id>
id ::  T.Text -> Props st
id = textProp "id"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/placeholder>
placeholder ::  T.Text -> Props st
placeholder = textProp "placeholder"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/checked>
checked ::  Bool -> Props st
checked = boolProp "checked"

-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/autofocus>
autofocus ::  Bool -> Props st
autofocus = boolProp "autofocus"

-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
className ::  T.Text -> Props st
className = textProp "class"
