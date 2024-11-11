{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Refract.DOM where

import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as ST
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.Writer.CPS as W

import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T

import Refract.DOM.Props (Ctx, Mod(Mod), Props, Props'(Props), Prop(PropText, PropBool, PropEvent, PropMap), key)
import qualified Replica.VDOM as VDOM

newtype UI' st a = UI (R.ReaderT (Ctx st) (W.Writer VDOM.HTML) a)
  deriving (Functor, Applicative, Monad)

type UI st = UI' st ()

el :: T.Text -> Props' st (UI st) -> UI st
el = elWithNamespace Nothing

el_ :: T.Text -> Props st -> UI st
el_ t props = el t (props >> pure empty)

elWithNamespace :: Maybe VDOM.Namespace -> T.Text -> Props' st (UI st) -> UI st
elWithNamespace ns name (Props props) = UI $ do
  (modState, st) <- R.ask

  children <- R.asks (W.execWriter . R.runReaderT ui)

  lift $ W.tell
    [ VDOM.VNode
        name
        (M.unions $ map (toProps modState st) mprops)
        ns
        children
    ]
  where
    (UI ui, mprops) = W.runWriter props

    toProps modState st (k, (PropEvent opts f)) = M.singleton k $ VDOM.AEvent opts $ \de -> case f de of Mod f -> modState (ST.execStateT $ R.runReaderT f modState)
    toProps modState st (k, (PropText v)) = M.singleton k $ VDOM.AText v
    toProps modState st (k, (PropBool v)) = M.singleton k $ VDOM.ABool v
    toProps modState st (k, (PropMap (Props m))) = M.singleton k $ VDOM.AMap $ M.unions $ map (toProps modState st) (W.execWriter m)
 
empty :: UI st
empty = UI $ pure ()

nest :: UI' st a -> Props' st (UI' st a)
nest = pure

text :: T.Text -> UI st
text txt = UI $ lift $ W.tell [ VDOM.VText txt ]

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div :: Props' st (UI st) -> UI st
div  = el "div"

div_ :: Props st -> UI st
div_  = el_ "div"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table :: Props' st (UI st) -> UI st
table  = el "table"

table_ :: Props st -> UI st
table_  = el_ "table"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead :: Props' st (UI st) -> UI st
thead  = el "thead"

thead_ :: Props st -> UI st
thead_  = el_ "thead"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody :: Props' st (UI st) -> UI st
tbody  = el "tbody"

tbody_ :: Props st -> UI st
tbody_  = el_ "tbody"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr :: Props' st (UI st) -> UI st
tr  = el "tr"

tr_ :: Props st -> UI st
tr_  = el_ "tr"

-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr>

trKeyed :: T.Text -> Props' st (UI st) -> UI st
trKeyed k props = el "tr" (key k >> props)

trKeyed_ :: T.Text -> Props st -> UI st
trKeyed_ k props = el_ "tr" (key k >> props)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th :: Props' st (UI st) -> UI st
th  = el "th"

th_ :: Props st -> UI st
th_  = el_ "th"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td :: Props' st (UI st) -> UI st
td  = el "td"

td_ :: Props st -> UI st
td_  = el_ "td"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot :: Props' st (UI st) -> UI st
tfoot  = el "tfoot"

tfoot_ :: Props st -> UI st
tfoot_  = el_ "tfoot"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section :: Props' st (UI st) -> UI st
section  = el "section"

section_ :: Props st -> UI st
section_  = el_ "section"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header :: Props' st (UI st) -> UI st
header  = el "header"

header_ :: Props st -> UI st
header_  = el_ "header"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer :: Props' st (UI st) -> UI st
footer  = el "footer"

footer_ :: Props st -> UI st
footer_  = el_ "footer"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button :: Props' st (UI st) -> UI st
button = el "button"

button_ :: Props st -> UI st
button_ = el_ "button"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form :: Props' st (UI st) -> UI st
form = el "form"

form_ :: Props st -> UI st
form_ = el_ "form"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p :: Props' st (UI st) -> UI st
p = el "p"

p_ :: Props st -> UI st
p_ = el_ "p"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s :: Props' st (UI st) -> UI st
s = el "s"

s_ :: Props st -> UI st
s_ = el_ "s"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul :: Props' st (UI st) -> UI st
ul = el "ul"

ul_ :: Props st -> UI st
ul_ = el_ "ul"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span :: Props' st (UI st) -> UI st
span = el "span"

span_ :: Props st -> UI st
span_ = el_ "span"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong :: Props' st (UI st) -> UI st
strong = el "strong"

strong_ :: Props st -> UI st
strong_ = el_ "strong"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li :: Props' st (UI st) -> UI st
li = el "li"

li_ :: Props st -> UI st
li_ = el_ "li"

-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li>
--
liKeyed :: T.Text -> Props' st (UI st) -> UI st
liKeyed k props = el "li" (key k >> props)

liKeyed_ :: T.Text -> Props st -> UI st
liKeyed_ k props = el_ "li" (key k >> props)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1 :: Props' st (UI st) -> UI st
h1 = el "h1"

h1_ :: Props st -> UI st
h1_ = el_ "h1"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2 :: Props' st (UI st) -> UI st
h2 = el "h2"

h2_ :: Props st -> UI st
h2_ = el_ "h2"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3 :: Props' st (UI st) -> UI st
h3 = el "h3"

h3_ :: Props st -> UI st
h3_ = el_ "h3"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4 :: Props' st (UI st) -> UI st
h4 = el "h4"

h4_ :: Props st -> UI st
h4_ = el_ "h4"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5 :: Props' st (UI st) -> UI st
h5 = el "h5"

h5_ :: Props st -> UI st
h5_ = el_ "h5"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6 :: Props' st (UI st) -> UI st
h6 = el "h6"

h6_ :: Props st -> UI st
h6_ = el_ "h6"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr :: Props st -> UI st
hr = el_ "hr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre :: Props' st (UI st) -> UI st
pre = el "pre"

pre_ :: Props st -> UI st
pre_ = el_ "pre"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input :: Props st -> UI st
input = el_ "input"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label :: Props' st (UI st) -> UI st
label = el "label"

label_ :: Props st -> UI st
label_ = el_ "label"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a :: Props' st (UI st) -> UI st
a = el "a"

a_ :: Props st -> UI st
a_ = el_ "a"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark :: Props' st (UI st) -> UI st
mark = el "mark"

mark_ :: Props st -> UI st
mark_ = el_ "mark"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby :: Props' st (UI st) -> UI st
ruby = el "ruby"

ruby_ :: Props st -> UI st
ruby_ = el_ "ruby"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt :: Props' st (UI st) -> UI st
rt = el "rt"

rt_ :: Props st -> UI st
rt_ = el_ "rt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp :: Props' st (UI st) -> UI st
rp = el "rp"

rp_ :: Props st -> UI st
rp_ = el_ "rp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi :: Props' st (UI st) -> UI st
bdi = el "bdi"

bdi_ :: Props st -> UI st
bdi_ = el_ "bdi"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo :: Props' st (UI st) -> UI st
bdo = el "bdo"

bdo_ :: Props st -> UI st
bdo_ = el_ "bdo"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr :: Props st -> UI st
wbr = el_ "wbr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details :: Props' st (UI st) -> UI st
details = el "details"

details_ :: Props st -> UI st
details_ = el_ "details"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary :: Props' st (UI st) -> UI st
summary = el "summary"

summary_ :: Props st -> UI st
summary_ = el_ "summary"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem :: Props' st (UI st) -> UI st
menuitem = el "menuitem"

menuitem_ :: Props st -> UI st
menuitem_ = el_ "menuitem"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu :: Props' st (UI st) -> UI st
menu = el "menu"

menu_ :: Props st -> UI st
menu_ = el_ "menu"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset :: Props' st (UI st) -> UI st
fieldset = el "fieldset"

fieldset_ :: Props st -> UI st
fieldset_ = el_ "fieldset"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend :: Props' st (UI st) -> UI st
legend = el "legend"

legend_ :: Props st -> UI st
legend_ = el_ "legend"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist :: Props' st (UI st) -> UI st
datalist = el "datalist"

datalist_ :: Props st -> UI st
datalist_ = el_ "datalist"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup :: Props' st (UI st) -> UI st
optgroup = el "optgroup"

optgroup_ :: Props st -> UI st
optgroup_ = el_ "optgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen :: Props' st (UI st) -> UI st
keygen = el "keygen"

keygen_ :: Props st -> UI st
keygen_ = el_ "keygen"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output :: Props' st (UI st) -> UI st
output = el "output"

output_ :: Props st -> UI st
output_ = el_ "output"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress :: Props' st (UI st) -> UI st
progress = el "progress"

progress_ :: Props st -> UI st
progress_ = el_ "progress"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter :: Props' st (UI st) -> UI st
meter = el "meter"

meter_ :: Props st -> UI st
meter_ = el_ "meter"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center :: Props' st (UI st) -> UI st
center = el "center"

center_ :: Props st -> UI st
center_ = el_ "center"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio :: Props' st (UI st) -> UI st
audio = el "audio"

audio_ :: Props st -> UI st
audio_ = el_ "audio"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video :: Props' st (UI st) -> UI st
video = el "video"

video_ :: Props st -> UI st
video_ = el_ "video"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source :: Props st -> UI st
source = el_ "source"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track :: Props st -> UI st
track = el_ "track"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed :: Props st -> UI st
embed = el_ "embed"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object :: Props' st (UI st) -> UI st
object = el "object"

object_ :: Props st -> UI st
object_ = el_ "object"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param :: Props st -> UI st
param = el_ "param"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins :: Props' st (UI st) -> UI st
ins = el "ins"

ins_ :: Props st -> UI st
ins_ = el_ "ins"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del :: Props' st (UI st) -> UI st
del = el "del"

del_ :: Props st -> UI st
del_ = el_ "del"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small :: Props' st (UI st) -> UI st
small = el "small"

small_ :: Props st -> UI st
small_ = el_ "small"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite :: Props' st (UI st) -> UI st
cite = el "cite"

cite_ :: Props st -> UI st
cite_ = el_ "cite"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn :: Props' st (UI st) -> UI st
dfn = el "dfn"

dfn_ :: Props st -> UI st
dfn_ = el_ "dfn"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr :: Props' st (UI st) -> UI st
abbr = el "abbr"

abbr_ :: Props st -> UI st
abbr_ = el_ "abbr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time :: Props' st (UI st) -> UI st
time = el "time"

time_ :: Props st -> UI st
time_ = el_ "time"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var :: Props' st (UI st) -> UI st
var = el "var"

var_ :: Props st -> UI st
var_ = el_ "var"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp :: Props' st (UI st) -> UI st
samp = el "samp"

samp_ :: Props st -> UI st
samp_ = el_ "samp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd :: Props' st (UI st) -> UI st
kbd = el "kbd"

kbd_ :: Props st -> UI st
kbd_ = el_ "kbd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption :: Props' st (UI st) -> UI st
caption = el "caption"

caption_ :: Props st -> UI st
caption_ = el_ "caption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup :: Props' st (UI st) -> UI st
colgroup = el "colgroup"

colgroup_ :: Props st -> UI st
colgroup_ = el_ "colgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col :: Props st -> UI st
col = el_ "col"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav :: Props' st (UI st) -> UI st
nav = el "nav"

nav_ :: Props st -> UI st
nav_ = el_ "nav"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article :: Props' st (UI st) -> UI st
article = el "article"

article_ :: Props st -> UI st
article_ = el_ "article"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside :: Props' st (UI st) -> UI st
aside = el "aside"

aside_ :: Props st -> UI st
aside_ = el_ "aside"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address :: Props' st (UI st) -> UI st
address = el "address"

address_ :: Props st -> UI st
address_ = el_ "address"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: Props' st (UI st) -> UI st
main_ = el "main"

main__ :: Props st -> UI st
main__ = el_ "main"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body :: Props' st (UI st) -> UI st
body = el "body"

body_ :: Props st -> UI st
body_ = el_ "body"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure :: Props' st (UI st) -> UI st
figure = el "figure"

figure_ :: Props st -> UI st
figure_ = el_ "figure"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption :: Props' st (UI st) -> UI st
figcaption = el "figcaption"

figcaption_ :: Props st -> UI st
figcaption_ = el_ "figcaption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl :: Props' st (UI st) -> UI st
dl = el "dl"

dl_ :: Props st -> UI st
dl_ = el_ "dl"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt :: Props' st (UI st) -> UI st
dt = el "dt"

dt_ :: Props st -> UI st
dt_ = el_ "dt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd :: Props' st (UI st) -> UI st
dd = el "dd"

dd_ :: Props st -> UI st
dd_ = el_ "dd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img :: Props st -> UI st
img = el_ "img"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe :: Props' st (UI st) -> UI st
iframe = el "iframe"

iframe_ :: Props st -> UI st
iframe_ = el_ "iframe"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas :: Props' st (UI st) -> UI st
canvas = el "canvas"

canvas_ :: Props st -> UI st
canvas_ = el_ "canvas"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math :: Props' st (UI st) -> UI st
math = el "math"

math_ :: Props st -> UI st
math_ = el_ "math"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select :: Props' st (UI st) -> UI st
select = el "select"

select_ :: Props st -> UI st
select_ = el_ "select"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option :: Props' st (UI st) -> UI st
option = el "option"

option_ :: Props st -> UI st
option_ = el_ "option"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea :: Props' st (UI st) -> UI st
textarea = el "textarea"

textarea_ :: Props st -> UI st
textarea_ = el_ "textarea"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub :: Props' st (UI st) -> UI st
sub = el "sub"

sub_ :: Props st -> UI st
sub_ = el_ "sub"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup :: Props' st (UI st) -> UI st
sup = el "sup"

sup_ :: Props st -> UI st
sup_ = el_ "sup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br :: Props st -> UI st
br = el_ "br"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol :: Props' st (UI st) -> UI st
ol = el "ol"

ol_ :: Props st -> UI st
ol_ = el_ "ol"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote :: Props' st (UI st) -> UI st
blockquote = el "blockquote"

blockquote_ :: Props st -> UI st
blockquote_ = el_ "blockquote"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code :: Props' st (UI st) -> UI st
code = el "code"

code_ :: Props st -> UI st
code_ = el_ "code"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em :: Props' st (UI st) -> UI st
em = el "em"

em_ :: Props st -> UI st
em_ = el_ "em"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i :: Props' st (UI st) -> UI st
i = el "i"

i_ :: Props st -> UI st
i_ = el_ "i"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b :: Props' st (UI st) -> UI st
b = el "b"

b_ :: Props st -> UI st
b_ = el_ "b"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u :: Props' st (UI st) -> UI st
u = el "u"

u_ :: Props st -> UI st
u_ = el_ "u"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q :: Props' st (UI st) -> UI st
q = el "q"

q_ :: Props st -> UI st
q_ = el_ "q"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script :: Props' st (UI st) -> UI st
script = el "script"

script_ :: Props st -> UI st
script_ = el_ "script"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link :: Props st -> UI st
link = el_ "link"
