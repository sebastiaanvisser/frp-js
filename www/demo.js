// Lift all elements in this demo to the FRP world.
// Todo: provide frp lifted DOM automatically.

var h1      = element  (document.getElementById('header'))

var red     = element  (document.getElementById('first'))
var blue    = element  (document.getElementById('second'))
var orange  = element  (document.getElementById('third'))
var magenta = element  (document.getElementById('fourth'))
var pink    = element  (document.getElementById('fifth'))

var t1      = element  (document.getElementById('t1'))
var t2      = element  (document.getElementById('t2'))
var t3      = element  (document.getElementById('t3'))
var t4      = element  (document.getElementById('t4'))
var t5      = element  (document.getElementById('t5'))

var sep     = element  (document.getElementById('sep'))

// Helpers.

var listToUL = 
  lift(function (a) "<ul>" + a.map(function (e) { return "<li>" + e + "</li>" }).join('\n') + "</ul>")

var upper = lift(function (a) a.map(function (x) x.toUpperCase() + ' (' + x.length + ')'))
var lower = lift(function (a) a.map(function (x) x.toLowerCase() + ' (' + x.length + ')'))

var length = lift(function (x) x.length)

var right  = function (x) add(x.left, x.width)
var bottom = function (x) add(x.top,  x.height)

function attach(a, b)
{
  a.width  (div(b.width,  C(2)))
  a.height (div(b.height, C(2)))
  a.left   (sub(b.left, div(a.width,  C(2))))
  a.top    (sub(b.top,  div(a.height, C(2))))
}

// Actual demo logic ------------------------------------------------------

title(list(mouse.down, mouse.x, mouse.y))

var w = mul(length(t1.text), C(5))
var h = mul(length(t2.text), C(5))

all
  ( blue.width
  , orange.width
  , magenta.height
  , pink.height
  )(w)

all
  ( blue.height
  , orange.height
  , magenta.width
  , pink.width
  )(h)

all
  ( blue.top
  , orange.top
  , pink.left
  , magenta.left
  )(mod(div(time, C(40)), C(100)))

red.height(C(300))
red.width(C(300))

sep.top(C(250))

red.left(max(C(200), sub(mouse.x, div(red.width,  C(2)))))    // read as:   red.left = max(200, mouse.x - red.width / 2)
red.top (max(sep.top, sub(mouse.y, div(red.height, C(2)))))

blue.left(right(red))
orange.left(sub(red.left, orange.width))

magenta.top(add(red.top, red.height))
pink.top(sub(red.top, pink.height))

attach(h1, red)

var sorted = sort(list
 ( t1.text
 , t2.text
 , t3.text
 , t4.text
 , t5.text
 ))

var cased = _if(mouse.down, upper(sorted), lower(sorted))

red.html(listToUL(cased))

body.color(_if(mouse.down, C('yellow'), C('green')))

