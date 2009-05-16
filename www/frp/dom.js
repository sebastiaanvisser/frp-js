// Mouse coordinates input.

var mouse = { x : frp(), y : frp() }
document.onmousemove =
  function (e)
  {
    mouse.x.set(e.clientX)
    mouse.y.set(e.clientY)
  }

// Mouse status input.

mouse.down = frp().set(false)
document.onmousedown = function (e) { mouse.down.set(true)  }
document.onmouseup   = function (e) { mouse.down.set(false) }

// Lift div element using geometry and contents.

function propEvent (obj, prop, ev)
{
  var p = property(obj, prop)
  obj[ev] = function () { p.set(obj[prop]) }
  return p
}

function element (e)
{
  var x = {
    left   : property(e.style, 'left')
  , top    : property(e.style, 'top')
  , width  : property(e.style, 'width')
  , height : property(e.style, 'height')
  , color  : property(e.style, 'backgroundColor')
  , html   : property(e,       'innerHTML')
  , text   : property(e,       'value')
  }

  e.onkeyup = function () { x.text.set(e.value) }

  return x
}

// Lift some common things from the browser environment.

var title   = property (document, 'title')
var body    = element  (document.body)

