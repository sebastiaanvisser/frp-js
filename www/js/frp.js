// Create some generic FRP node.

function frp (init, act)
{
  var x =
    function (src)
    {
      src.reactors.push(x)
      x.set(src.v)
      return x
    }

  x.reactors = []
  x.set =
    function (v)
    {
      x.v = v
      act && act.call(x, v)
      x.reactors.map(function (r) { r.set(x.v) })
      return x
    }
  init && x.set(init)

  return x
}

// Make a object property output.

function property (obj, prop)
  frp(obj[prop], function (v) obj[prop] = v )

function _event (obj, prop, ev)
{
  var p = property(obj, prop)
  obj[ev] = function () p.set(obj[prop])
  return p
}

// Combine several inputs using a function.

function lift (f)
  function ()
  {
    var x = frp()
    x.func = f
    x.sub = []

    function make (x, i)
      function (v)
      {
        x.sub[i] = v;
        x.set(x.func.apply(this, x.sub))
      }

    for (var i = 0; i < arguments.length; i++)
      (x.sub[i] = frp(0, make(x, i)))(arguments[i])

    return x
  }

function _switch (inp, a)
  lift(
    function (b, c)
    {
      if (this.last !== b)
        (this.i = this.i ? this.i + 1 : 1)
      this.last = b
      return c && c[this.i % c.length]
    })(inp, a)

// Mouse coordinates input.

mouseX = frp()
mouseY = frp()
document.onmousemove =
  function (e)
  {
    mouseX.set(e.clientX)
    mouseY.set(e.clientY)
  }

// Mouse status input.

mouseDown = frp(false)
document.onmousedown = function (e) mouseDown.set(true)
document.onmouseup   = function (e) mouseDown.set(false)

// Time input.

time = frp()
setInterval(function () time.set(Date.now()), 15)

