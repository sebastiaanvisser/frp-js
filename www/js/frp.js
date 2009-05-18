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
      if (v == x.v) return
      x.v = v
      if (act) act.call(x)
      x.reactors.map(function (r) { r.set(x.v) })
    }
  init === undefined || x.set(init)

  return x
}

// Combine several inputs using a function.
// :: ([a] -> b) -> [Val a] -> Val b

function combine (f)
  function ()
  {
    var x = frp()
    x.func = f
    x.sub = []

    function make (x, i)
      function ()
        x.set(x.func.apply(x, x.sub.map(function (s) s.v)))

    for (var i = 0; i < arguments.length; i++)
      (x.sub[i] = frp(undefined, make(x, i)))(arguments[i])
    return x
  }

// Let changes on the input let the output alternate between the a's.
// :: Val a -> Val [b] -> Val b

function _switch (inp, a)
  combine(
    function (b, c)
    {
      if (this.last !== b)
        (this.i = this.i ? this.i + 1 : 1)
      this.last = b
      return c && c[this.i % c.length]
    })(inp, a)

// Make a object property output.

function property (o, p)
  frp(o[p], function (v) o[p] = this.v )

function _event (o, p, ev)
{
  var pr = property(o, p)
  o[ev] = function () pr.set(o[p])
  return pr
}

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

