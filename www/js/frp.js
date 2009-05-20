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
      for (var i = 0; i < x.reactors.length; i++)
        x.reactors[i].set(x.v);
    }
  init === undefined || x.set(init)

  return x
}

function C(a,b)
{
  return function() 
  {
    return a(b.apply(undefined, arguments))
  }
}

// Combine several inputs using a function.
// :: ([a] -> b) -> [Val a] -> Val b

function $ (f)
{
  return function ()
  {
    var x = frp()
    x.func = f
    x.sub  = []
    x.subv = []

    function make (x, i)
    {
      return function ()
      {
        x.subv[i] = this.v
        return x.set(x.func.apply(x, x.subv));
        // return x.set(x.func.apply(x, x.sub.map(function (s) {return s.v})))
      }
    }

    for (var i = 0; i < arguments.length; i++)
      (x.sub[i] = frp(undefined, make(x, i)))(arguments[i])

    return x
  }
}

// Let changes on the input let the output alternate between the a's.
// :: Val a -> Val [b] -> Val b

function _switch (inp, a)
{
  return $(
    function (b, c)
    {
      if (this.last !== b)
        (this.i = this.i ? this.i + 1 : 1)
      this.last = b
      return c && c[this.i % c.length]
    }
  )(inp, a)
}

// Make a object property output.

function property (o, p)
{
  return frp(o[p], function (v) { return o[p] = this.v } )
}

function _event (o, p, ev)
{
  var pr = property(o, p)
  o[ev] = function () { return pr.set(o[p]) }
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
document.onmousedown = function (e) { return mouseDown.set(true)  }
document.onmouseup   = function (e) { return mouseDown.set(false) }

// Time input.

time = frp()
setInterval(function () { time.set(1 * new Date) }, 50)

