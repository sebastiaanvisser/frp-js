// Create some generic FRP node.

function frp (init, act)
{
  // Connector function.
  var x =
    function (src)
    {
      src.reactors.push(x)
      x.set(src.v)
      return x
    }

  // Primitive set function.
  x.set = 
    function (v)
    {
      if (v == x.v) return                        // Fixed point reached?
      x.v = v                                     // Update node with new value.
      if (act) act.call(x)                        // Real world side effect/
      for (var i = 0; i < x.reactors.length; i++) // Notify all reactors.
        x.reactors[i].set(x.v);
      return x                                    // Return self for chaining.
    }

  // Primitive get function.
  x.get = function () { return x.v }

  x.reactors = []

  // Initialize with default value.
  x.set(init)

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
      }
    }

    for (var i = 0; i < arguments.length; i++)
      (x.sub[i] = frp(undefined, make(x, i)))(arguments[i])

    return x
  }
}

function listify ()
{
  var tmp = []
  for (var i = 0; i < arguments.length; i++)
    tmp[i] = arguments[i]
  return tmp
}

// Let changes on the input let the output alternate between the a's.
// :: a :-> [b] :-> Val b

_switch = $(
  function (b, c)
  {
    if (this.last !== b)
      (this.i = this.i ? this.i + 1 : 1)
    this.last = b
    return c && c[this.i % c.length]
  }
)

// True between a transition of false to true of `a' and a transistion from
// false to true of `b`.
// :: Bool :-> Bool :~> Bool

fromto = $(
  function (a, b)
  {
    var n = this.v
    if (a === true) n = true
    if (b === true) n = false
    return n
  }
)

// Make a object property output.

function property (o, p)
{
  return frp(o[p], function (v) { return o[p] = this.v } )
}

function _event (o, p, ev)
{
  var pr = property(o, p)
  o[ev] =
    function (e)
    {
      e.preventDefault;
      return pr.set(o[p])
    }
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

