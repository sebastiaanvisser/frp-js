// Create some generic FRP node.

function frp (action)
{

  var x =
    function link (src)
    {
      src.reactors.push(x)
      x.set(src.value)
      return x
    }

  x.set =
    function (value)
    {
      if (action) action(value)
      x.value = value
      x.reactors.map(function (r) { r.set(value) })
      return x
    }

  x.reactors = []
  return x
}

// Make a object property output.

function property (obj, prop)
  frp(function (value) { obj[prop] = value }).set(obj[prop])

// Make a constant value input.

function C (value) frp().set(value)

// Combine several inputs using a function.

function combine (f, args)
{
  var x = frp()
  x.func = f
  x.sub = []

  function make (x, i)
  {
    return function (val)
    {
      x.sub[i] = val;
      x.set(x.func.apply(this, x.sub))
    }
  }

  for (var i = 0; i < args.length; i++) {
    x.sub[i] = frp(make(x, i))
    x.sub[i](args[i])
  }

  return x
}

function lift (f) function () combine(f, arguments)

var _if = lift(function (c, i, e) c ? i : e)

