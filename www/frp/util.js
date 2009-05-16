// Arithmetic combinators.

var add = lift(function (a, b) a + b )
var mul = lift(function (a, b) a * b )
var sub = lift(function (a, b) a - b )
var div = lift(function (a, b) a / b )
var mod = lift(function (a, b) a % b )

var max = lift(Math.max)
var min = lift(Math.min)

// List combinators.

var list =
  lift(
    function ()
    {
      var tmp = []
      for (var i = 0; i < arguments.length; i++)
        tmp[i] = arguments[i];
      return tmp
    }
  )

var sort = lift(Array.sort)

function all (a)
{
  var a = Array.slice(arguments)
  return function (src)
  {
    a.map(function (x) x(src))
  }
}
