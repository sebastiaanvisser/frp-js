var time = frp()

setInterval(
  function ()
  {
    time.set(1 * new Date)
  }, 15)

