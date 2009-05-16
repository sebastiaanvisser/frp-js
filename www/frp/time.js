var time = frp()
setInterval(function () { time.set(Date.now()) }, 15)
