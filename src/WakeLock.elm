port module WakeLock exposing (releaseWakeLock, requestWakeLock)


port requestWakeLock : () -> Cmd msg


port releaseWakeLock : () -> Cmd msg
