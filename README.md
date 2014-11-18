switchr
=======

An R package for managing, installing into, and seamlessly switching
between R package libraries ("software contexts")

To switch to a different library, creating it if necessary, use the `switchTo` 
function:
```
switchTo("mynewlib")
```

Then work normally, when you wish to switch back to your original library, 
use `switchBack`:

```
switchBack()
```

