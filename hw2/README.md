# hw2

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/username/TemplateHW/blob/master/hw2/LICENSE)

## Monad laws proof 
1. Left Identity: retrun a >>= f = f a
* ```haskell
return a >>= f 
  = Optional (Just (Just a)) >>= f > return
  = f a > bind-Just Just
```
