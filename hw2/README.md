# hw2

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/username/TemplateHW/blob/master/hw2/LICENSE)

## Monad laws proof 
1. Left Identity: retrun a >>= f = f a
* return a >>= f 
  = Optional (Just (Just a)) >>= f -- return
  = f a -- bind-Optional(Just (Just a))
2. Right Identity: m >>= return = m
* m = Optional Nothing:
    Optional Nothing >>= _ = Optional Nothing -- bind-Optional Noting
* m = Optional $ Just Nothing:
    Optional (Just Nothing) >>= _ = Optional (Just Nothing) -- bind-Optional (Just Nothing)
* m = Optional (Just (Just a)) >>= return
    = return a --bing-Optional (Just (Just a))
    = Optional (Just (Just a)) --return
3. Associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g)
* m = Optional Nothing:
    (Optional Nothing >>= f) >>= g
    = Optional Nothing >>= g --bind-Optional Nothing
    = Optional Nothing --bind-Optional Nothing
    
    Optional Nothing >>= _ = Optional Nothing --bind-Optional Nothing
* m = Optional (Just Nothing):
    (Optional (Just Nothing) >>= f) >>= g
    = Optional (Just Nothing) >>= g --bind-Optional (Just Nothing)
    = Optional $ Just Nothing --bind-Optional (Just Nothing)
    
    Optional (Just Nothing) >>= _ = Optional (Just Nothing) --bind-Optional (Just Nothing)
    
* m = Optional (Just (Just a)):
  (Optional (Just (Just a)) >>= f) >>= g
  = f a >>= g --bind-Optional (Just (Just a))
  
  Optional (Just (Just a)) >>= (\x -> f x >>= g)
  = (\x -> f x >>= g) a --bind-Optional (Just (Just a))
  = f a >>= g --function application
