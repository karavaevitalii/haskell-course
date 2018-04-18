# hw2

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/username/TemplateHW/blob/master/hw2/LICENSE)

## Monad laws proof 
1. Left Identity: retrun a >>= f = f a
* return a >>= f <br />
  = Optional (Just (Just a)) >>= f -- return <br />
  = f a -- bind-Optional(Just (Just a)) <br />
2. Right Identity: m >>= return = m
* m = Optional Nothing: <br />
    Optional Nothing >>= _ = Optional Nothing -- bind-Optional Noting <br />
* m = Optional $ Just Nothing: <br />
    Optional (Just Nothing) >>= _ = Optional (Just Nothing) -- bind-Optional (Just Nothing)
* m = Optional (Just (Just a)) >>= return <br />
    = return a --bing-Optional (Just (Just a)) <br />
    = Optional (Just (Just a)) --return
3. Associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g) 
* m = Optional Nothing: <br />
    (Optional Nothing >>= f) >>= g <br />
    = Optional Nothing >>= g --bind-Optional Nothing <br />
    = Optional Nothing --bind-Optional Nothing <br />
    
    Optional Nothing >>= _ = Optional Nothing --bind-Optional Nothing
* m = Optional (Just Nothing):
    (Optional (Just Nothing) >>= f) >>= g <br />
    = Optional (Just Nothing) >>= g --bind-Optional (Just Nothing) <br />
    = Optional $ Just Nothing --bind-Optional (Just Nothing) <br />
    
    Optional (Just Nothing) >>= _ = Optional (Just Nothing) --bind-Optional (Just Nothing)
    
* m = Optional (Just (Just a)): <br />
  (Optional (Just (Just a)) >>= f) >>= g <br />
  = f a >>= g --bind-Optional (Just (Just a)) <br />
  
  Optional (Just (Just a)) >>= (\x -> f x >>= g) <br />
  = (\x -> f x >>= g) a --bind-Optional (Just (Just a)) <br />
  = f a >>= g --function application
