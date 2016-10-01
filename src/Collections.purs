module D3.Collections (
    D3Collection(..)
  , module D3.Collections.Map
  ) where

import D3.Collections.Map

data D3Collection d = D3StartEnd d d
                    | D3ArrT (Array d)
                    | D3MapT (D3Map d)
