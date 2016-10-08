module D3.Collections (
    D3Collection(..)
  , module D3.Collections.Map
  ) where

import D3.Collections.Map

data D3Collection d = D3Range d d    -- no effects
                    | D3ArrT (Array d)  -- no effects
                    | D3MapT (D3Map d)  -- effects D3
