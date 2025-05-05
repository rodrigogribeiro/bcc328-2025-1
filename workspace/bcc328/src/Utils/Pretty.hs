module Utils.Pretty ( module Utils.Pretty 
                    , module Text.PrettyPrint.HughesPJ
                    ) where 

import Text.PrettyPrint.HughesPJ

class Pretty a where 
  ppr :: a -> Doc 

pretty :: Pretty a => a -> String 
pretty = render . ppr
