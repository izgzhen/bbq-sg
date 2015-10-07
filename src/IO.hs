module IO where


import Data.Text.Lazy
import Development.Shake as S


readFile' fp = pack <$> S.readFile' fp

writeFile' fp t = S.writeFile' fp (unpack t)
