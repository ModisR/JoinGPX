import System.Environment
import Text.XML.HXT.Core

import PickleGPX
 
main :: IO ()
main = getArgs >>= processArgs

processArgs :: [String] -> IO ()
processArgs [] = putStrLn "Please provide at least two files to combine."
processArgs [_] = processArgs []
processArgs files@(file : _) = do
    runX (
      (foldr
       (\a b -> a &&& b >>> arr (uncurry (:)))
       (arr (const []))
       (map
         (xunpickleDocument xpGPX
           [ withValidate no
           , withTrace 1
           , withRemoveWS yes
           , withPreserveComment no
           ]
         ) files
       )
      )
      >>>
      arr processActivities
      >>>
      xpickleDocument xpGPX
        [withIndent yes
        ] ("New_"++file)
      )
    return ()

processActivities :: [GPX] -> GPX
processActivities atys@(GPX c x s v ns m (Trk nm t _) : _) =
  GPX c x s v ns m (Trk nm t (atys >>= (tPts . aTrk)))
