import System.Environment
import Text.XML.HXT.Core
 
main :: IO ()
main = do
  files@(file : _) <- getArgs
  runX (
    (foldr
      (\a b -> a &&& b >>> arr (uncurry (:)))
      (arr (const []))
      (map (xunpickleDocument xpActivity
            [ withValidate no
            , withTrace 1
            , withRemoveWS yes
            , withPreserveComment no
            ]
           ) files)
    )
    >>>
    (arr processActivities)
    >>>
    xpickleDocument xpActivity
                    [withIndent yes
                    ] ("new_"++file)
    )
  return ()

processActivities :: [Activity] -> Activity
processActivities atys@(Aty c x s v ns m (Trk nm t _) : _) =
  Aty c x s v ns m (Trk nm t (atys >>= (tPts . aTrk)))

data Activity = Aty
  { aTime    :: String
  , aCreator :: String
  , xsi      :: String
  , schLoc   :: String
  , version  :: String
  , xmlns    :: String
  , aTrk     :: Track
  } deriving (Show, Eq)

data Track = Trk
  { tName   :: String
  , tType   :: String
  , tPts :: [TrackPoint]
  } deriving (Show, Eq)

data TrackPoint = TrkPt
  { pLat  :: String
  , pLon  :: String
  , pEle  :: String
  , pTime :: String
  } deriving (Show, Eq)
  
instance XmlPickler Activity where
  xpickle = xpActivity
  
instance XmlPickler Track where
  xpickle = xpTrack
  
instance XmlPickler TrackPoint where
  xpickle = xpTrackPoint

xpActivity :: PU Activity
xpActivity =
  xpElem "gpx" $
  xpWrap ( \(c, x, s, v, n, m, t) -> Aty c x s v n m t
         , \a -> (
             aTime a, aCreator a,
             xsi a, schLoc a, version a, xmlns a,
             aTrk a)
         ) $
  xp7Tuple (xpAttr "creator" xpText)
           (xpAttr "xmlns:xsi" xpText)
           (xpAttr "xsi:schemaLocation" xpText)
           (xpAttr "version" xpText)
           (xpAttr "xmlns" xpText)
           (xpElem "metadata" $ xpElem "time" xpText)
           xpTrack          

xpTrack :: PU Track
xpTrack =
  xpElem "trk" $
  xpWrap ( \(n, t, s) -> Trk n t s
         , \t -> (tName t, tType t, tPts t)) $
  xpTriple (xpElem "name" xpText)
           (xpElem "type" xpText)
           (xpElem "trkseg" $ xpList xpTrackPoint)

xpTrackPoint :: PU TrackPoint
xpTrackPoint =
  xpElem "trkpt" $
  xpWrap ( \(la, lo, e, t) -> TrkPt la lo e t
         , \p -> (pLat p, pLon p, pEle p, pTime p)) $
  xp4Tuple (xpAttr "lat"  xpText)
           (xpAttr "lon"  xpText)
           (xpElem "ele"  xpText)
           (xpElem "time" xpText)
