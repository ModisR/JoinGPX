module PickleGPX
  ( GPX   (..)
  , Track (..)
  , xpGPX
  ) where

import Text.XML.HXT.Core

data GPX = GPX
  { aTime    :: String
  , aCreator :: String
  , xsi      :: String
  , schLoc   :: String
  , version  :: String
  , xmlns    :: String
  , aTrk     :: Track
  }

data Track = Trk
  { tName :: String
  , tType :: String
  , tPts  :: [TrackPoint]
  }

data TrackPoint = TrkPt
  { pLat  :: String
  , pLon  :: String
  , pEle  :: String
  , pTime :: String
  }
  
instance XmlPickler GPX where
  xpickle = xpGPX
  
instance XmlPickler Track where
  xpickle = xpTrack
  
instance XmlPickler TrackPoint where
  xpickle = xpTrackPoint

xpGPX :: PU GPX
xpGPX =
  xpElem "gpx" $
  xpWrap ( \(c, x, s, v, n, m, t) -> GPX c x s v n m t
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
