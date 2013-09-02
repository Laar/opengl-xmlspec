module Text.OpenGL.Xml.ReadRegistry (
    readRegistry,
    parseRegistry,
    PreProcess
) where

import Prelude hiding ((.), id)

import Control.Category

import Text.OpenGL.Types
import Text.OpenGL.Xml.Pickle()
import Text.OpenGL.Xml.PreProcess

import Text.XML.HXT.Core

type PreProcess = Bool

-- TODO RelaxNG validation
readRegistry :: FilePath -> PreProcess -> IO (Either String Registry)
readRegistry fp pp = do
    results <- runX (
            readDocument readOpts fp >>> preProcess pp >>> parseRegistryArrow
        ) -- TODO: error handling
    return $ handleResults results
  where
    readOpts :: [SysConfig]
    readOpts = [withValidate no, withPreserveComment no]

preProcess :: (ArrowChoice a, ArrowXml a) => PreProcess -> a XmlTree XmlTree
preProcess pp = if pp then preProcessRegistry else id

-- | TODO: make it work (doesn't work with the <?xml ?> header.
parseRegistry :: String -> PreProcess -> Either String Registry
parseRegistry str pp = handleResults $ runLA (
        xread >>> preProcess pp >>> parseRegistryArrow
    ) str

handleResults :: [Either String Registry] -> Either String Registry
handleResults rs = case rs of
    []      -> Left "No parse"
    (_:_:_) -> Left "Multiple parse"
    [rc]    -> rc


parseRegistryArrow :: ArrowXml a => a XmlTree (Either String Registry)
parseRegistryArrow =
    removeAllWhiteSpace >>> -- This processing is needed for the non IO case.
    removeAllComment >>>
    arr (unpickleDoc' xpickle)
