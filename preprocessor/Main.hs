module Main (main) where

import System.Environment
import System.Exit

import Text.XML.HXT.Core
import Text.OpenGL.Xml.PreProcess(preProcessRegistry)

main :: IO ()
main = do
    argv <- getArgs
    (al, src, dst) <- cmdlineOpts argv
    [rc]  <- runX (application al src dst)
    if rc >= c_err
     then exitWith (ExitFailure (0-1))
     else exitWith ExitSuccess

-- | the dummy for the boring stuff of option evaluation,
-- -- usually done with 'System.Console.GetOpt'

cmdlineOpts     :: [String] -> IO (SysConfigList, String, String)
cmdlineOpts argv
    = return ([withValidate no], argv!!0, argv!!1)

-- | the main arrow
application   :: SysConfigList -> String -> String -> IOSArrow b Int
application cfg src dst
    = configSysVars cfg >>>
      readDocument [withPreserveComment yes] src >>>
      processChildren (preProcessRegistry `when` isElem) >>>
      writeDocument [] dst >>>
      getErrStatus
