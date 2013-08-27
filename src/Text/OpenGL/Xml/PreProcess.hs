module Text.OpenGL.Xml.PreProcess(preProcessRegistry) where

import Text.OpenGL.Xml.CParse

import Control.Category
import Prelude hiding (id, (.))
import Text.XML.HXT.Core

preProcessRegistry :: (ArrowChoice a, ArrowXml a) => a XmlTree XmlTree
preProcessRegistry
    = processTopDown
      ( ( processChildren
          ( (addBulkDef `when` isBulkDef) >>>
            (replaceSimpleTypes `when` isSimpleType)
          ) `when` hasName "types"
        ) >>>
        ( processChildren (replaceCommands `when` isCommand)
          `when` hasName "commands"
        )
      )
    where
        addBulkDef :: ArrowXml a => a XmlTree XmlTree
        addBulkDef = replaceChildren $ mkelem "ctype" [] [getChildren]
        isBulkDef :: ArrowXml a => a XmlTree XmlTree
        isBulkDef = hasName "type" >>> hasAttr "name"

        isSimpleType :: ArrowXml a => a XmlTree XmlTree
        isSimpleType = hasName "type" >>> getChildren >>> hasName "name"
        replaceSimpleTypes :: (ArrowChoice a, ArrowXml a) => a XmlTree XmlTree
        replaceSimpleTypes =
            pullOutName1 >>>
            processChildren isText >>> --Remove all non text parts (i.e. apientry)
            collapseXText >>>
            replaceChildren (getChildren >>> getText >>> parseType)

        isCommand :: ArrowXml a => a XmlTree XmlTree
        isCommand = hasName "command" >>> getChildren >>> hasName "proto"
        replaceCommands :: (ArrowChoice a, ArrowXml a) => a XmlTree XmlTree
        replaceCommands =
            pullOutCommandName
          where
            pullOutCommandName =
                (addAttr "name"
                    $< (getNamedChildren "proto" >>> getNamedChildren "name" >>> getChildText))
                >>> processChildren (
                        processChildren (
                            (getChildren `when` hasName "name") >>>
                            (getChildren `when` hasName "ptype")
                            )  `when` hasName "proto")
                >>> processChildren (
                        (pullOutName1 >>> processChildren (getChildren `when` hasName "ptype"))
                        `when` hasName "param")
                >>> collapseAllXText
                >>> processChildren ((replaceChildren $ getChildren >>> getText >>> parseFuncPart) `when` (hasName "param" <+> hasName "proto"))
                >>> processChildren (setElemName (mkName "return") `when` hasName "proto")

pullOutName1 :: ArrowXml a => a XmlTree XmlTree
pullOutName1 =
    (addAttr "name"
        $< (getNamedChildren "name" >>> getChildText))
    >>>
    processChildren ( getChildren `when` hasName "name")

getNamedChildren :: ArrowXml a => String -> a XmlTree XmlTree
getNamedChildren n = getChildren >>> hasName n

getChildText :: ArrowXml a => a XmlTree String
getChildText = getChildren >>> getText

parseType :: (ArrowChoice a, ArrowXml a) => a String XmlTree
parseType = arr parseStringTypeDef
     >>> ((arr show >>> mkText) ||| (xpickleVal xpickle >>> getChildren))

parseFuncPart :: (ArrowChoice a, ArrowXml a) => a String XmlTree
parseFuncPart =
    arr parseStringFuncPart >>>
    ((arr show >>> mkText) ||| (xpickleVal xpickle >>> getChildren))
