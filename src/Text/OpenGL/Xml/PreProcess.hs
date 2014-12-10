module Text.OpenGL.Xml.PreProcess(preProcessRegistry) where

import Text.OpenGL.Xml.CProcess

import Control.Category
import Prelude hiding (id, (.))
import Text.XML.HXT.Core

preProcessRegistry :: (ArrowChoice a, ArrowXml a) => a XmlTree XmlTree
preProcessRegistry
    = processTopDown
      ( ( processChildren
          (
              ( (addBulkDef `when` isBulkDef) >>>
                (replaceSimpleTypes `when` isSimpleType)
              ) `when` hasName "type"
          ) `when` hasName "types"
        ) >>>
        ( (replaceCommands `when` isCommand)
          `when` hasName "command"
        )
      )
    where
        addBulkDef :: ArrowXml a => a XmlTree XmlTree
        addBulkDef = replaceChildren $ mkelem "ctype" [] [getChildren]
        isBulkDef :: ArrowXml a => a XmlTree XmlTree
        isBulkDef = hasAttr "name"

        isSimpleType :: ArrowXml a => a XmlTree XmlTree
        isSimpleType = getChildren >>> hasName "name"
        replaceSimpleTypes :: (ArrowChoice a, ArrowXml a) => a XmlTree XmlTree
        replaceSimpleTypes =
            pullOutName1 >>>
            processChildren isText >>> --Remove all non text parts (i.e. apientry)
            collapseXText >>> -- Merge all text children.
            replaceChildren (this /> getText >>> parseType)

        isCommand :: ArrowXml a => a XmlTree XmlTree
        isCommand = getChildren >>> hasName "proto"
        replaceCommands :: (ArrowChoice a, ArrowXml a) => a XmlTree XmlTree
        replaceCommands =
            pullOutCommandName
          where
            pullOutCommandName =
                -- Extract the name of the command
                (addAttr "name"
                    $< (this /> hasName "proto" /> hasName "name" /> getText))
                -- Extract the name of each argument
                >>> processChildren (pullOutName1 `when` hasName "param")
                -- Remove name and ptype nodes (leaving the text content)
                >>> processChildren (
                        replaceWithChildrenWhen (hasNameWith $ \qn ->
                            localPart qn `elem` ["name", "ptype"])
                    )
                -- And patch merge the resulting text
                >>> collapseAllXText
                -- Do the actual C-type parsing
                >>> processChildren ((replaceChildren $ this /> getText >>> parseFuncPart)
                        `when` (hasName "param" <+> hasName "proto"))
                -- And rename the return type
                >>> processChildren (setElemName (mkName "return") `when` hasName "proto")

pullOutName1 :: ArrowXml a => a XmlTree XmlTree
pullOutName1 =
    (addAttr "name"
        $< (this /> hasName "name" /> getText))
    >>> -- and remove the name node (replacing it by its contensts)
    replaceWithChildrenWhen (hasName "name")

replaceWithChildrenWhen :: ArrowXml a => a XmlTree XmlTree -> a XmlTree XmlTree
replaceWithChildrenWhen p = processChildren (getChildren `when` p)

parseType :: (ArrowChoice a, ArrowXml a) => a String XmlTree
parseType = arr parseStringTypeDef
     >>> (mkText ||| (xpickleVal xpickle >>> getChildren))

parseFuncPart :: (ArrowChoice a, ArrowXml a) => a String XmlTree
parseFuncPart =
    arr parseStringFuncPart >>>
    (mkText ||| (xpickleVal xpickle >>> getChildren))
