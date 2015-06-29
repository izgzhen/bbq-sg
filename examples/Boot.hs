-- Bootstraping tool for a empty post

import BBQ.SG.Plugin
import System.Environment
import Config
import System.FilePath ((</>))

main = do
    args <- getArgs
    if length args /= 1 || length (head args) == 0 then do
        print usageInfo
    else do
        let filename = head args
        today <- getToday
        writeFile (markdownDir </> filename ++ ".md") (template today)


usageInfo = "usage: runghc Boot [filename]" ++
            "example: runghc Boot happy-new-year" ++
            "NOTE: don't add .md and filename might not be the title"

template today = "\n---\n" ++
                 "+ Tags: \n" ++
                 "+ Date: " ++ today ++ "\n" ++
                 "+ Author: " ++ defaultAuthor ++ "\n"

defaultAuthor  = "Your Name <youremail@somewhere>"
