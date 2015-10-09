{-
    The Post.hs is specifying things happening in the stage0 and stage1. But Index.sh will only happen in
    stage1, and it has some requirements over the information it hopes for.
-}

{-
    Maybe a light weight enhancement like the whamlet would be necessary
-}

module BBQ.Index where

import BBQ.Import
import qualified Data.HashMap.Lazy as HM
import BBQ.Route
import BBQ.Task

indexStage2 :: WriteTask Widgets
indexStage2 = WriteTask f deps
    where
        f widgets = do -- return target path and content
            let mPosts = HM.lookup "posts" widgets
            let html = $(hamletFile $(templDirQ "home.hamlet")) contentRender
            path <- filePath Home
            return (path, renderHtml html)

        deps = [$(templDirQ "home.hamlet")]

