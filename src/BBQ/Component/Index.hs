module BBQ.Component.Index where

import BBQ.Import
import qualified Data.HashMap.Lazy as HM
import BBQ.Route
import BBQ.Task

indexStage2 :: WriteTask Widgets
indexStage2 = WriteTask f deps
    where
        f widgets = do -- return target path and content
            let mPosts = HM.lookup "postsList" widgets
            let html = $(hamletFile $(templDirQ "index.hamlet")) contentRender
            path <- filePath Index
            return (path, renderHtml html)

        deps = [$(templDirQ "index.hamlet")]

