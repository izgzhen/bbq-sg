import BBQ.SG
import Index
import Posts
import Tags
import Config

config = Config_ staticDir
                 markdownDir
                 imgSrcDir
                 postsDir
                 imgStaDir
                 jsSrcDir
                 jsStaDir
                 cssSrcDir
                 cssStaDir
                 analyticsId

main = runSG config indexLayout postsLayout tagsLayout
