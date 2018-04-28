module Link where

import System.Directory

makeLink :: String -> String -> IO ()
makeLink aName aLink = do
    aHomeDirectory <- getHomeDirectory
    let aFile = aHomeDirectory ++ "/.tagFS/files/" ++ aName ++ ".html"
    writeFile aFile $
        "<html><body><script type=\"text/javascript\">" ++
            "window.location.href = \"" ++ aLink ++ "\";" ++
        "</script></body></html>"
