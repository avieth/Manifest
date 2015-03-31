{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Manifest.Manifest
import Manifest.Function
import Manifest.M
import Manifest.SmartStrategy
import Manifest.SQLite

type X a = M SmartStrategy (SmartStrategy a)

type PostId = Int
type Post = T.Text

recentPostsIds :: Function ReadOnly () [PostId]
recentPostsIds = function $ sqliteMultiple "posts.db" "recent_posts" "domain" "range"

postInfo :: Function ReadOnly PostId (Maybe PostInfo)
postInfo = function $ sqliteSingle "posts.db" "post_info" "domain" "range"

postViews :: Function ReadOnly PostId (Maybe Int)
postViews = function $ sqliteSingle "posts.db" "post_views" "domain" "range"

postFromId :: Function ReadOnly PostId (Maybe Post)
postFromId = function $ sqliteSingle "posts.db" "posts" "domain" "range"

maybeToList :: Maybe a -> [a]
maybeToList mebe = case mebe of
    Just x -> [x]
    Nothing -> []

recentPosts :: Function ReadOnly () [Post]
recentPosts = recentPostsIds ~> (shift maybeToList postFromId)

blog :: X Blog
blog = do
    main <- mainPane
    left <- leftPane
    renderPage <$> leftPane <*> mainPane
