{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import System.Environment (getArgs)
import Control.Monad (when, forM_)
import Control.Monad.IO.Class
import Debug.Trace (trace)

import Path (Path, Abs, Rel, File, Dir, (</>), toFilePath, parent, filename, dirname, parseRelDir)
import Path.IO (listDirRecurRel, resolveDir')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type PathParts = [Text]

type DirPair = (Path Abs Dir, Path Abs Dir)

data FileTree = FileTree
    { name       :: Text
    , population :: Int
    , children   :: Map Text FileTree
    } deriving (Eq, Show)

newFileTree :: Text -> FileTree
newFileTree name = FileTree name 0 Map.empty

addChild :: FileTree -> PathParts -> FileTree
addChild ft [] = ft
addChild ft@FileTree{..} (p:ps) = ft { children = newChildren, population = population + 1 }
    where
        oldChild = Map.findWithDefault (newFileTree p) p children
        newChild = addChild oldChild ps
        newChildren = Map.insert p newChild children

find :: FileTree -> FileTree -> PathParts -> [PathParts]
find ft1 ft2 ps | p2 < p1    = []
                | ft1 == ft2 = [ps]
                | otherwise  = concat cresults
    where
        p1 = population ft1
        p2 = population ft2
        cresults = (\ft' -> find ft1 ft' $ ps ++ [name ft']) <$> children ft2

findSubTrees :: Int -> FileTree -> FileTree -> PathParts -> [(FileTree, PathParts, PathParts)]
findSubTrees pop ft1 ft2 ps | population ft1 < pop = []
                            | null result          = concat cresults
                            | otherwise            = result
    where
        result = (ft1,ps,) <$> find ft1 ft2 []
        cresults = (\ft' -> findSubTrees pop ft' ft2 $ ps ++ [name ft']) <$> children ft1

fromList :: [PathParts] -> FileTree
fromList = foldl addChild (newFileTree "")

toText :: Path b t -> Text
toText = T.pack . toFilePath

toParts :: Path b t -> PathParts
toParts path = T.splitOn "/" $ toText path

fromParts :: MonadIO m => Path Abs Dir -> PathParts -> m (Path Abs Dir)
fromParts path parts = liftIO $ (path </>) <$> parseRelDir pp
    where
        pp = T.unpack $ T.intercalate "/" parts

isHidden :: Path b t -> Bool
isHidden path = any (T.isPrefixOf ".") $ toParts path

getFiles :: MonadIO m => Path Abs Dir -> m [Path Rel File]
getFiles dir = listDirRecurRel dir >>= \(file, files) -> return $ filter (not . isHidden) files

main :: IO ()
main = do
    args <- getArgs
    when (length args < 2) $ error "Need at least two arguments"
    (dir1:dir2:_) <- mapM resolveDir' args
    files1 <- getFiles dir1
    let ft1 = fromList $ toParts <$> files1
    files2 <- getFiles dir2
    let ft2 = fromList $ toParts <$> files2
    let subtrees = findSubTrees 1 ft1 ft2 []
    forM_ subtrees $ \(_,src,dst) -> do
        srcPath <- fromParts dir1 src
        dstPath <- fromParts dir2 dst
        putStrLn $ "rm -rf " <> show dstPath
        putStrLn $ "cp -r --reflink=always " <> show srcPath <> " " <> show dstPath
