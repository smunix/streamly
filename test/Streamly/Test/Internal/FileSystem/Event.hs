import Test.Hspec
import Test.Hspec.QuickCheck
import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import System.Directory
import System.FilePath ((</>))
import Streamly.Prelude (SerialT)
import Streamly.Internal.Data.Array.Storable.Foreign (Array)

import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Data.Array.Storable.Foreign as Array
import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
#if defined(CABAL_OS_DARWIN)
import qualified Streamly.Internal.FileSystem.Event.Darwin as Event
#elif defined(CABAL_OS_LINUX)
import qualified Streamly.Internal.FileSystem.Event.Linux as Event
#elif defined(CABAL_OS_WINDOWS)
import qualified Streamly.Internal.FileSystem.Event.Windows as Event
#else
#error "FS Events not supported on this platform
#endif

import System.IO
import qualified Data.Set as Set
#if !defined(CABAL_OS_WINDOWS)
import qualified Streamly.Internal.Unicode.Stream as U
import Data.Functor.Identity (runIdentity)
#endif

main :: IO ()
main = hspec $ do
    prop "Recursive directory events validator" validator
    prop "Staging directories cleanUp" cleanUp   

validator ::  Expectation
validator = testRunner `shouldReturn` "PASSED"

#if defined(CABAL_OS_WINDOWS)
cleanUp :: Expectation
cleanUp = destroy `shouldThrow` anyException
#else
cleanUp :: Expectation
cleanUp = destroy `shouldReturn` "CLEAN"
#endif

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
toUtf8 :: MonadIO m => String -> m (Array Word8)
toUtf8 = Array.fromStream . Unicode.encodeUtf8' . Stream.fromList

#if !defined(CABAL_OS_WINDOWS)
utf8ToString :: Array Word8 -> String
utf8ToString = runIdentity . Stream.toList . U.decodeUtf8' . Array.toStream
#endif
-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------
watchPaths :: NonEmpty (Array Word8) -> SerialT IO Event.Event
watchPaths = Event.watchTrees

baseList :: [String]
baseList =  -- ^ List of predefined RelPath_EventFlag_Dir to be matched
#if defined(CABAL_OS_WINDOWS)
    [ "Oct23_1"
    , "Oct23\\Oct24_1"
    , "Oct23\\Oct24\\Oct25_1"
    , "Oct23\\Oct24\\Oct25\\dirnew_1"
    , "Oct23\\Oct24\\Oct25_3"
    , "Oct23\\Oct24\\Oct25\\dirnew_4"
    , "Oct23\\Oct24\\Oct25\\renamed_5"
    , "Oct23\\Oct24\\Oct25_3"
    , "Oct23_3"
    , "Oct23\\Oct24\\Oct25\\Filecreate.txt_1"
    , "Oct23\\Oct24\\Oct25\\Filecreate.txt_3"
    , "Oct23\\Oct24\\Oct25\\Filecreate.txt_3"
    , "Oct23\\Oct24\\Oct25_3"
    , "Oct23\\Oct24\\Oct25\\Filecreate.txt_4"
    , "Oct23\\Oct24\\Oct25\\FileRenamed.txt_5"
    , "Oct23\\Oct24\\Oct25_3"
    , "Oct23\\Oct24\\Oct25\\FileRenamed.txt_2"
    , "Oct23\\Oct24\\Oct25\\renamed_2"
    ]
#else
    [ "Oct23_1073742080_Dir"
    , "Oct23_1073741856_Dir"
    , "Oct23_1073741825_Dir"
    , "Oct23_1073741840_Dir"
    , "Oct23/Oct24_1073741856_Dir"
    , "Oct23/Oct24_1073741825_Dir"  
    , "Oct23/Oct24/Oct25/dirnew_2048"
    , "Oct23/Oct24/Oct25/dirnew_1073741856_Dir"
    , "Oct23/Oct24/Oct25/dirnew_1073741825_Dir"
    , "Oct23/Oct24/Oct25/dirnew_1073741840_Dir"
    , "Oct23/Oct24/Oct25/dirnew_1073741888_Dir"
    , "Oct23/Oct24/Oct25/renamed_1073741952_Dir"
    , "Oct23/Oct24/Oct25/Filecreate.txt_256"
    , "Oct23/Oct24/Oct25/Filecreate.txt_32"
    , "Oct23/Oct24/Oct25/Filecreate.txt_2"
    , "Oct23/Oct24/Oct25/Filecreate.txt_8"
    , "Oct23/Oct24/Oct25/renamed_1073741856_Dir"
    , "Oct23/Oct24/Oct25/Filecreate.txt_64"
    , "Oct23/Oct24/Oct25/FileRenamed.txt_128"
    , "Oct23/Oct24/Oct25/FileRenamed.txt_512"
    ]
#endif

data Sync = Sync (MVar String)

driver :: Handle -> FilePath -> Sync -> Sync -> IO ()
driver handle rootPath (Sync m1) (Sync m2) = do
    let args = [rootPath]
    paths <- mapM toUtf8 args
    putMVar m1 "driverStart"
    print "Watch started >>>>>>>>>>>>>>>>>>"
    let strm = Stream.takeWhile eventPredicate $ watchPaths (NonEmpty.fromList paths)
    Stream.mapM_ ((hPutStrLn handle) . Event.showEventShort) strm
    print "Watch Finished %%%%%%%%%%%%%%%%%"
    putMVar m2 "driverSopped"

eventPredicate :: Event.Event -> Bool
eventPredicate ev =
#if defined(CABAL_OS_WINDOWS)
    if (Event.getRelPath ev) == "stop_event_tracker"
#else    
    if (utf8ToString $ Event.getRelPath ev) == "stop_event_tracker"
#endif    
    then False
    else True

initTest :: Sync -> IO Sync
initTest (Sync m)= do
    _ <- takeMVar m    
    cwd <- getCurrentDirectory
    let watchDir = (cwd </> "watch")
    threadDelay (1000000)  -- ^ wait for Event driver to process the Events
        >> createDirectoryIfMissing True (watchDir </> "Oct23")
    print "initTest >>>>>>>>>>>>>>>>>>"
    putMVar m "driverStart"
    return (Sync m)

testListing :: Sync -> IO Sync
testListing (Sync m) = do
    _ <- takeMVar m
    cwd <- getCurrentDirectory
    let watchDir = (cwd </> "watch")
        tpath = (watchDir </> "Oct23")
    _ <- threadDelay (1000000)  -- ^ wait for Event driver to process the Events
        >> listDirectory tpath
    print "testListing >>>>>>>>>>>>>>>>>>"
    putMVar m "driverStart"
    return (Sync m)

testCreateDir :: Sync -> IO Sync
testCreateDir (Sync m) = do
    _ <- takeMVar m
    cwd <- getCurrentDirectory
    let watchDir = (cwd </> "watch")
        tpath = (watchDir </> "Oct23" </> "Oct24" </> "Oct25" </> "dirnew")
    threadDelay (1000000)  -- ^ wait for Event driver to process the Events    
        >> createDirectoryIfMissing True tpath
    print "testCreateDir >>>>>>>>>>>>>>>>>>"
    putMVar m "driverStart"
    return (Sync m)

testRenameDir :: Sync -> IO Sync
testRenameDir (Sync m) = do
    _ <- takeMVar m    
    cwd <- getCurrentDirectory
    let watchDir = (cwd </> "watch")
        src = (watchDir </> "Oct23" </> "Oct24" </> "Oct25" </> "dirnew")
        tgt = (watchDir </> "Oct23" </> "Oct24" </> "Oct25" </> "renamed")
    threadDelay(1000000) >> renamePath src tgt
    print "testRenameDir >>>>>>>>>>>>>>>>>>"
    putMVar m "driverStart"
    return (Sync m)

testRemoveDir :: Sync -> IO Sync
testRemoveDir (Sync m) = do
    _ <- takeMVar m
    cwd <- getCurrentDirectory
    let watchDir = (cwd </> "watch")
        tpath = (watchDir </> "Oct23" </> "Oct24" </> "Oct25" </> "renamed")
    threadDelay (1000000)  -- ^ wait for Event driver to process the Events    
        >> removePathForcibly tpath
    threadDelay (100000)  -- ^ wait for Event driver to process the Events    
        >> createDirectoryIfMissing True (watchDir </> "stop_event_tracker")
    print "testRemoveDir >>>>>>>>>>>>>>>>>>"
    putMVar m "driverStart"
    return (Sync m)

testCreateFile :: Sync -> IO Sync
testCreateFile (Sync m) = do
    _ <- takeMVar m
    cwd <- getCurrentDirectory
    let watchDir = (cwd </> "watch")
        tpath = (watchDir </> "Oct23/Oct24/Oct25" </> "Filecreate.txt")
    handle <- openFile tpath WriteMode
    hPutStr handle "DONE"
    hClose handle
    print "testCreateFile >>>>>>>>>>>>>>>>>>"
    putMVar m "driverStart"
    return (Sync m)

testRenameFile :: Sync -> IO Sync
testRenameFile (Sync m) = do
    _ <- takeMVar m
    cwd <- getCurrentDirectory
    let watchDir = (cwd </> "watch")
        spath = (watchDir </> "Oct23/Oct24/Oct25" </> "Filecreate.txt")
        tpath = (watchDir </> "Oct23/Oct24/Oct25" </> "FileRenamed.txt")
    threadDelay (1000000)  -- ^ wait for Event driver to process the Events    
        >> renameFile spath tpath
    print "testRenameFile >>>>>>>>>>>>>>>>>>"
    putMVar m "driverStart"
    return (Sync m)

testRemoveFile :: Sync -> IO Sync
testRemoveFile (Sync m) = do
    _ <- takeMVar m
    cwd <- getCurrentDirectory
    let watchDir = (cwd </> "watch")
        tpath = (watchDir </> "Oct23/Oct24/Oct25" </> "FileRenamed.txt")
    threadDelay (1000000)  -- ^ wait for Event driver to process the Events    
        >> removeFile tpath
    print "testRemoveFile >>>>>>>>>>>>>>>>>>"
    putMVar m "driverStart"
    return (Sync m)

destroy :: IO [Char]
destroy = do
    cwd <- getCurrentDirectory
    let watchDir = (cwd </> "watch")
        resultPath = cwd </> "testResult"
    removePathForcibly watchDir
    removePathForcibly resultPath
    return "CLEAN"

stopTask :: IO ()
stopTask =  do
    yield
    print "stopTask @@@@@@@@@@@@@@"

validate :: IO [Char]
validate = do  -- ^ print Events matched or missed
    let baseSet = Set.fromList baseList
    resultSet <- processResult
    mapM_ ( \x -> 
        if Set.member x resultSet
        then  print (x ++ "....Matched")
        else print (x ++ "....Missed") ) 
        baseList    
    if baseSet `Set.isSubsetOf` resultSet
    then
        return "PASSED"
    else
        return "FAILED"

startTestRunner :: Sync -> IO ()
startTestRunner (Sync m) = do
    msg <- takeMVar m
    putMVar m msg
    m1 <- initTest (Sync m)
    m2 <- testCreateDir m1
    m3 <- testRenameDir m2
    m4 <- testListing m3
    m5 <- testCreateFile m4
    m6 <- testRenameFile m5
    m7 <- testRemoveFile m6
    m8 <- testRemoveDir m7
    _ <- testListing m8
    return ()

testRunner :: IO [Char]
testRunner = do
    hSetBuffering stdout NoBuffering
    pre <- newEmptyMVar
    post <- newEmptyMVar
    cwd <- getCurrentDirectory
    let resultPath = cwd </> "testResult"
        fpath = (resultPath </> "output.txt")
        rootPath = (cwd </> "watch")
    createDirectoryIfMissing True rootPath
    createDirectoryIfMissing True resultPath
    handle <- openFile fpath WriteMode
    tid <- forkIO $ driver handle rootPath (Sync pre) (Sync post)
    startTestRunner $ Sync pre
    _ <- takeMVar post
    stopTask
    hClose handle
    killThread tid
    validate

processResult :: IO (Set.Set String)
processResult = do
    cwd <- getCurrentDirectory
    let resultPath = cwd </> "testResult/output.txt"

    inh <- openFile resultPath ReadMode
    ltr <- mainloop inh []
    hClose inh
    return (Set.fromList ltr)

mainloop :: Handle -> [String] -> IO [String]
mainloop inh xs = do
    ineof <- hIsEOF inh
    if ineof
    then return xs
    else do
        inpStr <- hGetLine inh
        mainloop inh (inpStr : xs)