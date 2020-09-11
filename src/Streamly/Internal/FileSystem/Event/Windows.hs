{-# LANGUAGE ForeignFunctionInterface #-}

module Streamly.Internal.FileSystem.Event.Windows
    (   
      Config
    , Event
    , Toggle (..)
    , setFlag
    , setRecursiveMode
    , setModifiedFileName
    , setModifiedDirName
    , setModifiedAttribute
    , setModifiedSize
    , setModifiedLastWrite
    , setModifiedSecurity
    , setAllEvents
    , defaultConfig
    , getConfigFlag
    , eventStreamAggr
    , strPathToStrHandle
    , closePathHandleStream
    , watchPaths
    , watchPathsWith
    , watchTrees
    , watchTreesWith
    , getWatchHandle
    , readDirectoryChanges
    , getRelPath 
    , getRoot
    , isCreated
    , isDeleted 
    , isMovedFrom
    , isMovedTo
    , isModified 
    , isOverflow
    , showEvent      
    ) 
where

import Data.Bits ((.|.), (.&.), complement)         
import Data.List.NonEmpty (NonEmpty)
import Foreign.C.String (peekCWStringLen)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Storable (peekByteOff)    
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr, nullFunPtr, plusPtr) 
import Streamly.Prelude (SerialT, parallel)
import System.Win32.File (
                            FileNotificationFlag
                          , LPOVERLAPPED
                          , closeHandle
                          , createFile
                          , fILE_FLAG_BACKUP_SEMANTICS
                          , fILE_LIST_DIRECTORY
                          , fILE_NOTIFY_CHANGE_FILE_NAME
                          , fILE_NOTIFY_CHANGE_DIR_NAME
                          , fILE_NOTIFY_CHANGE_ATTRIBUTES
                          , fILE_NOTIFY_CHANGE_SIZE
                          , fILE_NOTIFY_CHANGE_LAST_WRITE
                          , fILE_NOTIFY_CHANGE_SECURITY     
                          , fILE_SHARE_READ
                          , fILE_SHARE_WRITE
                          , oPEN_EXISTING                        
                          )
import System.Win32.Types (BOOL, DWORD, HANDLE, LPVOID, LPDWORD, failIfFalse_)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Internal.Data.Stream.IsStream as S

-- | Watch configuration, used to specify the events of interest and the
-- behavior of the watch.
--
-- /Internal/
--
data Config = Config
    { watchRec :: BOOL
    , createFlags :: DWORD 
    }

-------------------------------------------------------------------------------
-- Boolean settings
-------------------------------------------------------------------------------

-- | Whether a setting is 'On' or 'Off'.
--
-- /Internal/
--
data Toggle = On | Off

setFlag :: DWORD -> Toggle -> Config -> Config
setFlag mask status cfg@Config{..} =
    let flags =
            case status of
                On -> createFlags .|. mask
                Off -> createFlags .&. complement mask
    in cfg {createFlags = flags}

-- | Set watch event on directory recursively.
--
-- /default: On/
--
-- /Internal/
--
setRecursiveMode :: BOOL -> Config -> Config
setRecursiveMode rec cfg@Config{} = cfg {watchRec = rec}

-- | Report when a file name is modified.
--
-- /default: On/
--
-- /Internal/
--
setModifiedFileName :: Toggle -> Config -> Config
setModifiedFileName = setFlag fILE_NOTIFY_CHANGE_FILE_NAME


-- | Report when a directory name is modified.
--
-- /default: On/
--
-- /Internal/
--
setModifiedDirName :: Toggle -> Config -> Config
setModifiedDirName = setFlag fILE_NOTIFY_CHANGE_DIR_NAME

-- | Report when a file attribute is modified.
--
-- /default: On/
--
-- /Internal/
--
setModifiedAttribute :: Toggle -> Config -> Config
setModifiedAttribute = setFlag fILE_NOTIFY_CHANGE_ATTRIBUTES

-- | Report when a file size is changed.
--
-- /default: On/
--
-- /Internal/
--
setModifiedSize :: Toggle -> Config -> Config
setModifiedSize = setFlag fILE_NOTIFY_CHANGE_SIZE   

-- | Report when a file last write time is changed.
--
-- /default: On/
--
-- /Internal/
--
setModifiedLastWrite :: Toggle -> Config -> Config
setModifiedLastWrite = setFlag fILE_NOTIFY_CHANGE_LAST_WRITE    

-- | Report when a file Security attributes is changed.
--
-- /default: On/
--
-- /Internal/
--
setModifiedSecurity :: Toggle -> Config -> Config
setModifiedSecurity = setFlag fILE_NOTIFY_CHANGE_SECURITY    

-- | Set all events 'On' or 'Off'.
--
-- /default: On/
--
-- /Internal/
--
setAllEvents :: Toggle -> Config -> Config
setAllEvents s cfg =
    ( setModifiedFileName s
    . setModifiedDirName s
    . setModifiedAttribute s
    . setModifiedSize s
    . setModifiedLastWrite s
    . setModifiedSecurity s
    ) cfg

defaultConfig :: Config
defaultConfig = setAllEvents On $ Config { watchRec = True, createFlags = 0 }

getConfigFlag :: Config -> DWORD
getConfigFlag Config{..} = createFlags

getConfigRecMode :: Config -> BOOL
getConfigRecMode Config{..} = watchRec

data Event = Event
    { 
      eventFlags :: DWORD  
    , eventRelPath :: String         
    , eventRootPath :: String 
    , totalBytes :: DWORD
    } deriving (Show, Ord, Eq)   

data FILE_NOTIFY_INFORMATION = FILE_NOTIFY_INFORMATION
    { fniNextEntryOffset :: DWORD
    , fniAction :: DWORD
    , fniFileName :: String
    } deriving Show    

type LPOVERLAPPED_COMPLETION_ROUTINE = FunPtr ((DWORD, DWORD, LPOVERLAPPED) -> IO ())

foreign import ccall safe "windows.h ReadDirectoryChangesW"
    c_ReadDirectoryChangesW :: HANDLE -> LPVOID -> DWORD -> BOOL -> DWORD -> LPDWORD -> LPOVERLAPPED -> LPOVERLAPPED_COMPLETION_ROUTINE -> IO BOOL

readDirectoryChangesW :: HANDLE -> Ptr FILE_NOTIFY_INFORMATION -> DWORD -> BOOL -> FileNotificationFlag -> LPDWORD -> IO ()
readDirectoryChangesW h buf bufSize wst f br =
    failIfFalse_ "ReadDirectoryChangesW" $ c_ReadDirectoryChangesW h (castPtr buf) bufSize wst f br nullPtr nullFunPtr  

readDirectoryChanges :: String -> HANDLE -> Bool -> FileNotificationFlag -> IO [Event]
readDirectoryChanges root h wst mask = do
    let maxBuf = 63 * 1024
    allocaBytes maxBuf $ \buffer -> do
        alloca $ \bret -> do
            readDirectoryChangesW h buffer (toEnum maxBuf) wst mask bret
            bytesRet <- peekByteOff bret 0
            readChangeEvents buffer root bytesRet
 
type FileAction = DWORD

fILE_ACTION_ADDED             :: FileAction
fILE_ACTION_ADDED             =  1

fILE_ACTION_REMOVED           :: FileAction
fILE_ACTION_REMOVED           =  2

fILE_ACTION_MODIFIED          :: FileAction
fILE_ACTION_MODIFIED          =  3

fILE_ACTION_RENAMED_OLD_NAME  :: FileAction
fILE_ACTION_RENAMED_OLD_NAME  =  4

fILE_ACTION_RENAMED_NEW_NAME  :: FileAction
fILE_ACTION_RENAMED_NEW_NAME  =  5

eventStreamAggr :: (HANDLE, FilePath, Config) -> SerialT IO Event
eventStreamAggr (handle, rootPath, cfg) =  do
    let recMode = getConfigRecMode cfg
        flagMasks = getConfigFlag cfg
    S.concatMap S.fromList $ S.repeatM $ readDirectoryChanges rootPath handle recMode flagMasks

strPathToStrHandle :: NonEmpty FilePath -> Config -> SerialT IO (HANDLE, FilePath, Config)
strPathToStrHandle paths cfg = do
    let pathStream = S.fromList (NonEmpty.toList paths)
        st2 = S.mapM getWatchHandle pathStream
    S.map ( \(h, f) -> (h, f , cfg) ) st2   

peekFNI :: Ptr FILE_NOTIFY_INFORMATION -> IO FILE_NOTIFY_INFORMATION
peekFNI buf = do
    neof <- peekByteOff buf 0
    acti <- peekByteOff buf 4
    fnle <- peekByteOff buf 8
    fnam <- peekCWStringLen
        (buf `plusPtr` 12, -- start of array
        fromEnum (fnle :: DWORD) `div` 2 ) -- fnle is the length in *bytes*, and a WCHAR is 2 bytes
    return $ FILE_NOTIFY_INFORMATION neof acti fnam

readChangeEvents :: Ptr FILE_NOTIFY_INFORMATION -> String -> DWORD -> IO [Event]
readChangeEvents pfni root bytesRet = do  
    fni <- peekFNI pfni
    let entry = Event{
                        eventFlags = fniAction fni 
                      , eventRelPath = fniFileName fni  
                      , eventRootPath = root
                      , totalBytes = bytesRet
                      }  
        nioff = fromEnum $ fniNextEntryOffset fni   
    entries <- if nioff == 0 
                    then return [] 
                    else readChangeEvents  (pfni `plusPtr` nioff)  root bytesRet
    return $ entry :entries

-- | Close a Directory handle.
--
-- /Internal/
--
closePathHandleStream :: SerialT IO (HANDLE, FilePath, Config) -> IO ()
closePathHandleStream = S.mapM_ ( \(h, _, _) -> closeHandle h)

-- | Start monitoring a list of file system paths for file system events with
-- the supplied configuration operation over the 'defaultConfig'. The
-- paths could be files or directories. When the path is a directory, only the
-- files and directories directly under the watched directory are monitored,
-- contents of subdirectories are not monitored.  Monitoring starts from the
-- current time onwards.
--
-- /Internal/
--
watchPathsWith :: (Config -> Config) -> NonEmpty FilePath -> SerialT IO Event
watchPathsWith f paths = do
    let cfg = f $ setRecursiveMode False defaultConfig
        sth = strPathToStrHandle paths cfg        
    S.after (closePathHandleStream sth) $ S.concatMapWith parallel eventStreamAggr sth

-- | Like 'watchPathsWith' but uses the 'defaultConfig' options.
--
-- @
-- watchPaths = watchPathsWith id
-- @
--
-- /Internal/
--
watchPaths :: NonEmpty FilePath -> SerialT IO Event
watchPaths = watchPathsWith id

-- | Start monitoring a list of file system paths for file system events with
-- the supplied configuration operation over the 'defaultConfig'. The
-- paths could be files or directories.  When the path is a directory, the
-- whole directory tree under it is watched recursively. Monitoring starts from
-- the current time onwards.
--
-- /Internal/
--
watchTreesWith :: (Config -> Config) -> NonEmpty FilePath -> SerialT IO Event   
watchTreesWith f paths = do
    let cfg = f $ setRecursiveMode True defaultConfig
        sth = strPathToStrHandle paths cfg        
    S.after (closePathHandleStream sth) $ S.concatMapWith parallel eventStreamAggr sth

-- | Like 'watchTreesWith' but uses the 'defaultConfig' options.
--
-- @
-- watchTrees = watchTreesWith id
-- @
--
watchTrees :: NonEmpty FilePath -> SerialT IO Event
watchTrees = watchTreesWith id    

-- | A handle for a watch.
getWatchHandle :: FilePath -> IO (HANDLE, FilePath)
getWatchHandle dir = do
    h <- createFile dir
        fILE_LIST_DIRECTORY -- Access mode
        (fILE_SHARE_READ .|. fILE_SHARE_WRITE) -- Share mode
        Nothing -- security attributes
        oPEN_EXISTING -- Create mode, we want to look at an existing directory
        fILE_FLAG_BACKUP_SEMANTICS -- File attribute, nb NOT using OVERLAPPED since we work synchronously
        Nothing -- No template file
    return (h, dir)


getFlag :: DWORD -> Event -> Bool
getFlag mask Event{..} = eventFlags == mask

getRelPath :: Event -> String
getRelPath Event{..} = eventRelPath

getRoot :: Event -> String
getRoot Event{..} = eventRootPath

isCreated :: Event -> Bool
isCreated = getFlag fILE_ACTION_ADDED

isDeleted :: Event -> Bool
isDeleted = getFlag fILE_ACTION_REMOVED

isMovedFrom :: Event -> Bool
isMovedFrom = getFlag fILE_ACTION_RENAMED_OLD_NAME

isMovedTo :: Event -> Bool
isMovedTo = getFlag fILE_ACTION_RENAMED_NEW_NAME

isModified :: Event -> Bool
isModified = getFlag fILE_ACTION_MODIFIED

-- |  If the buffer overflows, ReadDirectoryChangesW will still return true, 
-- but the entire contents of the buffer are discarded.
isOverflow :: Event -> Bool
isOverflow Event{..} = totalBytes == 0

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- | Convert an 'Event' record to a String representation.
showEvent :: Event -> String
showEvent ev@Event{..} =  
        "--------------------------"
    ++ "\nRoot = " ++ show (getRoot ev)
    ++ "\nPath = " ++ show (getRelPath ev)
    ++ "\nFlags " ++ show eventFlags    
    ++ showev isOverflow "Overflow"
    ++ showev isCreated "Created"
    ++ showev isDeleted "Deleted"
    ++ showev isModified "Modified"
    ++ showev isMovedFrom "MovedFrom"
    ++ showev isMovedTo "MovedTo"
    ++ "\n"

        where showev f str = if f ev then "\n" ++ str else ""