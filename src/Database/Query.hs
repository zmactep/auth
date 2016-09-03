module Database.Query
    ( run
    , makeQuery
    ) where

import           Control.Monad.Reader (asks)
import           Control.Monad.Trans  (MonadIO, lift, liftIO)
import           Data.Pool            (Pool, withResource)
import           Data.Text.Lazy       (Text)
import           Database.MongoDB     (Action, access, master)
import           ServerState          (ServerState (..), WebM)
import           Web.Scotty.Trans     (ActionT)

run :: ServerState -> Action IO a -> IO a
run ss act = liftIO $ withResource (pool ss) runPipe
    where runPipe pipe = access pipe master (db ss) act

makeQuery :: Action IO a -> ActionT Text WebM a
makeQuery query = lift $ asks (`run` query) >>= liftIO
