{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module System.Terminal.MonadInput where

import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.List

type Row     = Int
type Rows    = Int
type Column  = Int
type Columns = Int

-- | This monad describes an environment that maintains a stream of `Event`s
--   and offers out-of-band signaling for interrupts.
--
--     * An interrupt shall occur if the user either presses CTRL+C
--       or any other mechanism the environment designates for that purpose.
--     * Implementations shall maintain an signal flag that is set
--       when a signal occurs. Computations in this monad shall check and
--       reset this flag regularly. If the execution environment finds this
--       flag still set when trying to signal another interrupt, it shall
--       throw `Control.Exception.AsyncException.UserInterrupt` to the
--       seemingly unresponsive computation.
--     * When a signal occurs, an `Event.SignalEvent`
--       must be added to the event stream in the same transaction.
--       This allows to flush all unprocessed events from the stream that
--       occured before the interrupt.
class (MonadIO m) => MonadInput m where
  -- | Wait for the next signal or the next event transformed by a given mapper.
  --
  --    * The first mapper parameter is a transaction that succeeds as
  --      soon as a signal occurs. Executing this transaction
  --      resets the signal flag. If the signal is an interrupt and is not reset
  --      before a second interrupt occurs, the current thread shall
  --      receive an `Control.Exception.AsyncException.UserInterrupt`.
  --    * The second mapper parameter is a transaction that succeeds as
  --      as soon as the next event arrives and removes that event from the
  --      stream of events. It may be executed several times within the same
  --      transaction, but might not succeed every time.
  waitMapSignalAndEvents :: (STM Signal -> STM Event -> STM a) -> m a

-- | Wait for the next event.
--
--    * Returns as soon as an event occurs.
--    * This operation resets the signal flag when encountering a signal,
--      signaling responsiveness to the execution environment.
--    * `Event.SignalEvent`s occur in the event stream at their correct
--      position wrt to ordering of events. They are returned as regular
--      events. This is eventually not desired when trying to handle signals
--      with highest priority and `waitSignalOrEvent` or `waitSignalOrElse` should
--      be considered then.
waitEvent :: MonadInput m => m Event
waitEvent = waitMapSignalAndEvents $ \sig evs-> do
  ev <- evs
  case ev of
    SignalEvent {} -> void sig `orElse` pure ()
    _              -> pure ()
  pure ev

-- | Wait for the next event or a given transaction.
--
--    * Returns as soon as an event occurs or the transaction succeeds.
--    * This operation resets the signal flag when encountering a signal,
--      signaling responsiveness to the execution environment.
--    * `Event.SignalEvent`s occur in the event stream at their correct
--      position wrt to ordering of events. They are returned as regular
--      events. This is eventually not desired when trying to handle signals
--      with highest priority and `waitSignalOrElse` should
--      be considered then.
waitEventOrElse :: MonadInput m => STM a -> m (Either Event a)
waitEventOrElse stma = waitMapSignalAndEvents $ \sig evs -> do
  eva <- (Left <$> evs) `orElse` (Right <$> stma)
  case eva of
    Left (SignalEvent {}) -> void sig `orElse` pure ()
    _                     -> pure ()
  pure eva

-- | Wait simultaneously for the next signal or a given transaction.
--
--    * Returns `Left` on signal and `Right` when the supplied transaction
--      succeeds first.
--    * This operation resets the signal flag, signaling responsiveness
--      to the execution environment.
--    * All pending events are dropped in case of a signal.
waitSignalOrElse :: MonadInput m => STM a -> m (Either Signal a)
waitSignalOrElse stma = waitMapSignalAndEvents $ \sig evs ->
  (sig >>= \s -> dropPending evs >> pure (Left s)) `orElse` (Right <$> stma)
  where
    dropPending:: STM Event -> STM ()
    dropPending evs = ((evs >> pure True) `orElse` pure False) >>= \case
      True  -> dropPending evs
      False -> pure ()

data Key
  = CharKey Char
  | TabKey
  | SpaceKey
  | BackspaceKey
  | EnterKey
  | InsertKey
  | DeleteKey
  | HomeKey      -- ^ Pos 1
  | BeginKey
  | EndKey
  | PageUpKey
  | PageDownKey
  | EscapeKey
  | PrintKey
  | PauseKey
  | ArrowKey Direction
  | FunctionKey Int
  deriving (Eq,Ord,Show)

newtype Modifiers = Modifiers Int
  deriving (Eq, Ord, Bits)

instance Semigroup Modifiers where
  Modifiers a <> Modifiers b = Modifiers (a .|. b)

instance Monoid Modifiers where
  mempty = Modifiers 0

instance Show Modifiers where
  show (Modifiers 0) = "mempty"
  show (Modifiers 1) = "shiftKey"
  show (Modifiers 2) = "ctrlKey"
  show (Modifiers 4) = "altKey"
  show (Modifiers 8) = "metaKey"
  show i = "(" ++ intercalate " <> " ls ++ ")"
    where
      ls = foldl (\acc x-> if x .&. i /= mempty then show x:acc else acc) []
                 [metaKey, altKey, ctrlKey, shiftKey]

shiftKey, ctrlKey, altKey, metaKey :: Modifiers
shiftKey = Modifiers 1
ctrlKey  = Modifiers 2
altKey   = Modifiers 4
metaKey  = Modifiers 8

data Event
  = KeyEvent Key Modifiers
  | MouseEvent MouseEvent
  | WindowEvent WindowEvent
  | DeviceEvent DeviceEvent
  | SignalEvent Signal
  | OtherEvent String
  deriving (Eq,Ord,Show)

data MouseEvent
  = MouseMoved          (Int,Int)
  | MouseButtonPressed  (Int,Int) MouseButton
  | MouseButtonReleased (Int,Int) MouseButton
  | MouseButtonClicked  (Int,Int) MouseButton
  | MouseWheeled        (Int,Int) Direction
  deriving (Eq,Ord,Show)

data MouseButton
  = LeftMouseButton
  | RightMouseButton
  | OtherMouseButton
  deriving (Eq,Ord,Show)

data Direction
  = Upwards
  | Downwards
  | Leftwards
  | Rightwards
  deriving (Eq,Ord,Show)

data WindowEvent
  = WindowLostFocus
  | WindowGainedFocus
  | WindowSizeChanged (Int,Int)
  deriving (Eq, Ord, Show)

data DeviceEvent
  = DeviceAttributesReport String
  | CursorPositionReport (Int,Int)
  deriving (Eq, Ord, Show)

data Signal
  = InterruptSignal
  | OtherSignal BS.ByteString
  deriving (Eq, Ord, Show)
