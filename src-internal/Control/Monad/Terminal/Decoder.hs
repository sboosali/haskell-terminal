module Control.Monad.Terminal.Decoder where

import           Data.Char
import           Data.Monoid                  ((<>))

import           Control.Monad.Terminal.Input

-- | The type `Decoder` represents a finite state transducer.
--
--   Intermediate state can be passed as closures.
--   See below for an example.
newtype Decoder = Decoder { feedDecoder :: Modifiers -> Char -> Either Decoder [Event] }

ansiDecoder :: (Modifiers -> Char -> Maybe Event) -> Decoder
ansiDecoder specialChar = defaultMode
  where
    -- The default mode is the decoder's entry point.
    defaultMode :: Decoder
    defaultMode  = Decoder $ \mods c-> case specialChar mods c of
      Just ev -> Right [ev]
      Nothing
        -- In normal mode a NUL is interpreted as a fill character and skipped.
        | c == '\NUL' -> Right []
        -- ESC might or might not introduce an escape sequence.
        | c == '\ESC' -> Left escapeMode
        -- All other C0 control codes are mapped to their corresponding ASCII character + CTRL modifier.
        -- If the character is a special character, then two events are produced.
        | c <= '\US'  -> Right [KeyEvent (CharKey (toEnum $ (+64) $ fromEnum c)) (mods <> ctrlKey)]
        -- Space shall be interpreted as control code and translated to `SpaceKey` to be
        -- consistent with the handling of `TabKey` and other whitespaces.
        | c == '\SP'  -> Right [KeyEvent (CharKey ' ') mods, KeyEvent SpaceKey mods]
        -- All remaning characters of the Latin-1 block are returned as is.
        | c <  '\DEL' -> Right [KeyEvent (CharKey c) mods]
        -- Skip all other C1 control codes and DEL.
        | c <  '\xA0' -> Right []
        -- All other Unicode characters are returned as is.
        | otherwise   -> Right [KeyEvent (CharKey c) mods]

    -- This function shall be called if an ESC has been read in default mode
    -- and it is stil unclear whether this is the beginning of an escape sequence or not.
    -- NOTE: This function is total and consumes at least one more character of input.
    escapeMode :: Decoder
    escapeMode  = Decoder $ \mods c-> if
      -- Single escape key press is always followed by a NUL fill character
      -- by design (instead of timing). This makes reasoning and testing much easier
      -- and reliable.
      | c == '\NUL' -> Right [KeyEvent (CharKey '[') (mods <> ctrlKey), KeyEvent EscapeKey mods]
      | otherwise   -> Left (escapeSequenceMode c)

    -- This function shall be called with the escape sequence introducer.
    -- It needs to look at next character to decide whether this is
    -- a CSI sequence or an ALT-modified key or illegal state.
    escapeSequenceMode :: Char -> Decoder
    escapeSequenceMode c = Decoder $ \mods d-> if
      | d == '\NUL' && c > '\SP' && c <= '~' -> Right [KeyEvent (CharKey c) (mods <> altKey)]
      | d == '\NUL' && c >= '\xa0'           -> Right [KeyEvent (CharKey c) (mods <> altKey)]
      | d == '\NUL'                          -> Right $ case specialChar mods c of
                                                  Nothing -> []
                                                  Just ev -> case ev of
                                                    KeyEvent key m -> [KeyEvent key (mods <> m <> altKey)]
                                                    _              -> [ev]
      | c == 'O'                             -> Right (ss3Mode mods d)
      | c == '['                             -> csiMode d
      | otherwise                            -> Right []

    -- SS3 mode is another less well-known escape sequence mode.
    -- It is introduced by `\\ESCO`. Some terminal emulators use it for
    -- compatibility with veeery old terminals. SS3 mode only allows one
    -- subsequent character. Interpretation has been determined empirically
    -- and with reference to http://rtfm.etla.org/xterm/ctlseq.html
    ss3Mode :: Modifiers -> Char -> [Event]
    ss3Mode mods = \case
      'P' -> [KeyEvent (FunctionKey  1) mods]
      'Q' -> [KeyEvent (FunctionKey  2) mods]
      'R' -> [KeyEvent (FunctionKey  3) mods]
      'S' -> [KeyEvent (FunctionKey  4) mods]
      _   -> []

    -- ESC[ is followed by any number (including none) of parameter chars in the
    -- range 0–9:;<=>?, then by any number of intermediate chars
    -- in the range space and !"#$%&'()*+,-./, then finally by a single char in
    -- the range @A–Z[\]^_`a–z{|}~.
    -- For security reasons (untrusted input and denial of service) this parser
    -- only accepts a very limited number of characters for both parameter and
    -- intermediate chars.
    -- Unknown (not illegal) sequences are dropped, but it is guaranteed that
    -- they will be consumed completely and it is safe for the parser to
    -- return to normal mode afterwards. Illegal sequences cause the parser
    -- to consume the input up to the first violating character and then reject.
    -- The parser might be out of sync afterwards, but this is a protocol
    -- violation anyway. The parser's only job here is not to loop (consume
    -- and drop the illegal input!) and then to stop and fail reliably.
    csiMode :: Char -> Either Decoder [Event]
    csiMode c
      | c >= '0' && c <= '?' = Left $ f (charLimit - 1) [c]
      | c >= '!' && c <= '/' = Left $ g (charLimit - 1) [] [c]
      | c >= '@' && c <= '~' = Right $ interpretCSI [] [] c
      | otherwise            = Right [] -- Illegal state. Return to default mode.
      where
        charLimit :: Int
        charLimit  = 16
        -- Note: The following functions use recursion, but recursion is
        -- guaranteed to terminate and maximum recursion depth is only
        -- dependant on the constant `charLimit`. In case of errors the decoder
        -- will therefore recover to default mode after at most 32 characters.
        f :: Int -> String -> Decoder
        f 0 _  = defaultMode
        f i ps = Decoder $ const $ \x-> if
          | x >= '0' && x <= '?' -> Left $ f (i - 1) (x:ps)  -- More parameters.
          | x >= '!' && x <= '/' -> Left $ g charLimit ps [] -- Start of intermediates.
          | x >= '@' && x <= '~' -> Right $ interpretCSI (reverse ps) [] x
          | otherwise            -> Right [] -- Illegal state. Return to default mode.
        g :: Int -> String -> String -> Decoder
        g 0 _  _  = defaultMode
        g i ps is = Decoder $ const $ \x-> if
          | x >= '!' && x <= '/' -> Left $ g (i - 1) ps (x:is) -- More intermediates.
          | x >= '@' && x <= '~' -> Right $ interpretCSI (reverse ps) (reverse is) x
          | otherwise            -> Right [] -- Illegal state. Return to default mode.

interpretCSI :: String -> String -> Char -> [Event]
interpretCSI params _intermediates = \case
  '$'        -> [KeyEvent DeleteKey (altKey `mappend` shiftKey)]  -- urxvt, gnome-terminal
  '@'        -> []
  'A'        -> modified $ ArrowKey Upwards
  'B'        -> modified $ ArrowKey Downwards
  'C'        -> modified $ ArrowKey Rightwards
  'D'        -> modified $ ArrowKey Leftwards
  'E'        -> modified   BeginKey
  'F'        -> modified   EndKey
  'G'        -> []
  'H'        -> modified   HomeKey
  'I'        -> modified   TabKey
  'J'        -> []
  'K'        -> []
  'L'        -> []
  'M'        -> []
  'N'        -> []
  'O'        -> []
  'P'        -> modified (FunctionKey  1)
  'Q'        -> modified (FunctionKey  2)
  -- This sequence is ambiguous. xterm and derivatives use this to encode a modified F3 key as
  -- well as a cursor position report. There is no real solution to disambiguate these two
  -- other than context of expectation (cursor position report has probably been requested).
  -- This decoder shall simply emit both events and the user shall ignore unexpected events.
  'R'        -> modified (FunctionKey  3) ++ [DeviceEvent $ CursorPositionReport (fstNumber 1 - 1, sndNumber 1 - 1)]
  'S'        -> modified (FunctionKey  4)
  'T'        -> []
  'U'        -> []
  'V'        -> []
  'W'        -> []
  'X'        -> []
  'Y'        -> []
  'Z'        -> [KeyEvent TabKey shiftKey]
  '^'        -> case params of
    "2"  -> [KeyEvent InsertKey        ctrlKey]
    "3"  -> [KeyEvent DeleteKey        ctrlKey]
    "4"  -> [KeyEvent PageUpKey        ctrlKey]
    "7"  -> [KeyEvent PageDownKey      ctrlKey]
    "5"  -> [KeyEvent HomeKey          ctrlKey]
    "6"  -> [KeyEvent EndKey           ctrlKey]
    "11" -> [KeyEvent (FunctionKey  1) ctrlKey]
    "12" -> [KeyEvent (FunctionKey  2) ctrlKey]
    "13" -> [KeyEvent (FunctionKey  3) ctrlKey]
    "14" -> [KeyEvent (FunctionKey  4) ctrlKey]
    "15" -> [KeyEvent (FunctionKey  5) ctrlKey]
    "17" -> [KeyEvent (FunctionKey  6) ctrlKey]
    "18" -> [KeyEvent (FunctionKey  7) ctrlKey]
    "19" -> [KeyEvent (FunctionKey  8) ctrlKey]
    "20" -> [KeyEvent (FunctionKey  9) ctrlKey]
    "21" -> [KeyEvent (FunctionKey 10) ctrlKey]
    "23" -> [KeyEvent (FunctionKey 11) ctrlKey]
    "24" -> [KeyEvent (FunctionKey 12) ctrlKey]
    _    -> []
  'f' -> []
  'i' -> [KeyEvent PrintKey mempty]
  'm' -> []
  '~' -> case fstParam of
    "2"  -> modified InsertKey
    "3"  -> modified DeleteKey
    "5"  -> modified PageUpKey
    "6"  -> modified PageDownKey
    "9"  -> modified HomeKey
    "10" -> modified EndKey
    "11" -> modified (FunctionKey 1)
    "12" -> modified (FunctionKey 2)
    "13" -> modified (FunctionKey 3)
    "14" -> modified (FunctionKey 4)
    "15" -> modified (FunctionKey 5)
    "17" -> modified (FunctionKey 6)
    "18" -> modified (FunctionKey 7)
    "19" -> modified (FunctionKey 8)
    "20" -> modified (FunctionKey 9)
    "21" -> modified (FunctionKey 10)
    "23" -> modified (FunctionKey 11)
    "24" -> modified (FunctionKey 12)
    _    -> []
  _ -> []
  where
    fstParam :: String
    fstParam = takeWhile (/= ';') params
    sndParam :: String
    sndParam = takeWhile (/= ';') $ drop 1 $ dropWhile (/= ';') params
    fstNumber :: Int -> Int
    fstNumber i
      | not (null fstParam) && all isDigit fstParam = read fstParam
      | otherwise                                   = i
    sndNumber :: Int -> Int
    sndNumber i
      | not (null sndParam) && all isDigit sndParam = read sndParam
      | otherwise                                   = i
    modified key = case sndParam of
      ""  -> [KeyEvent key   mempty                       ]
      "2" -> [KeyEvent key   shiftKey                     ]
      "3" -> [KeyEvent key               altKey           ]
      "4" -> [KeyEvent key $ shiftKey <> altKey           ]
      "5" -> [KeyEvent key                         ctrlKey]
      "6" -> [KeyEvent key $ shiftKey <>           ctrlKey]
      "7" -> [KeyEvent key $             altKey <> ctrlKey]
      "8" -> [KeyEvent key $ shiftKey <> altKey <> ctrlKey]
      _   -> []
