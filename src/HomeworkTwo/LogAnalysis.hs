module HomeworkTwo.LogAnalysis where 

import Text.Read 

type ErrorSeverity
 = Int

data MessageType
   = Info
   | Warning
   | Error ErrorSeverity
   deriving (Show, Eq)

data ErrorCode
  = InvalidErrorCode
  | ValidErrorCode Int

data LogMessage
   = LogMessage MessageType Int String
   | Unknown String
   | Empty
   deriving (Show, Eq)

data Timestamp
  = InvalidTimestamp
  | ValidTimestamp Int

data MessageTree
  = Leaf
  | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

getSeverity :: MessageType -> ErrorSeverity  
getSeverity (Error severity) = severity 
getSeverity _ = error "Could not retrieve severity"

getTimestamp :: LogMessage -> Int
getTimestamp (LogMessage _ timestamp _) = timestamp
getTimestamp _ = error "Could not retrieve timestamp"

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ message) = message
getMessage _ = error "Could not retrieve message"

getType :: LogMessage -> MessageType
getType (LogMessage messageType _ _) = messageType
getType _ = error "Could not retrieve message type"

getLogMessage :: MessageTree -> LogMessage
getLogMessage (Node _ logMessage _) = logMessage
getLogMessage Leaf = error "Could not retrieve log message" 

getLeftNode :: MessageTree -> MessageTree
getLeftNode (Node leftNode _ _) = leftNode
getLeftNode Leaf = Leaf

getRightNode :: MessageTree -> MessageTree
getRightNode(Node _ _ rightNode) = rightNode
getRightNode Leaf = Leaf

parseMessage :: String -> LogMessage
parseMessage "" = Empty
parseMessage message = generateMessage (words message)

generateMessage :: [String] -> LogMessage
generateMessage [] = Empty
generateMessage [x] = Unknown x
generateMessage [x,y] = Unknown (unwords [x, y])
generateMessage ("I":rest) = toInfoMessage rest
generateMessage ("W":rest) = toWarningMessage rest
generateMessage ("E":rest) = toErrorMessage rest
generateMessage m@_ = Unknown (unwords m)

toInfoMessage :: [String] -> LogMessage
toInfoMessage m@(timestamp:rest) = case readMaybe timestamp :: Maybe Int of
  Just ts -> LogMessage Info ts (unwords rest)
  Nothing -> Unknown (unwords m)
toInfoMessage m@_ = Unknown (unwords m)

toWarningMessage :: [String] -> LogMessage
toWarningMessage m@(timestamp:rest) = case readMaybe timestamp :: Maybe Int of
  Just ts -> LogMessage Warning ts (unwords rest)
  Nothing -> Unknown (unwords m)
toWarningMessage m@_ = Unknown (unwords m)

toErrorMessage :: [String] -> LogMessage
toErrorMessage m@(errorSeverity:(timestamp:rest)) = case readMaybe errorSeverity :: Maybe Int of
  Just ec -> case readMaybe timestamp :: Maybe Int of
    Just ts -> LogMessage (Error ec) ts (unwords rest)
    Nothing -> Unknown (unwords m)
  Nothing -> Unknown (unwords m)
toErrorMessage m@_ = Unknown (unwords m)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) x = x
insert x Leaf = Node Leaf x Leaf
insert x (Node left message right)
  | insertingTimeStamp == currentTimeStamp = Node left x right
  | insertingTimeStamp > currentTimeStamp = Node left message (insert x right)
  | insertingTimeStamp < currentTimeStamp = Node (insert x left) message right
  where insertingTimeStamp = getTimestamp x
        currentTimeStamp = getTimestamp message
insert _ t@_ = t

build :: [LogMessage] -> MessageTree
build [] = Leaf
build [message] = insert message Leaf
build x = insert (last x) (build (init x))

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf x Leaf) = [x]
inOrder (Node left x right) = inOrder left ++ [getLogMessage completeNode] ++ inOrder right
  where completeNode = Node left x right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (LogMessage (Error severity) _ m:xs) =
  if severity > minSeverity then m : whatWentWrong xs else whatWentWrong xs
  where minSeverity = 50
whatWentWrong (_:xs) = whatWentWrong xs