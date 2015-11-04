{-# LANGUAGE LambdaCase #-}

module EventSource where

import EventSourceHelper

import Data.Functor (($>))
import qualified Data.Set as Set


-- Types:

--- A query is a fold over a list of events.
---
---   data QueryT f e a where
---   instance Functor f => Functor (QueryT f a)
---   instance Functor f => Profunctor (QueryT f)
---   instance Applicative f => Applicative (QueryT f e)
---
---   runQ :: QueryT f e a -> [e] -> f a

--- A command is a query that returns an event to be appended.
---
---   newtype CommandT f e = CommandT (QueryT f e e)
---
---   runC :: Functor f => CommandT f e -> [e] -> f [e]



-- Domain:

type UserId = String
type GroupId = String

data Event = GroupCreated GroupId
           | GroupDeleted GroupId
           | UserCreated UserId
           | UserDeleted UserId
           | UserAddedToGroup UserId GroupId
           | UserRemovedFromGroup UserId GroupId
  deriving (Show, Eq)

data Error = GroupDoesNotExist
           | UserDoesNotExist
           | DuplicateGroupId
           | DuplicateUserId
           | UserIsAlreadyMember
           | UserIsNotAMember
           | GroupIsNotEmpty
  deriving Show

type Result = Either Error

-- Signatures:

groupExists :: GroupId -> Query Event Bool
userExists :: UserId -> Query Event Bool

isMemberOfGroup :: UserId -> GroupId -> Query Event Bool
isMemberOfGroup' :: UserId -> GroupId -> QueryT Result Event Bool

findUsersInGroup :: GroupId -> Query Event (Set.Set UserId)
isGroupEmpty :: GroupId -> Query Event Bool

createGroup :: GroupId -> CommandT Result Event
deleteGroup :: GroupId -> CommandT Result Event
createUser :: UserId -> CommandT Result Event

addUserToGroup :: UserId -> GroupId -> CommandT Result Event
removeUserFromGroup :: UserId -> GroupId -> CommandT Result Event

-- Implementation:

groupExists gid = GroupCreated gid `lastHappenedAfter` GroupDeleted gid

userExists uid = UserCreated uid `lastHappenedAfter` UserDeleted uid

isMemberOfGroup uid gid = UserAddedToGroup uid gid `lastHappenedAfter` UserRemovedFromGroup uid gid

isMemberOfGroup' uid gid =
  ensure (userExists uid) UserDoesNotExist *>
  ensure (groupExists gid) GroupDoesNotExist *>
  query (uid `isMemberOfGroup` gid)

findUsersInGroup gid = fold1 $ \case
  UserAddedToGroup gid' uid     | gid == gid' -> Set.insert uid
  UserRemovedFromGroup gid' uid | gid == gid' -> Set.delete uid
  _ -> id

isGroupEmpty gid = Set.null <$> findUsersInGroup gid

createGroup gid =
  ensure (not <$> groupExists gid) DuplicateGroupId $>
  [GroupCreated gid]

deleteGroup gid =
  ensure (groupExists gid) GroupDoesNotExist $>
  ensure (isGroupEmpty gid) GroupIsNotEmpty $>
  [GroupDeleted gid]

createUser uid =
  ensure (not <$> userExists uid) DuplicateUserId $>
  [UserCreated uid]

addUserToGroup uid gid =
  ensure (userExists uid) UserDoesNotExist *>
  ensure (groupExists gid) GroupDoesNotExist *>
  ensure (not <$> uid `isMemberOfGroup` gid) UserIsAlreadyMember $>
  [UserAddedToGroup uid gid]

removeUserFromGroup uid gid =
  ensure (userExists uid) UserDoesNotExist *>
  ensure (groupExists gid) GroupDoesNotExist *>
  ensure (uid `isMemberOfGroup` gid) UserIsNotAMember $>
  [UserRemovedFromGroup uid gid]

scenario = runTX
  [ createGroup "foo"
  , createUser "bar"
  , addUserToGroup "bar" "foo"
  , createUser "baz"
  , addUserToGroup "baz" "foo"
  , removeUserFromGroup "bar" "foo"
  , deleteGroup "foo"
  ]
