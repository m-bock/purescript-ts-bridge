module SampleTypes where

data State = On | Off Int | Loading

type User =
  { name :: String
  , age :: Int
  , loggedIn :: Boolean
  }

newtype Id = Id String

user :: User
user =
  { name: "Me"
  , age: 23
  , loggedIn: true
  }

myFn :: Int -> String -> Boolean
myFn n str = true