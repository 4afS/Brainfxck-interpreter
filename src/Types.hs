module Types where

import RIO

data Token
  = GraterThan
  | LessThan
  | Plus
  | Minus
  | Dot
  | Comma
  | BracketLeft
  | BracketRight
  | Comment
  deriving (Eq, Show)

type Tokens = Vector Token

data Op
  = MoveRight
  | MoveLeft
  | Increment
  | Decrement
  | Put
  | Substitution
  | Loop Ops
  deriving (Eq, Show)

type Ops = Vector Op
