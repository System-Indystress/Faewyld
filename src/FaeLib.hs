module FaeLib where

import Data.Text (Text(..))
import qualified Data.Text as T
import Data.Map (Map(..))
import qualified Data.Map as M

-- Model
data Fae = PukFaerie        -- air, bios  and light
         | SelkieKelpie     -- water, electricity, and dusk
         | SprigganPortunes -- earth (soot and dirt), wood, and dawn
         | Leprechaun       -- fire, metal and darkness
  deriving (Show, Eq)
data Element =  Air
              | Bios
              | Water
              | Electricity
              | Earth
              | Wood
              | Fire
              | Metal
  deriving (Show, Eq)

data Alignment = Noon
                | Set
                | Rise
                | Twilight
  deriving (Show, Eq)

data Category  = Winged
               | Wispered
               | Fauna
               | Inspired
               | Finned
               | Solvent
               | Automata
               | Channeled
               | Sooted
               | Minerial
               | Flora
               | Wyld
               | Forge
               | Flare
               | Raw
               | Refined
  deriving (Show, Eq)


data Attack = Primal
            | Tinkered
            | Magical
            | Surprise

  deriving (Show,Eq)
data Defense = Hide
             | Dodge
             | Fortify
             | Block
             | Parry
             | Heal
  deriving (Show,Eq)

data Action = Attack | Defense
  deriving (Show, Eq)

type Vert = (Int,Int,Int)


data Name = Name { owner :: Maybe Text, ident :: Text}
  deriving (Show, Eq)

data Recruit =
  Recruit { familiar  :: Name
          , fae       :: Fae
          , element   :: Element
          , alignment :: Alignment
          , category  :: Category
          , attack    :: Attack
          , defense   :: Defense
          , level     :: Int
          , health    :: Int
          , spirit    :: Int
          }
  deriving (Show, Eq)

data Ability = Capture Int | Summon Int | Dismiss Int
             | Compel Int | Beckon Int | Possess Int
             | Scout Int | Prank Int | Manifest Int
             | Declare Int
  deriving (Show, Eq)

data City = North | South | East | West
          | NE    | SE    | NW   | SW
  deriving (Show,Eq)
data Sidhe =
  Sidhe { name      :: Name
        , active    :: [Name]
        , stored    :: [Name]
        , abilities :: [Ability]
        , home      :: City
        }
  deriving (Show, Eq)
data Job = Farmhand | Townfolk | Politician | Kindred
  deriving (Show,Eq)

data Human = { who  :: Name
             , what :: Job
             }
  deriving (Show, Eq)

data Status a =
  Status { position   :: (Vert,Vert)
         , moving     :: Maybe (Vert,Vert)
         , performing :: Maybe (a, Name)
         }
  deriving (Show,Eq)

data Phase = Peace | Prep | Battle (City, Int) (City, Int) | Outcome City
  deriving (Show, Eq)
data Structure =
    Keep Vert | Wall Vert | Bulwark Vert | Scaffold Vert -- city structures
  | Tangle Element | Wash Element | Flow Element         -- magical structures
  | Static Int | Fog Int | Drought Int |                 -- natural structures
  | Town Text | Villa | Camp                             -- human structures
  deriving (Show, Eq)

data Place =
  Place { title      :: Text
        , element    :: Element
        , owner      :: City
        , structures :: [(Structure, City, Vert)]
        , phase      :: Phase
        }

  deriving (Show Eq)
data Day  =  A | B | C | D | E | F | G | H
  deriving (Show, Eq)
type Hour = Int -- 0 to 100

type Time = (Day, Hour)

data Game =
  Game { players  :: Map Name (Status Ability)
       , recruits :: Map Name (Status Action)
       , sidhe    :: [Sidhe]
       , fae      :: [Fae]
       , phase    :: Phase
       , place    :: Place
       , time     :: Time
       }
  deriving (Show, Eq)


data VS = Strong | Weak | Draw
  deriving (Show, Eq)

-- rules
versus :: Element -> Element -> VS
versus Air Fire = Strong
versus Fire Air = Weak
versus Fire Wood = Strong
versus Wood Fire = Weak
versus Wood Water = Strong
versus Water Wood = Weak
versus Water Earth = Strong
versus Earth Water = Weak
versus Earth Electricity = Strong
versus Electricity Earth = Weak
versus Electricity Metal = Strong
versus Metal Electricity = Weak
versus Metal Bios        = Strong
versus Bios Metal        = Weak
versus Bios Air          = Strong
versus Air Bios          = Weak
versus _ _ = Draw
