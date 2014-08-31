module Constructor.SemanticProperties where

import GHC.Generics (Generic)
import Data.Hashable
import Constructor.Util

data VarProperty = Arg1 | Arg2
                 | Member1 | Member2
                 | Questioned
                 | Variants | Components
                 | Target
                 | Content
                 | Theme
                 | Topic
                 | Source
                 | Instrument
                 | Goal | Goal_to | Goal_in | Goal_on | Goal_by | Goal_action
                 | Receiver | Addressee
                 | Experiencer
                 | PerfectBackground
                 | Determiner
                 | Order
                 | Anchor
                 | RelTime
                 | Duration
                 | Relative
                 | OptativeModality
                 | Quantifier | Specifier_all
                 | AccordingTo
                 | Author
                 | Place
                 | Location
                 | Reason | Condition | WhenCondition | IfCondition
                 | Elaboration
                 | Size
                 | Mood
                 | Manner
                 | Domain
                 | VName
                 | Owner
                 | Message
                 | Property
                 | Kind
                 | Quality
                 | EllipsisAnchor1 | EllipsisAnchor2
                 deriving (Ord, Eq, Show, Generic)

data StrProperty = Type
                 | Negated
                 | Animate
                 | RusNumber | RusPerson | RusGender
                 | Given
                 | Number
                 | Conj
                 | Ellipsis | Elided
                 | SAnchor
                 | Name
                 | Irrealis
                 | DirectSpeech
                 | Not_anymore
                 | Also
                 | Question_mark | Dot
                 | Time
                 | Distinguished
                 | Emphasis | ButEmphasis | AndEmphasis
                 | Isolation | LeftIsolated | RightIsolated
                 deriving (Ord, Eq, Show, Generic)

instance Hashable VarProperty
instance Hashable StrProperty
