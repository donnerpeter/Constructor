module Constructor.SemanticProperties where

import GHC.Generics (Generic)
import Data.Hashable

data VarProperty = Arg1 | Arg2
                 | Member1 | Member2
                 | Questioned
                 | Variants
                 | Target
                 | Theme
                 | Topic
                 | Source
                 | Instrument
                 | Goal | Goal_to | Goal_in | Goal_on | Goal_by | Goal_action
                 | Receiver | Addressee
                 | Experiencer
                 | Companion
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
                 | Location | Location_in | Location_on | Location_at
                 | VTime
                 | Reason | Condition | WhenCondition | IfCondition
                 | Elaboration
                 | Size
                 | Mood
                 | Manner
                 | Domain
                 | State
                 | VName
                 | Owner
                 | Message
                 | Property
                 | Kind
                 | Quality
                 | Color
                 | EllipsisAnchor1 | EllipsisAnchor2 | EllipsisOriginal
                 deriving (Ord, Eq, Show, Generic)

data StrProperty = Type
                 | Negated | ClauseNegated
                 | Animate
                 | RusNumber | RusPerson | RusGender
                 | Given
                 | Number
                 | Conj | Hybrid | ConjStrong
                 | InSitu
                 | Elided | ElidedNoun
                 | SAnchor
                 | ModifierAdverb
                 | Name
                 | Irrealis
                 | DirectSpeech
                 | SituationKind
                 | Not_anymore
                 | Also
                 | So_there
                 | ProfessionCopula
                 | Question_mark | Dot | ParagraphEnd | SectionEnd | Exclamation_mark | Clausal
                 | ExclamativeQuestion
                 | Imperative
                 | Time
                 | Distinguished
                 | Imperfective
                 | Emphasis | ButEmphasis | AndEmphasis
                 | Isolation | LeftIsolated | RightIsolated
                 deriving (Ord, Eq, Show, Generic)

instance Hashable VarProperty
instance Hashable StrProperty
