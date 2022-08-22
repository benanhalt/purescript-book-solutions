module Test.MySolutions where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Data.Traversable 
import Control.Apply (lift2)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Validation.Semigroup (V, invalid)
import Data.AddressBook
import Data.AddressBook.Validation

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe = lift2 (+)

subMaybe :: Maybe Int -> Maybe Int -> Maybe Int
subMaybe = lift2 (-)

mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
mulMaybe = lift2 (*)

divMaybe :: Maybe Int -> Maybe Int -> Maybe Int
divMaybe = lift2 (/)


addApply :: forall f. Apply f => f Int -> f Int -> f Int
addApply = lift2 (+)

subApply :: forall f. Apply f => f Int -> f Int -> f Int
subApply = lift2 (-)

mulApply :: forall f. Apply f => f Int -> f Int -> f Int
mulApply = lift2 (*)

divApply :: forall f. Apply f => f Int -> f Int -> f Int
divApply = lift2 (/)


-- (Difficult) Write a function combineMaybe which has type forall a
-- f. Applicative f => Maybe (f a) -> f (Maybe a). This function takes
-- an optional computation with side-effects, and returns a
-- side-effecting computation which has an optional result.


combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just fa) = Just <$> fa

stateRegex :: Regex
stateRegex = unsafeRegex "^[A-Za-z]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S" noFlags

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a =
  address <$> matches "Street" nonEmptyRegex a.street
          <*> matches "City" nonEmptyRegex a.city
          <*> matches "State" stateRegex a.state

data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance eqTree :: Eq a => Eq (Tree a)
derive instance genericTree :: Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch l a r) =
    "(Branch "
    <> show l
    <> " "
    <> show a
    <> " "
    <> show r
    <> ")"

derive instance functorTree :: Functor Tree

instance foldableTree :: Foldable Tree where
  foldl f b Leaf = b
  foldl f b (Branch l a r) = foldl f (foldl f b l `f` a) r

  foldr f b Leaf = b
  foldr f b (Branch l a r) = foldr f (a `f` foldr f b r) l

  foldMap f Leaf = mempty
  foldMap f (Branch l a r) = foldMap f l <> f a <> foldMap f r

instance traversableTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch l a r) = Branch <$> traverse f l <*> f a <*> traverse f r

  sequence Leaf = pure Leaf
  sequence (Branch l a r) = Branch <$> sequence l <*> a <*> sequence r

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder f Leaf = pure Leaf
traversePreOrder f (Branch l a r) = ado
  fa <- f a
  fl <- traversePreOrder f l
  fr <- traversePreOrder f r
  in Branch fl fa fr

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder f Leaf = pure Leaf
traversePostOrder f (Branch l a r) = ado
  fl <- traversePostOrder f l
  fr <- traversePostOrder f r
  fa <- f a
  in Branch fl fa fr

type PersonOptionalAddress
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

personOptionalAddress :: String -> String -> Maybe Address -> Array PhoneNumber -> PersonOptionalAddress
personOptionalAddress firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: PersonOptionalAddress -> V Errors PersonOptionalAddress
validatePersonOptionalAddress p = ado
  firstName <- nonEmpty "First Name" p.firstName
  lastName  <- nonEmpty "Last Name" p.lastName
  address   <- traverse validateAddress p.homeAddress
  numbers   <- validatePhoneNumbers "Phone Numbers" p.phones
  in personOptionalAddress firstName lastName address numbers


sequenceUsingTraverse :: forall t b m. Traversable t => Applicative m => t (m b) -> m (t b)
sequenceUsingTraverse = traverse identity

traverseUsingSequence :: forall t a b m. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f ta = sequence $ f <$> ta
