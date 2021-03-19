module Turing.Optics.Lenses where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

-- Card

_id :: forall a r. Lens' { id :: a | r } a
_id = prop (SProxy :: SProxy "id")

_trueInstruction :: forall a r. Lens' { trueInstruction :: a | r } a
_trueInstruction = prop (SProxy :: SProxy "trueInstruction")

_falseInstruction :: forall a r. Lens' { falseInstruction :: a | r } a
_falseInstruction = prop (SProxy :: SProxy "falseInstruction")

-- Machine

_tape :: forall a r. Lens' { tape :: a | r } a
_tape = prop (SProxy :: SProxy "tape")

_position :: forall a r. Lens' { position :: a | r } a
_position = prop (SProxy :: SProxy "position")

_cards :: forall a r. Lens' { cards :: a | r } a
_cards = prop (SProxy :: SProxy "cards")

_card :: forall a r. Lens' { card :: a | r } a
_card = prop (SProxy :: SProxy "card")

_subState :: forall a r. Lens' { subState :: a | r } a
_subState = prop (SProxy :: SProxy "subState")

-- Instruction

_output :: forall a r. Lens' { output :: a | r } a
_output = prop (SProxy :: SProxy "output")

_moveRight :: forall a r. Lens' { moveRight :: a | r } a
_moveRight = prop (SProxy :: SProxy "moveRight")

_nextCardId :: forall a r. Lens' { nextCardId :: a | r } a
_nextCardId = prop (SProxy :: SProxy "nextCardId")
