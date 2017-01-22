-- Copyright 2009-2010 Corey O'Connor
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

#ifndef MIN_VERSION_base
#defined MIN_VERSION_base(x,y,z) 1
#endif

module Graphics.Vty.DisplayAttributes
    where

import Graphics.Vty.Attributes

import Data.Bits ((.&.))

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(..), mconcat)
#endif

-- | Given the previously applied display attributes as a FixedAttr and the current display
-- attributes as an Attr produces a FixedAttr that represents the current display attributes. This
-- is done by using the previously applied display attributes to remove the "KeepCurrent"
-- abstraction.
fixDisplayAttr :: FixedAttr -> Attr -> FixedAttr
fixDisplayAttr fattr attr
    = FixedAttr (fixStyle (fixedStyle fattr)     (attrStyle attr))
                (fixColor (fixedForeColor fattr) (attrForeColor attr))
                (fixColor (fixedBackColor fattr) (attrBackColor attr))
    where
        fixStyle _s Default           = defaultStyleMask
        fixStyle s KeepCurrent        = s
        fixStyle _s (SetTo newStyle)  = newStyle
        fixColor _c Default           = Nothing
        fixColor c KeepCurrent        = c
        fixColor _c (SetTo c)         = Just c

-- | difference between two display attributes. Used in the calculation of the operations required
-- to go from one display attribute to the next.
--
-- Previously, vty would reset display attributes to default then apply the new display attributes.
-- This turned out to be very expensive: A *lot* more data would be sent to the terminal than
-- required.
data DisplayAttrDiff = DisplayAttrDiff
    { styleDiffs    :: [StyleStateChange]
    , foreColorDiff :: DisplayColorDiff
    , backColorDiff :: DisplayColorDiff
    }
    deriving (Show)

instance Monoid DisplayAttrDiff where
    mempty = DisplayAttrDiff [] NoColorChange NoColorChange
    mappend d0 d1 =
        let ds  = simplifyStyleDiffs (styleDiffs d0)    (styleDiffs d1)
            fcd = simplifyColorDiffs (foreColorDiff d0) (foreColorDiff d1)
            bcd = simplifyColorDiffs (backColorDiff d0) (backColorDiff d1)
        in DisplayAttrDiff ds fcd bcd

-- | Used in the computation of a final style attribute change.
--
-- TODO(corey): not really a simplify but a monoid instance.
simplifyStyleDiffs :: [StyleStateChange] -> [StyleStateChange] -> [StyleStateChange]
simplifyStyleDiffs cs0 cs1 = cs0 `mappend` cs1

-- | Consider two display color attributes diffs. What display color attribute diff are these
-- equivalent to?
--
-- TODO(corey): not really a simplify but a monoid instance.
simplifyColorDiffs :: DisplayColorDiff -> DisplayColorDiff -> DisplayColorDiff
simplifyColorDiffs _cd             ColorToDefault = ColorToDefault
simplifyColorDiffs cd              NoColorChange  = cd
simplifyColorDiffs _cd             (SetColor !c)  = SetColor c

-- | Difference between two display color attribute changes.
data DisplayColorDiff
    = ColorToDefault
    | NoColorChange
    | SetColor !Color
    deriving (Show, Eq)

-- | Style attribute changes are transformed into a sequence of apply/removes of the individual
-- attributes.
data StyleStateChange
    = ApplyStandout
    | RemoveStandout
    | ApplyUnderline
    | RemoveUnderline
    | ApplyReverseVideo
    | RemoveReverseVideo
    | ApplyBlink
    | RemoveBlink
    | ApplyDim
    | RemoveDim
    | ApplyBold
    | RemoveBold
    deriving (Show, Eq)

-- | Determines the diff between two display&color attributes. This diff determines the operations
-- that actually get output to the terminal.
displayAttrDiffs :: FixedAttr -> FixedAttr -> DisplayAttrDiff
displayAttrDiffs attr attr' = DisplayAttrDiff
    { styleDiffs    = diffStyles (fixedStyle attr)      (fixedStyle attr')
    , foreColorDiff = diffColor  (fixedForeColor attr) (fixedForeColor attr')
    , backColorDiff = diffColor  (fixedBackColor attr) (fixedBackColor attr')
    }

diffColor :: Maybe Color -> Maybe Color -> DisplayColorDiff
diffColor Nothing  (Just c') = SetColor c'
diffColor (Just c) (Just c')
    | c == c'   = NoColorChange
    | otherwise = SetColor c'
diffColor Nothing  Nothing = NoColorChange
diffColor (Just _) Nothing = ColorToDefault

diffStyles :: Style -> Style -> [StyleStateChange]
diffStyles prev cur
    = mconcat
    [ styleDiff standout      ApplyStandout     RemoveStandout
    , styleDiff underline     ApplyUnderline    RemoveUnderline
    , styleDiff reverseVideo  ApplyReverseVideo RemoveReverseVideo
    , styleDiff blink         ApplyBlink        RemoveBlink
    , styleDiff dim           ApplyDim          RemoveDim
    , styleDiff bold          ApplyBold         RemoveBold
    ]
    where
        styleDiff s sm rm
            = case (0 == prev .&. s, 0 == cur .&. s) of
                -- not set in either
                (True, True)   -> []
                -- set in both
                (False, False) -> []
                -- now set
                (True, False)  -> [sm]
                -- now unset
                (False, True)  -> [rm]
