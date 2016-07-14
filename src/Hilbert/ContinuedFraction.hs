{-|
    Module      : Hilbert.ContinuedFraction
    Description : Handle continued fractions.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Handle continued fractions. This module uses mathematics described in

        * Beceanu, Marius. "Period of the Continued Fraction of √n." Thesis.
          Princeton University, 2003. Web.
          <http://web.math.princeton.edu/mathlab/jr02fall/Periodicity/mariusjp.pdf>.
          Junior Thesis.
        * Myerson, Gerry. "How to Detect When Continued Fractions Period
          Terminates." Mathematics Stack Exchange. Stack Exchange, 11 Dec. 2011.
          Web. 24 Apr. 2016. <http://math.stackexchange.com/a/90432>.
-}

module Hilbert.ContinuedFraction
  ( ContinuedFraction
  , mkPeriodic
  , mkAperiodic
  , repeatingPart
  , nonRepeatingPart
  , isPeriodic
  , toList
  , convergent
  , Hilbert.ContinuedFraction.Sqrt.sqrt
  ) where

import Hilbert.ContinuedFraction.Sqrt
import Hilbert.ContinuedFraction.Core
