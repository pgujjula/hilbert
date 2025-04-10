-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module Test.Util (throwsException) where

import Control.Exception (SomeException, catch, evaluate)
import Test.Tasty.HUnit (Assertion, (@?))

throwsException :: a -> Assertion
throwsException thunk =
  let result =
        (evaluate thunk >> pure False)
          `catch` (\(_ :: SomeException) -> pure True)
   in result @? "expected an exception but none was thrown"
