# SPDX-FileCopyrightText: Copyright Preetham Gujjula
# SPDX-License-Identifier: BSD-3-Clause
#
#!/bin/bash
stack runhaskell -- -isrc -itest test/Main.hs --match=$1
