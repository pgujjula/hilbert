# SPDX-FileCopyrightText: Copyright Preetham Gujjula
# SPDX-License-Identifier: BSD-3-Clause
#
#!/bin/bash
stack ghci hilbert:lib hilbert:test:hilbert-tests "$@"
