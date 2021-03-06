/* Copyright 2011  Petter Urkedal
 *
 * This file is part of the Viz Standard Library <http://www.vizlang.org/>.
 *
 * The Viz Standard Library (VSL) is free software: you can redistribute it
 * and/or modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * The VSL is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with the VSL.  If not, see <http://www.gnu.org/licenses/>.
 */

#define __STDC_LIMIT_MACROS 1
#define __STDC_CONSTANT_MACROS 1

#include <stddef.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>
#include "compiler/llvm_suppl.h"

void
LLVMAssertValidModule(LLVMModuleRef M)
{
    char *error = NULL;
    LLVMVerifyModule(M, LLVMAbortProcessAction, &error);
    LLVMDisposeMessage(error);
}

void
LLVMAssertValidFunction(LLVMValueRef F)
{
    LLVMVerifyFunction(F, LLVMAbortProcessAction);
}
