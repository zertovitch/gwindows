------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                      G N A T C O M . E R R O R S                         --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2018 David Botton                   --
--                                                                          --
-- MIT License                                                              --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining    --
-- a copy of this software and associated documentation files (the          --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          --
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
--                                                                          --
-- More information about GNATCOM and the most current version can          --
-- be located on the web at http://www.gnavi.org/gnatcom                    --
--                                                                          --
------------------------------------------------------------------------------

--  COM functions return the type HRESULT. HRESULTs often return one of three
--  general results:
--
--  S_OK     - Function returned succesfully
--  S_FALSE  - Logical false return, but not failure
--  E_FAIL   - Failure
--
--  Additionally there are numerous other standard error codes that are
--  returned.
--
--  This package provides subprograms to convert many of the standard
--  HRESULT return values to Ada exceptions. It is primarily used by thin
--  bindings to COM functions.

with GNATCOM.Types;
with System;

package GNATCOM.Errors is

   function SUCCEEDED (Result : GNATCOM.Types.HRESULT)
     return Boolean;
   --  Returns true for any HRESULT that does not have its failure bit set.

   function FAILED (Result : GNATCOM.Types.HRESULT)
     return Boolean;
   --  Returns true for any HRESULT that has not succeeded

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT);
   --  Check result for failure and raise Ada exceptions
   --  Stores the Result using Set_Last_HRESULT

   function Logical_Check (Result : in GNATCOM.Types.HRESULT)
     return Boolean;
   --  Returns false when Result is S_FALSE, other wise return
   --  true when succeded and raise Ada exceptions for failures
   --  Stores the Result using Set_Last_HRESULT

   function To_String (Result : GNATCOM.Types.HRESULT)
     return String;
   --  Looks up the HRESULT in the SYSTEM Message Table for a text
   --  description of the error

   function To_String (Address : System.Address) return String;
   --  Converts and address to a string

   function Get_Last_HRESULT return GNATCOM.Types.HRESULT;
   --  Returns the last HRESULT stored within the current task
   --  by Set_Last_HRESULT. Thick bindings created using BindCOM use
   --  Error_Check

   procedure Set_Last_HRESULT (Result : GNATCOM.Types.HRESULT);
   --  Stores the last HRESULT for the current task

   COM_ERROR : exception;
   --  General COM_ERROR (E_FAIL) or other failed hresult that does not
   --  have a mapped exception

   NOT_IMPLEMENTED_ERROR : exception;
   --  Raised when an attempt was made to call a function on a COM object
   --  that has not been implemented

   OUT_OF_MEMORY_ERROR : exception;
   --  Raised when out of memory

   INVALID_ARGUMENT_ERROR : exception;
   --  Raised when one or more of the arguments to a function are invalid

   NO_INTERFACE_ERROR : exception;
   --  Raised when requested interface is unavailable

   INVALID_POINTER_ERROR : exception;
   --  Raised for invalid pointers

   ABORT_ERROR : exception;
   --  Raised when operation is aborted

   ACCESS_DENIED_ERROR : exception;
   --  Raised for when a general access denied error occurs

   UNEXPECTED_ERROR : exception;
   --  Raised when an unexpected error occurs

   OBJECT_NOT_CONNECTED_ERROR : exception;
   --  Raised when an attempt to invoke a method on an object that is no
   --  longer connected to the process

   ERROR_IN_DB_COMMAND : exception;
   --  Raised when an database command (normally, a SQL
   --  command) contains error(s)

end GNATCOM.Errors;
