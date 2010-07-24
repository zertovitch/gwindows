------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                      G N A T C O M . E R R O R S                         --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. It is distributed in the hope that it will be useful,  but WITHOUT --
-- ANY WARRANTY;  without  even the  implied warranty of MERCHANTABILITY or --
-- FITNESS FOR A PARTICULAR PURPOSE.    See the GNU General  Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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
end GNATCOM.Errors;
