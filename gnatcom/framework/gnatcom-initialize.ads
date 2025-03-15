------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                   G N A T C O M . I N I T I A L I Z E                    --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2006 David Botton                   --
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

--  Handles initialization of COM libraries for COM clients or applications
--  using COM APIs. There is generally no need to call these initializiation
--  functions when creating InProc COM objects.

package GNATCOM.Initialize is

   procedure Initialize_COM;
   --  Initialize_COM must be called at least once at the start of any
   --  single threaded application that will use COM objects. If the
   --  application needs to exchange pointers freely between multiple
   --  tasks then Initialize_COM_Multi_Threaded should be called
   --  instead.
   --  Initialize_COM may also be called at the start of any task, but
   --  pointers to interfaces from other tasks may not be used nor may
   --  pointers from that task be used by others. The GIT (see
   --  GNATCOM.Iinterface for more information) may be used to get around
   --  this restriction.

   procedure Initialize_COM_Multi_Threaded;
   --  Initialize_COM_Multi_Threaded is called at least once at the start of:
   --  1) the application
   --  2) a task
   --  3) any protected subprogram
   --  where pointers to COM objects will be used and wish to be freely
   --  exchanged.

   procedure Uninitialize_COM;
   --  Should be called at the close of any task where an
   --  Initialize procedure was called. EXCEPT the main application thread.
   --  For example
   --  1) NOT At the end of an application that uses COM
   --  2) During the shutdown of a task where Initialize_COM_Multi_Threaded
   --     was called.
   --  4) At the end of any protected subprogram where pointers to COM
   --     objects will be used
   --  The framework will call Unitialize_COM for the application after
   --  all Interface_Types and their derivatives have been finalized.

   CHANGED_MODE_ERROR : exception;
   --  Raised when an attempt to execute both Initialize_COM and
   --  Initialize_COM_Multi_Threaded in the same application

end GNATCOM.Initialize;
