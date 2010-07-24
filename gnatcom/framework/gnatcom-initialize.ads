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
