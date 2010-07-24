------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                       G N A T C O M . C R E A T E                        --
--                                                                          --
--                                 S p e c                                  --
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

--  Base package for creation of COM objects

with Interfaces.C;

package GNATCOM.Create is

   Component_Count   : aliased Interfaces.C.long := 0;
   --  Number of components created that have not been deallocated

   Server_Lock_Count : aliased Interfaces.C.long := 0;
   --  Number of locks requested by clients on the server requesting the
   --  server not shut down even if there are no outstanding components,
   --  i.e. Component_Count > 0

   hInstance : Interfaces.C.long := 0;
   --  Contains the intstance handle of the container

   InProcServer : Boolean := True;
   --  Indicates to the framework that it is being used by an in-process
   --  server

   type Thread_Model is (Single, Multiple, Both);
   --  The Thread_Model type tells GNATCOM to create objects that
   --  conform to the following behaviors:
   --
   --  Single
   --  * Objects in this container are only accessed from the thread they
   --    were created in.
   --  * Multiple instances of the object, may be in different threads.
   --    Therefore, when creating objects of Thread_Model Single all global
   --    variables must be protected, but variables contained in the
   --    CoClass_Type records (CreateCOM generates a type derived from
   --    GNATCOM.Create.COM_Interface.CoClass_Type for each CoClass, i.e..
   --    COM Object, that contains the instance specific data for the COM
   --    object) need not be protected since all invocation of COM methods
   --    are guaranteed to be synchronized on the thread is was created on
   --    regardless of what thread makes the method invocation.
   --  * If need to access other COM objects from with in this object arise
   --    only CoInitialize, OleInitialize, CoInitializeEx (0,
   --    COINIT_APARTMENTTHREADED), or GNATCOM.Initialize.Initialize_COM
   --    may be called (but is not needed to be called) in the thread that
   --    it has been invoked by. Other tasks that are created may call the
   --    other initializers if needed.
   --  * GNATCOM.Initialize.Initialize_COM is called at startup of
   --    the LocalServer version of the object
   --  * In MS parlance, this object will be created in an STA = Single
   --    Threaded Apartment
   --
   --  Multiple
   --  * Objects in this container may be created in one thread and accessed
   --    from any other
   --  * This option is usually used only for objects in InProcServers that
   --    need to call CoInitializeEx (0, COINIT_MULTITHREADED),
   --    GNATCOM.Initialize.Initialize_Multi_Threaded or will have
   --    internal tasks that will be calling back in to the object.
   --  * Global and also Object specific data contained in the CoClass_Type
   --    (See above in Single) must be protected since multiple concurrent
   --    invocations of the object are possible
   --  * If an object invokes from a thread that called CoInitialize,
   --    OleInitialize, CoInitializeEx (0, COINIT_APARTMENTTHREADED),
   --    or GNATCOM.Initialize.Initialize_COM, and the object is an
   --    InProcServer (DLL) access to the object will be slower since
   --    invocations must take place through a proxy.
   --  * GNATCOM.Initialize.Initialize_COM_Multi_Threaded is called at
   --    startup of the LocalServer version of the object
   --  * Known in MS circles as a Free Threaded Object and are said to
   --    run only in an MTA - Mutli Threaded Apartment
   --
   --  Both
   --  * Objects in this container may be created in one thread and accessed
   --    from any other
   --  * Does not always suffer the above penalty for objects in
   --    InprocServers since object will be accessed in the way the is
   --    most efficient to the client based on how the thread the object
   --    was was created in initialized itself with one of the
   --    CoIntialize/GNATCOM.Initialize variants.
   --  * This option is the same as Multiple for LocalServers (Exe's)

   Use_Thread_Model : Thread_Model := Single;
   --  The default thread model is Single and can be changed by adding the
   --  following line to the genrated code in X-dll.adb in procedure main
   --  for InprocServers:
   --
   --  procedure Main is
   --  begin
   --     GNATCOM.Create.Use_Thread_Model := GNATCOM.Create.Both;
   --                                                       -- or Multiple
   --  ....
   --
   --  For the LocalServer version add in X-exe.adb:
   --  add the line:
   --
   --     GNATCOM.Create.Use_Thread_Model := GNATCOM.Create.Both;
   --                                                       -- or Multiple
   --
   --  after the begin of the X.Exe procedure.

   Main_Thread_ID : Interfaces.C.unsigned_long;
   --  Local servers store the thread ID of the main thread here

   procedure Can_Close;
   --  Called with in the framework to determine if the server should
   --  shutdown, i.e. when Component_Count and Server_Lock_Count both
   --  equal zero

   --  Notes:

   --  Createing Singletons = Class Objects :
   --
   --     Modify the generated create function for a class to return a static
   --  instance of the derived CoClass_Type. For example:
   --
   --
   --    Singleton : Pointer_To_BeepClass_Type := null;
   --
   --    function Create
   --      return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface
   --    is
   --       use type Interfaces.C.long;
   --    begin
   --       if Singleton = null then
   --          Singleton := new BeepClass_Type;
   --       end if;
   --
   --       Singleton.Ref_Count := Singleton.Ref_Count + 1;
   --       GNATCOM.Create.Component_Count :=
   --         GNATCOM.Create.Component_Count - 1;
   --       GNATCOM.Create.COM_Interface.Create_Object (Singleton);
   --
   --       return Object;
   --    end Create;
   --
   --  The additional Ref_Count is added to insure that no attempt to
   --  free the Singelton object is made when the last interface is
   --  destroyed.
   --
   --  The reduction of the Component_Count insures that server housing
   --  the singleton will shutdown when there are no more connections to it.
   --
   --  If the object is in a LocalServer, other processes can attach to and
   --  use the singleton object.
   --
   --  Removing the Component_Count reduction will cause a LocalServer to
   --  remain in memory once started even if no interfaces are in use
   --  and the original process that created it has since died.

end GNATCOM.Create;
