------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--        G N A T C O M . C R E A T E . C O M _ I N T E R F A C E           --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body GNATCOM.Create.COM_Interface is

   procedure Free_Object (Pointer : in out Pointer_To_CoClass);

   procedure Free_Interface (Pointer : in System.Address);

   function sync_add_and_fetch (Ref : access Interfaces.C.long;
                                Add : Integer) return Interfaces.C.long;
   pragma Import (Intrinsic, sync_add_and_fetch,
                  "__sync_add_and_fetch_4");

   function InterlockedIncrement (Ref : access Interfaces.C.long)
                                  return Interfaces.C.long is
   begin
      return sync_add_and_fetch (Ref, 1);
   end InterlockedIncrement;

   function InterlockedDecrement (Ref : access Interfaces.C.long)
                                  return Interfaces.C.long is
   begin
      return sync_add_and_fetch (Ref, -1);
   end InterlockedDecrement;

   ------------
   -- AddRef --
   ------------

   function AddRef (This : access COM_Interface_Type)
                   return Interfaces.C.unsigned_long
   is
      Result : Interfaces.C.long;
      pragma Warnings (Off, Result);
   begin
      --  Object wide reference increment
      Result := InterlockedIncrement (This.CoClass.Ref_Count'Access);

      --  Interface reference increment
      Result := InterlockedIncrement (This.Ref_Count'Access);

      return Interfaces.C.unsigned_long (This.Ref_Count);
   end AddRef;

   --------------------
   -- QueryInterface --
   --------------------

   function QueryInterface
     (This      : access COM_Interface_Type;
      riid      : in     GNATCOM.Types.Pointer_To_GUID;
      ppvObject : access GNATCOM.Types.Pointer_To_Void)
     return GNATCOM.Types.HRESULT
   is
      use type GNATCOM.Types.GUID;

      New_Interface : aliased Pointer_To_COM_Interface_Type;
      Result        :         Interfaces.C.long;
      pragma Warnings (Off, Result);
   begin
      --  GNATCOM implements COM objects using a technique often
      --  called "tear-off" interfaces. Interfaces are created
      --  as needed and requested. This behavior has many benefits
      --  and is also used in the C++ world when there is a need
      --  to dynamicly create interfaces and when there is a need
      --  to control resources on an interface by interface level.

      --  IUnknown must always return the "official" IUnknown for the
      --  object, so we retrieve it from the CoClass
      if riid.all = GNATCOM.Types.IID_IUnknown then
         --  Add a ref count to the Interface
         Result := InterlockedIncrement
           (This.CoClass.IUnknown.Ref_Count'Access);

         --  Add an Object wide ref count
         Result := InterlockedIncrement (This.CoClass.Ref_Count'Access);

         --  Set return interface to the "official" IUnknown
         ppvObject.all := This.CoClass.IUnknown.all'Address;

         return S_OK;
      end if;

      --  Loop through IID map in object to see if it supports the
      --  requested IID in riid
      for N in
        This.CoClass.IID_Map.all'First .. This.CoClass.IID_Map.all'Last
      loop
         if riid.all = This.CoClass.IID_Map (N).IID then

            --  Create a new Interface pointer, ie. a pointer
            --  to a record containing as the first member a
            --  pointer to a virtual function table, ala C++
            New_Interface := new COM_Interface_Type;

            --  Point to the virtual table of that matches the
            --  found IID
            New_Interface.Vtbl := This.CoClass.IID_Map (N).Vtbl;

            --  The next entries in the record contain a pointer to
            --  our object and the interfaces reference count
            New_Interface.CoClass := This.CoClass;

            --  Add a ref count to the Object wide ref count,
            --  the COM_Interface_Type already has a ref count
            --  built in, so we need not add a ref count to the
            --  interface also here.
            Result := InterlockedIncrement (This.CoClass.Ref_Count'Access);

            --  Set return to the new interface
            ppvObject.all := New_Interface.all'Address;

            return S_OK;
         end if;
      end loop;

      --  No interface matches set return to null
      ppvObject.all := System.Null_Address;

      --  See if there is a user provided QueryInterface by
      --  dispatching on the QueryInterface method of the Object
      --  for any custom handling of interfaces.
      return QueryInterface (This.CoClass.all,
                             This,
                             riid,
                             ppvObject);
   end QueryInterface;

   --------------------
   -- QueryInterface --
   --------------------

   function QueryInterface
     (Dispatch  : in     CoClass_Type;
      This      : access COM_Interface_Type;
      riid      : in     GNATCOM.Types.Pointer_To_GUID;
      ppvObject : access GNATCOM.Types.Pointer_To_Void)
     return GNATCOM.Types.HRESULT
   is
      pragma Warnings (Off, Dispatch);
      pragma Warnings (Off, This);
      pragma Warnings (Off, riid);
      pragma Warnings (Off, ppvObject);
   begin
      --  Default implementation of custom QueryInterface for Objects
      --  just returns E_NOINTERFACE. Any over ride on QueryInterface
      --  must return this value if it does not handle the QueryInteface
      --  request.
      return E_NOINTERFACE;
   end QueryInterface;

   -------------
   -- Release --
   -------------

   function Release (This : access COM_Interface_Type)
                    return Interfaces.C.unsigned_long
   is
      use type Interfaces.C.long;

      Result : Interfaces.C.long;
      pragma Warnings (Off, Result);
   begin

      --  Reduce the Object wide reference count
      Result := InterlockedDecrement (This.CoClass.Ref_Count'Access);

      --  Reduce the Interface ref count and check to see if this
      --  is the last release
      if InterlockedDecrement (This.Ref_Count'Access) /= 0 then
         return Interfaces.C.unsigned_long (This.Ref_Count);
      else
         --  Last reference to the interface released, so free interface
         Free_Interface (This.all'Address);
         return 0;
      end if;
   end Release;

   -------------
   -- Release --
   -------------

   procedure Release (This : access COM_Interface_Type) is
      Result : Interfaces.C.unsigned_long;
      pragma Warnings (Off, Result);
   begin
      --  The results of a release are in general bogus as
      --  any time proxies are introduced, the actual result
      --  returned to a client will not reflect the true
      --  result returned from a release. This function
      --  provides an easy way for other parts of the framework
      --  to call a release and ignore the return value.

      Result := Release (This);
   end Release;

   -----------------
   -- Free_Object --
   -----------------

   procedure Free_Object (Pointer : in out Pointer_To_CoClass) is
      procedure Free_CoClass is
         new Ada.Unchecked_Deallocation (CoClass_Type'Class,
                                         Pointer_To_CoClass);
      procedure Free is
         new Ada.Unchecked_Deallocation (COM_Interface_Type,
                                         Pointer_To_COM_Interface_Type);
   begin
      --  First deallocate the "Official" IUnknown Interface
      Free (Pointer.IUnknown);

      --  Deallocate the Object
      Free_CoClass (Pointer);
   end Free_Object;

   --------------------
   -- Free_Interface --
   --------------------

   procedure Free_Interface (Pointer : in System.Address) is
      use type Interfaces.C.long;

      function To_Pointer_To_COM_Interface_Type is
         new Ada.Unchecked_Conversion (System.Address,
                                       Pointer_To_COM_Interface_Type);
      procedure Free is
         new Ada.Unchecked_Deallocation (COM_Interface_Type,
                                         Pointer_To_COM_Interface_Type);

      P_Interface : Pointer_To_COM_Interface_Type;
      CoClass   : Pointer_To_CoClass;
      Result    : Interfaces.C.long;
      pragma Warnings (Off, Result);
   begin
      P_Interface := To_Pointer_To_COM_Interface_Type (Pointer);
      CoClass   := P_Interface.CoClass;

      --  If this is the official IUnknown, don't deallocate
      --  it may be needed again. It is deallocated when
      --  the object is deallocated.
      if P_Interface.CoClass.IUnknown /= P_Interface then
         Free (P_Interface);
      end if;

      if CoClass.Ref_Count < 1 then
         --  All interfaces to this COM object have been released
         --  so free the COM Object
         Free_Object (CoClass);

         --  Reduce the global component count since the object has
         --  been deallocated.
         Result := InterlockedDecrement
           (GNATCOM.Create.Component_Count'Access);
      end if;

      --  Initiate a check to see if the COM objects host container
      --  should shut down. This is a no-op for Inproc Servers.
      GNATCOM.Create.Can_Close;
   end Free_Interface;

   -------------------
   -- Create_Object --
   -------------------

   function Create_Object (Class_Object : Pointer_To_CoClass)
                          return Pointer_To_COM_Interface_Type
   is
      First_Interface : Pointer_To_COM_Interface_Type;
   begin
      --  Create out first Interface Pointer for the COM Object
      First_Interface := new COM_Interface_Type;

      --  Place pointer to object in to Interface
      First_Interface.CoClass := Class_Object;

      --  Make this first interface the "Official" IUnknown for
      --  the object
      if First_Interface.CoClass.IUnknown = null then
         First_Interface.CoClass.IUnknown := First_Interface;
      end if;

      return First_Interface;
   end Create_Object;

end GNATCOM.Create.COM_Interface;
