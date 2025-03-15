------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--               G N A T C O M . C R E A T E . F A C T O R Y                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
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

with System;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body GNATCOM.Create.Factory is

   CLASS_E_NOAGGREGATION : constant := 16#80040110#;

   procedure Free (Pointer : in System.Address);
   --  Free Factory

   function sync_add_and_fetch
     (Ref : access Interfaces.Unsigned_32;
      Add : Interfaces.Unsigned_32)
      return Interfaces.Unsigned_32
     with
       Import,
       Convention => Intrinsic,
       External_Name => "__sync_add_and_fetch_4";

   function sync_sub_and_fetch
     (Ref : access Interfaces.Unsigned_32;
      Add : Interfaces.Unsigned_32)
      return Interfaces.Unsigned_32
     with
       Import,
       Convention => Intrinsic,
       External_Name => "__sync_sub_and_fetch_4";

   function InterlockedIncrement (Ref : access Interfaces.Unsigned_32)
                                  return Interfaces.Unsigned_32 is
   begin
      return sync_add_and_fetch (Ref, 1);
   end InterlockedIncrement;

   function InterlockedDecrement (Ref : access Interfaces.Unsigned_32)
                                  return Interfaces.Unsigned_32 is
   begin
      return sync_sub_and_fetch (Ref, 1);
   end InterlockedDecrement;

   -- AddRef --

   function AddRef
     (This : access IClassFactory)
      return Interfaces.C.unsigned_long
   is
      Result : Interfaces.Unsigned_32;
      pragma Warnings (Off, Result);
   begin
      --  Add a ref count to the interface, which in this case
      --  is also the object
      Result := InterlockedIncrement (This.Ref_Count'Access);

      return Interfaces.C.unsigned_long (This.Ref_Count);
   end AddRef;

   -- CreateInstance --

   function CreateInstance
     (This      : access IClassFactory;
      pUnkOuter : in     GNATCOM.Types.Pointer_To_IUnknown;
      riid      : in     GNATCOM.Types.Pointer_To_GUID;
      ppvObject : access GNATCOM.Types.Pointer_To_Void)
      return GNATCOM.Types.HRESULT
   is
      use type GNATCOM.Types.Pointer_To_IUnknown;

      Object   : GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type;
      hr       : GNATCOM.Types.HRESULT;
      Result   : Interfaces.Unsigned_32;
      pragma Warnings (Off, Result);

      Refcount : Interfaces.C.unsigned_long;
      pragma Warnings (Off, Refcount);
   begin
      if pUnkOuter /= null then
         return CLASS_E_NOAGGREGATION;
      end if;

      --  Create Object
      Object := This.Create.all;

      --  Since we have created an object, we increment in the global
      --  component count. if the following QueryInterface fails,
      --  the release after it will also end up reducing this count.
      Result := InterlockedIncrement (GNATCOM.Create.Component_Count'Access);

      --  Ask the object for an interface with IID riid
      hr := GNATCOM.Create.COM_Interface.QueryInterface (Object,
                                                         riid,
                                                         ppvObject);

      --  When the object is created it and its first interface have a default
      --  reference count of 1. If the QueryInterface succeeded it is now 2
      --  if it didn't then the next release will reduce it to 0 and clean
      --  up the Interface (and in this case since there are no other
      --  interfaces referenced clean up the object)
      Refcount := GNATCOM.Create.COM_Interface.Release (Object);

      return hr;
   end CreateInstance;

   -- LockServer --

   function LockServer
     (This  : access IClassFactory;
      fLock : in     GNATCOM.Types.bool)
      return GNATCOM.Types.HRESULT
   is
      use type Interfaces.C.long;

      pragma Warnings (Off, This);

      Result : Interfaces.Unsigned_32;
      pragma Warnings (Off, Result);
   begin
      if fLock /= 0 then
         Result := InterlockedIncrement
           (GNATCOM.Create.Server_Lock_Count'Access);
      else
         Result := InterlockedDecrement
           (GNATCOM.Create.Server_Lock_Count'Access);
      end if;

      --  If this is a LocalServer, then a check will be performed
      --  to determine if the server should shutdown
      GNATCOM.Create.Can_Close;

      return S_OK;
   end LockServer;

   -- QueryInterface --

   function QueryInterface
     (This      : access IClassFactory;
      riid      : in     GNATCOM.Types.Pointer_To_GUID;
      ppvObject : access GNATCOM.Types.Pointer_To_Void)
      return GNATCOM.Types.HRESULT
   is
      use type GNATCOM.Types.GUID;

      Result : Interfaces.Unsigned_32;
      pragma Warnings (Off, Result);
   begin
      if riid.all = GNATCOM.Types.IID_IUnknown then
         ppvObject.all := This.all'Address;
      elsif riid.all = GNATCOM.Types.IID_IClassFactory then
         ppvObject.all := This.all'Address;
      else
         ppvObject.all := System.Null_Address;
         return E_NOINTERFACE;
      end if;

      --  When returning new interfaces from any function you must
      --  always add a reference count to that interface directly
      --  or through a call to its AddRef

      Result := InterlockedIncrement (This.Ref_Count'Access);
      return S_OK;
   end QueryInterface;

   -- Release --

   function Release
     (This : access IClassFactory)
      return Interfaces.C.unsigned_long
   is
      use type Interfaces.Unsigned_32;
   begin
      if InterlockedDecrement (This.Ref_Count'Access) /= 0 then
         return Interfaces.C.unsigned_long (This.Ref_Count);
      else
         --  Last reference to IClassFactory and IUnknown released,
         --  so free the object
         Free (This.all'Address);
         return 0;
      end if;
   end Release;

   -- Free --

   procedure Free (Pointer : in System.Address) is
      procedure Free is
         new Ada.Unchecked_Deallocation (IClassFactory,
                                         Pointer_To_IClassFactory);
      function To_Pointer_To_IClassFactory is
         new Ada.Unchecked_Conversion (System.Address,
                                       Pointer_To_IClassFactory);

      X : Pointer_To_IClassFactory;
   begin
      X := To_Pointer_To_IClassFactory (Pointer);
      Free (X);
   end Free;

end GNATCOM.Create.Factory;
