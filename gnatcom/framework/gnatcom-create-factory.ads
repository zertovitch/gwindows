------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--               G N A T C O M . C R E A T E . F A C T O R Y                --
--                                                                          --
--                                 S p e c                                  --
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

with GNATCOM.Types;
with GNATCOM.Create.COM_Interface;

package GNATCOM.Create.Factory is

   type IClassFactory;
   type Pointer_To_IClassFactory is access all IClassFactory;
   pragma No_Strict_Aliasing (Pointer_To_IClassFactory);

   --  Access function types for Class Factory

   type af_QueryInterface is access
     function (This      : access IClassFactory;
               riid      : GNATCOM.Types.Pointer_To_GUID;
               ppvObject : access GNATCOM.Types.Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (Stdcall, af_QueryInterface);

   type af_AddRef is access function (This : access IClassFactory)
     return Interfaces.C.unsigned_long;
   pragma Convention (Stdcall, af_AddRef);

   type af_Release is access function (This : access IClassFactory)
     return Interfaces.C.unsigned_long;
   pragma Convention (Stdcall, af_Release);

   type af_CreateInstance is access
     function (This      : access IClassFactory;
               pUnkOuter : in     GNATCOM.Types.Pointer_To_IUnknown;
               riid      : in     GNATCOM.Types.Pointer_To_GUID;
               ppvObject : access GNATCOM.Types.Pointer_To_Void)
   return GNATCOM.Types.HRESULT;
   pragma Convention (Stdcall, af_CreateInstance);

   type af_LockServer is access
     function (This  : access IClassFactory;
               fLock : in     GNATCOM.Types.bool)
     return GNATCOM.Types.HRESULT;
   pragma Convention (Stdcall, af_LockServer);

   --  Function specs for IClassFactory
   --
   --  The first three functions QueryInterface, Addref and Release are from
   --  IClassFactory's parent interface, IUnknown. This allows every interface
   --  to be treated polymorphically as IUnknown. Then QueryInterface can be
   --  used to query the object for support of an interface and retrieve a
   --  pointer to the COM object's interface that conforms to that interface.
   --  In essence QueryInterface is used to do a typesafe "cast" from one
   --  interface of a COM object to another.

   function QueryInterface
     (This      : access IClassFactory;
      riid      : in     GNATCOM.Types.Pointer_To_GUID;
      ppvObject : access GNATCOM.Types.Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (Stdcall, QueryInterface);
   --  Query COM object for the interface riid and return a pointer to it
   --  if possible. QueryInterface will call AddRef on the interface before
   --  returning it in ppvObject

   function AddRef (This : access IClassFactory)
     return Interfaces.C.unsigned_long;
   pragma Convention (Stdcall, AddRef);
   --  Add a reference count to the object

   function Release (This : access IClassFactory)
     return Interfaces.C.unsigned_long;
   pragma Convention (Stdcall, Release);
   --  Reduce reference count. If 0 free object from memory

   function CreateInstance
     (This      : access IClassFactory;
      pUnkOuter : in     GNATCOM.Types.Pointer_To_IUnknown;
      riid      : in     GNATCOM.Types.Pointer_To_GUID;
      ppvObject : access GNATCOM.Types.Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (Stdcall, CreateInstance);
   --  Create an instance of the COM object returning a pointer to
   --  an interface with the IID riid.

   function LockServer (This  : access IClassFactory;
                        fLock : in     GNATCOM.Types.bool)
     return GNATCOM.Types.HRESULT;
   pragma Convention (Stdcall, LockServer);
   --  if fLock is true add a lock to prevent the host server from unloading
   --  from memory.

   type IClassFactory_Vtbl_Record is
      record
         QueryInterface : af_QueryInterface := Factory.QueryInterface'Access;
         AddRef         : af_AddRef := Factory.AddRef'Access;
         Release        : af_Release := Factory.Release'Access;
         CreateInstance : af_CreateInstance := Factory.CreateInstance'Access;
         LockServer     : af_LockServer := Factory.LockServer'Access;
      end record;
   pragma Convention (C, IClassFactory_Vtbl_Record);
   type Pointer_To_IClassFactory_Vtbl is access all IClassFactory_Vtbl_Record;
   --  IClassFactory virtual function table

   IClassFactory_Vtbl : aliased IClassFactory_Vtbl_Record;

   type Creation_Function is access function
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type;

   type IClassFactory is
      record
         Vtbl      : Pointer_To_IClassFactory_Vtbl :=
           IClassFactory_Vtbl'Access;
         Ref_Count : aliased Interfaces.Unsigned_32 := 1;
         Create    : Creation_Function;
      end record;
   pragma Convention (C, IClassFactory);
   --  Create the IClassFactory interface
   --  Since the Class Factory Object only contains a single inheritance chain
   --  of interfaces, ie. IUnknown <-- IClassFactory. We just treat the
   --  IClassFactory Interface as the Object and its Ref_Count as the Object
   --  wide reference count

end GNATCOM.Create.Factory;
