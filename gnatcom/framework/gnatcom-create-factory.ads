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
         Ref_Count : aliased Interfaces.C.long := 1;
         Create    : Creation_Function;
      end record;
   pragma Convention (C, IClassFactory);
   --  Create the IClassFactory interface
   --  Since the Class Factory Object only contains a single inheritance chain
   --  of interfaces, ie. IUnknown <-- IClassFactory. We just treat the
   --  IClassFactory Interface as the Object and its Ref_Count as the Object
   --  wide reference count

end GNATCOM.Create.Factory;
