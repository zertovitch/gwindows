------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--        G N A T C O M . C R E A T E . C O M _ I N T E R F A C E           --
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

--  COM_Interface provides an implementation of IUnknown for COM objects
--  and provides the C++ style virtual function tables that comprise
--  the COM binary standard for interfaces

with GNATCOM.Types;
with System;

package GNATCOM.Create.COM_Interface is

   type GUID_Record is
      record
         IID  : aliased GNATCOM.Types.GUID;
         Vtbl : System.Address;
      end record;
   --  Map for IIDs to virtual function tables

   type GUID_Record_Array is array (Natural range <>) of GUID_Record;
   type Pointer_To_GUID_Record_Array is access all GUID_Record_Array;
   --  The GUID_Record_Array is used by QueryInterface to find
   --  which interfaces are implemented and retrieve their virtual
   --  function tables.

   type COM_Interface_Type;
   type Pointer_To_COM_Interface_Type is access all COM_Interface_Type;
   pragma No_Strict_Aliasing (Pointer_To_COM_Interface_Type);

   type CoClass_Type (IID_Map : Pointer_To_GUID_Record_Array) is tagged
      record
         Ref_Count : aliased Interfaces.C.long := 1;
         IUnknown  : Pointer_To_COM_Interface_Type := null;
      end record;
   --  New COM objects extend CoClass_Type with their own implementation
   --  data IID_Map contains a map of each implemented interface,
   --  Ref_Count keeps a class wide reference count and pIUnknown contains
   --  a pointer to the "official" IUnknown. COM specs state that every
   --  QueryInterface for IUnknown must return the same pointer to IUnknown
   --  Other interfaces need not always return the same pointer and in
   --  GNATCOM they frequently are not the same.

   type Pointer_To_CoClass is access all CoClass_Type'Class;
   --  Base type for CoClasses used with COM_Interface_Type

   type Af_QueryInterface is access
     function (This      : access COM_Interface_Type;
               riid      : in     GNATCOM.Types.Pointer_To_GUID;
               ppvObject : access GNATCOM.Types.Pointer_To_Void)
      return GNATCOM.Types.HRESULT;
   pragma Convention (Stdcall, Af_QueryInterface);

   type Af_AddRef is access function (This : access COM_Interface_Type)
     return Interfaces.C.unsigned_long;
   pragma Convention (Stdcall, Af_AddRef);

   type Af_Release is access function (This : access COM_Interface_Type)
     return Interfaces.C.unsigned_long;
   pragma Convention (Stdcall, Af_Release);

   function QueryInterface
     (This      : access COM_Interface_Type;
      riid      : in     GNATCOM.Types.Pointer_To_GUID;
      ppvObject : access GNATCOM.Types.Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (Stdcall, QueryInterface);
   --  Query COM object for the interface riid and return a pointer to it
   --  if possible. QueryInterface will call AddRef on the interface before
   --  returning it in ppvObject
   --  If riid is not found it dispatches to the CoClass version of
   --  QueryInterface

   function QueryInterface
     (Dispatch  : in     CoClass_Type;
      This      : access COM_Interface_Type;
      riid      : in     GNATCOM.Types.Pointer_To_GUID;
      ppvObject : access GNATCOM.Types.Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   --  QueryInterface extension
   --  Dispatched if no matching interface is found in the classes IID_Map
   --  by the internal QueryInterface implementation.
   --  The default implementation returns E_NOINTERFACE as should any
   --  version of QueryInterface that does not handle interface riid

   --  Function specs for IUnknown
   --
   --  The QueryInterface, Addref and Release are the first three functions
   --  in every interface since every interface is a child of IUnknown. This
   --  allows every interface to be treated polymorphically as
   --  IUnknown. Then QueryInterface can be used to query the object for
   --  support of an interface and retrieve a pointer to the COM object's
   --  interface that conforms to the requested interface.  In essence
   --  QueryInterface is used to do a typesafe "cast" from one interface of
   --  a COM object to another.

   function AddRef (This : access COM_Interface_Type)
     return Interfaces.C.unsigned_long;
   pragma Convention (Stdcall, AddRef);
   --  Add a reference count to the object

   function Release (This : access COM_Interface_Type)
     return Interfaces.C.unsigned_long;
   pragma Convention (Stdcall, Release);
   --  Reduce reference count. If 0 free object from memory

   procedure Release (This : access COM_Interface_Type);
   --  This procedure can be used to release user created objects
   --  such as those created by GNATCOM.Events.Event_Object.Create

   type IUnknown_Vtbl_Record is
      record
         QueryInterface : GNATCOM.Create.COM_Interface.Af_QueryInterface :=
           GNATCOM.Create.COM_Interface.QueryInterface'Access;
         AddRef         : GNATCOM.Create.COM_Interface.Af_AddRef :=
           GNATCOM.Create.COM_Interface.AddRef'Access;
         Release        : GNATCOM.Create.COM_Interface.Af_Release :=
           GNATCOM.Create.COM_Interface.Release'Access;
      end record;
   pragma Convention (C_PASS_BY_COPY, IUnknown_Vtbl_Record);
   type Pointer_To_IUnknown_Vtbl is access all IUnknown_Vtbl_Record;
   --  IUknown virtual function table

   IUnknown_Vtbl : aliased IUnknown_Vtbl_Record;

   type COM_Interface_Type is
      record
         Vtbl           : System.Address := IUnknown_Vtbl'Address;
         CoClass        : Pointer_To_CoClass;
         Ref_Count      : aliased Interfaces.C.long := 1;
      end record;
   pragma Convention (C_PASS_BY_COPY, COM_Interface_Type);
   --  The COM_Interface_Type is constructed so that the first element
   --  in the record is a pointer to the table of functions and
   --  additional members are private data elements of the
   --  COM_Interface_Type unavailable to clients of the COM object. The
   --  CoClass element holds a pointer to the object wide data elements
   --  of the COM object.

   function Create_Object (Class_Object : Pointer_To_CoClass) return
     Pointer_To_COM_Interface_Type;
   --  Creates the working COM object and returns the COM_Interface_Type
   --  for the IUnknown interface to the object

end GNATCOM.Create.COM_Interface;
