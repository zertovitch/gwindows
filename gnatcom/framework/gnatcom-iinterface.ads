------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                    G N A T C O M . I N T E R F A C E                     --
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

--  The Interface_Type encapsulates the functionality of IUnknown creating a
--  thick binding to COM Interfaces and Objects

--  One of the COM initialization methods must be called first before
--  using any COM Interface wrappers

with Ada.Finalization;
with Ada.Unchecked_Conversion;
with System;
with Interfaces.C;

with GNATCOM.Types;

pragma Elaborate_All (GNATCOM);

package GNATCOM.Iinterface is

   type Interface_Type is new Ada.Finalization.Controlled with private;

   procedure Create
     (This        : in out Interface_Type;
      From        : in     GNATCOM.Types.GUID;
      Server_Type : in     GNATCOM.Types.CLSCTX := GNATCOM.Types.CLSCTX_ALL);
   --  Creates new COM object using a CLSID

   procedure Create
     (This        : in out Interface_Type;
      From        : in     String;
      Server_Type : in     GNATCOM.Types.CLSCTX := GNATCOM.Types.CLSCTX_ALL);
   --  Creates new COM object using the COM PROGID

   procedure Create
     (This        : in out Interface_Type;
      From        : in     GNATCOM.Types.GUID;
      Key         : in     GNATCOM.Types.BSTR;
      Free_Key    : in     Boolean              := True;
      Server_Type : in     GNATCOM.Types.CLSCTX := GNATCOM.Types.CLSCTX_ALL);
   --  Creates new COM object using a CLSID and a License Key
   --  If Free_Key is true, the BSTR Key is deallocated

   function Get_Key (Object : in GNATCOM.Types.GUID) return GNATCOM.Types.BSTR;
   --  Gets a licese key if available for the object

   procedure CreateFromMoniker
     (This : in out Interface_Type;
      From : in     String);
   --  Creates a new COM object using a the DisplayName of a Moniker

   procedure CreateRemote
     (This   : in out Interface_Type;
      From   : in     GNATCOM.Types.GUID;
      Server : in     String);
   --  Creates the COM object on a remote machine using a CLSID

   procedure CreateRemote
     (This   : in out Interface_Type;
      From   : in     String;
      Server : in     String);
   --  Creates the COM object on a remote machine using a COM PROGID

   procedure Query
     (This : in out Interface_Type;
      From : in     Interface_Type'class);
   --  Queries and object through any of its interfaces to return and
   --  interface of the IID type set in the Interface_Type

   procedure Query
     (This    : in out Interface_Type;
      From    : in     Interface_Type'Class;
      Success : in out Boolean);
   --  Queries and object through any of its interfaces to return and
   --  interface of the IID type set in the Interface_Type

   procedure Set_IID
     (This : in out Interface_Type;
      IID  : in     GNATCOM.Types.GUID);
   --  Sets the IID to be used when for querying new interfaces

   function IID (This : Interface_Type) return GNATCOM.Types.GUID;
   --  Returns the currently set IID to use when querying new interface

   function Is_Attached (This : Interface_Type) return Boolean;
   --  Returns true if this is attached to a COM Interface

   procedure Attach
     (This : in out Interface_Type;
      From : in     System.Address);
   --  Attaches a COM interface to an Interface_Type.
   --  No Query is performed.

   procedure Attach
     (This : in out Interface_Type;
      From : in     GNATCOM.Types.Pointer_To_IUnknown);
   --  Attaches a COM interface to an Interface_Type.
   --  Performs a Query on the IUnknown to convert it to the curretly set IID

   procedure Attach
     (This : in out Interface_Type;
      From : in     GNATCOM.Types.VARIANT);
   --  Attaches an IUnknown COM interface contained in a VARIANT to an
   --  Interface_Type. Performs a Query on the IUnknown to convert it to
   --  the currently set IID

   function Pointer (This : Interface_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;
   --  Returns the internal interface pointer

   function Address (This : Interface_Type) return System.Address;
   --  Returns the address of the interface pointer

   procedure Free (This : in out Interface_Type);
   --  Calls Release on the attached interface and clears internal
   --  pointers.

   function IsEqual (Left  : in Interface_Type;
                     Right : in Interface_Type'Class)
                    return Boolean;
   --  Compares two Interfaces to see if they are from the same object
   --  this is done by querying each object for IUnknown and then comparing
   --  the interface pointers returned to see if they pointer to the same
   --  address. According to the COM specification all QueryInterfaces for
   --  IUnknown must return the same pointer value.

   type GIT_Cookie is new Interfaces.C.unsigned_long;
   --  Used for holding a reference value to an interface placed in the
   --  Global Interface Table

   function Put_In_GIT (This : Interface_Type) return GIT_Cookie;
   --  Place Interface in Global Interface Table.
   --
   --  Each process that uses COM has an associated table where
   --  interface pointers can be stored. Pointer stored in this table
   --  can be retrieved from any thread regardless of its thread model,
   --  ie. it automaticly marshalls interface pointers accross apartment
   --  boundries.

   procedure Remove_From_GIT (Cookie : in GIT_Cookie);
   --  Remove interface from Global Interface Table

   procedure Attach_From_GIT (This   : in out Interface_Type;
                              Cookie : in     GIT_Cookie);
   --  Attach an interface in the GIT to this type

   procedure Free (This : in GNATCOM.Types.BSTR);
   procedure Free (This : in GNATCOM.Types.VARIANT);
   --  Helper functions for freeing contents of BSTRs and VARIANTs in
   --  thick bindings

   CLASS_NOT_REGISTERED_ERROR : exception;
   --  Raised when an attempt to create a COM object has been performed and
   --  the object has not been registered on the system

   CLASS_NOT_LICENSED_ERROR : exception;
   --  Raised when unable to create object do to license violation or
   --  lack of license key
   --  A valid license key needs to be passed to the create function

   CLASS_NOT_AVAILABLE_ERROR : exception;
   --  Raised when object uncreatable

   INVALID_PROGID_ERROR : exception;
   --  Raised when a request to create a COM object with an invalid PROGID

   SERVER_FILE_NOT_FOUND_ERROR : exception;
   --  An attempt was made to load the COM server specified in the registry,
   --  but the file was not found

   SERVER_ERROR : exception;
   --  There is something wrong with the COM server either the file has an
   --  invalid image, or it did not properly establish itself as a COM server
   --  with the OS

   --  Wrappers directly to Interface_Type's IUnknown

   procedure Finalize (This : in out Interface_Type);
   procedure Adjust (This : in out Interface_Type);
   --  These procedures insure proper reference counting for IUknown

   procedure AddRef (This : in Interface_Type);
   --  Wrapper for IUknown::AddRef

   procedure Release (This : in Interface_Type);
   --  Wrapper for IUknown::Release
   --  Releases a reference count to the interface, COM objects free
   --  themselves from memory when all outstanding interfaces have had
   --  their reference counts reduced to zero

   function QueryInterface
     (This               : in     Interface_Type;
      IID                : in     GNATCOM.Types.GUID;
      Pointer_To_Pointer : access GNATCOM.Types.Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   --  Wrapper for IUknown::QueryInterface

   function To_Pointer_To_IUnknown is
     new Ada.Unchecked_Conversion
     (System.Address, GNATCOM.Types.Pointer_To_IUnknown);

   function To_VARIANT_From_Interface (From : in Interface_Type)
     return GNATCOM.Types.VARIANT;
   --  Create a VARIANT containing a pointer to the From interface
   --  Calls Addref. VARIANT should be destroyed when no longer needed.

private

   type Interface_Type is new Ada.Finalization.Controlled with
      record
         Interface_Address : aliased System.Address := System.Null_Address;
         IID               : aliased GNATCOM.Types.GUID :=
           GNATCOM.Types.IID_IUnknown;
      end record;

end GNATCOM.Iinterface;
