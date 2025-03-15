------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--         G N A T C O M . E V E N T S . E V E N T _ O B J E C T            --
--                                                                          --
--                                B o d y                                   --
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

package body GNATCOM.Events.Event_Object is

   function Event_GetTypeInfoCount
     (This    : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pctinfo : in     GNATCOM.Types.Pointer_To_unsigned)
     return GNATCOM.Types.HRESULT;

   function Event_GetTypeInfo
     (This    : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      itinfo  : in     Interfaces.C.unsigned;
      lcid    : in     Interfaces.C.unsigned_long;
      pptinfo : in     GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;

   function Event_GetIDsOfNames
     (This      : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      riid      : in     GNATCOM.Types.Pointer_To_GUID;
      rgszNames : in     GNATCOM.Types.Pointer_To_Pointer_To_char;
      cNames    : in     Interfaces.C.unsigned;
      lcid      : in     Interfaces.C.unsigned_long;
      rgdispid  : in     GNATCOM.Types.Pointer_To_long)
     return GNATCOM.Types.HRESULT;

   function Event_Invoke
     (This         : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      dispidMember : in     Interfaces.C.long;
      riid         : in     GNATCOM.Types.Pointer_To_GUID;
      lcid         : in     Interfaces.C.unsigned_long;
      wFlags       : in     Interfaces.C.unsigned_short;
      pdispparams  : in     GNATCOM.Types.Pointer_To_DISPPARAMS;
      pvarResult   : in     GNATCOM.Types.Pointer_To_VARIANT;
      pexcepinfo   : in     GNATCOM.Types.Pointer_To_EXCEPINFO;
      puArgErr     : in     GNATCOM.Types.Pointer_To_unsigned)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, Event_Invoke);

   type Event_Vtbl_Record is
      record
         IUnknown         : GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         GetTypeInfoCount : System.Address := Event_GetTypeInfoCount'Address;
         GetTypeInfo      : System.Address := Event_GetTypeInfo'Address;
         GetIDsOfNames    : System.Address := Event_GetIDsOfNames'Address;
         Invoke           : System.Address := Event_Invoke'Address;
      end record;
   pragma Convention (C_Pass_By_Copy, Event_Vtbl_Record);

   Event_Vtbl : aliased Event_Vtbl_Record;

   Event_Map : aliased GNATCOM.Create.COM_Interface.GUID_Record_Array :=
     (1 => (IID  => GNATCOM.Types.IID_IDispatch, -- Gets replaced with event's
            Vtbl => Event_Vtbl'Address),         -- IID
      2 => (IID => GNATCOM.Types.IID_IDispatch,
            Vtbl => Event_Vtbl'Address));

   type Event_Class is
     new GNATCOM.Create.COM_Interface.CoClass_Type (Event_Map'Access) with
      record
         Event_Invoke : Invoke_Function;
         Event_Object : Event_Pointer;
      end record;
   type Event_Class_Pointer is access all Event_Class;

   ------------
   -- Create --
   ------------

   function Create
     (Invoke       : Invoke_Function;
      Event_IID    : GNATCOM.Types.GUID;
      Event_Object : Event_Pointer      := null)
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type
   is
      Object  : Event_Class_Pointer := new Event_Class;

      Event_Interface :
        GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type :=
        new GNATCOM.Create.COM_Interface.COM_Interface_Type;

   begin
      Event_Interface.Vtbl := Event_Vtbl'Address;
      Event_Interface.Ref_Count := 1;
      Event_Interface.CoClass :=
        GNATCOM.Create.COM_Interface.Pointer_To_CoClass (Object);

      Object.IID_Map (1).IID := Event_IID;
      Object.IUnknown := Event_Interface;
      Object.Event_Invoke := Invoke;
      Object.Event_Object := Event_Object;
      return Event_Interface;
   end Create;

   ------------------
   -- Event_Invoke --
   ------------------

   function Event_Invoke
     (This         : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      dispidMember : in     Interfaces.C.long;
      riid         : in     GNATCOM.Types.Pointer_To_GUID;
      lcid         : in     Interfaces.C.unsigned_long;
      wFlags       : in     Interfaces.C.unsigned_short;
      pdispparams  : in     GNATCOM.Types.Pointer_To_DISPPARAMS;
      pvarResult   : in     GNATCOM.Types.Pointer_To_VARIANT;
      pexcepinfo   : in     GNATCOM.Types.Pointer_To_EXCEPINFO;
      puArgErr     : in     GNATCOM.Types.Pointer_To_unsigned)
     return GNATCOM.Types.HRESULT
   is
      pragma Warnings (Off, riid);
      pragma Warnings (Off, lcid);
      pragma Warnings (Off, pvarResult);
      pragma Warnings (Off, pexcepinfo);
      pragma Warnings (Off, puArgErr);

      Object  : constant Event_Class_Pointer :=
        Event_Class_Pointer (This.CoClass);
   begin
      Object.Event_Invoke (dispidMember,
                           wFlags,
                           pdispparams,
                           Object.Event_Object);
      return GNATCOM.S_OK;
   end Event_Invoke;

   ----------------------------
   -- Event_GetTypeInfoCount --
   ----------------------------

   function Event_GetTypeInfoCount
     (This    : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pctinfo : in     GNATCOM.Types.Pointer_To_unsigned)
     return GNATCOM.Types.HRESULT
   is
      pragma Warnings (Off, This);
      pragma Warnings (Off, pctinfo);
   begin
      return E_NOTIMPL;
   end Event_GetTypeInfoCount;

   -----------------------
   -- Event_GetTypeInfo --
   -----------------------

   function Event_GetTypeInfo
     (This    : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      itinfo  : in     Interfaces.C.unsigned;
      lcid    : in     Interfaces.C.unsigned_long;
      pptinfo : in     GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT
   is
      pragma Warnings (Off, This);
      pragma Warnings (Off, itinfo);
      pragma Warnings (Off, lcid);
      pragma Warnings (Off, pptinfo);
   begin
      return E_NOTIMPL;
   end Event_GetTypeInfo;

   -------------------------
   -- Event_GetIDsOfNames --
   -------------------------

   function Event_GetIDsOfNames
     (This      : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      riid      : in     GNATCOM.Types.Pointer_To_GUID;
      rgszNames : in     GNATCOM.Types.Pointer_To_Pointer_To_char;
      cNames    : in     Interfaces.C.unsigned;
      lcid      : in     Interfaces.C.unsigned_long;
      rgdispid  : in     GNATCOM.Types.Pointer_To_long)
     return GNATCOM.Types.HRESULT
   is
      pragma Warnings (Off, This);
      pragma Warnings (Off, riid);
      pragma Warnings (Off, rgszNames);
      pragma Warnings (Off, cNames);
      pragma Warnings (Off, lcid);
      pragma Warnings (Off, rgdispid);
   begin
      return E_NOTIMPL;
   end Event_GetIDsOfNames;

end GNATCOM.Events.Event_Object;
