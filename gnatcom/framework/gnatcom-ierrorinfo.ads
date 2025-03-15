------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                   G N A T C O M . I E R R O R I N F O                    --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Strings.Unbounded;
with GNATCOM.Types;
with GNATCOM.Iinterface;

package GNATCOM.IErrorInfo is

   procedure Create_IErrorInfo
     (Description     : in String;
      Source_PROGID   : in String              := "";
      Associated_GUID : in GNATCOM.Types.GUID  := GNATCOM.Types.GUID_NULL;
      Help_Context    : in GNATCOM.Types.DWORD := 0;
      Help_File_Path  : in String              := "");
   --  Creates and associates and Error object with this method invocation
   --  and thread
   --
   --  It is used in COM servers to create a COM error that can
   --  then be retrieved through Get_IErrorInfo or the Win32 API
   --  function GetErrorInfo when a COM call has failed.
   --
   --  To support IErrorInfo in your COM server each object that will
   --  be generating IErrorInfos for failure situations should include
   --  the interface ISupportErrorInfo. The InterfaceSupportsErrorInfo
   --  method should be implemented to return S_OK for each interface
   --  passed to it that will be creating IErrorInfos and S_FALSE for
   --  those not creating IErrorInfos

   function Supports_IErrorInfo
     (Object : in GNATCOM.Iinterface.Interface_Type'Class)
     return Boolean;
   --  Checks to see if IErrorInfos will be generated for this interface
   --  on the object. It does a Query for ISupportErrorInfo and then
   --  executes the InterfaceSupportsErrorInfo passing in the IID assigned
   --  to Object. This should be run before doing a Get_IErrorInfo

   function Get_IErrorInfo return String;
   --  Return just the description field of the IErrorInfo object of the
   --  last COM method error posted to this calling thread
   --
   --  Note: When using IDispatch based bindings (*_Object.ads) or the
   --        GNATCOM.Dispinterface package, IErrorInfos thrown will
   --        be unavailable through Get_IErrorInfo since they will
   --        have been captured by the OS. The Source_PROGID and the
   --        Description fields will be returned as part of the
   --        GNATCOM.Dispinterface.Invoke_Error error message

   procedure Get_IErrorInfo
     (Description     : out Ada.Strings.Unbounded.Unbounded_String;
      Source_PROGID   : out Ada.Strings.Unbounded.Unbounded_String;
      Associated_GUID : out GNATCOM.Types.GUID;
      Help_Context    : out GNATCOM.Types.DWORD;
      Help_File_Path  : out Ada.Strings.Unbounded.Unbounded_String);
   --  Return the values of the IErrorInfo of the last COM method error
   --  posted to this calling thread
   --
   --  Note: When using IDispatch based bindings (*_Object.ads) or the
   --        GNATCOM.Dispinterface package, IErrorInfos thrown will
   --        be unavailable through Get_IErrorInfo since they will
   --        have been captured by the OS. The Source_PROGID and the
   --        Description fields will be returned as part of the
   --        GNATCOM.Dispinterface.Invoke_Error error message

end GNATCOM.IErrorInfo;
