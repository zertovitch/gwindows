------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                           C O M _ S C O P E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $
--                                                                          --
--                  Copyright (C) 1999-2004 David Botton                    --
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
-- More information about GNATCOM and the most current version can          --
-- be located on the web at http://www.gnavi.org/gnatcom                    --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.IO;
with Interfaces.C;
with Ada.Strings.Unbounded;

with GNATCOM.Types;
with GNATCOM.ITypeLib_Interface;
with GNATCOM.ITypeInfo_Interface;
with GNATCOM.BSTR;
with GNATCOM.VARIANT;
with GNATCOM.GUID;
with GNATCOM.Errors;

package body COM_Scope is

   package BSTR renames GNATCOM.BSTR;
   package VARIANT renames GNATCOM.VARIANT;

   type Pointer_To_Source_Buffer is
     access all Source_Buffer.Source_Buffer_Type;

   Out_Buffer    : Pointer_To_Source_Buffer := null;
   Display_Stdio : constant Boolean := True;

   procedure New_Line;
   procedure Put_Line (S : String);

   procedure New_Line is
      use Source_Buffer;
   begin
      if Out_Buffer /= null then
         New_Line (Out_Buffer.all);
      end if;

      if Display_Stdio then
         GNAT.IO.New_Line;
      end if;
   end New_Line;

   procedure Put_Line (S : String) is
      use Source_Buffer;
   begin
      if Out_Buffer /= null then
         Put_Line (Out_Buffer.all, S);
      end if;

      if Display_Stdio then
         GNAT.IO.Put_Line (S);
      end if;
   end Put_Line;

   function Strip (Image_String : String) return String;
   --  Strips the space prefix off an Type'Image

   procedure Library_Information
     (Lib : GNATCOM.ITypeLib_Interface.ITypeLib_Type);
   --  Outputs library information

   procedure Element_Information
     (Lib   : GNATCOM.ITypeLib_Interface.ITypeLib_Type;
      Index : Interfaces.C.int);
   --  Outputs element information

   function Element_Kind (Kind : GNATCOM.Types.TYPEKIND) return String;
   --  Returns the TYPEKIND as a string

   function Variable_Kind (Kind : GNATCOM.Types.VARKIND) return String;
   --  Returns the VARKIND as a string

   function Function_Kind (Kind : GNATCOM.Types.FUNCKIND) return String;
   --  Returns the FUNCKIND as a string

   function Invoke_Kind (Kind : GNATCOM.Types.INVOKEKIND) return String;
   --  Returns the INVOKEKIND as a string

   function Call_Kind (Kind : GNATCOM.Types.CALLCONV) return String;
   --  Returns the CALLCONV as a string

   function Variant_Type (vt : GNATCOM.Types.VARTYPE) return String;
   --  Returns a VARTYPE as a string

   function Impl_Type (Flags : Interfaces.C.unsigned) return String;
   --  Returns a IMPLTYPEFLAG as a String

   function Type_Kind
     (Type_Desc : GNATCOM.Types.TYPEDESC;
      Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type)
     return String;
   --  Returns a TYPEDESC as a string

   -- Write --

   procedure Write (File_Name : in     String;
                    To_Buffer : in out  Source_Buffer.Source_Buffer_Type)
   is
      use GNATCOM.ITypeLib_Interface;
      use type Interfaces.C.unsigned;

      Lib        : ITypeLib_Type;
   begin
      Out_Buffer := To_Buffer'Unchecked_Access;

      Open (Lib, File_Name);
      Library_Information (Lib);
      New_Line;

      for N in 0 .. (GetTypeInfoCount (Lib) - 1) loop
         Element_Information (Lib, Interfaces.C.int (N));
         New_Line;
      end loop;
   end Write;

   -- Display --

   procedure Display (File_Name : String) is
      use GNATCOM.ITypeLib_Interface;
      use type Interfaces.C.unsigned;

      Lib        : ITypeLib_Type;
   begin
      Open (Lib, File_Name);
      Library_Information (Lib);
      New_Line;

      for N in 0 .. (GetTypeInfoCount (Lib) - 1) loop
         Element_Information (Lib, Interfaces.C.int (N));
         New_Line;
      end loop;

   end Display;

   -- Library_Information --

   procedure Library_Information
     (Lib : GNATCOM.ITypeLib_Interface.ITypeLib_Type)
   is
      use GNATCOM.ITypeLib_Interface;
      use type Interfaces.C.int;

      Name       : aliased GNATCOM.Types.BSTR;
      Doc_String : aliased GNATCOM.Types.BSTR;
      Attribs    : GNATCOM.Types.Pointer_To_TLIBATTR;
   begin
      GetDocumentation (Lib,
                        -1,
                        Name'Unchecked_Access,
                        Doc_String'Unchecked_Access,
                        null,
                        null);

      Put_Line ("Library Name             : " &
                BSTR.To_Ada (Name));
      Put_Line ("Library Documentation    : " &
                BSTR.To_Ada (Doc_String));

      Attribs := GetLibAttr (Lib);
      Put_Line ("Library Version          : " &
                Strip (Attribs.wMajorVerNum'Img) &
                "." &
                Strip (Attribs.wMinorVerNum'Img));

      Put_Line ("Library LIBID            : " &
                GNATCOM.GUID.To_String (Attribs.guid));

      ReleaseTLibAttr (Lib, Attribs);

      Put_Line ("Elements in Type Library : " &
                GetTypeInfoCount (Lib)'Img);

   end Library_Information;

   -- Element_Information --

   procedure Element_Information
     (Lib   : GNATCOM.ITypeLib_Interface.ITypeLib_Type;
      Index : Interfaces.C.int)
   is
      use GNATCOM.ITypeLib_Interface;
      use GNATCOM.ITypeInfo_Interface;
      use type Interfaces.C.unsigned_short;
      use type Interfaces.C.short;
      use type Interfaces.C.long;

      Name       : aliased GNATCOM.Types.BSTR;
      Doc_String : aliased GNATCOM.Types.BSTR;
      Info       : ITypeInfo_Type;
      Attribs    : GNATCOM.Types.Pointer_To_TYPEATTR;
   begin
      GetDocumentation (Lib,
                        Index,
                        Name'Unchecked_Access,
                        Doc_String'Unchecked_Access,
                        null,
                        null);

      Put_Line ("   Element Name             : " &
               BSTR.To_Ada (Name));
      Put_Line ("   Element Documentation    : " &
               BSTR.To_Ada (Doc_String));

      Put_Line ("   Element Kind             : " &
                Element_Kind (GetTypeInfoType (Lib,
                                               Index)));

      Attach (Info, GetTypeInfo (Lib, Index));

      Attribs := GetTypeAttr (Info);

      if (Attribs.wTypeFlags and GNATCOM.Types.TYPEFLAG_FDUAL) /= 0 then
         Put_Line ("   Element Interface        : Dual Inteface");
      end if;

      Put_Line ("   Element CLSID            : " &
               GNATCOM.GUID.To_String (Attribs.guid));
      Put_Line ("   Element # of Functions   : " &
               Strip (Attribs.cFuncs'Img));
      Put_Line ("   Element # of Variables   : " &
               Strip (Attribs.cVars'Img));
      Put_Line ("   Element # of Interfaces  : " &
               Strip (Attribs.cImplTypes'Img));
      Put_Line ("   Element Size (bytes)     : " &
               Strip (Attribs.cbSizeInstance'Img));
      Put_Line ("   Element Alignment        : " &
               Strip (Attribs.cbAlignment'Img));

      if Attribs.cImplTypes > 0 then
         for N in 0 .. Attribs.cImplTypes - 1 loop
            declare
               Ref_Info   : ITypeInfo_Type;
               Ref_Lib    : ITypeLib_Type;
               Ref_Index  : aliased Interfaces.C.int;
               Ref_Name   : aliased GNATCOM.Types.BSTR;
            begin

               Attach (Ref_Info, GetRefTypeInfo
                       (Info,
                        GetRefTypeOfImplType (Info,
                                              Interfaces.C.int (N))));

               Attach (Ref_Lib, GetContainingTypeLib
                       (Ref_Info,
                        Ref_Index'Unchecked_Access));

               GetDocumentation (Ref_Lib,
                                 Ref_Index,
                                 Ref_Name'Unchecked_Access,
                                 null,
                                 null,
                                 null);

               Put_Line ("      Element Reference Interface # : " &
                         Strip (Integer (N + 1)'Img));
               Put_Line ("      Element Reference Name        : " &
                         BSTR.To_Ada (Ref_Name));
               Put_Line ("      Element Implementation Type   : " &
                         Impl_Type (GetImplTypeFlags (Info,
                                                      Interfaces.C.int (N))));
            end;
         end loop;
      end if;

      for N in  0 .. Attribs.cVars - 1 loop
         declare
            Desc     : constant GNATCOM.Types.Pointer_To_VARDESC :=
              GetVarDesc (Info, Interfaces.C.int (N));
            Var_Name : aliased GNATCOM.Types.BSTR;
            Var_Desc : aliased GNATCOM.Types.BSTR;
         begin
            Put_Line ("      Variable #             : " &
                      Strip (N'Img));
            Put_Line ("      Variable DispID        : " &
                      Strip (Desc.memid'Img));

            GetDocumentation (Info,
                              Desc.memid,
                              Var_Name'Unchecked_Access,
                              Var_Desc'Unchecked_Access,
                              null,
                              null);

            Put_Line ("      Variable Name          : " &
                      BSTR.To_Ada (Var_Name));
            Put_Line ("      Variable Documentation : " &
                      BSTR.To_Ada (Var_Desc));
            Put_Line ("      Variable Kind          : " &
                      Variable_Kind (Desc.varkind));
            Put_Line ("      Variable Type          : " &
                      Type_Kind (Desc.elemdescVar.tdesc, Info));

            if (Desc.wVarFlags and GNATCOM.Types.VARFLAG_FREADONLY) /= 0 then
               Put_Line ("      Variable Read Only     : True");
            end if;

            if Desc.varkind = GNATCOM.Types.VAR_CONST then
               Put_Line ("      Variable Value         : " &
                         VARIANT.To_Ada (Desc.u.lpvarValue.all, False));
            else
               Put_Line ("      Variable Offset        : " &
                         Desc.u.oInst'Img);
            end if;

            ReleaseVarDesc (Info, Desc);

            New_Line;
         end;
      end loop;

      for N in  0 .. Attribs.cFuncs - 1 loop
         declare
            Desc      : constant GNATCOM.Types.Pointer_To_FUNCDESC :=
              GetFuncDesc (Info, Interfaces.C.int (N));
            Func_Name : aliased GNATCOM.Types.BSTR;
            Func_Desc : aliased GNATCOM.Types.BSTR;

            Param_Names   : aliased GNATCOM.Types.BSTR_PARAM_ARRAY;
            cNames        : aliased Interfaces.C.unsigned;
         begin
            Put_Line ("      Function #             : " &
                      Strip (N'Img));
            Put_Line ("      Function DispID        : " &
                      Strip (Desc.memid'Img));

            GetDocumentation (Info,
                              Desc.memid,
                              Func_Name'Unchecked_Access,
                              Func_Desc'Unchecked_Access,
                              null,
                              null);

            Put_Line ("      Function Name          : " &
                      BSTR.To_Ada (Func_Name));
            Put_Line ("      Function Documentation : " &
                      BSTR.To_Ada (Func_Desc));
            Put_Line ("      Function Kind          : " &
                      Function_Kind (Desc.funckind));
            Put_Line ("      Function invoke kind   : " &
                     Invoke_Kind (Desc.invkind));
            Put_Line ("      Function convention    : " &
                     Call_Kind (Desc.callconv));
            Put_Line ("      Function Return Type   : " &
                      Type_Kind (Desc.elemdescFunc.tdesc, Info));

            declare
               Dll_Name  : aliased GNATCOM.Types.BSTR;
               Dll_Entry : aliased GNATCOM.Types.BSTR;
               Ordinal   : aliased Interfaces.C.short;
            begin
               GetDllEntry (Info,
                            Desc.memid,
                            Desc.invkind,
                            Dll_Name'Unchecked_Access,
                            Dll_Entry'Unchecked_Access,
                            Ordinal'Unchecked_Access);

               Put_Line ("      Function in DLL        : " &
                         BSTR.To_Ada (Dll_Name));
               Put_Line ("      Function name in Dll   : " &
                         BSTR.To_Ada (Dll_Entry));
               Put_Line ("      Function ordinal       : " &
                         Strip (Ordinal'Img));
            exception
               when GNATCOM.Errors.COM_ERROR =>
                  null; -- Function is not contained in a DLL
            end;

            GetNames (Info,
                      Desc.memid,
                      Param_Names'Unchecked_Access,
                      GNATCOM.Types.MAX_PARAMS,
                      cNames'Unchecked_Access);

            for P in 0 .. Desc.cParams - 1 loop
               Put_Line ("         Parameter #         : " &
                         Strip (P'Img));
               Put_Line ("         Parameter Name      : " &
                         BSTR.To_Ada (Param_Names (P + 1)));
               Put_Line ("         Parameter Type      : " &
                         Type_Kind (Desc.lprgelemdescParam (P).tdesc,
                                    Info));

               if (Desc.lprgelemdescParam (P).paramdesc.wParamFlags
                   and GNATCOM.Types.PARAMFLAG_FOPT) /= 0
               then
                  Put_Line ("         Parameter Optional  : True");
               end if;

               if (Desc.lprgelemdescParam (P).paramdesc.wParamFlags
                   and GNATCOM.Types.PARAMFLAG_FRETVAL) /= 0
               then
                  Put_Line ("         Parameter is RetVal : True");
               end if;
            end loop;

            ReleaseFuncDesc (Info, Desc);

            New_Line;
         end;
      end loop;

      ReleaseTypeAttr (Info, Attribs);

   end Element_Information;

   -- Element_Kind --

   function Element_Kind (Kind : GNATCOM.Types.TYPEKIND) return String is
      use GNATCOM.Types;
   begin
      case Kind is
         when TKIND_ENUM =>
            return "Enumeration";
         when TKIND_RECORD =>
            return "Record";
         when TKIND_MODULE =>
            return "Module";
         when TKIND_INTERFACE =>
            return "Interface";
         when TKIND_DISPATCH =>
            return "Dispatch";
         when TKIND_COCLASS =>
            return "CoClass";
         when TKIND_ALIAS =>
            return "Alias";
         when TKIND_UNION =>
            return "Union";
         when others =>
            return "ERROR";
      end case;
   end Element_Kind;

   -- Variable_Kind --

   function Variable_Kind (Kind : GNATCOM.Types.VARKIND) return String is
      use GNATCOM.Types;
   begin
      case Kind is
         when VAR_PERINSTANCE =>
            return "Perinstance";
         when VAR_STATIC =>
            return "Static";
         when VAR_CONST =>
            return "Constant";
         when VAR_DISPATCH =>
            return "Dispatch";
         when others =>
            return "Error";
      end case;
   end Variable_Kind;

   -- Function_Kind --

   function Function_Kind (Kind : GNATCOM.Types.FUNCKIND) return String is
      use GNATCOM.Types;
   begin
      case Kind is
         when FUNC_VIRTUAL =>
            return "Virtual";
         when FUNC_PUREVIRTUAL =>
            return "Pure Virtual";
         when FUNC_NONVIRTUAL =>
            return "Non Virtual";
         when FUNC_STATIC =>
            return "Static";
         when FUNC_DISPATCH =>
            return "Dispatch";
         when others =>
            return "Error";
      end case;
   end Function_Kind;

   -- Invoke_Kind --

   function Invoke_Kind (Kind : GNATCOM.Types.INVOKEKIND) return String is
      use GNATCOM.Types;
   begin
      case Kind is
         when INVOKE_FUNC =>
            return "Function";
         when INVOKE_PROPERTYGET =>
            return "Property Get";
         when INVOKE_PROPERTYPUT =>
            return "Property Put";
         when INVOKE_PROPERTYPUTREF =>
            return "Property Put Ref";
         when others =>
            return "Error";
      end case;
   end Invoke_Kind;

   -- Call_Kind --

   function Call_Kind (Kind : GNATCOM.Types.CALLCONV) return String is
      use GNATCOM.Types;
   begin
      case Kind is
         when CC_FASTCALL =>
            return "Fast Call";
         when CC_CDECL =>
            return "C";
         when CC_PASCAL =>
            return "Pascal";
         when CC_STDCALL =>
            return "Stdcall";
         when CC_FPFASTCALL =>
            return "FP Fast Call";
         when CC_SYSCALL =>
            return "SYS Call";
         when others =>
            return "Error";
      end case;
   end Call_Kind;

   -- Variant_Type --

   function Variant_Type (vt : GNATCOM.Types.VARTYPE) return String is
      use GNATCOM.Types;
   begin
      case vt is
         when VT_EMPTY =>
            return "Empty";
         when VT_NULL =>
            return "Null";
         when VT_I2 =>
            return "16 bit Intger";
         when VT_I4 =>
            return "32 bit Integer";
         when VT_R4 =>
            return "Float";
         when VT_R8 =>
            return "Double";
         when VT_CY =>
            return "Currency";
         when VT_DATE =>
            return "Date";
         when VT_BSTR =>
            return "BSTR";
         when VT_DISPATCH =>
            return "Pointer To IDispatch";
         when VT_ERROR =>
            return "SCODE";
         when VT_BOOL =>
            return "Variant BOOL";
         when VT_VARIANT =>
            return "Variant";
         when VT_UNKNOWN =>
            return "Pointer to IUnknown";
         when VT_I1 =>
            return "8 bit Integer";
         when VT_UI1 =>
            return "8 bit Unsigned Integer";
         when VT_UI2 =>
            return "16 bit Unsigned Integer";
         when VT_UI4 =>
            return "32 bit Unsigned Integer";
         when VT_I8 =>
            return "64 bit Integer";
         when VT_UI8 =>
            return "64 bit Unsigned Integer";
         when VT_INT =>
            return "C int";
         when VT_UINT =>
            return "C unsigned int";
         when VT_VOID =>
            return "void";
         when VT_HRESULT =>
            return "HRESULT";
         when VT_PTR =>
            return "Pointer";
         when VT_SAFEARRAY =>
            return "SafeArray";
         when VT_CARRAY =>
            return "C Array";
         when VT_USERDEFINED =>
            return "User Defined";
         when VT_LPSTR =>
            return "C String";
         when VT_LPWSTR =>
            return "C Wide String";
         when VT_FILETIME =>
            return "File Time";
         when VT_BLOB =>
            return "BLOB";
         when VT_STREAM =>
            return "Stream";
         when VT_STORAGE =>
            return "Storage";
         when VT_STREAMED_OBJECT =>
            return "Streamed Object";
         when VT_STORED_OBJECT =>
            return "Stored Object";
         when VT_BLOB_OBJECT =>
            return "BLOB Object";
         when VT_CF =>
            return "CF";
         when VT_CLSID =>
            return "CLSID";
         when others =>
            return "Other" & vt'Img;
      end case;
   end Variant_Type;

   -- Impl_Type --

   function Impl_Type (Flags : Interfaces.C.unsigned) return String
   is
      use GNATCOM.Types;
      use Ada.Strings.Unbounded;
      use type Interfaces.C.unsigned;

      IStr : Unbounded_String;
   begin

      if (Flags and IMPLTYPEFLAG_FDEFAULT) /= 0 then
         Append (IStr, "Default ");
      end if;

      if (Flags and IMPLTYPEFLAG_FSOURCE) /= 0 then
         Append (IStr, "Source ");
      end if;

      if (Flags and IMPLTYPEFLAG_FRESTRICTED) /= 0 then
         Append (IStr, "Restricted ");
      end if;

      if (Flags and IMPLTYPEFLAG_FDEFAULTVTBL) /= 0 then
         Append (IStr, "Sink through VTBL");
      end if;

      return To_String (IStr);
   end Impl_Type;

   -- Type_Kind --

   function Type_Kind
     (Type_Desc : GNATCOM.Types.TYPEDESC;
      Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type)
     return String
   is
      use Ada.Strings.Unbounded;
      use GNATCOM.Types;
      use GNATCOM.ITypeInfo_Interface;
      use GNATCOM.ITypeLib_Interface;

      use type Interfaces.C.unsigned_short;
      use type Interfaces.C.short;
   begin
      if Type_Desc.vt = VT_PTR then
         return "Pointer to " &
           Type_Kind (Type_Desc.u.lptdesc.all, Type_Info);
      end if;

      if Type_Desc.vt = VT_CARRAY then
         declare
            Desc   : constant GNATCOM.Types.Pointer_To_ARRAYDESC :=
              Type_Desc.u.lpadesc;
            Dims   : Unbounded_String;
         begin
            for N in 0 .. Desc.cDims - 1 loop
               if N > 0 then
                  Append (Dims, ",");
               end if;

               Append (Dims, Desc.rgbounds (N).lLbound'Img &
                       " .." &
                       Integer'Image
                       (Integer (Desc.rgbounds (N).cElements) +
                        Integer (Desc.rgbounds (N).lLbound)));
            end loop;

            return "Array " &
              To_String (Dims) &
             " of " &
              Type_Kind (Desc.tdescElem,
                         Type_Info);
         end;
      end if;

      if Type_Desc.vt = VT_USERDEFINED then
            declare
               Ref_Info   : ITypeInfo_Type;
               Ref_Lib    : ITypeLib_Type;
               Ref_Index  : aliased Interfaces.C.int;
               Ref_Name   : aliased GNATCOM.Types.BSTR;
            begin

               Attach (Ref_Info, GetRefTypeInfo (Type_Info,
                                                 Type_Desc.u.hreftype));

               Attach (Ref_Lib, GetContainingTypeLib
                       (Ref_Info,
                        Ref_Index'Unchecked_Access));

               GetDocumentation (Ref_Lib,
                                 Ref_Index,
                                 Ref_Name'Unchecked_Access,
                                 null,
                                 null,
                                 null);
               return BSTR.To_Ada (Ref_Name);
            end;
      end if;

      return Variant_Type (Type_Desc.vt);
   end Type_Kind;

   -- Strip --

   function Strip (Image_String : String) return String
   is
   begin
      return Image_String (Image_String'First + 1 .. Image_String'Last);
   end Strip;

end COM_Scope;
