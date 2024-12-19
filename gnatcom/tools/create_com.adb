------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                           C R E A T E _ C O M                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $                              --
--                                                                          --
--                  Copyright (C) 1999-2014 David Botton                    --
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Calendar;
with Interfaces.C;

with GNAT.Case_Util; use GNAT.Case_Util;
with GNAT.HTable; use GNAT.HTable;

with GNATCOM.Types; use GNATCOM.Types;
with GNATCOM.ITypeLib_Interface; use GNATCOM.ITypeLib_Interface;
with GNATCOM.ITypeInfo_Interface; use GNATCOM.ITypeInfo_Interface;
with GNATCOM.BSTR;
with GNATCOM.VARIANT;
with GNATCOM.GUID;

with Source_Buffer; use Source_Buffer;

package body Create_COM is

   package BSTR renames GNATCOM.BSTR;
   package VARIANT renames GNATCOM.VARIANT;

   Base_Package   : Unbounded_String;
   Base_Spec_File : File_Type;
   Lib            : GNATCOM.ITypeLib_Interface.ITypeLib_Type;
   Library_Name   : aliased GNATCOM.Types.BSTR;
   Type_Buffer    : Source_Buffer_Type;
   Spec_Buffer    : Source_Buffer_Type;
   Pointer_Buffer : Source_Buffer_Type;
   Alias_Buffer   : Source_Buffer_Type;
   Event_Buffer   : Source_Buffer_Type;
   With_Buffer    : Source_Buffer_Type;
   Inproc_Buffer  : Source_Buffer_Type;
   Local_Buffer   : Source_Buffer_Type;
   Remote_Buffer  : Source_Buffer_Type;
   Class_Counter  : Positive := 1;

   --  Set up Type Map package

   subtype Map_Range is Positive range 1 .. 2000;

   function String_Hash is new Hash (Map_Range);

   function Unbounded_String_Hash (F : Unbounded_String) return Map_Range;
   function String_Equal (F1, F2 : Unbounded_String) return Boolean;

   package Type_Map is new Simple_HTable
     (Header_Num => Map_Range,
      Element    => Boolean,
      No_Element => False,
      Key        => Unbounded_String,
      Hash       => Unbounded_String_Hash,
      Equal      => String_Equal);

   use Type_Map;

   package Pred_Map is new Simple_HTable
     (Header_Num => Map_Range,
      Element    => Boolean,
      No_Element => False,
      Key        => Unbounded_String,
      Hash       => Unbounded_String_Hash,
      Equal      => String_Equal);

   package Size_Map is new Simple_HTable
     (Header_Num => Map_Range,
      Element    => Unbounded_String,
      No_Element => To_Unbounded_String (""),
      Key        => Unbounded_String,
      Hash       => Unbounded_String_Hash,
      Equal      => String_Equal);

   function Strip (Image_String : String) return String;
   --  Strips the space prefix off an Type'Image

   procedure Output_Header;
   --  Inserts type library information in to header of base spec

   procedure Output_Withs;
   --  Inserts withs in to base spec

   procedure Describe_Element
     (Type_Info : in     GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      Buffer    : in out Source_Buffer_Type);
   --  Outputs a description of the element to Buffer

   procedure Bind_Enumeration
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type);
   --  Bind enumeration types

   procedure Bind_Record
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type);
   --  Bind record types

   procedure Bind_Union
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type);
   --  Bind union types

   procedure Bind_Alias
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type);
   --  Bind alias types

   procedure Bind_CoClass
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type);
   --  Bind CoClass types

   procedure Bind_Module
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type);
   --  Bind Modules

   procedure Bind_Interface
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type);
   --  Bind Interface

   procedure Bind_Dispatch
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type);
   --  Bind Dispinterfaces

   procedure Bind_Thin_Elements
     (Type_Info   : in      GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      Bind_Buffer : in out  Source_Buffer_Type;
      Bind_Name   : in      String);
   --  Thin Bind elements

   procedure Bind_Thick_Elements
     (Type_Info   : in      GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      Head_Buffer : in out  Source_Buffer_Type;
      Body_Buffer : in out  Source_Buffer_Type;
      Bind_Name   : in      String;
      Class_Name  : in      String);
   --  Thick Bind elements

   procedure Bind_VtblElements
     (Type_Info   : in     GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      Bind_Buffer : in out Source_Buffer_Type;
      Bind_Name   : in     String;
      Max_Length  : in out Natural);
   --  Binds elements of a Vtbl

   function Type_Kind (Type_Desc : GNATCOM.Types.TYPEDESC;
                       Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
                       Buffer    : access Source_Buffer_Type;
                       Element   : String)
     return String;
   --  Returns the type of the variable as a string

   function Type_Size (Type_Name : String) return String;
   --  Returns a "static" expression of the number of bits used by Type_Size

   function Element_Kind (Kind : GNATCOM.Types.TYPEKIND) return String;
   --  String representation of Element Kind

   function To_Ada_Type (Variant_Type : GNATCOM.Types.VARTYPE) return String;
   --  Translates a VARTYPE to its equivelent Ada Type

   function Is_Reserved_Word (Identifier : String) return Boolean;
   --  Returns true if Identifier is a reserved word in Ada

   function Valid_Identifier (Identifier : String) return String;
   --  Creates a valid Ada Identifier

   function Remove_Base_Package (Identifier : String) return String;
   --  Removes the package specifications on the Identifier if they exist

   function Cify (File_Name : String) return String;
   --  Cifys a File_Name, ie. turns '\'s to '\\'s

   procedure Init_Predef;
   --  Initialize predefined types

   -- Bind --

   procedure Bind
     (File_Name : String;
      Base_Name : String)
   is
      use type Interfaces.C.int;
      use type Interfaces.C.unsigned;

      Attribs             : GNATCOM.Types.Pointer_To_TLIBATTR;
      Base_File_Name      : String := Base_Name;
      Type_Info           : ITypeInfo_Type;
      Element_Name        : aliased GNATCOM.Types.BSTR;
      Work_File           : File_Type;
   begin
      --  Open Library
      Open (Lib, File_Name);

      --  Load predefined types

      Init_Predef;

      --  Create Base Spec File
      Base_Package := To_Unbounded_String (Base_Name);

      To_Lower (Base_File_Name);
      Create (File => Base_Spec_File,
              Mode => Out_File,
              Name => Base_File_Name  & ".ads");

      if Verbose then
         Put_Line ("Openning file           : " & File_Name);
      end if;

      --  Output Library Description in to Spec

      GetDocumentation (Lib,
                        -1,
                        Library_Name'Unchecked_Access,
                        null,
                        null,
                        null);

      if Verbose then
         Put_Line ("Processing Type Library : " & BSTR.To_Ada (Library_Name,
                                                               False));
      end if;

      Output_Header;
      Output_Withs;

      Put_Line (Base_Spec_File, "package " & Base_Name & " is");
      New_Line (Base_Spec_File);

      Attribs := GetLibAttr (Lib);

      Increase_Indent (Spec_Buffer);
      Increase_Indent (Type_Buffer);
      Increase_Indent (Pointer_Buffer);
      Increase_Indent (Alias_Buffer);

      Put_Line (Spec_Buffer, "LIBID_" & BSTR.To_Ada (Library_Name, False) &
                " : aliased GNATCOM.Types.GUID :=");
      Put_Line (Spec_Buffer, "  GNATCOM.GUID.To_GUID (""" &
                GNATCOM.GUID.To_String (Attribs.guid) & """);");
      New_Line (Spec_Buffer);

      ReleaseTLibAttr (Lib, Attribs);

      --  Set up Inproc Server Spec

      Put_Line (Inproc_Buffer, "with Ada.Strings.Unbounded;");
      Put_Line (Inproc_Buffer, "with GNATCOM.Create.Inproc;");
      New_Line (Inproc_Buffer);
      Put_Line (Inproc_Buffer, "package " & Base_Name & ".Dll is");
      Increase_Indent (Inproc_Buffer);
      Put_Line (Inproc_Buffer, "pragma Linker_Options (""" &
                Base_File_Name & "rc.coff"");");
      New_Line (Inproc_Buffer);
      Put_Line (Inproc_Buffer, "procedure Main;");
      New_Line (Inproc_Buffer);
      Put_Line (Inproc_Buffer,
                "Object_Map : aliased " &
                "GNATCOM.Create.Inproc.Factory_Record_Array :=");

      --  Set up Local Server Body

      Put_Line (Local_Buffer, "with Ada.Strings.Unbounded;");
      Put_Line (Local_Buffer, "with GNATCOM.Create.Local_Server;");
      New_Line (Local_Buffer);
      Put_Line (Local_Buffer, "procedure " & Base_Name & ".Exe is");
      Increase_Indent (Local_Buffer);
      Put_Line (Local_Buffer, "pragma Linker_Options (""" &
                Base_File_Name & "rc.coff"");");
      New_Line (Local_Buffer);
      Put_Line (Local_Buffer,
                "Object_Map : aliased " &
                "GNATCOM.Create.Local_Server.Factory_Record_Array :=");

      --  Set up Remote Register Body

      Put_Line (Remote_Buffer, "with Ada.Strings.Unbounded;");
      Put_Line (Remote_Buffer, "with GNATCOM.Create.Remote_Register;");
      New_Line (Remote_Buffer);
      Put_Line (Remote_Buffer, "procedure " & Base_Name & ".Remote is");
      Increase_Indent (Remote_Buffer);
      Put_Line (Remote_Buffer, "pragma Linker_Options (""" &
                Base_File_Name & "rc.coff"");");
      New_Line (Remote_Buffer);
      Put_Line (Remote_Buffer,
                "Object_Map : aliased " &
                "GNATCOM.Create.Remote_Register.Factory_Record_Array :=");

      --  Start Binding

      for Index in 0 .. Interfaces.C.int (GetTypeInfoCount (Lib) - 1) loop
         GetDocumentation (Lib,
                           Index,
                           Element_Name'Unchecked_Access,
                           null,
                           null,
                           null);
         if Verbose then
            New_Line;
            Put_Line ("Binding Element #       : " &
                      Strip (Index'Img));
            Put_Line ("Element Name            : " &
                      BSTR.To_Ada (Element_Name));
            Put_Line ("Element Type            : " &
                      Element_Kind (GetTypeInfoType (Lib, Index)));
         end if;

         Attach (Type_Info, GetTypeInfo (Lib, Index));

         case GetTypeInfoType (Lib, Index) is
            when TKIND_ENUM =>
               Bind_Enumeration (Type_Info);
            when TKIND_RECORD =>
               Bind_Record (Type_Info);
            when TKIND_MODULE =>
               Bind_Module (Type_Info);
            when TKIND_INTERFACE =>
               Bind_Interface (Type_Info);
            when TKIND_DISPATCH =>
               Bind_Dispatch (Type_Info);
            when TKIND_COCLASS =>
               Bind_CoClass (Type_Info);
            when TKIND_ALIAS =>
               Bind_Alias (Type_Info);
            when TKIND_UNION =>
               Bind_Union (Type_Info);
            when others =>
               Put_Line ("WARNING - Unknown element type skipped");
         end case;
      end loop;

      --  Store Spec File

      New_Line (Spec_Buffer);

      Write (From_Buffer => Type_Buffer,
             To_Output   => Base_Spec_File);

      New_Line (Base_Spec_File);

      Write (From_Buffer => Pointer_Buffer,
             To_Output   => Base_Spec_File);

      New_Line (Base_Spec_File);

      Write (From_Buffer => Alias_Buffer,
             To_Output   => Base_Spec_File);

      New_Line (Base_Spec_File);

      Write (From_Buffer => Spec_Buffer,
             To_Output   => Base_Spec_File);

      Put_Line (Base_Spec_File, "end " & Base_Name & ";");

      Close (Base_Spec_File);

      --  Store Inproc Spec

      Put_Line (Inproc_Buffer, ");");

      Decrease_Indent (Inproc_Buffer);
      Decrease_Indent (Inproc_Buffer);
      New_Line (Inproc_Buffer);
      Put_Line (Inproc_Buffer, "end " & Base_Name & ".Dll;");

      Create (File => Work_File,
              Mode => Out_File,
              Name => Base_File_Name & "-dll.ads");

      Write (From_Buffer => With_Buffer,
             To_Output   => Work_File);
      Write (From_Buffer => Inproc_Buffer,
             To_Output   => Work_File);
      Close (Work_File);

      --  Store Inproce Server Body

      Create (File => Work_File,
              Mode => Out_File,
              Name => Base_File_Name & "-dll.adb");

      Put_Line (Work_File, "package body " & Base_Name & ".Dll is");
      Put_Line (Work_File, "   procedure Main is");
      Put_Line (Work_File, "   begin");
      Put_Line (Work_File,
                "      " &
                "GNATCOM.Create.Inproc.Factory_Map := Object_Map'Access;");
      Put_Line (Work_File,
                "      " &
                "GNATCOM.Create.Inproc.Init_Object (LIBID => LIBID_" &
                BSTR.To_Ada (Library_Name, False) & ");");
      Put_Line (Work_File, "   end Main;");
      New_Line (Work_File);
      Put_Line (Work_File, "begin");
      Put_Line (Work_File, "   Main;");
      Put_Line (Work_File, "end " & Base_Name & ".Dll;");
      Close (Work_File);

      --  Store Local Server Body

      Put_Line (Local_Buffer, ");");

      New_Line (Local_Buffer);
      Decrease_Indent (Local_Buffer);
      Decrease_Indent (Local_Buffer);
      Put_Line (Local_Buffer, "begin");
      Increase_Indent (Local_Buffer);
      Put_Line (Local_Buffer,
                "GNATCOM.Create.Local_Server.Factory_Map := " &
                "Object_Map'Unchecked_Access;");
      Put_Line (Local_Buffer,
                "GNATCOM.Create.Local_Server.Init_Object (LIBID_" &
                BSTR.To_Ada (Library_Name, False) & ");");
      Decrease_Indent (Local_Buffer);
      Put_Line (Local_Buffer, "end " & Base_Name & ".Exe;");

      Create (File => Work_File,
              Mode => Out_File,
              Name => Base_File_Name & "-exe.adb");

      Write (From_Buffer => With_Buffer,
             To_Output   => Work_File);
      Write (From_Buffer => Local_Buffer,
             To_Output   => Work_File);
      Close (Work_File);

      --  Store Remote Register Body

      Put_Line (Remote_Buffer, ");");

      New_Line (Remote_Buffer);
      Decrease_Indent (Remote_Buffer);
      Decrease_Indent (Remote_Buffer);
      Put_Line (Remote_Buffer, "begin");
      Increase_Indent (Remote_Buffer);
      Put_Line (Remote_Buffer,
                "GNATCOM.Create.Remote_Register.Factory_Map := " &
                "Object_Map'Unchecked_Access;");
      Put_Line (Remote_Buffer,
                "GNATCOM.Create.Remote_Register.Init_Object (LIBID_" &
                BSTR.To_Ada (Library_Name, False) & ");");
      Decrease_Indent (Remote_Buffer);
      Put_Line (Remote_Buffer, "end " & Base_Name & ".Remote;");

      Create (File => Work_File,
              Mode => Out_File,
              Name => Base_File_Name & "-remote.adb");

      Write (From_Buffer => Remote_Buffer,
             To_Output   => Work_File);
      Close (Work_File);

      --  Create RC File

      Create (File => Work_File,
              Mode => Out_File,
              Name => Base_File_Name & "rc.rc");

      Put_Line (Work_File,
                "1 VERSIONINFO");
      Put_Line (Work_File,
                "   FILEVERSION 1,0,0,1");
      Put_Line (Work_File,
                "   PRODUCTVERSION 1,0,0,1");
      Put_Line (Work_File,
                "   FILEFLAGSMASK 0x3fL");
      Put_Line (Work_File,
                "   FILEFLAGS 0x0L");
      Put_Line (Work_File,
                "   FILEOS 0x40004L");
      Put_Line (Work_File,
                "   FILETYPE 0x2L");
      Put_Line (Work_File,
                "   FILESUBTYPE 0x0L");
      Put_Line (Work_File,
                "   BEGIN");
      Put_Line (Work_File,
                "      BLOCK ""StringFileInfo""");
      Put_Line (Work_File,
                "      BEGIN");
      Put_Line (Work_File,
                "         BLOCK ""040904b0""");
      Put_Line (Work_File,
                "         BEGIN");
      Put_Line (Work_File,
                "            VALUE ""CompanyName"", ""Your Company Here\0""");
      Put_Line (Work_File,
                "            VALUE ""FileDescription"", """ &
                Base_Name &
                "\0""");
      Put_Line (Work_File,
                "            VALUE ""FileVersion"", ""1, 0, 0, 1\0""");
      Put_Line (Work_File,
                "            VALUE ""InternalName"", """ & Base_Name & "\0""");
      Put_Line (Work_File,
                "            VALUE ""LegalCopyright"", ""Copyright(c)\0""");
      Put_Line (Work_File,
                "            VALUE ""Originalfilename"", """ &
                Base_Name &
                "\0""");
      Put_Line (Work_File,
                "            VALUE ""ProductName"", """ & Base_Name & "\0""");
      Put_Line (Work_File,
                "            VALUE ""ProductVersion"", ""1, 0, 0, 1\0""");
      Put_Line (Work_File,
                "            VALUE ""OLESelfRegister"", """"");
      Put_Line (Work_File,
                "         END");
      Put_Line (Work_File,
                "      END");
      Put_Line (Work_File,
                "      BLOCK ""VarFileInfo""");
      Put_Line (Work_File,
                "      BEGIN");
      Put_Line (Work_File,
                "         VALUE ""Translation"", 0x409, 1200");
      Put_Line (Work_File,
                "      END");
      Put_Line (Work_File,
                "   END");
      New_Line (Work_File);
      Put_Line (Work_File,
                "1 TypeLib """ & Cify (File_Name) & """");
      Close (Work_File);

      --  Create Clean File

      Create (File => Work_File,
              Mode => Out_File,
              Name => "clean.bat");
      Put_Line (Work_File,
                "del b~*.*");
      Put_Line (Work_File,
                "del *.ali");
      Put_Line (Work_File,
                "del *.o");
      Put_Line (Work_File,
                "del *~");
      Put_Line (Work_File,
                "del *.dll");
      Put_Line (Work_File,
                "del *.exe");
      Close (Work_File);

      --  Create Make File

      Create (File => Work_File,
              Mode => Out_File,
              Name => "make.bat");

      Put_Line (Work_File,
                "windres " & Base_File_Name & "rc.rc " &
                Base_File_Name & "rc.coff");
      Put_Line (Work_File,
                "gnatmake " &
                Base_File_Name &
                "-exe");
      Put_Line (Work_File,
                "gnatmake " &
                Base_File_Name &
                "-remote");
      Put_Line (Work_File,
                "gnatmake " &
                Base_File_Name &
                "-dll");
      Put_Line (Work_File,
                "gnatdll -n -e " & Base_File_Name & ".def -d" &
                Base_File_Name &
                "-dll.dll " &
                Base_File_Name &
                "-dll.ali");
      Close (Work_File);

      --  Create DEF file

      Create (File => Work_File,
              Mode => Out_File,
              Name => Base_File_Name & ".def");

      Put_Line (Work_File,
                "EXPORTS");
      Put_Line (Work_File,
                "   DllGetClassObject=DllGetClassObject@12     @2");
      Put_Line (Work_File,
                "   DllCanUnloadNow=DllCanUnloadNow@0          @3");
      Put_Line (Work_File,
                "   DllRegisterServer=DllRegisterServer@0      @4");
      Put_Line (Work_File,
                "   DllUnregisterServer=DllUnregisterServer@0  @5");
      Close (Work_File);

      BSTR.Free (Library_Name);

      Clear (Type_Buffer);
      Clear (Spec_Buffer);
      Clear (Pointer_Buffer);
      Clear (Alias_Buffer);
      Clear (Event_Buffer);
      Clear (With_Buffer);
      Clear (Inproc_Buffer);
      Clear (Local_Buffer);
      Clear (Remote_Buffer);
   end Bind;

   -- Output_Header --

   procedure Output_Header
   is
      use type Interfaces.C.int;

      Name       : aliased GNATCOM.Types.BSTR;
      Doc_String : aliased GNATCOM.Types.BSTR;
      Attribs    : GNATCOM.Types.Pointer_To_TLIBATTR;
      Header     : Source_Buffer_Type;
   begin
      GetDocumentation (Lib,
                        -1,
                        Name'Unchecked_Access,
                        Doc_String'Unchecked_Access,
                        null,
                        null);

      Put_Comment (Header,
                   "Generated by             : CreateCOM");
      Put_Comment (Header,
                   "Generated on             : " &
                   VARIANT.To_Ada (VARIANT.To_VARIANT (Ada.Calendar.Clock)));

      Put_Comment (Header, "Library Name             : " &
                   BSTR.To_Ada (Name));
      Put_Comment (Header, "Library Documentation    : " &
                   BSTR.To_Ada (Doc_String));

      Attribs := GetLibAttr (Lib);

      Put_Comment (Header, "Library Version          : " &
                   Strip (Attribs.wMajorVerNum'Img) &
                   "." &
                   Strip (Attribs.wMinorVerNum'Img));

      Put_Comment (Header, "Library LIBID            : " &
                   GNATCOM.GUID.To_String (Attribs.guid));

      ReleaseTLibAttr (Lib, Attribs);

      Put_Comment (Header, "Elements in Type Library : " &
                   Strip (GetTypeInfoCount (Lib)'Img));
      New_Line (Header);

      Write (From_Buffer => Header,
             To_Output   => Base_Spec_File);

      Clear (Header);
   end Output_Header;

   -- Output_Withs --

   procedure Output_Withs is
   begin
      Put_Line (Base_Spec_File, "with Interfaces.C;");
      New_Line (Base_Spec_File);
      Put_Line (Base_Spec_File, "with GNATCOM.Create.COM_Interface;");
      Put_Line (Base_Spec_File, "with GNATCOM.Types;");
      Put_Line (Base_Spec_File, "with GNATCOM.GUID;");
      New_Line (Base_Spec_File);
   end Output_Withs;

   -- Describe_Element --

   procedure Describe_Element
     (Type_Info : in     GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      Buffer    : in out Source_Buffer_Type)
   is
      Ref_Lib    : ITypeLib_Type;
      Ref_Index  : aliased Interfaces.C.int;
      Name       : aliased GNATCOM.Types.BSTR;
      Doc        : aliased GNATCOM.Types.BSTR;
   begin
      Attach (Ref_Lib, GetContainingTypeLib
              (Type_Info,
               Ref_Index'Unchecked_Access));

      GetDocumentation (Ref_Lib,
                        Ref_Index,
                        Name'Unchecked_Access,
                        Doc'Unchecked_Access,
                        null,
                        null);

      Put_Comment (Buffer, "Element Index         :" & Ref_Index'Img);
      Put_Comment (Buffer, "Element Name          : " &
                   BSTR.To_Ada (Name));

      if BSTR.To_Ada (Doc, False) > "" then
         Put_Comment (Buffer, "Element Documentation : " &
                      BSTR.To_Ada (Doc));
      end if;

      Put_Comment (Buffer, "Element Type          : " &
                   Element_Kind (GetTypeInfoType (Ref_Lib, Ref_Index)));
      New_Line (Buffer);
   end Describe_Element;

   -- Bind_Enumeration --

   procedure Bind_Enumeration
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type)
   is
      use type Interfaces.C.short;

      Bind_Name   : constant String := Valid_Identifier (GetName (Type_Info));
      Bind_Buffer : Source_Buffer_Type;
      Attribs     : GNATCOM.Types.Pointer_To_TYPEATTR;
      Max_Length  : Natural := 0;
   begin
      if
        not Get (To_Unbounded_String (Bind_Name))
      then
         Increase_Indent (Bind_Buffer);

         Describe_Element (Type_Info, Bind_Buffer);

         Attribs := GetTypeAttr (Type_Info);

         Set (To_Unbounded_String (Bind_Name), True);

         Put (Type_Buffer, "subtype " &
              Bind_Name &
              " is Interfaces.C.");

         case Attribs.cbSizeInstance is
            when 1 =>
               Put_Line (Type_Buffer, "char;");
            when 2 =>
               Put_Line (Type_Buffer, "short;");
            when 4 =>
               Put_Line (Type_Buffer, "long;");
            when others =>
               Put_Line (Type_Buffer, "UNKNOWN");
         end case;

         for N in  0 .. Attribs.cVars - 1 loop
            declare
               Desc     : constant GNATCOM.Types.Pointer_To_VARDESC :=
                 GetVarDesc (Type_Info, Interfaces.C.int (N));
               Var_Name : aliased GNATCOM.Types.BSTR;
            begin
               GetDocumentation (Type_Info,
                                 Desc.memid,
                                 Var_Name'Unchecked_Access,
                                 null,
                                 null,
                                 null);

               declare
                  Name : constant String :=
                    Valid_Identifier (BSTR.To_Ada (Var_Name));
               begin
                  if Name'Length > Max_Length then
                     Max_Length := Name'Length;
                  end if;
               end;

               ReleaseVarDesc (Type_Info, Desc);
            end;
         end loop;

         for N in  0 .. Attribs.cVars - 1 loop
            declare
               Desc     : constant GNATCOM.Types.Pointer_To_VARDESC :=
                 GetVarDesc (Type_Info, Interfaces.C.int (N));
               Var_Name : aliased GNATCOM.Types.BSTR;
               Var_Desc : aliased GNATCOM.Types.BSTR;
               Name     : String (1 .. Max_Length);
            begin
               GetDocumentation (Type_Info,
                                 Desc.memid,
                                 Var_Name'Unchecked_Access,
                                 Var_Desc'Unchecked_Access,
                                 null,
                                 null);

               Ada.Strings.Fixed.Move
                 (Valid_Identifier (BSTR.To_Ada (Var_Name)),
                  Name);

               Put_Line (Bind_Buffer,
                         Name &
                         " : constant := " &
                         VARIANT.To_Ada (Desc.u.lpvarValue.all, False) &
                         ";");

               if BSTR.To_Ada (Var_Desc, False) > "" then
                  Put_Comment
                    (Bind_Buffer, BSTR.To_Ada (Var_Desc, False));
               end if;

               BSTR.Free (Var_Desc);
               ReleaseVarDesc (Type_Info, Desc);
            end;
         end loop;

         ReleaseTypeAttr (Type_Info, Attribs);

         Write (From_Buffer => Bind_Buffer,
                To_Buffer   => Spec_Buffer);

         Clear (Bind_Buffer);
      end if;

   end Bind_Enumeration;

   -- Bind_Record --

   procedure Bind_Record
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type)
   is
      use type Interfaces.C.short;

      Bind_Name   : constant String := Valid_Identifier (GetName (Type_Info));
      Pre_Buffer  : aliased Source_Buffer_Type;
      Bind_Buffer : Source_Buffer_Type;
      Attribs     : GNATCOM.Types.Pointer_To_TYPEATTR;
      Max_Length  : Natural := 0;
   begin
      if
        not Get (To_Unbounded_String (Bind_Name))
      then
         Increase_Indent (Bind_Buffer);
         Increase_Indent (Pre_Buffer);

         Describe_Element (Type_Info, Bind_Buffer);

         Set (To_Unbounded_String (Bind_Name), True);

         Put_Line (Type_Buffer, "type " & Bind_Name & ";");

         Put_Line (Bind_Buffer,
                   "type " & Bind_Name & " is");

         Increase_Indent (Bind_Buffer);

         Put_Line (Bind_Buffer, "record");

         Increase_Indent (Bind_Buffer);

         Attribs := GetTypeAttr (Type_Info);

         for N in  0 .. Attribs.cVars - 1 loop
            declare
               Desc     : constant GNATCOM.Types.Pointer_To_VARDESC :=
                 GetVarDesc (Type_Info, Interfaces.C.int (N));
               Var_Name : aliased GNATCOM.Types.BSTR;
            begin
               GetDocumentation (Type_Info,
                                 Desc.memid,
                                 Var_Name'Unchecked_Access,
                                 null,
                                 null,
                                 null);

               declare
                  Name : constant String :=
                    Valid_Identifier (BSTR.To_Ada (Var_Name));
               begin
                  if Name'Length > Max_Length then
                     Max_Length := Name'Length;
                  end if;
               end;

               ReleaseVarDesc (Type_Info, Desc);
            end;
         end loop;

         for N in  0 .. Attribs.cVars - 1 loop
            declare
               Desc         : constant GNATCOM.Types.Pointer_To_VARDESC :=
                 GetVarDesc (Type_Info, Interfaces.C.int (N));
               Var_Name     : aliased GNATCOM.Types.BSTR;
               Var_Desc     : aliased GNATCOM.Types.BSTR;
               Name         : String (1 .. Max_Length);
            begin
               GetDocumentation (Type_Info,
                                 Desc.memid,
                                 Var_Name'Unchecked_Access,
                                 Var_Desc'Unchecked_Access,
                                 null,
                                 null);
               declare
                  Element_Type : constant String :=
                    Type_Kind (Desc.elemdescVar.tdesc,
                               Type_Info,
                               Pre_Buffer'Access,
                               BSTR.To_Ada (Var_Name,
                                            False));
               begin
                  Ada.Strings.Fixed.Move
                    (Valid_Identifier (BSTR.To_Ada (Var_Name)),
                     Name);

                  Put_Line (Bind_Buffer, Name & " : " & Element_Type & ";");
               end;

               if BSTR.To_Ada (Var_Desc, False) > "" then
                  Put_Comment
                    (Bind_Buffer, BSTR.To_Ada (Var_Desc, False));
               end if;

               BSTR.Free (Var_Desc);

               ReleaseVarDesc (Type_Info, Desc);
            end;
         end loop;

         Decrease_Indent (Bind_Buffer);

         Put_Line (Bind_Buffer, "end record;");

         Decrease_Indent (Bind_Buffer);

         Put_Line (Bind_Buffer,
                   "pragma Convention (C_Pass_By_Copy, " & Bind_Name & ");");

         Put_Line (Bind_Buffer,
                   "for " & Bind_Name & " use");

         Increase_Indent (Bind_Buffer);

         Put_Line (Bind_Buffer, "record");

         Increase_Indent (Bind_Buffer);

         for N in  0 .. Attribs.cVars - 1 loop
            declare
               Desc         : constant GNATCOM.Types.Pointer_To_VARDESC :=
                 GetVarDesc (Type_Info, Interfaces.C.int (N));
               Var_Name     : aliased GNATCOM.Types.BSTR;
               Var_Desc     : aliased GNATCOM.Types.BSTR;
               Name         : String (1 .. Max_Length);
               Start        : Natural := 0;
            begin
               GetDocumentation (Type_Info,
                                 Desc.memid,
                                 Var_Name'Unchecked_Access,
                                 Var_Desc'Unchecked_Access,
                                 null,
                                 null);
               declare
                  Element_Type : constant String :=
                    Type_Kind (Desc.elemdescVar.tdesc,
                               Type_Info,
                               Pre_Buffer'Access,
                               BSTR.To_Ada (Var_Name,
                                            False));
                  Element_Size : constant String := Type_Size (Element_Type);
               begin
                  Ada.Strings.Fixed.Move
                    (Valid_Identifier (BSTR.To_Ada (Var_Name)),
                     Name);

                  Start := Natural (Desc.u.oInst) * 8;

                  Put_Line (Bind_Buffer, Name & " at 0 range" & Start'Img &
                            " .." & Start'Img & " + " &
                            Element_Size & " - 1;");
               end;

               BSTR.Free (Var_Desc);

               ReleaseVarDesc (Type_Info, Desc);
            end;
         end loop;

         Decrease_Indent (Bind_Buffer);

         Put_Line (Bind_Buffer, "end record;");

         Decrease_Indent (Bind_Buffer);

         if
           not Get (To_Unbounded_String ("Size_Of_" & Bind_Name))
         then
            declare
               Size : constant Natural := Natural (Attribs.cbSizeInstance) * 8;
            begin

               New_Line (Pre_Buffer);
               Put_Line (Pre_Buffer, "Size_Of_" & Bind_Name
                         & " : constant :=" &
                         Size'Img & ";");

               Size_Map.Set (To_Unbounded_String (Bind_Name),
                             To_Unbounded_String ("Size_Of_" & Bind_Name));
               Set (To_Unbounded_String ("Size_Of_" & Bind_Name), True);
            end;
         end if;

         Put_Line (Bind_Buffer,
                   "for " & Bind_Name & "'size use Size_Of_" &
                   Bind_Name & ";");

         if Attribs.cbAlignment < 8 then
            Put_Line (Bind_Buffer,
                      "for " & Bind_Name & "'alignment use" &
                      Attribs.cbAlignment'Img & ";");
         end if;

         ReleaseTypeAttr (Type_Info, Attribs);

         Write (From_Buffer => Pre_Buffer,
                To_Buffer   => Spec_Buffer);

         New_Line (Spec_Buffer);

         Write (From_Buffer => Bind_Buffer,
                To_Buffer   => Spec_Buffer);

         Clear (Pre_Buffer);
         Clear (Bind_Buffer);
      end if;
   end Bind_Record;

   -- Bind_Union --

   procedure Bind_Union
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type)
   is
      use type Interfaces.C.short;

      Bind_Name   : constant String := Valid_Identifier (GetName (Type_Info));
      Pre_Buffer  : aliased Source_Buffer_Type;
      Bind_Buffer : Source_Buffer_Type;
      Attribs     : GNATCOM.Types.Pointer_To_TYPEATTR;
      Max_Length  : Natural := 0;
   begin
      if
        not Get (To_Unbounded_String (Bind_Name))
      then
         Increase_Indent (Bind_Buffer);
         Increase_Indent (Pre_Buffer);

         Set (To_Unbounded_String (Bind_Name), True);

         Put_Line (Type_Buffer, "type " & Bind_Name & ";");

         Describe_Element (Type_Info, Bind_Buffer);

         Attribs := GetTypeAttr (Type_Info);

         Put_Line (Bind_Buffer,
                   "subtype " &
                   Bind_Name & "_Range" &
                   " is Positive range 1 .." &
                   Attribs.cVars'Img & ";");

         Put_Line (Bind_Buffer,
                   "type " &
                   Bind_Name &
                   " (Which : " &
                   Bind_Name & "_Range" &
                   " := 1) is");

         Increase_Indent (Bind_Buffer);

         Put_Line (Bind_Buffer, "record");

         Increase_Indent (Bind_Buffer);

         Put_Line (Bind_Buffer, "case Which is");

         Increase_Indent (Bind_Buffer);

         for N in  0 .. Attribs.cVars - 1 loop
            declare
               Desc     : constant GNATCOM.Types.Pointer_To_VARDESC :=
                 GetVarDesc (Type_Info, Interfaces.C.int (N));
               Var_Name : aliased GNATCOM.Types.BSTR;
            begin
               GetDocumentation (Type_Info,
                                 Desc.memid,
                                 Var_Name'Unchecked_Access,
                                 null,
                                 null,
                                 null);

               declare
                  Name : constant String :=
                    Valid_Identifier (BSTR.To_Ada (Var_Name));
               begin
                  if Name'Length > Max_Length then
                     Max_Length := Name'Length;
                  end if;
               end;

               ReleaseVarDesc (Type_Info, Desc);
            end;
         end loop;

         for N in  0 .. Attribs.cVars - 1 loop
            declare
               Desc         : constant GNATCOM.Types.Pointer_To_VARDESC :=
                 GetVarDesc (Type_Info, Interfaces.C.int (N));
               Var_Name     : aliased GNATCOM.Types.BSTR;
               Var_Desc     : aliased GNATCOM.Types.BSTR;
               Name         : String (1 .. Max_Length);
            begin

               Put_Line (Bind_Buffer, "when" & Integer (N + 1)'Img & " =>");

               Increase_Indent (Bind_Buffer);

               GetDocumentation (Type_Info,
                                 Desc.memid,
                                 Var_Name'Unchecked_Access,
                                 Var_Desc'Unchecked_Access,
                                 null,
                                 null);
               declare
                  Element_Type : constant String :=
                    Type_Kind (Desc.elemdescVar.tdesc,
                               Type_Info,
                               Pre_Buffer'Access,
                               BSTR.To_Ada (Var_Name,
                                            False));
               begin
                  Ada.Strings.Fixed.Move
                    (Valid_Identifier (BSTR.To_Ada (Var_Name)),
                     Name);

                  Put_Line (Bind_Buffer, Name & " : " & Element_Type & ";");
               end;

               if BSTR.To_Ada (Var_Desc, False) > "" then
                  Put_Comment
                    (Bind_Buffer, BSTR.To_Ada (Var_Desc, False));
               end if;

               BSTR.Free (Var_Desc);

               ReleaseVarDesc (Type_Info, Desc);
            end;

            Decrease_Indent (Bind_Buffer);
         end loop;

         Decrease_Indent (Bind_Buffer);

         Put_Line (Bind_Buffer, "end case;");

         Decrease_Indent (Bind_Buffer);

         Put_Line (Bind_Buffer, "end record;");

         Decrease_Indent (Bind_Buffer);

         Put_Line (Bind_Buffer,
                   "pragma Convention (C_Pass_By_Copy, " & Bind_Name & ");");

         Put_Line (Bind_Buffer,
                   "pragma Unchecked_Union (" & Bind_Name & ");");

         if
           not Get (To_Unbounded_String ("Size_Of_" & Bind_Name))
         then
            declare
               Size : constant Natural := Natural (Attribs.cbSizeInstance) * 8;
            begin

               New_Line (Pre_Buffer);
               Put_Line (Pre_Buffer, "Size_Of_" & Bind_Name
                         & " : constant :=" &
                         Size'Img & ";");

               Size_Map.Set (To_Unbounded_String (Bind_Name),
                             To_Unbounded_String ("Size_Of_" & Bind_Name));
               Set (To_Unbounded_String ("Size_Of_" & Bind_Name), True);
            end;
         end if;

         Put_Line (Bind_Buffer,
                   "for " & Bind_Name & "'size use Size_Of_" &
                   Bind_Name & ";");

         ReleaseTypeAttr (Type_Info, Attribs);

         Write (From_Buffer => Pre_Buffer,
                To_Buffer   => Spec_Buffer);

         New_Line (Spec_Buffer);

         Write (From_Buffer => Bind_Buffer,
                To_Buffer   => Spec_Buffer);

         Clear (Pre_Buffer);
         Clear (Bind_Buffer);
      end if;
   end Bind_Union;

   -- Bind_Alias --

   procedure Bind_Alias
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type)
   is
      Bind_Name   : constant String := Valid_Identifier (GetName (Type_Info));
      Pre_Buffer  : aliased Source_Buffer_Type;
      Bind_Buffer : aliased Source_Buffer_Type;
      Attribs     : GNATCOM.Types.Pointer_To_TYPEATTR;
   begin
      if
        not Get (To_Unbounded_String (Bind_Name))
      then
         Increase_Indent (Bind_Buffer);

         Set (To_Unbounded_String (Bind_Name), True);

         Put_Line (Type_Buffer, "type " & Bind_Name & ";");

         Put_Line (Bind_Buffer, "type " & Bind_Name & " is");

         Attribs := GetTypeAttr (Type_Info);

         declare
            Element_Type : constant String :=
              Type_Kind (Attribs.tdescAlias,
                         Type_Info,
                         Pre_Buffer'Access,
                         "");
         begin
            Put_Line (Bind_Buffer, "  new " & Element_Type & ";");

            if Ada.Strings.Fixed.Index (Element_Type, "Pointer_To_") > 0 then
               if
                 not Get (To_Unbounded_String ("Size_Of_" & Bind_Name))
               then
                  Size_Map.Set
                    (To_Unbounded_String (Bind_Name),
                     To_Unbounded_String ("GNATCOM.Types.Size_Of_Pointers"));
                  Set (To_Unbounded_String ("Size_Of_" & Bind_Name), True);
               end if;
            end if;
         end;

         ReleaseTypeAttr (Type_Info, Attribs);

         Write (From_Buffer => Pre_Buffer,
                To_Buffer   => Alias_Buffer);

         New_Line (Alias_Buffer);

         Write (From_Buffer => Bind_Buffer,
                To_Buffer   => Alias_Buffer);

         Clear (Pre_Buffer);
         Clear (Bind_Buffer);
      end if;
   end Bind_Alias;

   -- Bind_CoClass --

   procedure Bind_CoClass
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type)
   is
      use type Interfaces.C.short;
      use type Interfaces.C.unsigned;

      Bind_Name    : constant String := Valid_Identifier (GetName (Type_Info));
      Bind_Buffer  : Source_Buffer_Type;
      Attribs      : GNATCOM.Types.Pointer_To_TYPEATTR;
      Head_File    : File_Type;
      Body_File    : File_Type;
      Head_Buffer  : Source_Buffer_Type;
      Body_Buffer  : Source_Buffer_Type;
      Map_Buffer   : Source_Buffer_Type;
      Map_Count    : Natural := 1;
      Has_Dual     : Boolean := False;
   begin
      Increase_Indent (Bind_Buffer);
      Increase_Indent (Map_Buffer);

      Describe_Element (Type_Info, Bind_Buffer);

      Put_Line (Bind_Buffer, "CLSID_" & Bind_Name &
                " : aliased GNATCOM.Types.GUID :=");

      Attribs := GetTypeAttr (Type_Info);

      Put_Line (Bind_Buffer, "  GNATCOM.GUID.To_GUID (""" &
                GNATCOM.GUID.To_String (Attribs.guid) &
                """);");

      Write (From_Buffer => Bind_Buffer,
             To_Buffer   => Spec_Buffer);

      --  Setup Class Files

      Create (File => Head_File,
              Mode => Out_File,
              Name => Ada.Strings.Fixed.Translate
              (To_String (Base_Package) &
               "-" &
               Bind_Name &
               ".ads",
               Lower_Case_Map));

      Create (File => Body_File,
              Mode => Out_File,
              Name => Ada.Strings.Fixed.Translate
              (To_String (Base_Package) &
               "-" &
               Bind_Name &
               ".adb",
               Lower_Case_Map));

      New_Line (Head_Buffer);
      Put_Line (Head_Buffer,
                "package " &
                To_String (Base_Package) &
                "." &
                Bind_Name &
                " is");
      Increase_Indent (Head_Buffer);
      New_Line (Head_Buffer);

      Put_Line (Body_Buffer,
                "package body " &
                To_String (Base_Package) &
                "." &
                Bind_Name &
                " is");
      Increase_Indent (Body_Buffer);
      New_Line (Body_Buffer);

      Put_Line (Map_Buffer,
                "GUID_Map : aliased " &
                "GNATCOM.Create.COM_Interface.GUID_Record_Array :=");
      Put (Map_Buffer, "  (");
      Increase_Indent (Map_Buffer);

      --  Bind Interfaces to Class

      if Attribs.cImplTypes > 0 then
         for N in 0 .. Attribs.cImplTypes - 1 loop
            declare
               use type Interfaces.C.unsigned_short;
               use type Interfaces.C.int;

               Ref_Info : ITypeInfo_Type;
               Attribs  : GNATCOM.Types.Pointer_To_TYPEATTR;
            begin

               --  if interfaces is not marked [source] in the idl
               if
                 (GetImplTypeFlags (Type_Info, Interfaces.C.int (N))
                  and
                  IMPLTYPEFLAG_FSOURCE)
                 = 0
               then
                  Attach (Ref_Info, GetRefTypeInfo
                          (Type_Info,
                           GetRefTypeOfImplType (Type_Info,
                                                 Interfaces.C.int (N))));

                  Attribs := GetTypeAttr (Ref_Info);

                  if
                    (Attribs.wTypeFlags and GNATCOM.Types.TYPEFLAG_FDUAL)
                    /= 0
                  then
                     Has_Dual := True;
                     ReleaseTypeAttr (Ref_Info, Attribs);

                     Attach (Ref_Info,
                             GetRefTypeInfo (Ref_Info,
                                             GetRefTypeOfImplType
                                             (Ref_Info, -1)));
                  else
                     ReleaseTypeAttr (Ref_Info, Attribs);
                  end if;

                  declare
                     Int_Name   : constant String := GetName (Ref_Info);
                     Max_Length : Natural := 0;
                  begin
                     if Map_Count > 1 then
                        Put_Line (Map_Buffer, ",");
                     end if;

                     Put (Map_Buffer, Strip (Map_Count'Img) & " => (IID_" &
                          Int_Name & ", " & Int_Name & "_Vtbl'Address)");
                     Map_Count := Map_Count + 1;

                     if Has_Dual then
                        Has_Dual := False;

                        --  Only place an entry for IDispatch for the
                        --  [default] dual interface
                        if
                          (GetImplTypeFlags (Type_Info, Interfaces.C.int (N))
                           and
                           IMPLTYPEFLAG_FDEFAULT)
                          /= 0
                        then
                           Put_Line (Map_Buffer, ",");
                           Put (Map_Buffer, Strip (Map_Count'Img) &
                                " => (GNATCOM.Types.IID_IDispatch, " &
                                Int_Name & "_Vtbl'Address)");
                           Map_Count := Map_Count + 1;
                        end if;
                     end if;

                     Bind_Thick_Elements
                       (Ref_Info, Head_Buffer, Body_Buffer,
                        Int_Name, Bind_Name);

                     Put_Line (Head_Buffer, "type " & Int_Name &
                               "_Vtbl_Record is");
                     Increase_Indent (Head_Buffer);
                     Put_Line (Head_Buffer, "record");
                     Increase_Indent (Head_Buffer);

                     Bind_VtblElements
                       (Ref_Info, Head_Buffer, Int_Name, Max_Length);

                     Decrease_Indent (Head_Buffer);
                     Put_Line (Head_Buffer, "end record;");
                     Decrease_Indent (Head_Buffer);
                     Put_Line (Head_Buffer,
                               "pragma Convention (C_Pass_By_Copy, " &
                               Int_Name & "_Vtbl_Record);");
                     New_Line (Head_Buffer);
                     Put_Line (Head_Buffer,
                               "type Pointer_To_" &
                               Int_Name &
                               "_Vtbl_Record is");
                     Put_Line (Head_Buffer,
                               "  access all " &
                               Int_Name & "_Vtbl_Record;");
                     New_Line (Head_Buffer);
                     Put_Line (Head_Buffer,
                               Int_Name & "_Vtbl : aliased " &
                               Int_Name & "_Vtbl_Record;");
                     New_Line (Head_Buffer);
                  end;
               end if;
            end;
         end loop;
      end if;

      --  Commit Files

      Put_Line (Map_Buffer, ");");

      Write (From_Buffer => Map_Buffer,
             To_Buffer   => Head_Buffer);

      New_Line (Head_Buffer);

      Put_Line (Head_Buffer, "type " & Bind_Name & "_Type is");
      Put_Line (Head_Buffer,
                "  new GNATCOM.Create.COM_Interface.CoClass_Type" &
                " (GUID_Map'Access) with");
      Increase_Indent (Head_Buffer);
      Put_Line (Head_Buffer, "record");
      Increase_Indent (Head_Buffer);
      Put_Line (Head_Buffer, "null;");
      Decrease_Indent (Head_Buffer);
      Put_Line (Head_Buffer, "end record;");
      Decrease_Indent (Head_Buffer);
      New_Line (Head_Buffer);
      Put_Line (Head_Buffer, "type Pointer_To_" & Bind_Name & "_Type is");
      Put_Line (Head_Buffer, "  access all " & Bind_Name & "_Type;");
      New_Line (Head_Buffer);
      Put_Line (Head_Buffer, "function Create");
      Put_Line (Head_Buffer,
                "  return " &
                "GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type;");
      New_Line (Head_Buffer);

      Put_Line (Body_Buffer, "function Create");
      Put_Line (Body_Buffer,
                "  return " &
                "GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type");
      Put_Line (Body_Buffer, "is");
      Put_Line (Body_Buffer, "begin");
      Increase_Indent (Body_Buffer);
      Put_Line (Body_Buffer,
                "return GNATCOM.Create.COM_Interface.Create_Object");
      Put_Line (Body_Buffer, "  (new " & Bind_Name & "_Type);");
      Decrease_Indent (Body_Buffer);
      Put_Line (Body_Buffer, "end Create;");

      Decrease_Indent (Head_Buffer);
      Decrease_Indent (Body_Buffer);

      Put_Line (Head_Buffer,
                "end " & To_String (Base_Package) & "." & Bind_Name &
                ";");

      Put_Line (Body_Buffer,
                "end " & To_String (Base_Package) & "." & Bind_Name &
                ";");

      Write (From_Buffer => Head_Buffer,
             To_Output   => Head_File);

      Write (From_Buffer => Body_Buffer,
             To_Output   => Body_File);

      Close (Head_File);
      Close (Body_File);

      --  Add to with list for Dlls and Exes

      Put_Line (With_Buffer,
                "with " &
                To_String (Base_Package) &
                "." &
                Bind_Name &
                ";");

      --  Add entry to Object_Map for Inprocs, Local Servers, and
      --  Remote Register

      if Class_Counter = 1 then
         Put (Inproc_Buffer, "  (");
         Increase_Indent (Inproc_Buffer);
         Put (Local_Buffer, "  (");
         Increase_Indent (Local_Buffer);
         Put (Remote_Buffer, "  (");
         Increase_Indent (Remote_Buffer);
      else
         Put_Line (Inproc_Buffer, ",");
         Put_Line (Local_Buffer, ",");
         Put_Line (Remote_Buffer, ",");
      end if;

      --  Inproc
      Put_Line (Inproc_Buffer,
                Strip (Class_Counter'Img) &
                " => (CLSID_" & Bind_Name & ",");
      Increase_Indent (Inproc_Buffer);
      Increase_Indent (Inproc_Buffer);
      Put_Line (Inproc_Buffer, Bind_Name & ".Create'Access,");
      Put_Line (Inproc_Buffer,
                "Ada.Strings.Unbounded.To_Unbounded_String");
      Put_Line (Inproc_Buffer,
                "(""" & BSTR.To_Ada (Library_Name, False) &
                "." & Bind_Name & """),");
      Put_Line (Inproc_Buffer,
                "Ada.Strings.Unbounded.To_Unbounded_String (""1""),");
      Put_Line (Inproc_Buffer,
                "Ada.Strings.Unbounded.To_Unbounded_String");
      Put (Inproc_Buffer, "(""" & Bind_Name & """))");
      Decrease_Indent (Inproc_Buffer);
      Decrease_Indent (Inproc_Buffer);

      --  Local
      Put_Line (Local_Buffer,
                Strip (Class_Counter'Img) &
                " => (CLSID_" & Bind_Name & ",");
      Increase_Indent (Local_Buffer);
      Increase_Indent (Local_Buffer);
      Put_Line (Local_Buffer, Bind_Name & ".Create'Access,");
      Put_Line (Local_Buffer,
                "Ada.Strings.Unbounded.To_Unbounded_String");
      Put_Line (Local_Buffer,
                "(""" & BSTR.To_Ada (Library_Name, False) &
                "." & Bind_Name & """),");
      Put_Line (Local_Buffer,
                "Ada.Strings.Unbounded.To_Unbounded_String (""1""),");
      Put_Line (Local_Buffer,
                "Ada.Strings.Unbounded.To_Unbounded_String");
      Put_Line (Local_Buffer, "(""" & Bind_Name & """),");
      Put_Line (Local_Buffer, "null,");
      Put (Local_Buffer, "0)");
      Decrease_Indent (Local_Buffer);
      Decrease_Indent (Local_Buffer);

      --  Remote Register

      Put_Line (Remote_Buffer,
                Strip (Class_Counter'Img) &
                " => (CLSID_" & Bind_Name & ",");
      Increase_Indent (Remote_Buffer);
      Increase_Indent (Remote_Buffer);
      Put_Line (Remote_Buffer,
                "Ada.Strings.Unbounded.To_Unbounded_String");
      Put_Line (Remote_Buffer,
                "(""" & BSTR.To_Ada (Library_Name, False) &
                "." & Bind_Name & """),");
      Put_Line (Remote_Buffer,
                "Ada.Strings.Unbounded.To_Unbounded_String (""1""),");
      Put_Line (Remote_Buffer,
                "Ada.Strings.Unbounded.To_Unbounded_String");
      Put (Remote_Buffer, "(""" & Bind_Name & """))");
      Decrease_Indent (Remote_Buffer);
      Decrease_Indent (Remote_Buffer);

      --  Get ready for next Class

      Class_Counter := Class_Counter + 1;

      --  Clean up

      ReleaseTypeAttr (Type_Info, Attribs);

      Clear (Head_Buffer);
      Clear (Body_Buffer);
      Clear (Map_Buffer);
      Clear (Bind_Buffer);
   end Bind_CoClass;

   -- Bind_Module --

   procedure Bind_Module
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type)
   is
      use type Interfaces.C.short;
      use type Interfaces.C.unsigned_short;

      Bind_Name   : constant String := Valid_Identifier (GetName (Type_Info));
      Pre_Buffer  : aliased Source_Buffer_Type;
      Bind_Buffer : Source_Buffer_Type;
      Attribs     : GNATCOM.Types.Pointer_To_TYPEATTR;
      Module_File : File_Type;
      Max_Length  : Natural := 0;
   begin
      Create (File => Module_File,
              Mode => Out_File,
              Name => Ada.Strings.Fixed.Translate (To_String (Base_Package) &
                                                   "-" &
                                                   Bind_Name &
                                                   ".ads",
                                                   Lower_Case_Map));

      Put_Line (Bind_Buffer,
                "package " &
                To_String (Base_Package) &
                "." &
                Bind_Name &
                " is");

      Increase_Indent (Bind_Buffer);
      Increase_Indent (Pre_Buffer);

      Attribs := GetTypeAttr (Type_Info);

      for N in  0 .. Attribs.cVars - 1 loop
         declare
            Desc     : constant GNATCOM.Types.Pointer_To_VARDESC :=
              GetVarDesc (Type_Info, Interfaces.C.int (N));
            Var_Name : aliased GNATCOM.Types.BSTR;
         begin
            GetDocumentation (Type_Info,
                              Desc.memid,
                              Var_Name'Unchecked_Access,
                              null,
                              null,
                              null);

            declare
               Name : constant String :=
                 Valid_Identifier (BSTR.To_Ada (Var_Name));
            begin
               if Name'Length > Max_Length then
                  Max_Length := Name'Length;
               end if;
            end;

            ReleaseVarDesc (Type_Info, Desc);
         end;
      end loop;

      for N in  0 .. Attribs.cVars - 1 loop
         declare
            Desc         : constant GNATCOM.Types.Pointer_To_VARDESC :=
              GetVarDesc (Type_Info, Interfaces.C.int (N));
            Var_Name     : aliased GNATCOM.Types.BSTR;
            Var_Desc     : aliased GNATCOM.Types.BSTR;
            Name         : String (1 .. Max_Length);
         begin
            GetDocumentation (Type_Info,
                              Desc.memid,
                              Var_Name'Unchecked_Access,
                              Var_Desc'Unchecked_Access,
                              null,
                              null);

            Ada.Strings.Fixed.Move
              (Valid_Identifier (BSTR.To_Ada (Var_Name)),
               Name);

            Put (Bind_Buffer, Name);

            case Desc.elemdescVar.tdesc.vt is
               when VT_LPSTR =>
                  Put_Line (Bind_Buffer,
                       " : constant char_array :=");
                  Put_Line (Bind_Buffer,
                            "  Interfaces.C.To_C (""" &
                            VARIANT.To_Ada (Desc.u.lpvarValue.all) &
                            """);");
               when VT_LPWSTR =>
                  Put_Line (Bind_Buffer,
                       " : constant wchar_array :=");
                  Put_Line (Bind_Buffer,
                            "  Interfaces.C.To_C (""" &
                            VARIANT.To_Ada (Desc.u.lpvarValue.all) &
                            """);");
               when others =>
                  Put_Line (Bind_Buffer, " : constant := " &
                            VARIANT.To_Ada (Desc.u.lpvarValue.all) &
                            ";");
            end case;

            if BSTR.To_Ada (Var_Desc, False) > "" then
               Put_Comment
                 (Bind_Buffer, BSTR.To_Ada (Var_Desc, False));
            end if;

            BSTR.Free (Var_Desc);

            ReleaseVarDesc (Type_Info, Desc);
         end;
      end loop;

      New_Line (Bind_Buffer);

      for N in  0 .. Attribs.cFuncs - 1 loop
         declare
            Desc      : constant GNATCOM.Types.Pointer_To_FUNCDESC :=
              GetFuncDesc (Type_Info, Interfaces.C.int (N));
            Func_Desc : aliased GNATCOM.Types.BSTR;

            Param_Names   : aliased GNATCOM.Types.BSTR_PARAM_ARRAY;
            cNames        : aliased Interfaces.C.unsigned;
         begin
            if Desc.elemdescFunc.tdesc.vt = VT_VOID then
               Put (Bind_Buffer, "procedure " &
                    Valid_Identifier (GetFunctionName (Type_Info,
                                                       Desc)));
            else
               Put (Bind_Buffer, "function " &
                    Valid_Identifier (GetFunctionName (Type_Info,
                                                       Desc)));
            end if;

            if Desc.cParams > 0 then
               New_Line (Bind_Buffer);
               Put (Bind_Buffer, "  (");
            end if;

            GetNames (Type_Info,
                      Desc.memid,
                      Param_Names'Unchecked_Access,
                      GNATCOM.Types.MAX_PARAMS,
                      cNames'Unchecked_Access);

            Max_Length := 0;

            for P in 0 .. Desc.cParams - 1 loop
               declare
                  Name : constant String := Valid_Identifier
                    (BSTR.To_Ada (Param_Names (P + 1), False));
               begin
                  if Name'Length > Max_Length then
                     Max_Length := Name'Length;
                  end if;
               end;
            end loop;

            for P in 0 .. Desc.cParams - 1 loop
               declare
                  ParamName : String := Valid_Identifier
                    (BSTR.To_Ada (Param_Names (P + 1)));
                  Name      : String (1 .. Max_Length);
               begin
                  if ParamName > "" then
                     Ada.Strings.Fixed.Move (ParamName, Name);
                  else
                     Ada.Strings.Fixed.Move ("P" & Strip (Integer (P + 1)'Img),
                                             Name);
                  end if;

                  Put (Bind_Buffer, Name);

                  Put (Bind_Buffer, " : " &
                       Type_Kind (Desc.lprgelemdescParam (P).tdesc,
                                  Type_Info,
                                  Pre_Buffer'Access,
                                  ParamName));

                  if P < Desc.cParams - 1 then
                     Put_Line (Bind_Buffer, ";");
                     Put (Bind_Buffer, "   ");
                  end if;
               end;
            end loop;

            if Desc.cParams > 0 then
               Put (Bind_Buffer, ")");
            end if;

            if Desc.elemdescFunc.tdesc.vt = VT_VOID then
               Put_Line (Bind_Buffer, ";");
            else
               New_Line (Bind_Buffer);
               Put_Line (Bind_Buffer,
                         "  return " &
                         Type_Kind (Desc.elemdescFunc.tdesc,
                                    Type_Info,
                                    Pre_Buffer'Access,
                                    "") &
                         ";");
            end if;

            GetDocumentation (Type_Info,
                              Desc.memid,
                              null,
                              Func_Desc'Unchecked_Access,
                              null,
                              null);

            if BSTR.To_Ada (Func_Desc, False) > "" then
               Put_Comment
                 (Bind_Buffer, BSTR.To_Ada (Func_Desc, False));
            end if;

            BSTR.Free (Func_Desc);

            declare
               use type Interfaces.C.long;

               Dll_Name  : aliased GNATCOM.Types.BSTR;
               Dll_Entry : aliased GNATCOM.Types.BSTR;
               Ordinal   : aliased Interfaces.C.short;
            begin
               GetDllEntry (Type_Info,
                            Desc.memid,
                            Desc.invkind,
                            Dll_Name'Unchecked_Access,
                            Dll_Entry'Unchecked_Access,
                            Ordinal'Unchecked_Access);
               BSTR.Free (Dll_Name);

               if Desc.callconv = CC_CDECL then
                  Put (Bind_Buffer, "pragma Import (C, ");
               else
                  Put (Bind_Buffer, "pragma Import (StdCall, ");
               end if;

               Put_Line (Bind_Buffer,
                         Valid_Identifier (GetFunctionName (Type_Info, Desc)) &
                         ", """ &
                         BSTR.To_Ada (Dll_Entry) &
                         """);");
            end;

            ReleaseFuncDesc (Type_Info, Desc);
         end;

         New_Line (Bind_Buffer);

      end loop;

      ReleaseTypeAttr (Type_Info, Attribs);

      Decrease_Indent (Bind_Buffer);

      Put_Line (Bind_Buffer,
                "end " & To_String (Base_Package) & "." & Bind_Name & ";");

      Write (From_Buffer => Pre_Buffer,
             To_Buffer   => Spec_Buffer);

      Write (From_Buffer => Bind_Buffer,
             To_Output   => Module_File);

      Close (Module_File);

      Clear (Pre_Buffer);
      Clear (Bind_Buffer);
   end Bind_Module;

   -- Bind_Thin_Elements --

   procedure Bind_Thin_Elements
     (Type_Info   : in      GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      Bind_Buffer : in out  Source_Buffer_Type;
      Bind_Name   : in      String)
   is
      use type Interfaces.C.short;

      Attribs    : GNATCOM.Types.Pointer_To_TYPEATTR;
      Max_Length : Natural := 0;
      Pre_Buffer : aliased Source_Buffer_Type;
   begin
      Attribs := GetTypeAttr (Type_Info);

      if Attribs.cImplTypes > 0 then
         declare
            Ref_Info : ITypeInfo_Type;
         begin
            Attach (Ref_Info,
                    GetRefTypeInfo (Type_Info,
                                    GetRefTypeOfImplType (Type_Info, 0)));

            Bind_Thin_Elements (Ref_Info, Bind_Buffer, Bind_Name);
         end;
      end if;

      for N in  0 .. Attribs.cFuncs - 1 loop
         declare
            Desc      : constant GNATCOM.Types.Pointer_To_FUNCDESC :=
              GetFuncDesc (Type_Info, Interfaces.C.int (N));
            Func_Desc : aliased GNATCOM.Types.BSTR;

            Param_Names   : aliased GNATCOM.Types.BSTR_PARAM_ARRAY;
            cNames        : aliased Interfaces.C.unsigned;
         begin
            Put_Line (Bind_Buffer,
                      "type af_" &
                      Bind_Name &
                      "_" &
                      Valid_Identifier (GetFunctionName (Type_Info, Desc)) &
                      " is access");
            Put (Bind_Buffer, "  function (");

            GetNames (Type_Info,
                      Desc.memid,
                      Param_Names'Unchecked_Access,
                      GNATCOM.Types.MAX_PARAMS,
                      cNames'Unchecked_Access);

            Max_Length := 4; -- Legth of 'This' parameter

            for P in 0 .. Desc.cParams - 1 loop
               declare
                  Name : constant String := Valid_Identifier
                    (BSTR.To_Ada (Param_Names (P + 1), False));
               begin
                  if Name'Length > Max_Length then
                     Max_Length := Name'Length;
                  end if;
               end;
            end loop;

            declare
               Name : String (1 .. Max_Length);
            begin
               Ada.Strings.Fixed.Move ("This", Name);
               Put_Line (Bind_Buffer, Name & " : access");
               Put (Bind_Buffer,
                    "              " &
                    "GNATCOM.Create.COM_Interface.COM_Interface_Type");
               if Desc.cParams > 0 then
                  Put_Line (Bind_Buffer, ";");
                  Put (Bind_Buffer, "            ");
               end if;
            end;

            for P in 0 .. Desc.cParams - 1 loop
               declare
                  ParamName : String := Valid_Identifier
                    (BSTR.To_Ada (Param_Names (P + 1)));
                  Name      : String (1 .. Max_Length);
               begin
                  if ParamName > "" then
                     Ada.Strings.Fixed.Move (ParamName, Name);
                  else
                     Ada.Strings.Fixed.Move ("P" & Strip (Integer (P + 1)'Img),
                                             Name);
                  end if;

                  Put (Bind_Buffer,
                       Name &
                       " : " &
                       Type_Kind (Desc.lprgelemdescParam (P).tdesc,
                                  Type_Info,
                                  Pre_Buffer'Access,
                                  ParamName));

                  if P < Desc.cParams - 1 then
                     Put_Line (Bind_Buffer, ";");
                     Put (Bind_Buffer, "            ");
                  end if;
               end;
            end loop;

            Put_Line (Bind_Buffer, ")");

            Put_Line (Bind_Buffer,
                      "  return " &
                      Type_Kind (Desc.elemdescFunc.tdesc,
                                 Type_Info,
                                 Pre_Buffer'Access,
                                 "") &
                      ";");

            GetDocumentation (Type_Info,
                              Desc.memid,
                              null,
                              Func_Desc'Unchecked_Access,
                              null,
                              null);

            if BSTR.To_Ada (Func_Desc, False) > "" then
               Put_Comment
                 (Bind_Buffer, BSTR.To_Ada (Func_Desc, False));
            end if;

            BSTR.Free (Func_Desc);

            Put_Line (Bind_Buffer, "pragma Convention (StdCall, af_" &
                      Bind_Name & "_" &
                      Valid_Identifier (GetFunctionName (Type_Info, Desc)) &
                      ");");

            ReleaseFuncDesc (Type_Info, Desc);
         end;

         New_Line (Bind_Buffer);

      end loop;

      ReleaseTypeAttr (Type_Info, Attribs);
      Clear (Pre_Buffer);
   end Bind_Thin_Elements;

   -- Bind_Thick_Elements --

   procedure Bind_Thick_Elements
     (Type_Info   : in      GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      Head_Buffer : in out  Source_Buffer_Type;
      Body_Buffer : in out  Source_Buffer_Type;
      Bind_Name   : in      String;
      Class_Name  : in      String)
   is
      use type Interfaces.C.short;
      use type Interfaces.C.unsigned_short;

      Attribs    : GNATCOM.Types.Pointer_To_TYPEATTR;
      Max_Length : Natural := 0;
      Pre_Buffer : aliased Source_Buffer_Type;
   begin
      Attribs := GetTypeAttr (Type_Info);

      if Attribs.cImplTypes > 0 then
         declare
            Ref_Info : ITypeInfo_Type;
            Ref_Attr : GNATCOM.Types.Pointer_To_TYPEATTR;
         begin
            Attach (Ref_Info,
                    GetRefTypeInfo (Type_Info,
                                    GetRefTypeOfImplType (Type_Info, 0)));

            Ref_Attr := GetTypeAttr (Ref_Info);

            if
              Ref_Attr.guid /= GNATCOM.Types.IID_IUnknown
            then
               Bind_Thick_Elements (Ref_Info, Head_Buffer,
                                    Body_Buffer, Bind_Name, Class_Name);
            end if;

            ReleaseTypeAttr (Ref_Info, Ref_Attr);
         end;
      end if;

      for N in  0 .. Attribs.cFuncs - 1 loop
         declare
            Desc        : constant GNATCOM.Types.Pointer_To_FUNCDESC :=
              GetFuncDesc (Type_Info, Interfaces.C.int (N));
            Func_Name   : constant String := Bind_Name & "_" &
              Valid_Identifier (GetFunctionName (Type_Info, Desc));
            Func_Desc   : aliased GNATCOM.Types.BSTR;

            Param_Names : aliased GNATCOM.Types.BSTR_PARAM_ARRAY;
            cNames      : aliased Interfaces.C.unsigned;
         begin
            GetNames (Type_Info,
                      Desc.memid,
                      Param_Names'Unchecked_Access,
                      GNATCOM.Types.MAX_PARAMS,
                      cNames'Unchecked_Access);

            Max_Length := 4; -- Legth of 'This' and 'Free' parameters

            for P in 0 .. Desc.cParams - 1 loop
               declare
                  Name : constant String := Valid_Identifier
                    (BSTR.To_Ada (Param_Names (P + 1), False));
               begin
                  if Name'Length > Max_Length then
                     Max_Length := Name'Length;
                  end if;
               end;
            end loop;

            Put_Line (Head_Buffer, "function " & Func_Name);
            Put_Line (Body_Buffer, "function " & Func_Name);

            Put (Head_Buffer, "  (");
            Increase_Indent (Head_Buffer);

            Put (Body_Buffer, "  (");
            Increase_Indent (Body_Buffer);

            declare
               Name : String (1 .. Max_Length);
            begin
               Ada.Strings.Fixed.Move ("This", Name);
               Put_Line (Head_Buffer, Name & " : access");
               Put (Head_Buffer,
                    "  " &
                    "GNATCOM.Create.COM_Interface.COM_Interface_Type");

               Put_Line (Body_Buffer, Name & " : access");
               Put (Body_Buffer,
                    "  " &
                    "GNATCOM.Create.COM_Interface.COM_Interface_Type");

               if Desc.cParams > 0 then
                  Put_Line (Head_Buffer, ";");
                  Put_Line (Body_Buffer, ";");
               end if;
            end;

            for P in 0 .. Desc.cParams - 1 loop
               declare
                  ParamName : String := Valid_Identifier
                    (BSTR.To_Ada (Param_Names (P + 1), False));
                  Name      : String (1 .. Max_Length);
               begin
                  if ParamName > "" then
                     Ada.Strings.Fixed.Move (ParamName, Name);
                  else
                     Ada.Strings.Fixed.Move ("P" &
                                             Strip (Integer (P + 1)'Img),
                                             Name);
                  end if;

                  Put (Head_Buffer, Name);
                  Put (Body_Buffer, Name);

                  Put (Head_Buffer, " : " &
                       Type_Kind (Desc.lprgelemdescParam (P).tdesc,
                                  Type_Info,
                                  Pre_Buffer'Access,
                                  ParamName));

                  Put (Body_Buffer, " : " &
                       Type_Kind (Desc.lprgelemdescParam (P).tdesc,
                                  Type_Info,
                                  Pre_Buffer'Access,
                                  ParamName));

                  if P < Desc.cParams - 1 then
                     Put_Line (Head_Buffer, ";");
                     Put_Line (Body_Buffer, ";");
                  end if;
               end;
            end loop;

            Put_Line (Head_Buffer, ")");
            Decrease_Indent (Head_Buffer);
            Put_Line (Body_Buffer, ")");
            Decrease_Indent (Body_Buffer);

            Put_Line (Head_Buffer,
                      "  return " &
                      Type_Kind (Desc.elemdescFunc.tdesc,
                                 Type_Info,
                                 Pre_Buffer'Access,
                                 "") &
                      ";");
            Put_Line (Head_Buffer,
                 "pragma Convention (StdCall, " & Func_Name & ");");

            Put_Line (Body_Buffer,
                      "  return " &
                      Type_Kind (Desc.elemdescFunc.tdesc,
                                 Type_Info,
                                 Pre_Buffer'Access,
                                 ""));

            GetDocumentation (Type_Info,
                              Desc.memid,
                              null,
                              Func_Desc'Unchecked_Access,
                              null,
                              null);

            if BSTR.To_Ada (Func_Desc, False) > "" then
               Put_Comment
                 (Head_Buffer, BSTR.To_Ada (Func_Desc, False));
            end if;

            Put_Line (Body_Buffer, "is");
            Increase_Indent (Body_Buffer);
            Put_Line (Body_Buffer,
                      "Object : Pointer_To_" & Class_Name & "_Type :=");
            Put_Line (Body_Buffer,
                      "  Pointer_To_" & Class_Name & "_Type (This.CoClass);");
            Decrease_Indent (Body_Buffer);
            Put_Line (Body_Buffer, "begin");
            Increase_Indent (Body_Buffer);

            Put_Line (Body_Buffer, "return GNATCOM.E_NOTIMPL;");

            Decrease_Indent (Body_Buffer);

            Put_Line (Body_Buffer, "end " & Func_Name & ";");

            BSTR.Free (Func_Desc);

            ReleaseFuncDesc (Type_Info, Desc);
         end;

         New_Line (Head_Buffer);
         New_Line (Body_Buffer);

      end loop;

      ReleaseTypeAttr (Type_Info, Attribs);
      Clear (Pre_Buffer);
   end Bind_Thick_Elements;

   -- Bind_VtblElements --

   procedure Bind_VtblElements
     (Type_Info   : in      GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      Bind_Buffer : in out  Source_Buffer_Type;
      Bind_Name   : in      String;
      Max_Length  : in out  Natural)
   is
      use type Interfaces.C.short;

      Attribs : GNATCOM.Types.Pointer_To_TYPEATTR;
   begin
      Attribs := GetTypeAttr (Type_Info);

      for N in  0 .. Attribs.cFuncs - 1 loop
         declare
            Desc : constant GNATCOM.Types.Pointer_To_FUNCDESC :=
              GetFuncDesc (Type_Info, Interfaces.C.int (N));
            Name : constant String := Valid_Identifier
              (GetFunctionName (Type_Info, Desc));
         begin
            if Name'Length > Max_Length then
               Max_Length := Name'Length;
            end if;
         end;
      end loop;

      if Attribs.cImplTypes > 0 then
         declare
            Ref_Info : ITypeInfo_Type;
            Ref_Attr : GNATCOM.Types.Pointer_To_TYPEATTR;
         begin
            Attach (Ref_Info,
                    GetRefTypeInfo (Type_Info,
                                    GetRefTypeOfImplType (Type_Info, 0)));

            Ref_Attr := GetTypeAttr (Ref_Info);

            if
              Ref_Attr.guid = GNATCOM.Types.IID_IUnknown
            then
               if Max_Length < 8 then  -- Length of "IUnknown"
                  Max_Length := 8;
               end if;

               declare
                  Name     : String (1 .. Max_Length);
               begin
                  Ada.Strings.Fixed.Move ("IUnknown", Name);
                  Put_Line
                    (Bind_Buffer,
                     Name &
                     " : GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;");
               end;
            else
               Bind_VtblElements
                 (Ref_Info, Bind_Buffer, Bind_Name, Max_Length);
            end if;

            ReleaseTypeAttr (Ref_Info, Ref_Attr);
         end;
      end if;

      for N in  0 .. Attribs.cFuncs - 1 loop
         declare
            Desc     : constant GNATCOM.Types.Pointer_To_FUNCDESC :=
              GetFuncDesc (Type_Info, Interfaces.C.int (N));
            FuncName : constant String := Valid_Identifier
              (GetFunctionName (Type_Info, Desc));
            Name     : String (1 .. Max_Length);
         begin
            Ada.Strings.Fixed.Move (FuncName, Name);
            Put_Line (Bind_Buffer,
                      Name & " : af_" & Bind_Name & "_" & FuncName & " :=");
            Put_Line (Bind_Buffer,
                      "  " & Bind_Name & "_" & FuncName & "'Access;");
         end;
      end loop;

      ReleaseTypeAttr (Type_Info, Attribs);
   end Bind_VtblElements;

   -- Bind_Interface --

   procedure Bind_Interface
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type)
   is
      Bind_Name   : constant String := Valid_Identifier (GetName (Type_Info));
      Pre_Buffer  : aliased Source_Buffer_Type;
      Bind_Buffer : Source_Buffer_Type;
      Attribs     : GNATCOM.Types.Pointer_To_TYPEATTR;
   begin
      if
        not Get (To_Unbounded_String (Bind_Name))
      then
         --  Setup thin buffers and types

         Increase_Indent (Bind_Buffer);
         Increase_Indent (Pre_Buffer);

         Set (To_Unbounded_String (Bind_Name), True);

         Put_Line (Type_Buffer, "subtype " & Bind_Name & " is");
         Put_Line (Type_Buffer,
                   "  GNATCOM.Create.COM_Interface.COM_Interface_Type;");

         if
           not Get (To_Unbounded_String ("Pointer_To_" & Bind_Name))
         then
            Set (To_Unbounded_String ("Pointer_To_" & Bind_Name), True);
            Put_Line (Pointer_Buffer,
                      "type Pointer_To_" &
                      Bind_Name &
                      " is access all " & Bind_Name & ";");
         end if;

         Describe_Element (Type_Info, Bind_Buffer);

         --  Do thin bind

         Attribs := GetTypeAttr (Type_Info);

         Put_Line (Bind_Buffer, "IID_" & Bind_Name &
                   " : aliased GNATCOM.Types.GUID :=");

         Put_Line (Bind_Buffer, "  GNATCOM.GUID.To_GUID (""" &
                   GNATCOM.GUID.To_String (Attribs.guid) &
                   """);");

         New_Line (Bind_Buffer);

         Bind_Thin_Elements (Type_Info, Bind_Buffer, Bind_Name);

         --  Commit thin binding

         Write (From_Buffer => Pre_Buffer,
                To_Buffer   => Spec_Buffer);

         New_Line (Spec_Buffer);

         Write (From_Buffer => Bind_Buffer,
                To_Buffer   => Spec_Buffer);

         ReleaseTypeAttr (Type_Info, Attribs);

         Clear (Pre_Buffer);
         Clear (Bind_Buffer);
      end if;
   end Bind_Interface;

   -- Bind_Dispatch --

   procedure Bind_Dispatch
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type)
   is
      use type Interfaces.C.short;
      use type Interfaces.C.unsigned_short;

      Bind_Name   : constant String := Valid_Identifier (GetName (Type_Info));
      Pre_Buffer  : aliased Source_Buffer_Type;
      Bind_Buffer : Source_Buffer_Type;
      Attribs     : GNATCOM.Types.Pointer_To_TYPEATTR;
      Max_Length  : Natural := 0;
      Is_Dual     : Boolean := False;
   begin
      if
        not Get (To_Unbounded_String (Bind_Name))
      then
         --  Setup thin buffers and types

         Increase_Indent (Bind_Buffer);
         Increase_Indent (Pre_Buffer);

         Attribs := GetTypeAttr (Type_Info);

         if (Attribs.wTypeFlags and GNATCOM.Types.TYPEFLAG_FDUAL) /= 0 then
            Is_Dual := True;
         end if;

         Describe_Element (Type_Info, Bind_Buffer);

         --  Do thin bind

         if Is_Dual then
            declare
               use type Interfaces.C.int;

               Ref_Info : ITypeInfo_Type;
            begin
               Attach (Ref_Info,
                       GetRefTypeInfo (Type_Info,
                                       GetRefTypeOfImplType (Type_Info, -1)));

               Bind_Interface (Ref_Info);
            end;
         else
            Put_Line (Bind_Buffer, "IID_" & Bind_Name &
                      " : aliased GNATCOM.Types.GUID :=");

            Put_Line (Bind_Buffer, "  GNATCOM.GUID.To_GUID (""" &
                      GNATCOM.GUID.To_String (Attribs.guid) &
                      """);");
            New_Line (Bind_Buffer);

            Set (To_Unbounded_String (Bind_Name), True);

            Put_Line (Type_Buffer, "subtype " & Bind_Name &
                      " is GNATCOM.Types.Pointer_To_IDispatch;");

            New_Line (Bind_Buffer);
         end if;

         for N in  0 .. Attribs.cVars - 1 loop
            declare
               Desc     : constant GNATCOM.Types.Pointer_To_VARDESC :=
                 GetVarDesc (Type_Info, Interfaces.C.int (N));
               Var_Name : aliased GNATCOM.Types.BSTR;
            begin
               GetDocumentation (Type_Info,
                                 Desc.memid,
                                 Var_Name'Unchecked_Access,
                                 null,
                                 null,
                                 null);

               declare
                  Name : constant String :=
                    Valid_Identifier (BSTR.To_Ada (Var_Name));
               begin
                  if Name'Length > Max_Length then
                     Max_Length := Name'Length;
                  end if;
               end;

               ReleaseVarDesc (Type_Info, Desc);
            end;
         end loop;

         for N in  0 .. Attribs.cFuncs - 1 loop
            declare
               Desc : constant GNATCOM.Types.Pointer_To_FUNCDESC :=
                 GetFuncDesc (Type_Info, Interfaces.C.int (N));
               Name : constant String :=
                 Valid_Identifier (GetFunctionName (Type_Info, Desc));
            begin
               if
                 (not Is_Dual)
                 or else
                 (N > 6)
               then
                  if Name'Length > Max_Length then
                     Max_Length := Name'Length;
                  end if;
               end if;

               ReleaseFuncDesc (Type_Info, Desc);
            end;
         end loop;

         for N in  0 .. Attribs.cVars - 1 loop
            declare
               Desc     : constant GNATCOM.Types.Pointer_To_VARDESC :=
                 GetVarDesc (Type_Info, Interfaces.C.int (N));
               Var_Name : aliased GNATCOM.Types.BSTR;
               Name     : String (1 .. Max_Length);
            begin
               GetDocumentation (Type_Info,
                                 Desc.memid,
                                 Var_Name'Unchecked_Access,
                                 null,
                                 null,
                                 null);

               Ada.Strings.Fixed.Move
                 (Valid_Identifier (BSTR.To_Ada (Var_Name)),
                  Name);

               Put_Line
                 (Bind_Buffer,
                  Bind_Name & "_" & Name & " : constant := " &
                  Strip (Desc.memid'Img) & ";");

               ReleaseVarDesc (Type_Info, Desc);
            end;
         end loop;

         for N in  0 .. Attribs.cFuncs - 1 loop
            declare
               Desc      : constant GNATCOM.Types.Pointer_To_FUNCDESC :=
                 GetFuncDesc (Type_Info, Interfaces.C.int (N));
               Func_Name : constant String :=
                 Valid_Identifier (GetFunctionName (Type_Info, Desc));
               Name      : String (1 .. Max_Length);
            begin
               if
                 (not Is_Dual)
                 or else
                 (N > 6)
               then
                  Ada.Strings.Fixed.Move
                    (Valid_Identifier (Func_Name), Name);

                  Put_Line
                    (Bind_Buffer,
                     Bind_Name & "_" & Name & " : constant := " &
                     Strip (Desc.memid'Img) & ";");
               end if;

               ReleaseFuncDesc (Type_Info, Desc);
            end;
         end loop;

         New_Line (Bind_Buffer);

         --  Commit thin binding

         Write (From_Buffer => Pre_Buffer,
                To_Buffer   => Spec_Buffer);

         New_Line (Spec_Buffer);

         Write (From_Buffer => Bind_Buffer,
                To_Buffer   => Spec_Buffer);

         ReleaseTypeAttr (Type_Info, Attribs);

         Clear (Pre_Buffer);
         Clear (Bind_Buffer);
      end if;
   end Bind_Dispatch;

   -- Type_Kind --

   function Type_Kind (Type_Desc : GNATCOM.Types.TYPEDESC;
                       Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
                       Buffer    : access Source_Buffer_Type;
                       Element   : String)
     return String
   is
      use type Interfaces.C.short;
      use type Interfaces.C.unsigned_short;
   begin
      if Type_Desc.vt = VT_PTR then
         declare
            Full_Point_Type : constant String := Valid_Identifier
              (Type_Kind (Type_Desc.u.lptdesc.all, Type_Info, Buffer, ""));
            Point_Type      : constant String := Valid_Identifier
              (Remove_Base_Package (Full_Point_Type));
         begin
            if Pred_Map.Get
              (To_Unbounded_String ("Pointer_To_" & Point_Type))
            then
               return "GNATCOM.Types." & "Pointer_To_" & Point_Type;
            end if;

            if not Get (To_Unbounded_String ("Pointer_To_" & Point_Type)) then
               Set (To_Unbounded_String ("Pointer_To_" & Point_Type), True);
               Put_Line (Pointer_Buffer,
                         "type Pointer_To_" & Point_Type &
                         " is access all " & Full_Point_Type & ";");
            end if;

            return "Pointer_To_" & Point_Type;
         end;
      end if;

      if Type_Desc.vt = VT_CARRAY then
         declare
            Array_Desc : constant GNATCOM.Types.Pointer_To_ARRAYDESC :=
              Type_Desc.u.lpadesc;
            Array_Type : constant String :=
              Type_Kind (Array_Desc.tdescElem, Type_Info, Buffer, "");
            Array_Name : constant String := Remove_Base_Package (Array_Type) &
              "_" &
              Valid_Identifier (GetName (Type_Info)) &
              "_" &
              Valid_Identifier (Element) &
              "_Array";
            Total_Elements : Natural := 0;
         begin
            if not Get (To_Unbounded_String (Array_Name)) then
               Set (To_Unbounded_String (Array_Name), True);
               Put_Line (Buffer.all,
                         "type " & Array_Name & " is");

               Put (Buffer.all, "  array (Integer range");

               for N in 0 .. Array_Desc.cDims - 1 loop
                  if N > 0 then
                     Put (Buffer.all, ",");
                  end if;

                  Put (Buffer.all, Array_Desc.rgbounds (N).lLbound'Img &
                       " .." &
                       Integer'Image
                       (Integer (Array_Desc.rgbounds (N).cElements) +
                        Integer (Array_Desc.rgbounds (N).lLbound) - 1));
                  Total_Elements := Total_Elements +
                    Natural (Array_Desc.rgbounds (N).cElements);
               end loop;

               Put_Line (Buffer.all, ") of " & Array_Type & ";");

               Put_Line (Buffer.all, "Size_Of_" & Array_Name &
                         " : constant := " &
                         Type_Size (Array_Type) & " *" &
                         Total_Elements'Img & ";");
               Size_Map.Set (To_Unbounded_String (Array_Name),
                             To_Unbounded_String ("Size_Of_" & Array_Name));
            end if;

            if Array_Name = Element then
               return To_String (Base_Package) & "." & Array_Name;
            else
               return Array_Name;
            end if;
         end;
      end if;

      if Type_Desc.vt = VT_USERDEFINED then
         declare
            use type Interfaces.C.long;

            Ref_Info   : ITypeInfo_Type;
            The_Info   : ITypeInfo_Type;
         begin
            Attach (Ref_Info, GetRefTypeInfo (Type_Info,
                                              Type_Desc.u.hreftype));

            if GetTypeKind (Ref_Info) = TKIND_COCLASS then
               declare
                  Ref_Ref_Info  : ITypeInfo_Type;
               begin
                  Attach (Ref_Ref_Info, GetRefTypeInfo
                          (Ref_Info,
                           GetRefTypeOfImplType (Ref_Info,
                                                 Interfaces.C.int (0))));

                  The_Info := Ref_Ref_Info;
               end;
            else
               The_Info := Ref_Info;
            end if;

            declare
               User_Type : constant String :=
                 Valid_Identifier (GetName (The_Info));
            begin
               if Pred_Map.Get
                 (To_Unbounded_String (User_Type))
               then
                  if
                    not Get (To_Unbounded_String ("Size_Of_" & User_Type))
                  then
                     declare
                        Attribs : GNATCOM.Types.Pointer_To_TYPEATTR;
                        Size    : Natural;
                     begin
                        Attribs := GetTypeAttr (The_Info);

                        Size := Natural (Attribs.cbSizeInstance) * 8;

                        New_Line (Buffer.all);
                        Put_Line (Buffer.all, "Size_Of_" & User_Type &
                                  " : constant :=" & Size'Img & ";");

                        Size_Map.Set
                          (To_Unbounded_String ("GNATCOM.Types." & User_Type),
                           To_Unbounded_String
                           ("Size_Of_" & User_Type));

                        Set (To_Unbounded_String ("Size_Of_" & User_Type),
                             True);

                        ReleaseTypeAttr (Type_Info, Attribs);
                     end;
                  end if;
                  return "GNATCOM.Types." & User_Type;
               end if;

               if not Get (To_Unbounded_String (User_Type)) then
                  Put_Line ("Binding Dependancy      : " & User_Type);
                  Put_Line ("Binding Dependancy Type : " &
                            Element_Kind (GetTypeKind (The_Info)));

                  case GetTypeKind (The_Info) is
                     when TKIND_ENUM =>
                        Bind_Enumeration (The_Info);
                     when TKIND_RECORD =>
                        Bind_Record (The_Info);
                     when TKIND_MODULE =>
                        Bind_Module (The_Info);
                     when TKIND_INTERFACE =>
                        Bind_Interface (The_Info);
                     when TKIND_DISPATCH =>
                        Bind_Dispatch (The_Info);
                     when TKIND_COCLASS =>
                        Bind_CoClass (The_Info);
                     when TKIND_ALIAS =>
                        Bind_Alias (The_Info);
                     when TKIND_UNION =>
                        Bind_Union (The_Info);
                     when others =>
                        Put_Line ("WARNING - Unknown element type skipped");
                  end case;

                  New_Line (Spec_Buffer);

               end if;

               if User_Type = Element then
                  return To_String (Base_Package) & "." & User_Type;
               else
                  return User_Type;
               end if;
            end;
         end;
      end if;

      return To_Ada_Type (Type_Desc.vt);
   end Type_Kind;

   -- Type_Size --

   function Type_Size (Type_Name : String) return String is
      TSize  : constant String :=
        To_String (Size_Map.Get (To_Unbounded_String
                                 ("GNATCOM.Types." & Type_Name)));
      TSize2  : constant String :=
        To_String (Size_Map.Get (To_Unbounded_String (Type_Name)));
   begin
      if TSize /= "" then
         return TSize;
      elsif TSize2 /= "" then
         return TSize2;
      elsif Ada.Strings.Fixed.Index (Type_Name, "Pointer_To_") > 0 then
         return "GNATCOM.Types.Size_Of_Pointers";
      else
         return Type_Name & "'Size";
      end if;
   end Type_Size;

   -- Strip --

   function Strip (Image_String : String) return String
   is
   begin
      if Image_String (Image_String'First) = ' ' then
         return Image_String (Image_String'First + 1 .. Image_String'Last);
      else
         return Image_String;
      end if;
   end Strip;

   -- Element_Kind --

   function Element_Kind (Kind : GNATCOM.Types.TYPEKIND) return String is
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

   -- To_Ada_Type --

   function To_Ada_Type (Variant_Type : GNATCOM.Types.VARTYPE) return String
   is
   begin
      case Variant_Type is
         when VT_EMPTY =>
            return "Unsupported";
         when VT_NULL =>
            return "Unsupported";
         when VT_I2 =>
            return "Interfaces.C.short";
         when VT_I4 =>
            return "Interfaces.C.long";
         when VT_R4 =>
            return "Interfaces.C.C_float";
         when VT_R8 =>
            return "Interfaces.C.double";
         when VT_CY =>
            return "GNATCOM.Types.CURRENCY";
         when VT_DATE =>
            return "GNATCOM.Types.DATE";
         when VT_BSTR =>
            return "GNATCOM.Types.BSTR";
         when VT_DISPATCH =>
            return "GNATCOM.Types.Pointer_To_IDispatch";
         when VT_ERROR =>
            return "GNATCOM.Types.SCODE";
         when VT_BOOL =>
            return "GNATCOM.Types.VARIANT_BOOL";
         when VT_VARIANT =>
            return "GNATCOM.Types.VARIANT";
         when VT_UNKNOWN =>
            return "GNATCOM.Types.Pointer_To_IUnknown";
         when VT_I1 =>
            return "Interfaces.C.char";
         when VT_UI1 =>
            return "Interfaces.C.unsigned_char";
         when VT_UI2 =>
            return "Interfaces.C.unsigned_short";
         when VT_UI4 =>
            return "Interfaces.C.unsigned_long";
         when VT_I8 =>
            return "GNATCOM.Types.LONGLONG";
         when VT_UI8 =>
            return "GNATCOM.Types.DWORDLONG";
         when VT_INT =>
            return "Interfaces.C.int";
         when VT_UINT =>
            return "Interfaces.C.unsigned";
         when VT_VOID =>
            return "GNATCOM.Types.Void";
         when VT_HRESULT =>
            return "GNATCOM.Types.HRESULT";
         when VT_PTR =>
            return "access";
         when VT_SAFEARRAY =>
            return "GNATCOM.Types.Pointer_To_SAFEARRAY";
         when VT_CARRAY =>
            return "access";
         when VT_USERDEFINED =>
            return "Unsupported";
         when VT_LPSTR =>
            return "GNATCOM.Types.LPSTR";
         when VT_LPWSTR =>
            return "GNATCOM.Types.LPWSTR";
         when VT_FILETIME =>
            return "GNATCOM.Types.FILETIME";
         when VT_BLOB =>
            return "GNATCOM.Types.BLOB";
         when VT_STREAM =>
            return "Unsupported";
         when VT_STORAGE =>
            return "Unsupported";
         when VT_STREAMED_OBJECT =>
            return "Unsupported";
         when VT_STORED_OBJECT =>
            return "Unsupported";
         when VT_BLOB_OBJECT =>
            return "Unsupported";
         when VT_CF =>
            return "Unsupported";
         when VT_CLSID =>
            return "GNATCOM.Types.GUID";
         when others =>
            return "Unsupported";
      end case;
   end To_Ada_Type;

   -- Is_Reserved_Word --

   function Is_Reserved_Word (Identifier : String) return Boolean is
      Reserved_Words : constant String :=
        " abort else new select abs elsif not separate abstract end null" &
        " subtype accept entry access exception of tagged aliased exit or" &
        " task all others terminate and for out then array function type" &
        " at package generic pragma until begin goto private use body" &
        " procedure if protected when case in while constant is raise" &
        " with range declare limited record xor delay loop rem delta" &
        " renames digits mod requeue do return reverse free true false" &
        " retval ";
      Search_String : constant String :=
        Ada.Strings.Fixed.Translate (Identifier,
                                     Lower_Case_Map);
   begin
      if Ada.Strings.Fixed.Index (Source  => Reserved_Words,
                                  Pattern => " " & Search_String & " ") > 0
      then
         return True;
      end if;

      return False;
   end Is_Reserved_Word;

   -- Valid_Identifier --

   function Valid_Identifier (Identifier : String) return String is
      New_String : String := Identifier;
   begin
      if Identifier'Length = 0 then
         return Identifier;
      end if;

      if New_String (New_String'First) = '_' then
         New_String (New_String'First) := Under_Line_Replace;
      end if;

      if New_String (New_String'Last) = '_' then
         New_String (New_String'Last) := Under_Line_Replace;
      end if;

      for N in New_String'First + 1 .. New_String'Last loop

         if (New_String (N - 1) = '_') and (New_String (N) = '_') then
            New_String (N - 1) := Under_Line_Replace;
         end if;

      end loop;

      if Is_Reserved_Word (New_String) then
         return Under_Line_Replace & New_String;
      end if;

      return New_String;
   end Valid_Identifier;

   -- Remove_Base_Package --

   function Remove_Base_Package (Identifier : String) return String is
      Pos : Natural;
   begin
      Pos := Ada.Strings.Fixed.Index (Identifier, ".", Ada.Strings.Backward);

      if Pos = 0 then
         return Identifier;
      else
         return Identifier (Pos + 1 .. Identifier'Last);
      end if;

   end Remove_Base_Package;

   -- Cify --

   function Cify (File_Name : String) return String is
      Tmp : String (File_Name'First .. File_Name'Last * 2);
      C   : Integer := File_Name'First;
   begin
      for N in File_Name'Range loop

         if File_Name (N) = '\' then
            Tmp (C) := '\';
            C := C + 1;
            Tmp (C) := '\';
         else
            Tmp (C) := File_Name (N);
         end if;

         C := C + 1;
      end loop;

      return Tmp (Tmp'First .. C - 1);
   end Cify;

   -- Unbounded_String_Hash --

   function Unbounded_String_Hash (F : Unbounded_String) return Map_Range is
   begin
      return String_Hash (To_String (F));
   end Unbounded_String_Hash;

   -- String_Equal --

   function String_Equal (F1, F2 : Unbounded_String) return Boolean is
   begin
      return F1 = F2;
   end String_Equal;

   -- Init_Predef --

   procedure Init_Predef is
   begin
      Pred_Map.Set (To_Unbounded_String ("HRESULT"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_HRESULT"), True);
      Pred_Map.Set (To_Unbounded_String ("SCODE"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_SCODE"), True);
      Pred_Map.Set (To_Unbounded_String ("Void"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_Void"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_Pointer_To_Void"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_char"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_Pointer_To_char"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_wchar_t"), True);
      Pred_Map.Set (To_Unbounded_String ("LPSTR"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_LPSTR"), True);
      Pred_Map.Set (To_Unbounded_String ("LPWSTR"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_LPWSTR"), True);
      Pred_Map.Set (To_Unbounded_String ("CURRENCY"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_CURRENCY"), True);
      Pred_Map.Set (To_Unbounded_String ("DATE"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_DATE"), True);
      Pred_Map.Set (To_Unbounded_String ("VARIANT_BOOL"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_VARIANT_BOOL"), True);
      Pred_Map.Set (To_Unbounded_String ("LONGLONG"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_LONGLONG"), True);
      Pred_Map.Set (To_Unbounded_String ("DWORD"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_DWORD"), True);
      Pred_Map.Set (To_Unbounded_String ("DWORDLONG"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_DWORDLONG"), True);
      Pred_Map.Set (To_Unbounded_String ("BYTE"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_BYTE"), True);
      Pred_Map.Set (To_Unbounded_String ("bool"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_bool"), True);
      Pred_Map.Set (To_Unbounded_String ("DECIMAL"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_DECIMAL"), True);
      Pred_Map.Set (To_Unbounded_String ("BLOB"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_BLOB"), True);
      Pred_Map.Set (To_Unbounded_String ("SAFEARRAYBOUND"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_SAFEARRAYBOUND"), True);
      Pred_Map.Set (To_Unbounded_String ("SAFEARRAY"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_SAFEARRAY"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_Pointer_To_SAFEARRAY"), True);
      Pred_Map.Set (To_Unbounded_String ("GUID"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_GUID"), True);
      Pred_Map.Set (To_Unbounded_String ("BSTR"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_BSTR"), True);
      Pred_Map.Set (To_Unbounded_String ("VARIANT"), True);
      Pred_Map.Set (To_Unbounded_String ("IEnumVARIANT"), True);
      Pred_Map.Set (To_Unbounded_String ("IGlobalInterfaceTable"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_int"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_short"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_long"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_double"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_C_float"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_unsigned"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_unsigned_long"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_unsigned_short"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_VARIANT"), True);
      Pred_Map.Set (To_Unbounded_String ("VARTYPE"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_VARTYPE"), True);
      Pred_Map.Set (To_Unbounded_String ("DISPPARAMS"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_DISPPARAMS"), True);
      Pred_Map.Set (To_Unbounded_String ("EXCEPINFO"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_EXCEPINFO"), True);
      Pred_Map.Set (To_Unbounded_String ("CLSCTX"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_CLSCTX"), True);
      Pred_Map.Set (To_Unbounded_String ("TYPEKIND"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_TYPEKIND"), True);
      Pred_Map.Set (To_Unbounded_String ("SYSKIND"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_TO_SYSKIND"), True);
      Pred_Map.Set (To_Unbounded_String ("DESCKIND"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_DESCKIND"), True);
      Pred_Map.Set (To_Unbounded_String ("FUNCKIND"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_FUNCKIND"), True);
      Pred_Map.Set (To_Unbounded_String ("INVOKEKIND"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_INVOKEKIND"), True);
      Pred_Map.Set (To_Unbounded_String ("CALLCONV"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_CALLCONV"), True);
      Pred_Map.Set (To_Unbounded_String ("VARKIND"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_VARKIND"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_TYPEDESC"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_ARRAYDESC"), True);
      Pred_Map.Set (To_Unbounded_String ("TYPEDESC"), True);
      Pred_Map.Set (To_Unbounded_String ("IDLDESC"), True);
      Pred_Map.Set (To_Unbounded_String ("TYPEATTR"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_TYPEATTR"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_Pointer_To_TYPEATTR"), True);
      Pred_Map.Set (To_Unbounded_String ("ARRAYDESC"), True);
      Pred_Map.Set (To_Unbounded_String ("PARAMDESC"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_PARAMDESC"), True);
      Pred_Map.Set (To_Unbounded_String ("ELEMDESC"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_ELEMDESC"), True);
      Pred_Map.Set (To_Unbounded_String ("FUNCDESC"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_FUNCDESC"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_Pointer_To_FUNCDESC"), True);
      Pred_Map.Set (To_Unbounded_String ("VARDESC"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_VARDESC"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_Pointer_To_VARDESC"), True);
      Pred_Map.Set (To_Unbounded_String ("TLIBATTR"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_TLIBATTR"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_Pointer_To_TLIBATTR"), True);
      Pred_Map.Set (To_Unbounded_String ("LICINFO"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_LICINFO"), True);
      Pred_Map.Set (To_Unbounded_String ("CONNECTDATA"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_CONNECTDATA"), True);

      Size_Map.Set (To_Unbounded_String ("GNATCOM.Types.BSTR"),
                    To_Unbounded_String ("GNATCOM.Types.Size_Of_Pointers"));
      Set (To_Unbounded_String ("Size_Of_BSTR"), True);

      Size_Map.Set (To_Unbounded_String ("GNATCOM.Types.LPSTR"),
                    To_Unbounded_String ("GNATCOM.Types.Size_Of_Pointers"));
      Set (To_Unbounded_String ("Size_Of_LPSTR"), True);

      Size_Map.Set (To_Unbounded_String ("GNATCOM.Types.LPWSTR"),
                    To_Unbounded_String ("GNATCOM.Types.Size_Of_Pointers"));
      Set (To_Unbounded_String ("Size_Of_LPWSTR"), True);

      Size_Map.Set (To_Unbounded_String ("GNATCOM.Types.GUID"),
                    To_Unbounded_String ("GNATCOM.Types.Size_Of_GUID"));
      Set (To_Unbounded_String ("Size_Of_GUID"), True);

      Size_Map.Set (To_Unbounded_String ("GNATCOM.Types.VARIANT"),
                    To_Unbounded_String ("GNATCOM.Types.Size_Of_VARIANT"));
      Set (To_Unbounded_String ("Size_Of_VARIANT"), True);

      Size_Map.Set (To_Unbounded_String ("GNATCOM.Types.CURRENCY"),
                    To_Unbounded_String ("GNATCOM.Types.Size_Of_CURRENCY"));
      Set (To_Unbounded_String ("Size_Of_CURRENCY"), True);

      Size_Map.Set (To_Unbounded_String ("GNATCOM.Types.LONGLONG"),
                    To_Unbounded_String ("GNATCOM.Types.Size_Of_LONGLONG"));
      Set (To_Unbounded_String ("Size_Of_LONGLONG"), True);

      Size_Map.Set (To_Unbounded_String ("GNATCOM.Types.DWORDLONG"),
                    To_Unbounded_String ("GNATCOM.Types.Size_Of_DWORDLONG"));
      Set (To_Unbounded_String ("Size_Of_DWORDLONG"), True);

      Size_Map.Set (To_Unbounded_String ("GNATCOM.Types.BLOB"),
                    To_Unbounded_String ("GNATCOM.Types.Size_Of_BLOB"));
      Set (To_Unbounded_String ("Size_Of_BLOB"), True);
   end Init_Predef;

end Create_COM;
