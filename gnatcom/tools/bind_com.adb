------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                             B I N D _ C O M                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.4 $
--                                                                          --
--                  Copyright (C) 1999-2004, 2006 David Botton                    --
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
with GNATCOM.Errors;
with Source_Buffer; use Source_Buffer;

package body Bind_COM is
   package BSTR renames GNATCOM.BSTR;
   package VARIANT renames GNATCOM.VARIANT;

   Base_Package   : Unbounded_String;
   Base_Spec_File : File_Type;
   Lib            : GNATCOM.ITypeLib_Interface.ITypeLib_Type;
   Type_Buffer    : Source_Buffer_Type;
   Spec_Buffer    : Source_Buffer_Type;
   Pointer_Buffer : Source_Buffer_Type;
   Event_Buffer   : Source_Buffer_Type;

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

   package Event_Map is new Simple_HTable
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

   procedure Bind_Dispatch_Events
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type);
   --  Bind Dispinterfaces Events

   procedure Bind_Thin_Elements
     (Type_Info   : in      GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      Bind_Buffer : in out  Source_Buffer_Type;
      Bind_Name   : in      String);
   --  Thin Bind elements

   procedure Bind_Thick_Elements
     (Type_Info   : in      GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      Head_Buffer : in out  Source_Buffer_Type;
      Body_Buffer : in out  Source_Buffer_Type;
      Bind_Name   : in      String);
   --  Thick Bind elements

   procedure Bind_VtblElements
     (Type_Info   : in     GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      Bind_Buffer : in out Source_Buffer_Type;
      Bind_Name   : in     String;
      Max_Length  : in out Natural);
   --  Binds elements of a Vtbl

   procedure Bind_Thick_Dispatch
     (Type_Info   : in      GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      Head_Buffer : in out  Source_Buffer_Type;
      Body_Buffer : in out  Source_Buffer_Type;
      Bind_Name   : in      String);
   --  Thick bind dispinterfaces

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

   procedure Init_Predef;
   --  Initialize predefined types

   -- Bind --

   procedure Bind (GUID      : String;
                   Major_Ver : String;
                   Minor_Ver : String;
                   Base_Name : String)
   is
      function QueryPathOfRegTypeLib
        (guid           : access GNATCOM.Types.GUID;
         wMaj           : Interfaces.C.unsigned_short;
         wMin           : Interfaces.C.unsigned_short;
         lcid           : Interfaces.C.long;
         lpbstrPathName : access GNATCOM.Types.BSTR)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall,
                       QueryPathOfRegTypeLib,
                       "QueryPathOfRegTypeLib");
      LIBID     : aliased GNATCOM.Types.GUID := GNATCOM.GUID.To_GUID (GUID);
      File_Name : aliased GNATCOM.Types.BSTR := null;
   begin
      GNATCOM.Errors.Error_Check
        (QueryPathOfRegTypeLib
         (LIBID'Access,
          Interfaces.C.unsigned_short'Value (Major_Ver),
          Interfaces.C.unsigned_short'Value (Minor_Ver),
          0,
          File_Name'Access));
      Bind (BSTR.To_Ada (File_Name), Base_Name);
   end Bind;

   procedure Bind
     (File_Name : String;
      Base_Name : String)
   is
      use type Interfaces.C.int;
      use type Interfaces.C.unsigned;

      Attribs             : GNATCOM.Types.Pointer_To_TLIBATTR;
      Base_Spec_File_Name : String := Base_Name & ".ads";
      Library_Name        : aliased GNATCOM.Types.BSTR;
      Type_Info           : ITypeInfo_Type;
      Element_Name        : aliased GNATCOM.Types.BSTR;
   begin
      Open (Lib, File_Name);

      Init_Predef;

      Base_Package := To_Unbounded_String (Base_Name);

      To_Lower (Base_Spec_File_Name);
      Create (File => Base_Spec_File,
              Mode => Out_File,
              Name => Base_Spec_File_Name);

      if Verbose then
         Put_Line ("Openning file           : " & File_Name);
      end if;

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

      Put_Line (Spec_Buffer, "LIBID_" & BSTR.To_Ada (Library_Name) &
                " : aliased GNATCOM.Types.GUID :=");
      Put_Line (Spec_Buffer, "  GNATCOM.GUID.To_GUID (""" &
                GNATCOM.GUID.To_String (Attribs.guid) & """);");
      New_Line (Spec_Buffer);

      ReleaseTLibAttr (Lib, Attribs);

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

      New_Line (Spec_Buffer);

      Write (From_Buffer => Type_Buffer,
             To_Output   => Base_Spec_File);

      New_Line (Base_Spec_File);

      Write (From_Buffer => Pointer_Buffer,
             To_Output   => Base_Spec_File);

      New_Line (Base_Spec_File);

      Write (From_Buffer => Spec_Buffer,
             To_Output   => Base_Spec_File);

      Put_Line (Base_Spec_File, "end " & Base_Name & ";");

      Close (Base_Spec_File);

      Clear (Type_Buffer);
      Clear (Spec_Buffer);
      Clear (Pointer_Buffer);
      Clear (Event_Buffer);
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
                   "Generated by             : BindCOM");
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
      Put_Line (Base_Spec_File, "with Ada.Unchecked_Conversion;");
      Put_Line (Base_Spec_File, "with Interfaces.C;");
      New_Line (Base_Spec_File);
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
                   "for " & Bind_Name & "'Size use Size_Of_" &
                   Bind_Name & ";");

         if Attribs.cbAlignment < 8 then
            Put_Line (Bind_Buffer,
                      "for " & Bind_Name & "'Alignment use" &
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
         Set (To_Unbounded_String (Bind_Name), True);

         Put_Line (Type_Buffer, "type " & Bind_Name & ";");

         Put_Line (Bind_Buffer, "type " & Bind_Name & " is");

         Attribs := GetTypeAttr (Type_Info);

         declare
            Element_Type : constant String := Type_Kind (Attribs.tdescAlias,
                                                         Type_Info,
                                                         Pre_Buffer'Access,
                                                         "");
         begin
            Put_Line (Bind_Buffer, "  new " & Element_Type & ";");

            if
            not Get (To_Unbounded_String ("Size_Of_" & Bind_Name))
            then
               if Ada.Strings.Fixed.Index
                 (Element_Type, "Pointer_To_") > 0
               then
                  Size_Map.Set
                    (To_Unbounded_String (Bind_Name),
                     To_Unbounded_String ("GNATCOM.Types.Size_Of_Pointers"));
               else
                  Put_Line (Bind_Buffer,
                            "Size_Of_" & Bind_Name & " : constant := " &
                            Type_Size (Element_Type) & ";");
                  Size_Map.Set
                    (To_Unbounded_String (Bind_Name),
                     To_Unbounded_String ("Size_Of_" & Bind_Name));
               end if;
               Set (To_Unbounded_String ("Size_Of_" & Bind_Name), True);
            end if;
         end;

         ReleaseTypeAttr (Type_Info, Attribs);

         Write (From_Buffer => Pre_Buffer,
                To_Buffer   => Spec_Buffer);

         New_Line (Spec_Buffer);

         Write (From_Buffer => Bind_Buffer,
                To_Buffer   => Spec_Buffer);

         Clear (Pre_Buffer);
         Clear (Bind_Buffer);
      end if;
   end Bind_Alias;

   -- Bind_Dispatch_Events --

   procedure Bind_Dispatch_Events
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type)
   is
      use type Interfaces.C.short;
      use type Interfaces.C.unsigned_short;

      Bind_Name   : constant String := Valid_Identifier (GetName (Type_Info));
      Attribs     : GNATCOM.Types.Pointer_To_TYPEATTR;
      Is_Dual     : Boolean := False;
      Head_File   : File_Type;
      Body_File   : File_Type;
      Head_Buffer : Source_Buffer_Type;
      Body_Buffer : Source_Buffer_Type;
   begin
      if
        not Event_Map.Get (To_Unbounded_String (Bind_Name))
      then
         Put_Line ("Binding Event Interface : " & Bind_Name);

         Event_Map.Set (To_Unbounded_String (Bind_Name), True);
         Put_Line (Event_Buffer, "type " & Bind_Name & ";");

         Attribs := GetTypeAttr (Type_Info);

         if (Attribs.wTypeFlags and GNATCOM.Types.TYPEFLAG_FDUAL) /= 0 then
            Is_Dual := True;
         end if;

         --  Setup Files

         Create (File => Head_File,
                 Mode => Out_File,
                 Name => Ada.Strings.Fixed.Translate
                 (To_String (Base_Package) &
                  "-" &
                  Bind_Name &
                  "_events.ads",
                  Lower_Case_Map));

         Create (File => Body_File,
                 Mode => Out_File,
                 Name => Ada.Strings.Fixed.Translate
                 (To_String (Base_Package) &
                  "-" &
                  Bind_Name &
                  "_events.adb",
                  Lower_Case_Map));

         Put_Line (Head_Buffer, "with GNATCOM.Events.Event_Object;");
         Put_Line (Head_Buffer, "with GNATCOM.Create.COM_Interface;");
         Put_Line (Head_Buffer, "with GNATCOM.IInterface;");
         New_Line (Head_Buffer);
         Put_Line (Head_Buffer,
                   "package " &
                   To_String (Base_Package) &
                   "." &
                   Bind_Name &
                   "_Events is");
         Increase_Indent (Head_Buffer);
         New_Line (Head_Buffer);
         Put_Line (Head_Buffer, "type " & Bind_Name & "_Event is");
         Put_Line (Head_Buffer,
                   "  new GNATCOM.Events.Event_Object.Event_Type" &
                   " with null record;");
         New_Line (Head_Buffer);

         Put_Line (Head_Buffer,
                   "function Create (From :" &
                    " in GNATCOM.Events.Event_Object.Event_Pointer)");
         Put_Line
           (Head_Buffer,
            "  return GNATCOM.Create.COM_Interface." &
            "Pointer_To_COM_Interface_Type;");
         New_Line (Head_Buffer);

         Put_Line (Head_Buffer, "procedure Set_Events");
         Put_Line
           (Head_Buffer,
            "  (This            : in out " &
            "GNATCOM.Events.IConnectionPoint_Type;");
         Increase_Indent (Head_Buffer);
         Put_Line
           (Head_Buffer,
            "For_Object      : in     " &
            "GNATCOM.IInterface.Interface_Type'Class;");
         Put_Line (Head_Buffer, "Event_Interface :");
         Put_Line
           (Head_Buffer,
            "  access GNATCOM.Create.COM_Interface.COM_Interface_Type;");
         Put_Line (Head_Buffer, "Free            : Boolean := True);");
         Decrease_Indent (Head_Buffer);
         New_Line (Head_Buffer);

         Put_Line (Body_Buffer,
                   "package body " &
                   To_String (Base_Package) &
                   "." &
                   Bind_Name &
                   "_Events is");
         Increase_Indent (Body_Buffer);
         New_Line (Body_Buffer);

         --  Set up Invoke

         Put_Line (Body_Buffer, "procedure Invoke");
         Put_Line (Body_Buffer,
                   "  (dispidMember : in Interfaces.C.long;");
         Increase_Indent (Body_Buffer);
         Put_Line (Body_Buffer,
                   "wFlags       : in Interfaces.C.unsigned_short;");
         Put_Line (Body_Buffer,
                   "pdispparams  : in GNATCOM.Types.Pointer_To_DISPPARAMS;");
         Put_Line
           (Body_Buffer,
            "Event_Object : in GNATCOM.Events.Event_Object.Event_Pointer)");
         Decrease_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "is");
         Increase_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "use type Interfaces.C.long;");
         Decrease_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "begin");
         Increase_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "case dispidMember is");
         Increase_Indent (Body_Buffer);

         for N in  0 .. Attribs.cFuncs - 1 loop
            declare
               Desc      : constant GNATCOM.Types.Pointer_To_FUNCDESC :=
                 GetFuncDesc (Type_Info, Interfaces.C.int (N));
               Func_Name : constant String :=
                 Valid_Identifier (GetFunctionName (Type_Info, Desc));
            begin
               if
                 (not Is_Dual)
                 or else
                 (N > 6)
               then
                  Put_Line (Body_Buffer, "when " & Bind_Name &
                            "_" & Func_Name & "=>");
                  Increase_Indent (Body_Buffer);
                  Put_Line (Body_Buffer, Func_Name);
                  Put (Body_Buffer, "  (" & Bind_Name &
                       "_Event'Class (Event_Object.all)");

                  for P in reverse 0 .. Desc.cParams - 1 loop
                     if P = Desc.cParams - 1 then
                        Put_Line (Body_Buffer, ",");
                     end if;

                     Put (Body_Buffer,
                          "   pdispparams.rgvarg (" &
                          Strip (Integer (P)'Img) & ")");

                     if P > 0 then
                        Put_Line (Body_Buffer, ",");
                     end if;
                  end loop;

                  Put_Line (Body_Buffer, ");");
                  Decrease_Indent (Body_Buffer);
               end if;

               ReleaseFuncDesc (Type_Info, Desc);
            end;
         end loop;

         Put_Line (Body_Buffer, "when others =>");
         Increase_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "null;");
         Decrease_Indent (Body_Buffer);
         Decrease_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "end case;");
         Decrease_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "end Invoke;");
         New_Line (Body_Buffer);

         --  Add Create and Set_Event bodies

         Put_Line (Body_Buffer,
                   "function Create (From :" &
                    " in GNATCOM.Events.Event_Object.Event_Pointer)");
         Put_Line
           (Body_Buffer,
            "  return GNATCOM.Create.COM_Interface." &
            "Pointer_To_COM_Interface_Type");
         Put_Line (Body_Buffer, "is");
         Put_Line (Body_Buffer, "begin");
         Increase_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "return GNATCOM.Events.Event_Object.Create");
         Put_Line (Body_Buffer, "  (Invoke'Access,");
         Increase_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "IID_" & Bind_Name & ",");
         Put_Line (Body_Buffer, "From);");
         Decrease_Indent (Body_Buffer);
         Decrease_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "end Create;");
         New_Line (Body_Buffer);

         Put_Line (Body_Buffer, "procedure Set_Events");
         Put_Line
           (Body_Buffer,
            "  (This            : in out " &
            "GNATCOM.Events.IConnectionPoint_Type;");
         Increase_Indent (Body_Buffer);
         Put_Line
           (Body_Buffer,
            "For_Object      : in     " &
            "GNATCOM.IInterface.Interface_Type'Class;");
         Put_Line (Body_Buffer, "Event_Interface :");
         Put_Line
           (Body_Buffer,
            "  access GNATCOM.Create.COM_Interface.COM_Interface_Type;");
         Put_Line (Body_Buffer, "Free            : Boolean := True)");
         Decrease_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "is");
         Put_Line (Body_Buffer, "begin");
         Increase_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "GNATCOM.Events.Set_Events");
         Put_Line (Body_Buffer, "  (This,");
         Increase_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "For_Object,");
         Put_Line (Body_Buffer, "IID_" & Bind_Name & ",");
         Put_Line (Body_Buffer, "Event_Interface,");
         Put_Line (Body_Buffer, "Free);");
         Decrease_Indent (Body_Buffer);
         Decrease_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "end Set_Events;");
         New_Line (Body_Buffer);

         --  Set up procedures

         for N in  0 .. Attribs.cFuncs - 1 loop
            declare
               Desc        : constant GNATCOM.Types.Pointer_To_FUNCDESC :=
                 GetFuncDesc (Type_Info, Interfaces.C.int (N));
               Func_Name   : constant String :=
                 Valid_Identifier (GetFunctionName (Type_Info, Desc));
               Max_Length  : Natural := 4;
               Param_Names : aliased GNATCOM.Types.BSTR_PARAM_ARRAY;
               cNames      : aliased Interfaces.C.int;
            begin
               if
                 (not Is_Dual)
                 or else
                 (N > 6)
               then

                  GetNames (Type_Info,
                            Desc.memid,
                            Param_Names'Unchecked_Access,
                            GNATCOM.Types.MAX_PARAMS,
                            cNames'Unchecked_Access);

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

                  Put_Line (Head_Buffer, "procedure " & Func_Name);
                  Put_Line (Body_Buffer, "procedure " & Func_Name);

                  Put (Head_Buffer, "  (");
                  Increase_Indent (Head_Buffer);

                  Put (Body_Buffer, "  (");
                  Increase_Indent (Body_Buffer);

                  declare
                     Name : String (1 .. Max_Length);
                  begin
                     Ada.Strings.Fixed.Move ("This", Name);

                     Put (Head_Buffer, Name & " : " & Bind_Name &
                          "_Event");
                     Put (Body_Buffer, Name & " : " & Bind_Name &
                          "_Event");

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
                           Ada.Strings.Fixed.Move
                             (ParamName, Name);
                        else
                           Ada.Strings.Fixed.Move
                             ("P" & Strip (Integer (P + 1)'Img),
                              Name);
                        end if;

                        Put (Head_Buffer, Name & " : GNATCOM.Types.VARIANT");
                        Put (Body_Buffer, Name & " : GNATCOM.Types.VARIANT");

                        if P < Desc.cParams - 1 then
                           Put_Line (Head_Buffer, ";");
                           Put_Line (Body_Buffer, ";");
                        end if;
                     end;
                  end loop;

                  Put_Line (Head_Buffer, ");");
                  Decrease_Indent (Head_Buffer);
                  New_Line (Head_Buffer);

                  Put_Line (Body_Buffer, ")");
                  Decrease_Indent (Body_Buffer);
                  Put_Line (Body_Buffer, "is");
                  Put_Line (Body_Buffer, "begin");
                  Increase_Indent (Body_Buffer);
                  Put_Line (Body_Buffer, "null;");
                  Decrease_Indent (Body_Buffer);
                  Put_Line (Body_Buffer, "end " & Func_Name & ";");
                  New_Line (Body_Buffer);
               end if;
            end;
         end loop;

         --  Commit

         Decrease_Indent (Head_Buffer);
         Put_Line (Head_Buffer,
                   "end " & To_String (Base_Package) & "." & Bind_Name &
                   "_Events;");

         Decrease_Indent (Body_Buffer);
         Put_Line (Body_Buffer,
                   "end " & To_String (Base_Package) & "." & Bind_Name &
                   "_Events;");

         Write (From_Buffer => Head_Buffer,
                To_Output   => Head_File);

         Write (From_Buffer => Body_Buffer,
                To_Output   => Body_File);

         Close (Head_File);
         Close (Body_File);

         --  Clean up

         ReleaseTypeAttr (Type_Info, Attribs);

         Clear (Head_Buffer);
         Clear (Body_Buffer);
      end if;

   end Bind_Dispatch_Events;

   -- Bind_CoClass --

   procedure Bind_CoClass
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type)
   is
      use type Interfaces.C.short;
      use type Interfaces.C.long;
      use type Interfaces.C.unsigned;

      Bind_Name   : constant String := Valid_Identifier (GetName (Type_Info));
      Attribs     : GNATCOM.Types.Pointer_To_TYPEATTR;
   begin
      Describe_Element (Type_Info, Spec_Buffer);

      Put_Line (Spec_Buffer, "CLSID_" & Bind_Name &
                " : aliased GNATCOM.Types.GUID :=");

      Attribs := GetTypeAttr (Type_Info);

      Put_Line (Spec_Buffer, "  GNATCOM.GUID.To_GUID (""" &
                GNATCOM.GUID.To_String (Attribs.guid) &
                """);");

      if Attribs.cImplTypes > 0 then
         for N in 0 .. Attribs.cImplTypes - 1 loop
            declare
               Ref_Info   : ITypeInfo_Type;
               Ref_Lib    : ITypeLib_Type;
               Ref_Index  : aliased Interfaces.C.int;
            begin

               if
                 (GetImplTypeFlags (Type_Info, Interfaces.C.int (N))
                  and
                  IMPLTYPEFLAG_FSOURCE)
                 /= 0
               then
                  Attach (Ref_Info, GetRefTypeInfo
                          (Type_Info,
                           GetRefTypeOfImplType (Type_Info,
                                                 Interfaces.C.int (N))));

                  Attach (Ref_Lib, GetContainingTypeLib
                          (Ref_Info,
                           Ref_Index'Unchecked_Access));

                  if
                    GetTypeInfoType (Ref_Lib, Ref_Index) = TKIND_DISPATCH
                  then
                     Bind_Dispatch (Ref_Info);
                     New_Line (Spec_Buffer);
                     Bind_Dispatch_Events (Ref_Info);
                  end if;
               end if;
            end;
         end loop;
      end if;

      ReleaseTypeAttr (Type_Info, Attribs);

   end Bind_CoClass;

   -- Bind_Module --

   procedure Bind_Module
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type)
   is
      use type Interfaces.C.unsigned_short;
      use type Interfaces.C.short;

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
                       " : constant Interfaces.C.char_array :=");
                  Put_Line (Bind_Buffer,
                            "  Interfaces.C.To_C (""" &
                            VARIANT.To_Ada (Desc.u.lpvarValue.all) &
                            """);");
               when VT_LPWSTR =>
                  Put_Line (Bind_Buffer,
                       " : constant Interfaces.C.wchar_array :=");
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
            cNames        : aliased Interfaces.C.int;
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
      Ret_Val    : Integer := -1;
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
            use type Interfaces.C.unsigned_short;

            Desc      : constant GNATCOM.Types.Pointer_To_FUNCDESC :=
              GetFuncDesc (Type_Info, Interfaces.C.int (N));
            Func_Desc : aliased GNATCOM.Types.BSTR;

            Param_Names   : aliased GNATCOM.Types.BSTR_PARAM_ARRAY;
            cNames        : aliased Interfaces.C.int;
            Force_Proc    : Boolean := False;
         begin
            Put_Line (Bind_Buffer,
                      "type af_" &
                      Bind_Name &
                      "_" &
                      Valid_Identifier (GetFunctionName (Type_Info, Desc)) &
                      " is access");

            if Desc.elemdescFunc.tdesc.vt = VT_VOID then
               Force_Proc := True;
            end if;

            if Force_Proc then
               Put (Bind_Buffer, "  procedure (");
            else
               Put (Bind_Buffer, "  function (");
            end if;

            GetNames (Type_Info,
                      Desc.memid,
                      Param_Names'Unchecked_Access,
                      GNATCOM.Types.MAX_PARAMS,
                      cNames'Unchecked_Access);

            Max_Length := 4; -- Legth of 'This' parameter
            Ret_Val := -1;

            for P in 0 .. Desc.cParams - 1 loop
               declare
                  Name : constant String := Valid_Identifier
                    (BSTR.To_Ada (Param_Names (P + 1), False));
               begin
                  if (Desc.lprgelemdescParam (P).paramdesc.wParamFlags
                      and GNATCOM.Types.PARAMFLAG_FRETVAL) /= 0
                  then
                     Ret_Val := Integer (P);
                  end if;

                  if Name'Length > Max_Length then
                     Max_Length := Name'Length;
                  end if;
               end;
            end loop;

            declare
               Name : String (1 .. Max_Length);
            begin
               Ada.Strings.Fixed.Move ("This", Name);
               Put (Bind_Buffer, Name & " : access " & Bind_Name);
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

                  if Integer (P) = Ret_Val then
                     Put (Bind_Buffer, "RetVal");
                  else
                     Put (Bind_Buffer, Name);
                  end if;

                  if
                    (Desc.lprgelemdescParam (P).tdesc.vt = VT_PTR)
                    and then
                    (Desc.lprgelemdescParam
                     (P).tdesc.u.lptdesc.vt = VT_VARIANT)
                  then
                     Put (Bind_Buffer, " : access GNATCOM.Types.VARIANT");
                  else
                     Put (Bind_Buffer, " : " &
                          Type_Kind (Desc.lprgelemdescParam (P).tdesc,
                                     Type_Info,
                                     Pre_Buffer'Access,
                                     ParamName));
                  end if;

                  if P < Desc.cParams - 1 then
                     Put_Line (Bind_Buffer, ";");
                     Put (Bind_Buffer, "            ");
                  end if;
               end;
            end loop;

            if Force_Proc then
               Put_Line (Bind_Buffer, ");");
            else
               Put_Line (Bind_Buffer, ")");

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

            Put_Line (Bind_Buffer, "pragma Convention (StdCall, af_" &
                      Bind_Name & "_" &
                      Valid_Identifier (GetFunctionName (Type_Info, Desc)) &
                      ");");

            ReleaseFuncDesc (Type_Info, Desc);
         end;

         New_Line (Bind_Buffer);

      end loop;

      begin
         ReleaseTypeAttr (Type_Info, Attribs);
      exception
         when GNATCOM.Errors.COM_ERROR =>
            null; --  2014-03-18 GdM: Workaround for
                  --  Element # 21, IShellFolderViewDual3, Command:
                  --  bindcom shell32.dll Shell
      end;

      Clear (Pre_Buffer);
   end Bind_Thin_Elements;

   -- Bind_Thick_Elements --

   procedure Bind_Thick_Elements
     (Type_Info   : in      GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      Head_Buffer : in out  Source_Buffer_Type;
      Body_Buffer : in out  Source_Buffer_Type;
      Bind_Name   : in      String)
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
              Ref_Attr.guid /= GNATCOM.Types.IID_IDispatch
              and then
              Ref_Attr.guid /= GNATCOM.Types.IID_IUnknown
            then
               Bind_Thick_Elements (Ref_Info, Head_Buffer,
                                    Body_Buffer, Bind_Name);
            end if;

            ReleaseTypeAttr (Ref_Info, Ref_Attr);
         end;
      end if;

      for N in  0 .. Attribs.cFuncs - 1 loop
         declare
            Desc        : constant GNATCOM.Types.Pointer_To_FUNCDESC :=
              GetFuncDesc (Type_Info, Interfaces.C.int (N));
            Func_Name   : constant String :=
              Valid_Identifier (GetFunctionName (Type_Info, Desc));
            Func_Desc   : aliased GNATCOM.Types.BSTR;

            Param_Names : aliased GNATCOM.Types.BSTR_PARAM_ARRAY;
            cNames      : aliased Interfaces.C.int;
            Ret_Val     : Integer := -1;
            Force_Func  : Boolean := False;
            Force_Proc  : Boolean := False;
            Param_Count : Interfaces.C.short := 0;
            Has_Free    : Boolean := False;
            Free_Buffer : Source_Buffer_Type;
         begin
            Increase_Indent (Free_Buffer);
            Increase_Indent (Free_Buffer);
            Increase_Indent (Free_Buffer);

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
                  if (Desc.lprgelemdescParam (P).paramdesc.wParamFlags
                      and GNATCOM.Types.PARAMFLAG_FRETVAL) /= 0
                  then
                     Ret_Val := Integer (P);
                  end if;

                  if
                    Desc.lprgelemdescParam (P).tdesc.vt = VT_VARIANT
                    or
                    Desc.lprgelemdescParam (P).tdesc.vt = VT_BSTR
                  then
                     Has_Free := True;

                     if Name = "" then
                        Put_Line (Free_Buffer,
                                  "GNATCOM.IInterface.Free (P" &
                                  Strip (Integer (P + 1)'Img)  & ");");
                     else
                        Put_Line (Free_Buffer,
                                  "GNATCOM.IInterface.Free (" & Name & ");");
                     end if;
                  end if;

                  if Name'Length > Max_Length then
                     Max_Length := Name'Length;
                  end if;
               end;
            end loop;

            if Desc.elemdescFunc.tdesc.vt /= VT_HRESULT then
               if Desc.elemdescFunc.tdesc.vt = VT_VOID then
                  Force_Proc := True;
               else
                  Force_Func := True;
               end if;
            end if;

            if Ret_Val > -1 or Force_Func then
               Put_Line (Head_Buffer, "function " & Func_Name);
               Put_Line (Body_Buffer, "function " & Func_Name);
            else
               Put_Line (Head_Buffer, "procedure " & Func_Name);
               Put_Line (Body_Buffer, "procedure " & Func_Name);
            end if;

            Put (Head_Buffer, "  (");
            Increase_Indent (Head_Buffer);

            Put (Body_Buffer, "  (");
            Increase_Indent (Body_Buffer);

            declare
               Name : String (1 .. Max_Length);
            begin
               Ada.Strings.Fixed.Move ("This", Name);
               Put (Head_Buffer, Name & " : " & Bind_Name & "_Type");
               Put (Body_Buffer, Name & " : " & Bind_Name & "_Type");
               if Ret_Val > -1 then
                  if Desc.cParams > 1 then
                     Put_Line (Head_Buffer, ";");
                     Put_Line (Body_Buffer, ";");
                  end if;
               else
                  if Desc.cParams > 0 then
                     Put_Line (Head_Buffer, ";");
                     Put_Line (Body_Buffer, ";");
                  end if;
               end if;
            end;

            for P in 0 .. Desc.cParams - 1 loop
               if Integer (P) /= Ret_Val then
                  declare
                     ParamName : String := Valid_Identifier
                       (BSTR.To_Ada (Param_Names (P + 1), False));
                     Name      : String (1 .. Max_Length);
                  begin
                     Param_Count := Param_Count + 1;

                     if ParamName > "" then
                        Ada.Strings.Fixed.Move (ParamName, Name);
                     else
                        Ada.Strings.Fixed.Move ("P" &
                                                Strip (Integer (P + 1)'Img),
                                                Name);
                     end if;

                     Put (Head_Buffer, Name);
                     Put (Body_Buffer, Name);

                     if
                       (Desc.lprgelemdescParam (P).tdesc.vt = VT_PTR)
                       and then
                       (Desc.lprgelemdescParam
                        (P).tdesc.u.lptdesc.vt = VT_VARIANT)
                     then
                        Put_Line (Head_Buffer,
                                  " : access GNATCOM.Types.VARIANT :=");
                        Put (Head_Buffer, "  GNATCOM.Types.PVARIANT_MISSING");
                        Put_Line (Body_Buffer,
                                  " : access GNATCOM.Types.VARIANT :=");
                        Put (Body_Buffer, "  GNATCOM.Types.PVARIANT_MISSING");
                     else
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
                     end if;

                     if (Desc.lprgelemdescParam (P).paramdesc.wParamFlags
                         and GNATCOM.Types.PARAMFLAG_FOPT) /= 0
                     then
                        if
                          Desc.lprgelemdescParam (P).tdesc.vt = VT_VARIANT
                        then
                           Put (Head_Buffer,
                                "  := GNATCOM.Types.VARIANT_MISSING");
                           Put (Body_Buffer,
                                "  := GNATCOM.Types.VARIANT_MISSING");
                        end if;
                     end if;

                     if Ret_Val > -1 then
                        if Param_Count < Desc.cParams - 1 then
                           Put_Line (Head_Buffer, ";");
                           Put_Line (Body_Buffer, ";");
                        end if;
                     else
                        if Param_Count < Desc.cParams then
                           Put_Line (Head_Buffer, ";");
                           Put_Line (Body_Buffer, ";");
                        end if;
                     end if;
                  end;
               end if;
            end loop;

            if Has_Free then
               Put_Line (Head_Buffer, ";");
               Put_Line (Body_Buffer, ";");
               declare
                  Name : String (1 .. Max_Length);
               begin
                  Ada.Strings.Fixed.Move ("Free", Name);
                  Put (Head_Buffer, Name & " : Boolean := True");
                  Put (Body_Buffer, Name & " : Boolean := True");
               end;
            end if;

            if Ret_Val > -1 or Force_Func then
               Put_Line (Head_Buffer, ")");
               Decrease_Indent (Head_Buffer);
               Put_Line (Body_Buffer, ")");
               Decrease_Indent (Body_Buffer);
            end if;

            if Force_Func then
               Put_Line (Head_Buffer,
                         "  return " &
                           Type_Kind (Desc.elemdescFunc.tdesc,
                                      Type_Info,
                                      Pre_Buffer'Access,
                                      "") &
                           ";");
               Put_Line (Body_Buffer,
                         "  return " &
                           Type_Kind (Desc.elemdescFunc.tdesc,
                                      Type_Info,
                                      Pre_Buffer'Access,
                                      ""));
               Put_Line (Body_Buffer, "is");
            elsif Ret_Val > -1 then
               --  Remove Pointer
               Put_Line (Head_Buffer,
                         "  return " &
                         Type_Kind
                         (Desc.lprgelemdescParam
                          (Interfaces.C.short (Ret_Val)).tdesc.u.lptdesc.all,
                          Type_Info,
                          Pre_Buffer'Access,
                          "") &
                         ";");
               Put_Line (Body_Buffer,
                         "  return " &
                         Type_Kind
                         (Desc.lprgelemdescParam
                          (Interfaces.C.short (Ret_Val)).tdesc.u.lptdesc.all,
                          Type_Info,
                          Pre_Buffer'Access,
                          ""));
               Put_Line (Body_Buffer, "is");
            else
               Put_Line (Head_Buffer, ");");
               Decrease_Indent (Head_Buffer);
               Put_Line (Body_Buffer, ")");
               Decrease_Indent (Body_Buffer);
               Put_Line (Body_Buffer, "is");
            end if;

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

            if Ret_Val > -1 then
               Increase_Indent (Body_Buffer);
               Put_Line (Body_Buffer,
                         " RetVal : aliased " &
                         Type_Kind
                         (Desc.lprgelemdescParam
                          (Interfaces.C.short (Ret_Val)).tdesc.u.lptdesc.all,
                          Type_Info,
                          Pre_Buffer'Access,
                          "") &
                         ";");
               Decrease_Indent (Body_Buffer);
            end if;

            Put_Line (Body_Buffer, "begin");
            Increase_Indent (Body_Buffer);

            if Force_Func then
               Put_Line (Body_Buffer, "return");
               Put (Body_Buffer, "  (");
            elsif Force_Proc = False then
               Put_Line (Body_Buffer, "GNATCOM.Errors.Error_Check");
               Put (Body_Buffer, "  (");
            end if;

            Put_Line (Body_Buffer,
                      "Pointer (This).Vtbl." & Func_Name);
            Increase_Indent (Body_Buffer);
            Put (Body_Buffer, "(Pointer (This)");
            if Desc.cParams > 0 then
               Put_Line (Body_Buffer, ",");
            end if;

            for P in 0 .. Desc.cParams - 1 loop
               declare
                  Name : constant String := Valid_Identifier
                    (BSTR.To_Ada (Param_Names (P + 1)));
               begin
                  if Integer (P) = Ret_Val then
                     Put (Body_Buffer, " RetVal'Unchecked_Access");
                  else
                     if Name = "" then
                        Put (Body_Buffer, " P" &
                             Strip (Integer (P + 1)'Img));
                     else
                        Put (Body_Buffer, " " & Name);
                     end if;
                  end if;

                  if P < Desc.cParams - 1 then
                     Put_Line (Body_Buffer, ",");
                  end if;
               end;
            end loop;

            if Force_Proc then
               Put_Line (Body_Buffer, ");");
            else
               Put_Line (Body_Buffer, "));");
            end if;

            New_Line (Body_Buffer);

            Decrease_Indent (Body_Buffer);

            if Has_Free then
               Put_Line (Body_Buffer, "if Free then");
               Write (From_Buffer => Free_Buffer,
                      To_Buffer   => Body_Buffer);
               Put_Line (Body_Buffer, "end if;");
               New_Line (Body_Buffer);
            end if;

            if Ret_Val > -1 then
               Put_Line (Body_Buffer, "return RetVal;");
            end if;

            Decrease_Indent (Body_Buffer);
            Put_Line (Body_Buffer, "end " & Func_Name & ";");

            BSTR.Free (Func_Desc);

            ReleaseFuncDesc (Type_Info, Desc);

            Clear (Free_Buffer);
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
            Desc      : constant GNATCOM.Types.Pointer_To_FUNCDESC :=
              GetFuncDesc (Type_Info, Interfaces.C.int (N));
            Name      : constant String := Valid_Identifier
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
         begin
            Attach (Ref_Info,
                    GetRefTypeInfo (Type_Info,
                                    GetRefTypeOfImplType (Type_Info, 0)));

            Bind_VtblElements (Ref_Info, Bind_Buffer, Bind_Name, Max_Length);
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
                      Name & " : af_" & Bind_Name & "_" & FuncName & ";");
         end;
      end loop;

      ReleaseTypeAttr (Type_Info, Attribs);
   end Bind_VtblElements;

   -- Bind_Interface --

   procedure Bind_Interface
     (Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type)
   is
      use type Interfaces.C.short;
      use type Interfaces.C.unsigned_short;

      Bind_Name   : constant String := Valid_Identifier (GetName (Type_Info));
      Pre_Buffer  : aliased Source_Buffer_Type;
      Bind_Buffer : Source_Buffer_Type;
      Attribs     : GNATCOM.Types.Pointer_To_TYPEATTR;
      Max_Length  : Natural := 0;
      Head_File   : File_Type;
      Body_File   : File_Type;
      Head_Buffer : Source_Buffer_Type;
      Body_Buffer : Source_Buffer_Type;
      Is_Dual     : Boolean := False;
   begin
      if
        not Get (To_Unbounded_String (Bind_Name))
      then
         --  Setup thin buffers and types

         Set (To_Unbounded_String (Bind_Name), True);

         Put_Line (Type_Buffer, "type " & Bind_Name & ";");

         if not Get (To_Unbounded_String ("Pointer_To_" & Bind_Name)) then
            Set (To_Unbounded_String ("Pointer_To_" & Bind_Name), True);
            Put_Line (Pointer_Buffer,
                      "type Pointer_To_" &
                      Bind_Name &
                      " is access all " & Bind_Name & ";");
         end if;

         Describe_Element (Type_Info, Bind_Buffer);

         --  Do thin bind

         Attribs := GetTypeAttr (Type_Info);

         if (Attribs.wTypeFlags and GNATCOM.Types.TYPEFLAG_FDUAL) /= 0 then
            Is_Dual := True;
         end if;

         Put_Line (Bind_Buffer, "IID_" & Bind_Name &
                   " : aliased GNATCOM.Types.GUID :=");

         Put_Line (Bind_Buffer, "  GNATCOM.GUID.To_GUID (""" &
                   GNATCOM.GUID.To_String (Attribs.guid) &
                   """);");

         New_Line (Bind_Buffer);

         Bind_Thin_Elements (Type_Info, Bind_Buffer, Bind_Name);

         Put_Line (Bind_Buffer, "type " & Bind_Name & "Vtbl;");

         Put_Line (Bind_Buffer,
                   "type Pointer_To_" &
                   Bind_Name &
                   "Vtbl is access all " & Bind_Name & "Vtbl;");

         New_Line (Bind_Buffer);

         Put_Line (Bind_Buffer, "type " & Bind_Name & " is");
         Increase_Indent (Bind_Buffer);
         Put_Line (Bind_Buffer, "record");
         Increase_Indent (Bind_Buffer);
         Put_Line (Bind_Buffer, "Vtbl : Pointer_To_" & Bind_Name & "Vtbl;");
         Decrease_Indent (Bind_Buffer);
         Put_Line (Bind_Buffer, "end record;");
         Decrease_Indent (Bind_Buffer);
         Put_Line (Bind_Buffer,
                   "pragma Convention (C_Pass_By_Copy, " &
                   Bind_Name & ");");

         New_Line (Bind_Buffer);

         Put_Line (Bind_Buffer, "type " & Bind_Name & "Vtbl is");
         Increase_Indent (Bind_Buffer);
         Put_Line (Bind_Buffer, "record");
         Increase_Indent (Bind_Buffer);

         Bind_VtblElements (Type_Info, Bind_Buffer, Bind_Name, Max_Length);

         Decrease_Indent (Bind_Buffer);
         Put_Line (Bind_Buffer, "end record;");
         Decrease_Indent (Bind_Buffer);
         Put_Line (Bind_Buffer,
                   "pragma Convention (C_Pass_By_Copy, " &
                   Bind_Name & "Vtbl);");
         New_Line (Bind_Buffer);
         Put_Line (Bind_Buffer, "function To_Pointer_To_" & Bind_Name & " is");
         Put_Line (Bind_Buffer, "  new Ada.Unchecked_Conversion");
         Put_Line (Bind_Buffer, "  (GNATCOM.Types.Pointer_To_Void," &
                   " Pointer_To_" & Bind_Name & ");");
         New_Line (Bind_Buffer);

         --  Commit thin binding

         Write (From_Buffer => Pre_Buffer,
                To_Buffer   => Spec_Buffer);

         New_Line (Spec_Buffer);

         Write (From_Buffer => Bind_Buffer,
                To_Buffer   => Spec_Buffer);

         --  Setup thick files and buffers

         Create (File => Head_File,
                 Mode => Out_File,
                 Name => Ada.Strings.Fixed.Translate
                 (To_String (Base_Package) &
                  "-" &
                  Bind_Name &
                  "_interface.ads",
                  Lower_Case_Map));

         Create (File => Body_File,
                 Mode => Out_File,
                 Name => Ada.Strings.Fixed.Translate
                 (To_String (Base_Package) &
                  "-" &
                  Bind_Name &
                  "_interface.adb",
                  Lower_Case_Map));

         if Is_Dual then
            Put_Line (Head_Buffer, "with GNATCOM.Dispinterface;");
            Put_Line (Body_Buffer, "with GNATCOM.IInterface;");
            New_Line (Body_Buffer);
         else
            Put_Line (Head_Buffer, "with GNATCOM.IInterface;");
         end if;

         New_Line (Head_Buffer);
         Put_Line (Head_Buffer,
                   "package " &
                   To_String (Base_Package) &
                   "." &
                   Bind_Name &
                   "_Interface is");
         Increase_Indent (Head_Buffer);
         New_Line (Head_Buffer);
         Put_Line (Head_Buffer, "type " & Bind_Name & "_Type is");
         if Is_Dual then
            Put_Line
              (Head_Buffer,
               "  new GNATCOM.Dispinterface.Dispinterface_Type" &
               " with null record;");
         else
            Put_Line
              (Head_Buffer,
               "  new GNATCOM.IInterface.Interface_Type with null record;");
         end if;

         New_Line (Head_Buffer);
         Put_Line (Head_Buffer, "procedure Initialize (This : in out " &
                     Bind_Name & "_Type);");
         New_Line (Head_Buffer);
         Put_Line (Head_Buffer, "function Pointer (This : " &
                     Bind_Name & "_Type)");
         Put_Line (Head_Buffer, "  return Pointer_To_" &
                     Bind_Name & ";");
         New_Line (Head_Buffer);
         Put_Line (Head_Buffer, "procedure Attach (This    : in out " &
                     Bind_Name & "_Type;");
         Put_Line (Head_Buffer, "                  Pointer : in     " &
                   "Pointer_To_" & Bind_Name & ");");
         New_Line (Head_Buffer);

         Put_Line (Body_Buffer, "with GNATCOM.Errors;");
         New_Line (Body_Buffer);
         Put_Line (Body_Buffer,
                   "package body " &
                   To_String (Base_Package) &
                   "." &
                   Bind_Name &
                   "_Interface is");
         Increase_Indent (Body_Buffer);
         New_Line (Body_Buffer);
         Put_Line (Body_Buffer, "procedure Initialize (This : in out " &
                     Bind_Name & "_Type) is");
         Put_Line (Body_Buffer, "begin");
         Increase_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "Set_IID (This, IID_" & Bind_Name & ");");
         Decrease_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "end Initialize;");
         New_Line (Body_Buffer);
         Put_Line (Body_Buffer, "function Pointer (This : " &
                     Bind_Name & "_Type)");
         Put_Line (Body_Buffer, "  return Pointer_To_" &
                   Bind_Name);
         Put_Line (Body_Buffer, "is");
         Put_Line (Body_Buffer, "begin");
         Increase_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "return To_Pointer_To_" & Bind_Name &
                     " (Address (This));");
         Decrease_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "end Pointer;");
         New_Line (Body_Buffer);
         Put_Line (Body_Buffer, "procedure Attach (This    : in out " &
                     Bind_Name & "_Type;");
         Put_Line (Body_Buffer, "                  Pointer : in     " &
                   "Pointer_To_" & Bind_Name & ")");
         Put_Line (Body_Buffer, "is");
         Put_Line (Body_Buffer, "begin");
         Increase_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "Attach (This," &
                   " GNATCOM.IInterface.To_Pointer_To_IUnknown");
         Put_Line (Body_Buffer, "        (Pointer.all'Address));");
         Decrease_Indent (Body_Buffer);
         Put_Line (Body_Buffer, "end Attach;");
         New_Line (Body_Buffer);

         Bind_Thick_Elements (Type_Info, Head_Buffer, Body_Buffer, Bind_Name);

         --  Commit Thick Spec/Body Files

         Decrease_Indent (Head_Buffer);
         Decrease_Indent (Body_Buffer);

         Put_Line (Head_Buffer,
                   "end " & To_String (Base_Package) & "." & Bind_Name &
                   "_Interface;");

         Put_Line (Body_Buffer,
                   "end " & To_String (Base_Package) & "." & Bind_Name &
                   "_Interface;");

         Write (From_Buffer => Head_Buffer,
                To_Output   => Head_File);

         Write (From_Buffer => Body_Buffer,
                To_Output   => Body_File);

         Close (Head_File);
         Close (Body_File);

         --  Clean up

         ReleaseTypeAttr (Type_Info, Attribs);

         Clear (Head_Buffer);
         Clear (Body_Buffer);
         Clear (Pre_Buffer);
         Clear (Bind_Buffer);
      end if;

   end Bind_Interface;

   -- Bind_Thick_Dispatch --

   procedure Bind_Thick_Dispatch
     (Type_Info   : in      GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      Head_Buffer : in out  Source_Buffer_Type;
      Body_Buffer : in out  Source_Buffer_Type;
      Bind_Name   : in      String)
   is
      use type Interfaces.C.short;
      use type Interfaces.C.unsigned_short;
      use type Interfaces.C.long;

      Attribs    : GNATCOM.Types.Pointer_To_TYPEATTR;
      FStart     : Interfaces.C.short := 0;
   begin
      Attribs := GetTypeAttr (Type_Info);

      for N in  0 .. Attribs.cVars - 1 loop
         declare
            Desc     : constant GNATCOM.Types.Pointer_To_VARDESC :=
              GetVarDesc (Type_Info, Interfaces.C.int (N));
            Var_Name : aliased GNATCOM.Types.BSTR;
            Var_Desc : aliased GNATCOM.Types.BSTR;
         begin
            GetDocumentation (Type_Info,
                              Desc.memid,
                              Var_Name'Unchecked_Access,
                              Var_Desc'Unchecked_Access,
                              null,
                              null);

            declare
               Name : constant String := BSTR.To_Ada (Var_Name);
            begin
               Put_Line (Head_Buffer,
                         "function Get_" &
                         Valid_Identifier (Name) &
                         " (This : " & Bind_Name & "_Type)");
               Put_Line (Head_Buffer,
                         "  return GNATCOM.Types.VARIANT;");

               if BSTR.To_Ada (Var_Desc, False) > "" then
                  Put_Comment
                    (Head_Buffer, BSTR.To_Ada (Var_Desc, False));
               end if;
               BSTR.Free (Var_Desc);

               New_Line (Head_Buffer);

               Put_Line (Body_Buffer,
                         "function Get_" &
                         Valid_Identifier (Name) &
                         " (This : " & Bind_Name & "_Type)");
               Put_Line (Body_Buffer, "  return GNATCOM.Types.VARIANT");
               Put_Line (Body_Buffer, "is");
               Put_Line (Body_Buffer, "begin");
               Increase_Indent (Body_Buffer);
               Put_Line (Body_Buffer, "return Get (This, " &
                         Bind_Name & "_" &
                         Valid_Identifier (Name) &
                         ");");
               Decrease_Indent (Body_Buffer);
               Put_Line (Body_Buffer,
                         "end Get_" &
                         Valid_Identifier (Name) &
                         ";");

               New_Line (Body_Buffer);

               if (Desc.wVarFlags and VARFLAG_FREADONLY) = 0 then
                  Put_Line (Head_Buffer,
                            "procedure Put_" &
                            Valid_Identifier (Name));
                  Put_Line (Head_Buffer,
                            "  (This : " & Bind_Name & "_Type;");
                  Put_Line (Head_Buffer,
                            "   Value : GNATCOM.Types.VARIANT;");
                  Put_Line (Head_Buffer,
                            "   Free  : Boolean := True);");

                  if BSTR.To_Ada (Var_Desc, False) > "" then
                     Put_Comment
                       (Head_Buffer, BSTR.To_Ada (Var_Desc, False));
                  end if;
                  New_Line (Head_Buffer);

                  BSTR.Free (Var_Desc);

                  Put_Line (Body_Buffer,
                            "procedure Put_" &
                            Valid_Identifier (Name));
                  Put_Line (Body_Buffer,
                            "  (This : " & Bind_Name & "_Type;");
                  Put_Line (Body_Buffer,
                            "   Value : GNATCOM.Types.VARIANT;");
                  Put_Line (Body_Buffer,
                            "   Free  : Boolean := True)");
                  Put_Line (Body_Buffer, "is");
                  Put_Line (Body_Buffer, "begin");
                  Increase_Indent (Body_Buffer);
                  Put_Line (Body_Buffer, "Put (This, " &
                            Bind_Name & "_" &
                            Valid_Identifier (Name) &
                            ", Value, Free);");
                  Decrease_Indent (Body_Buffer);
                  Put_Line (Body_Buffer,
                            "end Put_" &
                            Valid_Identifier (Name) &
                            ";");
                  New_Line (Body_Buffer);
               end if;
            end;
            ReleaseVarDesc (Type_Info, Desc);
         end;
      end loop;

      if (Attribs.wTypeFlags and GNATCOM.Types.TYPEFLAG_FDUAL) /= 0 then
         FStart := 7;
      end if;

      for N in  FStart .. Attribs.cFuncs - 1 loop
         declare
            Desc        : constant GNATCOM.Types.Pointer_To_FUNCDESC :=
              GetFuncDesc (Type_Info, Interfaces.C.int (N));
            Func_Name   : constant String :=
              Valid_Identifier (GetFunctionName (Type_Info, Desc));
            Func_Desc   : aliased GNATCOM.Types.BSTR;

            Param_Names : aliased GNATCOM.Types.BSTR_PARAM_ARRAY;
            cNames      : aliased Interfaces.C.int;
            Max_Length  : Natural := 4;
         begin
            GetNames (Type_Info,
                      Desc.memid,
                      Param_Names'Unchecked_Access,
                      GNATCOM.Types.MAX_PARAMS,
                      cNames'Unchecked_Access);

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

            if Desc.elemdescFunc.tdesc.vt /= VT_VOID then
               Put_Line (Head_Buffer, "function " & Func_Name);
               Put_Line (Body_Buffer, "function " & Func_Name);
            else
               Put_Line (Head_Buffer, "procedure " & Func_Name);
               Put_Line (Body_Buffer, "procedure " & Func_Name);
            end if;

            Put (Head_Buffer, "  (");
            Increase_Indent (Head_Buffer);

            Put (Body_Buffer, "  (");
            Increase_Indent (Body_Buffer);

            declare
               Name : String (1 .. Max_Length);
            begin
               Ada.Strings.Fixed.Move ("This", Name);
               Put (Head_Buffer, Name & " : " & Bind_Name & "_Type");
               Put (Body_Buffer, Name & " : " & Bind_Name & "_Type");
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
                     Ada.Strings.Fixed.Move ("P" & Strip (Integer (P + 1)'Img),
                                             Name);
                  end if;

                  Put (Head_Buffer, Name & " : GNATCOM.Types.VARIANT");
                  Put (Body_Buffer, Name & " : GNATCOM.Types.VARIANT");

                  if (Desc.lprgelemdescParam (P).paramdesc.wParamFlags
                      and GNATCOM.Types.PARAMFLAG_FOPT) /= 0
                  then
                     New_Line (Head_Buffer);
                     New_Line (Body_Buffer);
                     Put (Head_Buffer,
                          "  := GNATCOM.Types.VARIANT_MISSING");
                     Put (Body_Buffer,
                          "  := GNATCOM.Types.VARIANT_MISSING");
                  end if;

                  if P < Desc.cParams - 1 then
                     Put_Line (Head_Buffer, ";");
                     Put_Line (Body_Buffer, ";");
                  end if;
               end;
            end loop;

            if
              Desc.cParams > 0 and Desc.invkind /= INVOKE_PROPERTYPUTREF
            then
               Put_Line (Head_Buffer, ";");
               Put_Line (Body_Buffer, ";");
               declare
                  Name : String (1 .. Max_Length);
               begin
                  Ada.Strings.Fixed.Move ("Free", Name);
                  Put (Head_Buffer, Name & " : Boolean := True");
                  Put (Body_Buffer, Name & " : Boolean := True");
               end;
            end if;

            if Desc.elemdescFunc.tdesc.vt /= VT_VOID then
               Put_Line (Head_Buffer, ")");
               Decrease_Indent (Head_Buffer);
               Put_Line (Body_Buffer, ")");
               Decrease_Indent (Body_Buffer);

               Put_Line (Head_Buffer, "  return GNATCOM.Types.VARIANT;");
               Put_Line (Body_Buffer, "  return GNATCOM.Types.VARIANT");
               Put_Line (Body_Buffer, "is");
            else
               Put_Line (Head_Buffer, ");");
               Decrease_Indent (Head_Buffer);
               Put_Line (Body_Buffer, ")");
               Decrease_Indent (Body_Buffer);
               Put_Line (Body_Buffer, "is");
            end if;

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

            Put_Line (Body_Buffer, "begin");
            Increase_Indent (Body_Buffer);

            if Desc.cParams > 0 then
               case Desc.invkind is
                  when INVOKE_FUNC =>
                     if Desc.elemdescFunc.tdesc.vt /= VT_VOID then
                        Put_Line (Body_Buffer, "return Invoke");
                     else
                        Put_Line (Body_Buffer, "Invoke");
                     end if;
                  when INVOKE_PROPERTYGET =>
                     Put_Line (Body_Buffer, "return Get");
                  when INVOKE_PROPERTYPUT =>
                     Put_Line (Body_Buffer, "Put");
                  when INVOKE_PROPERTYPUTREF =>
                     Put_Line (Body_Buffer, "PutRef");
                  when others =>
                     Put_Line ("ERROR: Bad invoke kind");
               end case;
               Put_Line (Body_Buffer,
                         "  (This,");
               Increase_Indent (Body_Buffer);
               Put_Line (Body_Buffer,
                         Bind_Name & "_" & Func_Name &
                         ",");
               Put_Line
                 (Body_Buffer, "GNATCOM.Dispinterface.Parameter_Array'");
               Put (Body_Buffer, "(");
            else
               case Desc.invkind is
                  when INVOKE_FUNC =>
                     if Desc.elemdescFunc.tdesc.vt /= VT_VOID then
                        Put_Line (Body_Buffer,
                                  "return Invoke (This, " &
                                  Bind_Name & "_" & Func_Name &
                                  ");");
                     else
                        Put_Line (Body_Buffer,
                                  "Invoke (This, " &
                                  Bind_Name & "_" & Func_Name &
                                  ");");
                     end if;
                  when INVOKE_PROPERTYGET =>
                     Put_Line (Body_Buffer,
                               "return Get (This, " &
                               Bind_Name & "_" & Func_Name &
                               ");");
                  when others =>
                     Put_Line ("ERROR: Bad invoke kind");
               end case;
            end if;

            for P in reverse 0 .. Desc.cParams - 1 loop
               declare
                  Name : constant String := Valid_Identifier
                    (BSTR.To_Ada (Param_Names (P + 1)));
               begin
                  if P < Desc.cParams - 1 then
                     Put (Body_Buffer, " ");
                  end if;

                  Put (Body_Buffer,
                       Strip (Integer (Desc.cParams - P)'Img)
                       & " => ");

                  if Name = "" then
                     Put (Body_Buffer, "P" & Strip (Integer (P + 1)'Img));
                  else
                     Put (Body_Buffer, Name);
                  end if;

                  if P > 0 then
                     Put_Line (Body_Buffer, ",");
                  end if;
               end;
            end loop;

            if Desc.cParams > 0 then
               if Desc.invkind /= INVOKE_PROPERTYPUTREF then
                  Put_Line (Body_Buffer, "),");
                  Put_Line (Body_Buffer, "Free);");
               else
                  Put_Line (Body_Buffer, "));");
               end if;
               Decrease_Indent (Body_Buffer);
            end if;

            Decrease_Indent (Body_Buffer);

            Put_Line (Body_Buffer, "end " & Func_Name & ";");

            BSTR.Free (Func_Desc);

            ReleaseFuncDesc (Type_Info, Desc);
         end;

         New_Line (Head_Buffer);
         New_Line (Body_Buffer);

      end loop;

      ReleaseTypeAttr (Type_Info, Attribs);
   end Bind_Thick_Dispatch;

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
      Head_File   : File_Type;
      Body_File   : File_Type;
      Head_Buffer : Source_Buffer_Type;
      Body_Buffer : Source_Buffer_Type;
   begin
      if
        not Get (To_Unbounded_String (Bind_Name))
      then
         --  Setup thin buffers and types

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
            Set (To_Unbounded_String (Bind_Name), True);

            Put_Line (Bind_Buffer, "IID_" & Bind_Name &
                      " : aliased GNATCOM.Types.GUID :=");

            Put_Line (Bind_Buffer, "  GNATCOM.GUID.To_GUID (""" &
                      GNATCOM.GUID.To_String (Attribs.guid) &
                      """);");
            New_Line (Bind_Buffer);

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

         --  Setup thick files and buffers

         Create (File => Head_File,
                 Mode => Out_File,
                 Name => Ada.Strings.Fixed.Translate
                 (To_String (Base_Package) &
                  "-" &
                  Bind_Name &
                  "_object.ads",
                  Lower_Case_Map));

         Create (File => Body_File,
                 Mode => Out_File,
                 Name => Ada.Strings.Fixed.Translate
                 (To_String (Base_Package) &
                  "-" &
                  Bind_Name &
                  "_object.adb",
                  Lower_Case_Map));

         Put_Line (Head_Buffer, "with GNATCOM.Dispinterface;");
         New_Line (Head_Buffer);
         Put_Line (Head_Buffer,
                   "package " &
                   To_String (Base_Package) &
                   "." &
                   Bind_Name &
                   "_Object is");
         Increase_Indent (Head_Buffer);
         New_Line (Head_Buffer);
         Put_Line (Head_Buffer, "type " & Bind_Name & "_Type is");
         Put_Line
           (Head_Buffer,
            "  new GNATCOM.Dispinterface.Dispinterface_Type" &
            " with null record;");
         New_Line (Head_Buffer);

         Put_Line (Body_Buffer,
                   "package body " &
                   To_String (Base_Package) &
                   "." &
                   Bind_Name &
                   "_Object is");
         Increase_Indent (Body_Buffer);
         New_Line (Body_Buffer);

         Bind_Thick_Dispatch (Type_Info, Head_Buffer, Body_Buffer, Bind_Name);

         --  Commit Thick Spec/Body Files

         Decrease_Indent (Head_Buffer);
         Decrease_Indent (Body_Buffer);

         Put_Line (Head_Buffer,
                   "end " & To_String (Base_Package) & "." & Bind_Name &
                   "_Object;");

         Put_Line (Body_Buffer,
                   "end " & To_String (Base_Package) & "." & Bind_Name &
                   "_Object;");

         Write (From_Buffer => Head_Buffer,
                To_Output   => Head_File);

         Write (From_Buffer => Body_Buffer,
                To_Output   => Body_File);

         Close (Head_File);
         Close (Body_File);

         --  Clean up

         ReleaseTypeAttr (Type_Info, Attribs);

         Clear (Head_Buffer);
         Clear (Body_Buffer);
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
      use type Interfaces.C.unsigned_short;
      use type Interfaces.C.short;
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
               return "GNATCOM.Types.Pointer_To_" & Point_Type;
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
      Pred_Map.Set (To_Unbounded_String ("IUnknown"), True);
      Pred_Map.Set (To_Unbounded_String ("IDispatch"), True);
      Pred_Map.Set (To_Unbounded_String ("VARIANT"), True);
      Pred_Map.Set (To_Unbounded_String ("ITypeLib"), True);
      Pred_Map.Set (To_Unbounded_String ("ITypeInfo"), True);
      Pred_Map.Set (To_Unbounded_String ("ICreateTypeLib"), True);
      Pred_Map.Set (To_Unbounded_String ("ICreateTypeInfo"), True);
      Pred_Map.Set (To_Unbounded_String ("ITypeComp"), True);
      Pred_Map.Set (To_Unbounded_String ("IClassFactory"), True);
      Pred_Map.Set (To_Unbounded_String ("IClassFactory2"), True);
      Pred_Map.Set (To_Unbounded_String ("IConnectionPointContainer"), True);
      Pred_Map.Set (To_Unbounded_String ("IConnectionPoint"), True);
      Pred_Map.Set (To_Unbounded_String ("IEnumConnectionPoints"), True);
      Pred_Map.Set (To_Unbounded_String ("IEnumConnections"), True);
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
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_IUnknown"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_IDispatch"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_Pointer_To_IUnknown"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_Pointer_To_IDispatch"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_VARIANT"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_ITypeLib"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_Pointer_To_ITypeLib"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_ITypeInfo"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_Pointer_To_ITypeInfo"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_ICreateTypeLib"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_Pointer_To_ICreateTypeLib"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_ICreateTypeInfo"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_Pointer_To_ICreateTypeInfo"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_ITypeComp"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_Pointer_To_ITypeComp"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_IClassFactory"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_IClassFactory2"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_IConnectionPointContainer"), True);
      Pred_Map.Set (To_Unbounded_String
           ("Pointer_To_Pointer_To_IConnectionPointContainer"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_IConnectionPoint"), True);
      Pred_Map.Set (To_Unbounded_String
           ("Pointer_To_Pointer_To_IConnectionPoint"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_IEnumConnectionPoints"), True);
      Pred_Map.Set (To_Unbounded_String
           ("Pointer_To_Pointer_To_IEnumConnectionPoints"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_IEnumConnections"), True);
      Pred_Map.Set (To_Unbounded_String
           ("Pointer_To_Pointer_To_IEnumConnections"), True);
      Pred_Map.Set (To_Unbounded_String ("Pointer_To_IEnumVARIANT"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_Pointer_To_IEnumVARIANT"), True);
      Pred_Map.Set
        (To_Unbounded_String ("Pointer_To_IGlobalInterfaceTable"), True);
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

end Bind_COM;
