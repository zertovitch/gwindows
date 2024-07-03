------------------------------------------------------------------------------
--                                                                          --
--       GNAVI - The GNU Ada Visual Interface - Open Source Visual RAD      --
--                                                                          --
--                      G N A V I _ T E M P L A T E S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
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
-- More information about GNAVI and the most current version can            --
-- be located on the web at http://www.gnavi.org                            --
--                                                                          --
------------------------------------------------------------------------------

--  Handle template code change library

with Templates_Parser;

package GNAVI_Templates is

   -------------------------------------------------------------------------
   --  General Template Related Subprograms
   -------------------------------------------------------------------------

   procedure Template_Dir (Dir : String);
   function Template_Dir return String;
   --  Set/Get template directory

   function Load_Template (Template_Name : String) return String;
   --  Load template from template directory

   type Embedded_Template_Kind is
      (on_create_template,
       window_package_spec_template,
       window_package_body_template,
       application_template,
       handler_template);

   function Load_Template (Template : Embedded_Template_Kind; Template_Name : String := "") return String;
   --  Load template from embedded collection

   function Load_File (File_Spec : String) return String;
   --  Load file in to String

   procedure Write_File (File_Spec : String; Contents : String);
   --  Write file with contents

   procedure Update_File (File_Spec : String; Contents : String);
   --  Write file only if Contents is not identical to what is already
   --  in file

   procedure Execute (File_Spec     : String;
                      Template_Name : String;
                      Trans_Table   : Templates_Parser.Translate_Table);
   --  Execute translation of template and write out to File_Spec

   procedure Execute (File_Spec     : String;
                      Template      : Embedded_Template_Kind;
                      Trans_Table   : Templates_Parser.Translate_Table);
   --  Execute translation of embedded template and write out to File_Spec

   function NL return String;
   --  Return new line (LF or CR/LF)

   type New_Line_Mode_Type is (DOS, UNIX);

   procedure Set_New_Line_Mode (Mode : New_Line_Mode_Type);
   --  Sets what NL should return

   -------------------------------------------------------------------------
   --  Code modification and change subprograms
   -------------------------------------------------------------------------

   procedure Check_For_With (File_Spec  : String;
                             With_String : String);
   --  Searches for 'with' and adds it if needed

   procedure Check_For_Handler (Package_Name : String;
                                Handler_Name : String;
                                Handler_Type : String);
   --  Searches for handler and adds it if needed

   function With_Of (Type_Name : String) return String;
   --  Returns the package name for Type_Name

   procedure Set_Control_Block (File_Spec : String;
                                Block     : String);
   --  Sets control block in file

   GNAVI_Comment_Removed_Error : exception;
   GNAVI_File_Load_Error       : exception;
   GNAVI_File_Write_Error      : exception;

end GNAVI_Templates;
