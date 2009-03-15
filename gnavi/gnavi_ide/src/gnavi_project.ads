with GWindows;
with GNAVI_XML;
with GNAT.OS_Lib;

package GNAVI_Project is

   type GNAVI_Project_Type is new GNAVI_XML.GNAVI_XML_Type with private;

   procedure Load_Project (Project   : in out GNAVI_Project_Type;
                           File_Name : in     GWindows.GString);
   --  Load XML GNAVI project

   procedure Save_Project (Project : in out GNAVI_Project_Type);
   --  Save Project

   procedure Save_As_Project (Project   : in out GNAVI_Project_Type;
                              File_Name : in     GWindows.GString);
   --  Save Project As ...

   procedure Close_Project (Project : in out GNAVI_Project_Type);
   --  Close Project

   function Is_Loaded (Project : in GNAVI_Project_Type) return Boolean;
   --  Returns true if a project is loaded

   function Has_Changed (Project : in GNAVI_Project_Type) return Boolean;
   --  Returns true if changes have been made to the projects and it
   --  has not been saved.

   function Project_Name (Project : in GNAVI_Project_Type)
                         return GWindows.GString;

   function Window_Count (Project : in GNAVI_Project_Type) return Natural;

   function Window_Name (Project : in GNAVI_Project_Type;
                         Index   : in Positive)
                        return GWindows.GString;

   function Window_File_Name  (Project : in GNAVI_Project_Type;
                               Index   : in Positive)
                              return GWindows.GString;

   procedure Add_Window  (Project   : in out GNAVI_Project_Type;
                          File_Name : in     GWindows.GString);
   --  Add window to project

   procedure Delete_Window (Project : in out GNAVI_Project_Type;
                            Index   : in     Positive);
   --  Delete window from project

   function File_Count (Project : in GNAVI_Project_Type) return Natural;

   function File_Name (Project : in GNAVI_Project_Type;
                       Index   : in Positive)
                      return GWindows.GString;

   procedure Add_File  (Project   : in out GNAVI_Project_Type;
                        File_Name : in     GWindows.GString);
   --  Add window to project

   procedure Delete_File (Project : in out GNAVI_Project_Type;
                          Index   : in     Positive);
   --  Delete window from project


   procedure Run_ICG (Project : in out GNAVI_Project_Type);
   --  Run interactive code generator on this project

   procedure Compile (Project : in out GNAVI_Project_Type);
   --  Compile project

   procedure Run (Project : in out GNAVI_Project_Type);
   --  Run project

   No_Project_Loaded : exception;

   ICG_Path_Exists      : GNAT.OS_Lib.String_Access;
   GNATMAKE_Path_Exists : GNAT.OS_Lib.String_Access;

private

     type GNAVI_Project_Type is new GNAVI_XML.GNAVI_XML_Type with
        record
           File_Name   : GWindows.GString_Unbounded;
           Load_State  : Boolean := False;
           Dirty_State : Boolean := False;
        end record;

end GNAVI_Project;
