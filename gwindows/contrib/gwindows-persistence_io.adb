------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--               G W I N D O W S . P E R S I S T E N C E _ I O              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 2023 Gautier de Montmollin                 --
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
-- More information about GWindows and the latest current release can       --
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.GStrings,
     GWindows.Registry;

--  This requires the Config package:
--    https://sourceforge.net/projects/ini-files/
--    https://github.com/zertovitch/ini-files
with Config;

with Ada.Command_Line,
     Ada.Directories,
     Ada.IO_Exceptions,
     Ada.Text_IO;

package body GWindows.Persistence_IO is

  use GWindows, GWindows.GStrings;

  ----------------------------------------------
  --  Persistence using the Windows Registry  --
  ----------------------------------------------

  kname : constant GString := "Software\" & To_GString_From_String (app_display_name);
  use GWindows.Registry;

  function Read_Reg_Key (key : Persistence_Key) return GString is
  begin
    return Get_Value (kname, To_GString_From_String (key'Image), HKEY_CURRENT_USER);
  end Read_Reg_Key;

  procedure Write_Reg_Key (key : Persistence_Key; value : GString) is
  begin
    Register (kname, To_GString_From_String (key'Image), value, HKEY_CURRENT_USER);
  end Write_Reg_Key;

  ----------------------------------------------
  --  Persistence using a configuration file  --
  ----------------------------------------------

  function Config_Name return String is
    full : constant String := Ada.Command_Line.Command_Name;
    last : Natural := full'First - 1;
  begin
    for i in full'Range loop
      if full (i) = '\' or full (i) = '/' then
        last := i;
      end if;
    end loop;
    return full (full'First .. last) & app_file_name & ".cfg";
  end Config_Name;

  app_section : constant String := app_display_name & " user options";

  procedure Create_New_Config is
    use Ada.Text_IO;
    nf : File_Type;
  begin
    Create (nf, Out_File, Config_Name);
    Put_Line (nf, ";  This is the configuration file for " & app_display_name & ',');
    Put_Line (nf, ";  in the ""stealth / no-trace-in-registry"" mode.");
    Put_Line (nf, ";  NB: the settings are the same for all users.");
    Put_Line (nf, ";  Delete this file for using the registry again.");
    Put_Line (nf, ";  " & app_display_name & " Web site: " & app_url);
    Put_Line (nf, ";");
    Put_Line (nf, '[' & app_section & ']');
    for key in Persistence_Key loop
      Put_Line (nf, key'Image & '=');
    end loop;
    Close (nf);
  end Create_New_Config;

  function Read_Cfg_Key (key : Persistence_Key) return GString is
    cfg : Config.Configuration;
  begin
    cfg.Init (Config_Name);
    return To_GString_From_String (cfg.Value_Of ("*", key'Image));
  end Read_Cfg_Key;

  procedure Write_Cfg_Key (key : Persistence_Key; value : GString) is
    cfg : Config.Configuration;
  begin
    cfg.Init (Config_Name);
    for attempt in 1 .. 2 loop
      begin
        cfg.Replace_Value (app_section, key'Image, To_String (value));
      exception
        when Config.Location_Not_Found =>
          Create_New_Config;
      end;
    end loop;
  exception
    when Ada.IO_Exceptions.Use_Error =>  --  Read-only
      null;  --  Do nothing, lea.exe and lea.cfg may be on a read-only device.
  end Write_Cfg_Key;

  ---------------------------------------------------------------------
  --  Persistence using either the registry or a configuration file  --
  ---------------------------------------------------------------------

  function Is_Config_File_Available return Boolean is
  begin
    return Ada.Directories.Exists (Config_Name);
  end Is_Config_File_Available;

  function Read_Key (key : Persistence_Key) return GString is
  begin
    if Is_Config_File_Available then
      return Read_Cfg_Key (key);
    else
      return Read_Reg_Key (key);
    end if;
  end Read_Key;

  procedure Write_Key (key : Persistence_Key; value : GString) is
  begin
    if Is_Config_File_Available then
      Write_Cfg_Key (key, value);
    else
      Write_Reg_Key (key, value);
    end if;
  end Write_Key;

end GWindows.Persistence_IO;
