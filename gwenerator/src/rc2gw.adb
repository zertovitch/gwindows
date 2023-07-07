--------------------------------------------------------------------------
--  RC2GW.adb
--
--  RC2GW translates a Resource Compiler script file (.rc or .dlg)
--  into an Ada package for the GWindows GUI system.
--
--  Copyright (c) Gautier de Montmollin 2008 .. 2019
--  SWITZERLAND
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found 28-Jul-2008 on the site
--  http://www.opensource.org/licenses/mit-license.php
----------------------------------------------------------------------------
--
--  Change log:
--
--  24-Jun-2009 GdM: Added the -c option (initialize controls, for testing)
--  ~ May-2009 GdM: Added the -t option (test generation)
--  29-Aug-2008 GdM: Added the -x and -y options
--  28-Jul-2008 GdM: Created
--

with Ada.Calendar,
     Ada.Command_Line,
     Ada.Characters.Handling,
     Ada.Directories,
     Ada.Text_IO;

with rc_io, RC_Help, YYParse;

procedure RC2GW is

  use Ada.Text_IO;

  Inp_Opened  : Boolean := False;
  check_newer : Boolean := False;

  function Is_RC_newer_than_Ada return Boolean is
    use RC_Help, Ada.Calendar, Ada.Directories;
    rn : constant String := S (RC_Help.source_name);
    an : constant String := RC_to_Package_name (rn, True, True) & ".ads";
  begin
    return
      rn /= "" and then
      Exists (rn) and then
      ((an  =  "" or else not Exists (an)) or else
        Modification_Time (rn) > Modification_Time (an));
  end Is_RC_newer_than_Ada;

  procedure Syntax is
  begin
    Put_Line (Current_Error, "Syntax: RC2GW [option] input_file");
    New_Line (Current_Error);
    Put_Line (Current_Error, "RC2GW translates a Resource Compiler script file (.rc or .dlg)");
    Put_Line (Current_Error, "into an Ada package for the GWindows GUI system.");
    New_Line (Current_Error);
    Put_Line (Current_Error, "options:  -c  : initialize some controls with fake contents, for test/debug");
    Put_Line (Current_Error, "          -n  : translate only if input file is newer than generated files");
    Put_Line (Current_Error, "          -s  : put each item (dialog, menu) in a separate package");
    Put_Line (Current_Error, "          -t  : generate test procedure");
    Put_Line (Current_Error, "          -x# : set base_unit_x as # (default:" & Positive'Image (RC_Help.base_unit_x) & ')');
    Put_Line (Current_Error, "          -y# : set base_unit_y as # (default:" & Positive'Image (RC_Help.base_unit_y) & ')');
    New_Line (Current_Error);
    Put_Line (Current_Error, "RC Grammar version: " & RC_Help.Grammar_Version);
  end Syntax;

  use Ada.Characters.Handling, Ada.Command_Line;

begin
  RC_Help.Reset_Globals;

  for i in 1 .. Argument_Count loop
    declare
      arg : constant String := Argument (i);
      u_arg : constant String := To_Upper (arg);
    begin
      if u_arg'Length > 1 and then
        (u_arg (1) = '-' or u_arg (1) = '/')
      then
        case u_arg (2) is
          when 'X' =>
            RC_Help.base_unit_x := Positive'Value (u_arg (3 .. u_arg'Last));
          when 'Y' =>
            RC_Help.base_unit_y := Positive'Value (u_arg (3 .. u_arg'Last));
          when 'S' =>
            RC_Help.separate_items := True;
          when 'T' =>
            RC_Help.generate_test := True;
          when 'C' =>
            RC_Help.initialize_controls := True;
          when 'N' =>
            check_newer := True;
          when others =>  -- includes "-h", "/?" etc.
            Syntax;
            return;
        end case;
      else -- no option
        if Inp_Opened then          -- Two inputs ?!
          rc_io.Close_Input;
          Syntax;
          return;
        else
          RC_Help.source_name := RC_Help.U (arg);
          if (not check_newer) or else Is_RC_newer_than_Ada then
            begin
              rc_io.Open_Input (fname => arg);
              Inp_Opened := True;
              Put_Line (Current_Error,
                "RC2GW: transcripting '" & arg &
                "' to GWindows Ada sources.");
              Put_Line (Current_Error,
                " . . . . RC Grammar version: " & RC_Help.Grammar_Version);
            exception
              when Name_Error =>
                Put_Line (Current_Error, "Input file '" & arg &
                  "' not found.");
                Syntax;
                return;
            end;
          exit;
          else
            Put_Line (Current_Error, "Generated code is already up-to-date");
            return;
          end if;
        end if;
      end if;
    end;
  end loop;

  if not Inp_Opened then
    Put_Line (Current_Error, "Missing input file!");
    Syntax;
    return;
  end if;

  RC_Help.has_input := Inp_Opened;

  RC_Help.Ada_Begin;

  YYParse;

  if Inp_Opened then
    rc_io.Close_Input;
  end if;

  Put_Line (Current_Error, "RC2GW is done.");
end RC2GW;
