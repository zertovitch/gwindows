--------------------------------------------------------------------------
--  RC2GW.adb
--
--  RC2GW translates a Resource Compiler script file (.rc or .dlg)
--  into an Ada package for the GWindows GUI system.
--
--  Copyright (c) Gautier de Montmollin 2008
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

-- NB: this is the MIT License, as found 28-Jul-2008 on the site
-- http://www.opensource.org/licenses/mit-license.php
----------------------------------------------------------------------------
--
-- Change log:
--
-- 29-Aug-2008 GdM: Added the x and y options
-- 28-Jul-2008 GdM: Created
--

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Text_IO;                       use Ada.Text_IO;

with RC_IO, RC_Help, YYParse;

procedure RC2GW is
  Inp_Opened  : Boolean := False;

  procedure Syntax is
  begin
    Put_Line( Standard_Error, "Syntax: RC2GW [option] input_file" );
    New_Line( Standard_Error );
    Put_Line( Standard_Error, "RC2GW translates a Resource Compiler script file (.rc or .dlg)" );
    Put_Line( Standard_Error, "into an Ada package for the GWindows GUI system." );
    New_Line( Standard_Error );
    Put_Line( Standard_Error, "options:  -x# : set base_unit_x as # (default:" & Positive'Image(RC_Help.base_unit_x) & ')' );
    Put_Line( Standard_Error, "          -y# : set base_unit_y as # (default:" & Positive'Image(RC_Help.base_unit_y) & ')' );
    Put_Line( Standard_Error, "          -s  : put each item (dialog, menu) in a separate package");
    Put_Line( Standard_Error, "          -t  : generate test procedure");
  end Syntax;

begin
  RC_Help.Reset_globals;

  for i in 1..Argument_Count loop
    declare
      arg: constant String:= Argument(i);
      u_arg: constant String:= To_Upper( arg );
    begin
      if u_arg'length > 1 and then
        (u_arg(1) = '-' or u_arg(1) = '/') then
        case u_arg(2) is
          when 'X' =>
            RC_Help.base_unit_x:= Positive'Value(u_arg(3..u_arg'Last));
          when 'Y' =>
            RC_Help.base_unit_y:= Positive'Value(u_arg(3..u_arg'Last));
          when 'S' =>
            RC_Help.separate_items:= True;
          when 'T' =>
            RC_Help.generate_Test:= True;
          when others =>  -- includes "-h", "/?" etc.
            Syntax;
            return;
        end case;
      else -- no option
        if Inp_Opened then          -- Two inputs ?!
          RC_IO.Close_Input;
          Syntax;
          return;
        else
          RC_Help.source_name:= RC_Help.U(arg);
          begin
            RC_IO.Open_Input (fname => arg);
            Inp_Opened := True;
            Put_Line(Standard_error,
              "RC2GW: transcripting '" & arg &
              "' to GWindows Ada sources." );
          exception
            when Name_Error =>
              Put_Line( Standard_Error, "Input file '" & arg &
                "' not found." );
              Syntax;
              return;
          end;
        end if;
      end if;
    end;
  end loop;

  if not Inp_opened then
    Put_Line(Standard_error,"Missing input file!");
    Syntax;
    return;
  end if;

  RC_Help.has_input:= inp_opened;

  RC_Help.Ada_Begin;

  YYParse;

  if Inp_Opened then
    RC_IO.Close_Input;
  end if;

end RC2GW;
