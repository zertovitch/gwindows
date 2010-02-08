with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;          use Ada.Text_IO.Unbounded_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;

package body GWens.IO is

  package Key_IO is new Enumeration_IO(Key); use Key_IO;
  package Bool_IO is new Enumeration_IO(Boolean); use Bool_IO;
  package RCC_IO is new Enumeration_IO(RC_compiler_choice); use RCC_IO;

  function S(Source: Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U(Source: String) return Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  ----------
  -- Load --
  ----------

  procedure Load (file_name: in String; proj: out GWen; success: out Boolean) is
    f: File_Type;
    k: Key;
    fresh_gwen: GWen;
    -- ^ initialized with defaults;
    -- for the case of an incomplete .gwen file,
    -- e.g. saved with an older version
    dummy: Character;
    is_gwen_file: Boolean:= False;
  begin
    proj:= fresh_gwen;
    Open(f, In_File, file_name);
    Skip_Line(f);
    while not End_of_File(f) loop
      begin
        Get(f, k);
        case k is
          when RC_name =>
            Get(f, dummy); -- absorb the first separating ' '
            Get_Line(f, proj.RC_Name);
            is_gwen_file:= True;
          when RC_listen =>
            Get(f, proj.RC_Listen);
          when RC_auto_trans =>
            Get(f, proj.RC_auto_trans);
          when RC_compile =>
            Get(f, proj.RC_compile);
          --
          when separate_items =>
            Get(f, proj.separate_items);
          when base_x =>
            Get(f, proj.base_x);
          when base_y =>
            Get(f, proj.base_y);
          when base_defaults =>
            Get(f, proj.base_defaults);
          when initialize_controls =>
            Get(f, proj.initialize_controls);
          --
          when Ada_main =>
            Get(f, dummy); -- absorb the first separating ' '
            Get_Line(f, proj.Ada_Main);
          when Ada_listen =>
            Get(f, proj.Ada_listen);
          when Ada_auto_build =>
            Get(f, proj.Ada_auto_build);
          --
          when Ada_command =>
            Get(f, dummy); -- absorb the first separating ' '
            Get_Line(f, proj.Ada_command);
          --
          when show_details =>
            Get(f, proj.show_details);
        end case;
      exception
        when Data_Error => -- item from a later version
          Skip_Line(f);
      end;
    end loop;
    Close(f);
    if is_gwen_file then
      proj.name:= U(file_name);
      proj.titled:= True; -- project has at least now a file name
    end if;
    success:= is_gwen_file;
  end Load;

  ----------
  -- Save --
  ----------

  procedure Save (proj: in out GWen) is
    f: File_Type;
  begin
    Create(f, Out_File, S(proj.name));
    Put_Line(f,
      "This is a GWenerator project file (a ""GWen"")." &
      " Visit GWenerator on the Web: http://sf.net/projects/gnavi"
    );
    for k in Key loop
      Put(f, k);
      Put(f, ' ');
      case k is
        when RC_name =>
          Put_Line(f, proj.RC_Name);
        when RC_listen =>
          Put(f, proj.RC_Listen);
          New_Line(f);
        when RC_auto_trans =>
          Put(f, proj.RC_auto_trans);
          New_Line(f);
        when RC_compile =>
          Put(f, proj.RC_compile);
          Put_Line(f, " -- compilation of .rc script to a .rbj object file");
        --
        when separate_items =>
          Put(f, proj.separate_items);
          New_Line(f);
        when base_x =>
          Put(f, proj.base_x);
          New_Line(f);
        when base_y =>
          Put(f, proj.base_y);
          New_Line(f);
        when base_defaults =>
          Put(f, proj.base_defaults);
          New_Line(f);
        when initialize_controls =>
          Put(f, proj.initialize_controls);
          New_Line(f);
        --
        when Ada_main =>
          Put_Line(f, proj.Ada_Main);
        when Ada_listen =>
          Put(f, proj.Ada_listen);
          New_Line(f);
        when Ada_auto_build =>
          Put(f, proj.Ada_auto_build);
          New_Line(f);
        --
        when Ada_command =>
          Put_Line(f, proj.Ada_command);
        --
        when show_details =>
          Put(f, proj.show_details);
          New_Line(f);
      end case;
    end loop;
    Close(f);
    proj.modified:= False;
  end Save;

end GWens.IO;
