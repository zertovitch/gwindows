with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package GWens is

  type RC_compiler_choice is (
    none,
    -- rc_cvtres, -- the Microsoft way
    windres       -- GNU
  );

  type GWen is record
    name          : Unbounded_String:= To_Unbounded_String("Untitled");
    titled        : Boolean:= False;
    modified      : Boolean:= False;
    show_details  : Boolean:= False;
    --
    -- Data to be saved:
    --
    RC_name       : Unbounded_String;
    RC_listen     : Boolean:= True;
    RC_auto_trans : Boolean:= True;
    RC_compile    : RC_compiler_choice:= windres;
    --
    separate_items      : Boolean:= False;
    base_x              : Integer:= 6;
    base_y              : Integer:= 13;
    base_defaults       : Boolean:= True;
    initialize_controls : Boolean:= False;
    --
    Ada_main      : Unbounded_String;
    Ada_listen    : Boolean:= True;
    Ada_auto_build: Boolean:= True;
    --
    Ada_command   : Unbounded_String:= To_Unbounded_String("gnatmake -i %1");
  end record;

end GWens;