package GWens.IO is

  --  Keys for easy and safe load / save through a file or the registry.
  --  Each key correspond to a record field of the type GWen.

  type Key is
  (
    RC_name,
    RC_listen,
    RC_auto_trans,
    RC_compile,
    --
    separate_items,
    base_x,
    base_y,
    base_defaults,
    initialize_controls,
    --
    Ada_main,
    Ada_listen,
    Ada_auto_build,
    --
    Ada_command,
    --
    show_details,
    show_ada_build
  );

  --  Load / Save through files

  procedure Load (file_name : in String; proj : out GWen; success : out Boolean);

  procedure Save (proj : in out GWen);

end GWens.IO;
