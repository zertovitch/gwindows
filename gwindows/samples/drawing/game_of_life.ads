package Game_of_Life is  --  Abstract Game of Life, could be moved out of this demo.
   type State is (Dead, Alive);
   type Map_Type is array (Positive range <>, Positive range <>) of State;
   --
   procedure Move (
      current_map : in     Map_Type;
      new_map     :    out Map_Type
   );

   procedure Clear (Map : out Map_Type);

   type Figure is (
     Point,
     --  Still lifes
     Block,
     --  Oscillators
     Blinker,
     Beacon,
     Pulsar,
     Pentadecathlon,
     --  Spaceships
     Glider,
     LWSS,
     MWSS,
     HWSS
   );

   procedure Add_Figure (Map : out Map_Type; xc, yc : Integer; f : Figure; s : State);

end Game_of_Life;
