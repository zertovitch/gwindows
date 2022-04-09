--  Abstract Game of Life (pure Ada, unrelated to any graphics system).
  
package Game_of_Life is

   type State is (Dead, Alive);

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

   type Map_Type is array (Positive range <>, Positive range <>) of State;

   procedure Add_Figure (Map : out Map_Type; xc, yc : Integer; f : Figure; s : State);

   procedure Clear (Map : out Map_Type);

   procedure Move (
      current_map : in     Map_Type;
      new_map     :    out Map_Type
   );

end Game_of_Life;
