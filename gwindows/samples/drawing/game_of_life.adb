package body Game_of_Life is
   procedure Move (
      current_map : in     Map_Type;
      new_map     :    out Map_Type
   )
   is
      w : constant Positive := current_map'Last (1);
      h : constant Positive := current_map'Last (2);
      function Count_Visible_Occupied (x, y : Positive) return Natural is
        occ : Natural := 0;
        --
        procedure Scan_Direction (dx, dy : Integer) is
        pragma Inline (Scan_Direction);
           xx : constant Integer := x + dx;
           yy : constant Integer := y + dy;
        begin
           loop
              exit when xx not in 1 .. w;
              exit when yy not in 1 .. h;
              case current_map (xx, yy) is
                 when Alive => occ := occ + 1; exit;
                 when Dead  => exit;
              end case;
              --  The following can be activated on more complex rules.
              --  See Advent of Code 2020, Day 11.
              --
              --  xx := xx + dx;
              --  yy := yy + dy;
           end loop;
        end Scan_Direction;
      begin
         for dx in -1 .. 1 loop
            for dy in -1 .. 1 loop
               if dx /= 0 or else dy /= 0 then
                  Scan_Direction (dx, dy);
               end if;
            end loop;
         end loop;
         return occ;
      end Count_Visible_Occupied;
      --
      occ : Natural;
      new_state : State;
   begin
      for x in 1 .. w loop
         for y in 1 .. h loop
            new_state := current_map (x, y);
            occ := Count_Visible_Occupied (x, y);
            case current_map (x, y) is
               when Dead =>
                  if occ = 3 then
                     new_state := Alive;
                  end if;
               when Alive =>
                  if occ < 2 or occ > 3 then
                     new_state := Dead;
                  end if;
            end case;
            new_map (x, y) := new_state;
         end loop;
      end loop;
   end Move;

   procedure Clear (Map : out Map_Type) is
   begin
      Map := (others => (others => Dead));
   end Clear;

   type Dimension is record Width, Height : Positive; end record;

   Dims : constant array (Figure) of Dimension :=
   (
     Point           =>  (1,  1),
     Block           =>  (2,  2),
     Blinker         =>  (3,  1),
     Beacon          =>  (4,  4),
     Pulsar          => (15, 15),
     Pentadecathlon  =>  (3,  8),
     Glider          =>  (3,  3),
     LWSS            =>  (5,  4),
     MWSS            =>  (6,  5),
     HWSS            =>  (7,  5)
   );

   procedure Add_Figure (Map : out Map_Type; xc, yc : Integer; f : Figure; s : State) is
     wm1 : constant Positive := Dims (f).Width - 1;
     hm1 : constant Positive := Dims (f).Height - 1;
     x, y : Integer;
     --
     procedure P (pat : String; x, y : Integer) is
       xx : Integer := x;
     begin
       for c of pat loop
         if c /= ' ' then
           Map (xx, y) := s;
         end if;
         xx := xx + 1;
       end loop;
     end P;
     --
   begin
      x := xc - wm1 / 2;
      y := yc - hm1 / 2;
      if          x        in Map'Range (1)
        and then (x + wm1) in Map'Range (1)
        and then  y        in Map'Range (2)
        and then (y + hm1) in Map'Range (2)
      then
         case f is
            when Point =>
               Map (x, y) := s;
            when Block =>
               P ("**", x, y);
               P ("**", x, y + 1);
            when Blinker =>
               P ("***", x, y);
            when Glider =>
               P (" * ", x, y);
               P ("  *", x, y + 1);
               P ("***", x, y + 2);
            when Beacon =>
               P ("**  ", x, y);
               P ("**  ", x, y + 1);
               P ("  **", x, y + 2);
               P ("  **", x, y + 3);
            when Pulsar =>
               P ("    *     *    ", x, y);
               P ("    *     *    ", x, y +  1);
               P ("    **   **    ", x, y +  2);
               P ("               ", x, y +  3);
               P ("***  ** **  ***", x, y +  4);
               P ("  * * * * * *  ", x, y +  5);
               P ("    **   **    ", x, y +  6);
               P ("               ", x, y +  7);
               P ("    **   **    ", x, y +  8);
               P ("  * * * * * *  ", x, y +  9);
               P ("***  ** **  ***", x, y + 10);
               P ("               ", x, y + 11);
               P ("    **   **    ", x, y + 12);
               P ("    *     *    ", x, y + 13);
               P ("    *     *    ", x, y + 14);
            when Pentadecathlon =>
               P ("***", x, y);
               P ("* *", x, y +  1);
               P ("***", x, y +  2);
               P ("***", x, y +  3);
               P ("***", x, y +  4);
               P ("***", x, y +  5);
               P ("* *", x, y +  6);
               P ("***", x, y +  7);
            when LWSS =>
               P (" **  ", x, y);
               P ("**** ", x, y + 1);
               P ("** **", x, y + 2);
               P ("  ** ", x, y + 3);
            when MWSS =>
               P (" *****", x, y);
               P ("*    *", x, y + 1);
               P ("     *", x, y + 2);
               P ("*   * ", x, y + 3);
               P ("  *   ", x, y + 4);
            when HWSS =>
               P (" ******", x, y);
               P ("*     *", x, y + 1);
               P ("      *", x, y + 2);
               P ("*    * ", x, y + 3);
               P ("  **   ", x, y + 4);
         end case;
      end if;
   end Add_Figure;

end Game_of_Life;
