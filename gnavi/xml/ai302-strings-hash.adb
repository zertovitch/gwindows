------------------------------------------------------------------------------
--                                                                          --
--                   AI-302 Reference Implementation                        --
--                                                                          --
--              Copyright (C) 2003-2004 Matthew J Heaney                    --
--                                                                          --
-- The AI-302 Reference Implementation is free software; you can            --
-- redistribute it and/or modify it under terms of the GNU General Public   --
-- License as published by the Free Software Foundation; either version 2,  --
-- or (at your option) any later version.  Charles is distributed in the    --
-- hope that it will be useful, but WITHOUT ANY WARRANTY; without even the  --
-- implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. --
-- See the GNU General Public License for more details.  You should have    --
-- received a copy of the GNU General Public License distributed with       --
-- Charles;  see file COPYING.TXT.  If not, write to the Free Software      --
-- Foundation,  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                          --
-- As a special exception, if other files instantiate generics from this    --
-- unit, or you link this unit with other files to produce an executable,   --
-- this unit does not by itself cause the resulting executable to be        --
-- covered by the GNU General Public License.  This exception does not      --
-- however invalidate any other reasons why the executable file might be    --
-- covered by the GNU Public License.                                       --
--                                                                          --
-- The AI-302 Reference Implementation is maintained by Matthew J Heaney.   --
--                                                                          --
-- mailto:matthewjheaney@earthlink.net                                      --
-- http://home.earthlink.net/~matthewjheaney/index.html                     --
--                                                                          --
------------------------------------------------------------------------------

--  source of this algorithm: GNAT.HTable.Hash (g-htable.adb)

function AI302.Strings.Hash (Key : String) return Containers.Hash_Type is

   use AI302.Containers;

   function Rotate_Left
     (Value  : Hash_Type;
      Amount : Natural) return Hash_Type;

   pragma Import (Intrinsic, Rotate_Left);

   Tmp : Hash_Type := 0;

begin

   for J in Key'Range loop
      Tmp := Rotate_Left (Tmp, 1) + Character'Pos (Key (J));
   end loop;

   return Tmp;

end AI302.Strings.Hash;
