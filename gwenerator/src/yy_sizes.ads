-- 14-Jan-2006: this package sets values which were previously
--              hardcoded in the the ayacc-generated yyparse

package yy_sizes is

   -- the size of the value and state stacks
   --  Affects error 'Stack size exceeded on state_stack'

   stack_size : constant Natural := 100_000;
   -- This size allows to absorb Doom 3's largest level, mc_underground.proc

end yy_sizes;
