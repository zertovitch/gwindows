with Win32.Winuser;
with Win32.Winbase;

package body GNATExample is

   ---------------
   -- Can_Close --
   ---------------

   procedure Can_Close is
      use type Interfaces.C.Long;

      bResult : Win32.BOOL;
   begin
      if
        (Server_Lock_Count = 0)
        and
        (Component_Count = 0)
        and
        (InProcServer /= True)
      then
         bResult :=
           Win32.Winuser.PostThreadMessage (Win32.Winbase.GetCurrentThreadId,
                                            Win32.Winuser.WM_QUIT,
                                            0,
                                            0);
      end if;
   end Can_Close;

end GNATExample;
