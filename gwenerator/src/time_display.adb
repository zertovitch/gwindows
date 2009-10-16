--  Returns date & time for a log file, e.g. "2002/10/21   16:29:51"
--  Ada 83 compatible.
--  32- or 64-bit: Compaq Ada (83), GNAT (95), ObjectAda (95)
--  16-bit:        Meridian (83) -> Long_Integer is 32-bit
--  16-bit:        Janus 2.x (83): KO: no Long_Integer
--
--  Test program in following comment:
--
--   with Text_IO,Time_display;procedure Test is begin Text_IO.Put(Time_display);end;

with Calendar;

function Time_display(
  T      : Calendar.Time:= Calendar.Clock;
  Seconds: Boolean      := True
) return String
is
  use Calendar;
  subtype Sec_int is Long_Integer; -- must contain 86_400
  m, s : Sec_int;

begin
  s := Sec_int( Calendar.Seconds(T) );
  m := s / 60;

  declare
    -- + 100: trick for obtaining 0x
    sY : constant String:= Integer'Image( Year(T));
    sM : constant String:= Integer'Image( Month(T) + 100);
    sD : constant String:= Integer'Image(  Day(T)  + 100);
    shr: constant String:= Sec_int'Image( m  /  60 + 100);
    smn: constant String:= Sec_int'Image( m mod 60 + 100);
    ssc: constant String:= Sec_int'Image( s mod 60 + 100);

  begin
    return
      sY( sY'Last-3 .. sY'Last ) & '/' &  -- not Year 10'000 compliant.
      sM( sM'Last-1 .. sM'Last ) & '/' &
      sD( sD'Last-1 .. sD'Last ) &
      "   " &
      shr( shr'Last-1 .. shr'Last ) & ':' &
      smn( smn'Last-1 .. smn'Last ) & ':' &
      ssc( ssc'Last-1 .. ssc'Last );
  end;

end Time_display;
