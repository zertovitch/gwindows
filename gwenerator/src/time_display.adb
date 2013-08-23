--  Time_display returns date & time, current or given.
--  E.g.: "2013/08/01  05:49:51"
--  Useful for a log file or a display of a lengthy operation.
--  This is Ada 83 compatible. Format accepted by SQL queries.
--
--    32- or 64-bit: DEC/Compaq/HP Ada (83), GNAT (95/2005), ObjectAda (95)
--    16-bit:        Meridian (83) -> Long_Integer is 32-bit
--    16-bit:        Janus 2.x (83): KO: no Long_Integer
--
--  Test program in following comment:
--
--   with Text_IO,Time_display;procedure Test is begin Text_IO.Put(Time_display);end;

with Calendar;

function Time_display(
  T        : Calendar.Time:= Calendar.Clock;
  Seconds  : Boolean      := True;
  Intra_day: Boolean      := True
)
  return String
is
  use Calendar;
  subtype Sec_int is Long_Integer; -- must contain 86_400
  s : constant Sec_int:= Sec_int( Calendar.Seconds(T) );
  m : constant Sec_int:= s / 60;
  -- + 100: trick for obtaining 0x
  sY : constant String:= Integer'Image( Year(T));
  sM : constant String:= Integer'Image( Month(T) + 100);
  sD : constant String:= Integer'Image(  Day(T)  + 100);
  shr: constant String:= Sec_int'Image( m  /  60 + 100);
  smn: constant String:= Sec_int'Image( m mod 60 + 100);
  ssc: constant String:= Sec_int'Image( s mod 60 + 100);
  --
  function Optional_seconds return String is
  begin
    if Seconds then
      return ':' & ssc( ssc'Last-1 .. ssc'Last );
    else
      return "";
    end if;
  end Optional_seconds;
  --
  function Optional_intra_day return String is
  begin
    if Intra_day then
      return
        "  " &
        shr( shr'Last-1 .. shr'Last ) & ':' &
        smn( smn'Last-1 .. smn'Last ) & Optional_seconds;
    else
      return "";
    end if;
  end Optional_intra_day;

begin
  return
    sY( sY'Last-3 .. sY'Last ) & '/' &  -- not Year 10'000 compliant.
    sM( sM'Last-1 .. sM'Last ) & '/' &
    sD( sD'Last-1 .. sD'Last ) &
    Optional_intra_day;
end Time_display;
