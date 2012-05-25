package body VSTRINGS is

  -- local declarations

  FILL_CHAR : constant CHARACTER := ASCII.NUL;

--  procedure FORMAT(THE_STRING : in out VSTRING; OLDLEN : in STRINDEX := LAST) is
--    -- fill the string with FILL_CHAR to null out old values

--    begin -- FORMAT (Local Procedure)
--      THE_STRING.VALUE(THE_STRING.LEN + 1 .. OLDLEN) := 
--                                        (others => FILL_CHAR);
--    end FORMAT;

-- CvdL: the above assignment will be compiled wrongly with gcc-2.6.0
-- and gnat-1.83. It overwrites parts if the runtime stack by writing
-- fill_char for a fixed amount(?) of times.
-- Maybe the error is in the caller... it is called FORMAT(Str, 0)!!!
-- The following procedure is far from optimal but it doesn't break
-- NOTE: gcc-2.6.2 and gnat-2.00 do it wrong too!

  procedure FORMAT(THE_STRING : in out VSTRING; OLDLEN : in STRINDEX := LAST) 
  is
    -- fill the string with FILL_CHAR to null out old values

    begin -- FORMAT (Local Procedure)
      for I in the_string.len+1..oldlen loop
        THE_STRING.VALUE(I) := FILL_CHAR;
      end loop;
    end FORMAT;


  -- bodies of visible operations

  function LEN(FROM : VSTRING) return STRINDEX is
    begin
      return FROM.LEN;
    end LEN;


  --  function MAX(FROM : VSTRING) return STRINDEX is
  --    begin
  --      return LAST;
  --    end MAX;


  function STR(FROM : VSTRING) return STRING is
    begin
      return FROM.VALUE(FIRST .. FROM.LEN);
    end STR;


  function CHAR(FROM : VSTRING; POSITION : STRINDEX := FIRST)
                 return CHARACTER is
    begin
      if POSITION not in FIRST .. FROM.LEN
        then raise CONSTRAINT_ERROR;
       end if;
      return FROM.VALUE(POSITION);
    end CHAR;


  function "<" (LEFT: VSTRING; RIGHT: VSTRING) return BOOLEAN is
    begin -- "<"
      return LEFT.VALUE < RIGHT.VALUE;
    end "<";


  function ">" (LEFT: VSTRING; RIGHT: VSTRING) return BOOLEAN is
    begin -- ">"
      return LEFT.VALUE > RIGHT.VALUE;
    end ">";


  function "<=" (LEFT: VSTRING; RIGHT: VSTRING) return BOOLEAN is
    begin -- "<="
      return LEFT.VALUE <= RIGHT.VALUE;
    end "<=";


  function ">=" (LEFT: VSTRING; RIGHT: VSTRING) return BOOLEAN is
    begin -- ">="
      return LEFT.VALUE >= RIGHT.VALUE;
    end ">=";


  procedure PUT(FILE : in FILE_TYPE; ITEM : in VSTRING) is
    begin -- PUT
      PUT(FILE, ITEM.VALUE(FIRST .. ITEM.LEN));
    end PUT;

  procedure Put(ITEM : in VSTRING) is
    begin -- PUT
      PUT(ITEM.VALUE(FIRST .. ITEM.LEN));
    end PUT;


  procedure PUT_LINE(FILE : in FILE_TYPE; ITEM : in VSTRING) is
    begin -- PUT_LINE
      PUT_LINE(FILE, ITEM.VALUE(FIRST .. ITEM.LEN));
    end PUT_LINE;

  procedure PUT_LINE(ITEM : in VSTRING) is
    begin -- PUT_LINE
      PUT_LINE(ITEM.VALUE(FIRST .. ITEM.LEN));
    end PUT_LINE;


  procedure GET(FILE : in FILE_TYPE; ITEM : out VSTRING;
                LENGTH : in STRINDEX := LAST) is
    begin -- GET
      if LENGTH not in FIRST .. LAST
        then raise CONSTRAINT_ERROR;
       end if;

      ITEM := NUL;
      for INDEX in FIRST .. LENGTH loop
        GET(FILE, ITEM.VALUE(INDEX));
        ITEM.LEN := INDEX;
       end loop;
    end GET;

  procedure GET(ITEM : out VSTRING; LENGTH : in STRINDEX := LAST) is
    begin -- GET
      if LENGTH not in FIRST .. LAST
        then raise CONSTRAINT_ERROR;
       end if;

      ITEM := NUL;
      for INDEX in FIRST .. LENGTH loop
        GET(ITEM.VALUE(INDEX));
        ITEM.LEN := INDEX;
       end loop;
    end GET;


  procedure GET_LINE(FILE : in FILE_TYPE; ITEM : in out VSTRING) is

    OLDLEN : constant STRINDEX := ITEM.LEN;

    begin -- GET_LINE
      GET_LINE(FILE, ITEM.VALUE, ITEM.LEN);
      FORMAT(ITEM, OLDLEN);
    end GET_LINE;
       
  procedure GET_LINE(ITEM : in out VSTRING) is

    OLDLEN : constant STRINDEX := ITEM.LEN;

    begin -- GET_LINE
      GET_LINE(ITEM.VALUE, ITEM.LEN);
      FORMAT(ITEM, OLDLEN);
    end GET_LINE;


  function SLICE(FROM : VSTRING; FRONT, BACK : STRINDEX) return VSTRING is

    begin -- SLICE
      if ((FRONT not in FIRST .. FROM.LEN) or else 
         (BACK not in FIRST .. FROM.LEN)) and then FRONT <= BACK
        then raise CONSTRAINT_ERROR;
       end if;

      return Vstr(FROM.VALUE(FRONT .. BACK));
    end SLICE;


  function SUBSTR(FROM : VSTRING; START, LENGTH : STRINDEX) return VSTRING is

    begin -- SUBSTR
      if (START not in FIRST .. FROM.LEN) or else
         ((START + LENGTH - 1 not in FIRST .. FROM.LEN)
          and then (LENGTH > 0))
        then raise CONSTRAINT_ERROR;
       end if;

      return Vstr(FROM.VALUE(START .. START + LENGTH -1));
    end SUBSTR;


  function DELETE(FROM : VSTRING; FRONT, BACK : STRINDEX) return VSTRING is

    TEMP : VSTRING := FROM;

    begin -- DELETE
      if ((FRONT not in FIRST .. FROM.LEN) or else
         (BACK not in FIRST .. FROM.LEN)) and then FRONT <= BACK
        then raise CONSTRAINT_ERROR;
       end if;

      if FRONT > BACK then return FROM; end if;
      TEMP.LEN := FROM.LEN - (BACK - FRONT) - 1;

      TEMP.VALUE(FRONT .. TEMP.LEN) := FROM.VALUE(BACK + 1 .. FROM.LEN);
      FORMAT(TEMP, FROM.LEN);
      return TEMP;
    end DELETE;


  function INSERT(TARGET: VSTRING; ITEM: VSTRING;
                  POSITION : STRINDEX := FIRST) return VSTRING is

    TEMP : VSTRING;

    begin -- INSERT
      if POSITION not in FIRST .. TARGET.LEN
        then raise CONSTRAINT_ERROR;
       end if;

      if TARGET.LEN + ITEM.LEN > LAST
        then raise CONSTRAINT_ERROR;
        else TEMP.LEN := TARGET.LEN + ITEM.LEN;
       end if;

      TEMP.VALUE(FIRST .. POSITION - 1) := TARGET.VALUE(FIRST .. POSITION - 1);
      TEMP.VALUE(POSITION .. (POSITION + ITEM.LEN - 1)) :=
        ITEM.VALUE(FIRST .. ITEM.LEN);
      TEMP.VALUE((POSITION + ITEM.LEN) .. TEMP.LEN) :=
        TARGET.VALUE(POSITION .. TARGET.LEN);

      return TEMP;
    end INSERT;

  function INSERT(TARGET: VSTRING; ITEM: STRING;
                  POSITION : STRINDEX := FIRST) return VSTRING is
    begin -- INSERT
      return INSERT(TARGET, VSTR(ITEM), POSITION);
    end INSERT;
  
  function INSERT(TARGET: VSTRING; ITEM: CHARACTER;
                  POSITION : STRINDEX := FIRST) return VSTRING is
    begin -- INSERT
      return INSERT(TARGET, VSTR(ITEM), POSITION);
    end INSERT;


  function APPEND(TARGET: VSTRING; ITEM: VSTRING; POSITION : STRINDEX)
                  return VSTRING is

    TEMP : VSTRING;
    POS : constant STRINDEX := POSITION;

    begin -- APPEND
      if POSITION not in FIRST .. TARGET.LEN
        then raise CONSTRAINT_ERROR;
       end if;

      if TARGET.LEN + ITEM.LEN > LAST
        then raise CONSTRAINT_ERROR;
        else TEMP.LEN := TARGET.LEN + ITEM.LEN;
       end if;

      TEMP.VALUE(FIRST .. POS) := TARGET.VALUE(FIRST .. POS);
      TEMP.VALUE(POS + 1 .. (POS + ITEM.LEN)) := ITEM.VALUE(FIRST .. ITEM.LEN);
      TEMP.VALUE((POS + ITEM.LEN + 1) .. TEMP.LEN) :=
        TARGET.VALUE(POS + 1 .. TARGET.LEN);

      return TEMP;
    end APPEND;

  function APPEND(TARGET: VSTRING; ITEM: STRING; POSITION : STRINDEX)
                  return VSTRING is
    begin
      return APPEND(TARGET, VSTR(ITEM), POSITION);
    end APPEND;

  function APPEND(TARGET: VSTRING; ITEM: CHARACTER; POSITION : STRINDEX)
                  return VSTRING is
    begin
      return APPEND(TARGET, VSTR(ITEM), POSITION);
    end APPEND;


  function APPEND(TARGET: VSTRING; ITEM: VSTRING) return VSTRING is
    begin
      return APPEND(TARGET, ITEM, TARGET.LEN);
    end APPEND;

  function APPEND(TARGET: VSTRING; ITEM: STRING) return VSTRING is
    begin
      return APPEND(TARGET, VSTR(ITEM), TARGET.LEN);
    end APPEND;

  function APPEND(TARGET: VSTRING; ITEM: CHARACTER) return VSTRING is
    begin
      return APPEND(TARGET, VSTR(ITEM), TARGET.LEN);
    end APPEND;


  function REPLACE(TARGET: VSTRING; ITEM: VSTRING;
                   POSITION : STRINDEX := FIRST) return VSTRING is
    TEMP : VSTRING;

    begin -- REPLACE
      if POSITION not in FIRST .. TARGET.LEN
        then raise CONSTRAINT_ERROR;
       end if;

      if POSITION + ITEM.LEN - 1 <= TARGET.LEN
        then TEMP.LEN := TARGET.LEN;
        elsif POSITION + ITEM.LEN - 1 > LAST
          then raise CONSTRAINT_ERROR;
          else TEMP.LEN := POSITION + ITEM.LEN - 1;
       end if;

      TEMP.VALUE(FIRST .. POSITION - 1) := TARGET.VALUE(FIRST .. POSITION - 1);
      TEMP.VALUE(POSITION .. (POSITION + ITEM.LEN - 1)) := 
        ITEM.VALUE(FIRST .. ITEM.LEN);
      TEMP.VALUE((POSITION + ITEM.LEN) .. TEMP.LEN) :=
        TARGET.VALUE((POSITION + ITEM.LEN) .. TARGET.LEN);

      return TEMP;
    end REPLACE;

  function REPLACE(TARGET: VSTRING; ITEM: STRING;
                   POSITION : STRINDEX := FIRST) return VSTRING is
    begin
      return REPLACE(TARGET, VSTR(ITEM), POSITION);
    end REPLACE;

  function REPLACE(TARGET: VSTRING; ITEM: CHARACTER;
                   POSITION : STRINDEX := FIRST) return VSTRING is
    begin
      return REPLACE(TARGET, VSTR(ITEM), POSITION);
    end REPLACE;


  function "&"(LEFT:VSTRING; RIGHT : VSTRING) return VSTRING is
    TEMP : VSTRING;

    begin -- "&"
      if LEFT.LEN + RIGHT.LEN > LAST
        then raise CONSTRAINT_ERROR;
        else TEMP.LEN := LEFT.LEN + RIGHT.LEN;
       end if;

      TEMP.VALUE(FIRST .. TEMP.LEN) := LEFT.VALUE(FIRST .. LEFT.LEN) &
        RIGHT.VALUE(FIRST .. RIGHT.LEN);
      return TEMP;
    end "&";

  function "&"(LEFT:VSTRING; RIGHT : STRING) return VSTRING is
    begin
      return LEFT & VSTR(RIGHT);
    end "&";

  function "&"(LEFT:VSTRING; RIGHT : CHARACTER) return VSTRING is
    begin
      return LEFT & VSTR(RIGHT);
    end "&";

  function "&"(LEFT : STRING; RIGHT : VSTRING) return VSTRING is
    begin
      return VSTR(LEFT) & RIGHT;
    end "&";

  function "&"(LEFT : CHARACTER; RIGHT : VSTRING) return VSTRING is
    begin
      return VSTR(LEFT) & RIGHT;
    end "&";


  Function INDEX(WHOLE : VSTRING; PART : VSTRING; OCCURRENCE : NATURAL := 1)
                 return STRINDEX is

    NOT_FOUND : constant NATURAL := 0;
    INDEX : NATURAL := FIRST;
    COUNT : NATURAL := 0;

    begin -- INDEX
      if PART = NUL then return NOT_FOUND; -- by definition
        end if;

      while INDEX + PART.LEN - 1 <= WHOLE.LEN and then COUNT < OCCURRENCE loop
        if WHOLE.VALUE(INDEX .. PART.LEN + INDEX - 1) =
           PART.VALUE(1 .. PART.LEN)
          then COUNT := COUNT + 1;
         end if;
        INDEX := INDEX + 1;
       end loop;

      if COUNT = OCCURRENCE
        then return INDEX - 1;
        else return NOT_FOUND;
       end if;
    end INDEX;

  Function INDEX(WHOLE : VSTRING; PART : STRING; OCCURRENCE : NATURAL := 1)
                 return STRINDEX is

    begin
      return Index(WHOLE, VSTR(PART), OCCURRENCE);
    end INDEX;


  Function INDEX(WHOLE : VSTRING; PART : CHARACTER; OCCURRENCE : NATURAL := 1)
                 return STRINDEX is

    begin
      return Index(WHOLE, VSTR(PART), OCCURRENCE);
    end INDEX;


  function RINDEX(WHOLE: VSTRING; PART:VSTRING; OCCURRENCE:NATURAL := 1) 
                 return STRINDEX is

    NOT_FOUND : constant NATURAL := 0;
    INDEX : INTEGER := WHOLE.LEN - (PART.LEN -1);
    COUNT : NATURAL := 0;

    begin -- RINDEX
      if PART = NUL then return NOT_FOUND; -- by definition
        end if;

      while INDEX >= FIRST and then COUNT < OCCURRENCE loop
        if WHOLE.VALUE(INDEX .. PART.LEN + INDEX - 1) =
           PART.VALUE(1 .. PART.LEN)
          then COUNT := COUNT + 1;
         end if;
        INDEX := INDEX - 1;
       end loop;

      if COUNT = OCCURRENCE
        then
          if COUNT > 0
            then return INDEX + 1;
            else return NOT_FOUND;
           end if;
        else return NOT_FOUND;
       end if;
    end RINDEX;

  Function RINDEX(WHOLE : VSTRING; PART : STRING; OCCURRENCE : NATURAL := 1)
                 return STRINDEX is
    begin
      return RINDEX(WHOLE, VSTR(PART), OCCURRENCE);
    end RINDEX;


  Function RINDEX(WHOLE : VSTRING; PART : CHARACTER; OCCURRENCE : NATURAL := 1)
                 return STRINDEX is
    begin
      return RINDEX(WHOLE, VSTR(PART), OCCURRENCE);
    end RINDEX;


  function VSTR(FROM : CHARACTER) return VSTRING is
    
    TEMP : VSTRING;

    begin -- VSTR
      if LAST < 1
        then raise CONSTRAINT_ERROR;
        else TEMP.LEN := 1;
       end if;

      TEMP.VALUE(FIRST) := FROM;
      return TEMP;
    end VSTR;


  function VSTR(FROM : STRING) return VSTRING is

    TEMP : VSTRING;

    begin -- VSTR
      if FROM'LENGTH > LAST
        then raise CONSTRAINT_ERROR;
        else TEMP.LEN := FROM'LENGTH;
       end if;

      TEMP.VALUE(FIRST .. FROM'LENGTH) := FROM;
      return TEMP;
    end VSTR;

  Function "+" (FROM : STRING) return VSTRING is
    begin
      return VSTR(FROM);
    end "+";

  Function "+" (FROM : CHARACTER) return VSTRING is
    begin
     return VSTR(FROM);
    end "+";


  function CONVERT(X : FROM) return TO is
    begin
      return VSTR(STR(X));
    end CONVERT;   
end VSTRINGS;
