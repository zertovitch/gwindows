with Rc_Tokens;

package yyroutines is

  use Rc_Tokens;

  function YYLex return Token;

  procedure UnYYLex(tok : Token);

end yyroutines;
