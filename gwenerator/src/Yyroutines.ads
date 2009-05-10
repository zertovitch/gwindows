with RC_tokens;

package yyroutines is

  use RC_tokens;

  function YYLex return Token;

  procedure UnYYLex(tok : Token);

end yyroutines;
