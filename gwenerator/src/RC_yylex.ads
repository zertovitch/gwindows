with Rc_Tokens; use Rc_Tokens;
with YYLex;

function RC_yylex return Token renames YYLex;
