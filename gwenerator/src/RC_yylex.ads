with RC_tokens; use RC_tokens;
with yylex;

function RC_yylex return token renames yylex;
