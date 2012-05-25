package parser is
  procedure build_eof_action;
  procedure yyerror(msg: string);
  procedure YYParse;
  def_rule:integer;
end parser;
