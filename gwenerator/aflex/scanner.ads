with parse_tokens; use parse_tokens;

package scanner is
    call_yylex : boolean := false;
    function get_token return Token;
end scanner;
