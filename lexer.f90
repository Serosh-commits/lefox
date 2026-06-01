module lexer_mod
    implicit none

    integer, parameter :: TOK_EOF = 0
    integer, parameter :: TOK_ERROR = 1
    integer, parameter :: TOK_IDENT = 2
    integer, parameter :: TOK_INT = 3
    integer, parameter :: TOK_FLOAT = 4
    integer, parameter :: TOK_STRING = 5
    integer, parameter :: TOK_CHAR = 6
    integer, parameter :: TOK_LPAREN = 7
    integer, parameter :: TOK_RPAREN = 8
    integer, parameter :: TOK_LBRACE = 9
    integer, parameter :: TOK_RBRACE = 10
    integer, parameter :: TOK_LBRACKET = 11
    integer, parameter :: TOK_RBRACKET = 12
    integer, parameter :: TOK_SEMICOLON = 13
    integer, parameter :: TOK_COMMA = 14
    integer, parameter :: TOK_DOT = 15
    integer, parameter :: TOK_TILDE = 16
    integer, parameter :: TOK_QUESTION = 17
    integer, parameter :: TOK_AT = 18
    integer, parameter :: TOK_HASH = 19
    integer, parameter :: TOK_CARET = 20
    integer, parameter :: TOK_PERCENT = 21
    integer, parameter :: TOK_PLUS = 22
    integer, parameter :: TOK_PLUS_EQ = 23
    integer, parameter :: TOK_MINUS = 24
    integer, parameter :: TOK_MINUS_EQ = 25
    integer, parameter :: TOK_ARROW = 26
    integer, parameter :: TOK_STAR = 27
    integer, parameter :: TOK_STAR_EQ = 28
    integer, parameter :: TOK_SLASH = 29
    integer, parameter :: TOK_SLASH_EQ = 30
    integer, parameter :: TOK_ASSIGN = 31
    integer, parameter :: TOK_EQ = 32
    integer, parameter :: TOK_BANG = 33
    integer, parameter :: TOK_NE = 34
    integer, parameter :: TOK_LT = 35
    integer, parameter :: TOK_LE = 36
    integer, parameter :: TOK_LSHIFT = 37
    integer, parameter :: TOK_GT = 38
    integer, parameter :: TOK_GE = 39
    integer, parameter :: TOK_RSHIFT = 40
    integer, parameter :: TOK_AMP = 41
    integer, parameter :: TOK_LOGICAL_AND = 42
    integer, parameter :: TOK_PIPE = 43
    integer, parameter :: TOK_LOGICAL_OR = 44
    integer, parameter :: TOK_COLON = 45
    integer, parameter :: TOK_DOUBLE_COLON = 46
    integer, parameter :: TOK_ELLIPSIS = 47

    integer, parameter :: MAX_TOKEN_TEXT = 256
    integer, parameter :: MAX_BUFFER_SIZE = 8192

    type token_t
        integer :: kind
        integer :: line
        integer :: col
        character(len=MAX_TOKEN_TEXT) :: text
        integer :: text_len
        integer(kind=1) :: ival
        real(kind=8) :: fval
    end type token_t

    type lexer_t
        character(len=MAX_BUFFER_SIZE) :: buffer
        integer :: buffer_len
        integer :: pos
        integer :: line
        integer :: col
        type(token_t) :: tok
    end type lexer_t

contains

    function is_alpha(c) result(res)
        character, intent(in) :: c
        logical :: res
        res = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')
    end function is_alpha

    function is_digit(c) result(res)
        character, intent(in) :: c
        logical :: res
        res = (c >= '0' .and. c <= '9')
    end function is_digit

    function is_alnum(c) result(res)
        character, intent(in) :: c
        logical :: res
        res = is_alpha(c) .or. is_digit(c)
    end function is_alnum

    function is_hexdigit(c) result(res)
        character, intent(in) :: c
        logical :: res
        res = is_digit(c) .or. (c >= 'A' .and. c <= 'F') .or. (c >= 'a' .and. c <= 'f')
    end function is_hexdigit

    function is_whitespace(c) result(res)
        character, intent(in) :: c
        logical :: res
        res = (c == ' ' .or. c == char(9) .or. c == char(10) .or. c == char(13))
    end function is_whitespace

    function peekc(L) result(c)
        type(lexer_t), intent(inout) :: L
        character :: c
        if (L%pos <= L%buffer_len) then
            c = L%buffer(L%pos:L%pos)
        else
            c = char(0)
        end if
    end function peekc

    function peekc2(L) result(c)
        type(lexer_t), intent(inout) :: L
        character :: c
        if (L%pos+1 <= L%buffer_len) then
            c = L%buffer(L%pos+1:L%pos+1)
        else
            c = char(0)
        end if
    end function peekc2

    function getc_(L) result(c)
        type(lexer_t), intent(inout) :: L
        character :: c
        if (L%pos <= L%buffer_len) then
            c = L%buffer(L%pos:L%pos)
            L%pos = L%pos + 1
            if (c == char(10)) then
                L%line = L%line + 1
                L%col = 1
            else
                L%col = L%col + 1
            end if
        else
            c = char(0)
        end if
    end function getc_

    function at_end(L) result(res)
        type(lexer_t), intent(inout) :: L
        logical :: res
        res = (L%pos > L%buffer_len)
    end function at_end

    subroutine emit(L, kind, text, tlen)
        type(lexer_t), intent(inout) :: L
        integer, intent(in) :: kind
        character(len=*), intent(in) :: text
        integer, intent(in) :: tlen
        L%tok%kind = kind
        L%tok%line = L%line
        L%tok%col = L%col
        L%tok%text = repeat(' ', MAX_TOKEN_TEXT)
        L%tok%text(1:tlen) = text(1:tlen)
        L%tok%text_len = tlen
        L%tok%ival = 0_int1
        L%tok%fval = 0.0d0
    end subroutine emit

    subroutine emit_error(L, msg)
        type(lexer_t), intent(inout) :: L
        character(len=*), intent(in) :: msg
        write(*,'(I0,":",I0,": error: ",A)') L%tok%line, L%tok%col, trim(msg)
        call emit(L, TOK_ERROR, msg, len_trim(msg))
    end subroutine emit_error

    function lex_escape(L) result(c)
        type(lexer_t), intent(inout) :: L
        character :: c
        character :: next_c
        integer :: val
        integer :: io_stat
        
        next_c = getc_(L)
        
        select case(next_c)
            case('n')
                c = char(10)
            case('t')
                c = char(9)
            case('r')
                c = char(13)
            case('0')
                c = char(0)
            case('a')
                c = char(7)
            case('b')
                c = char(8)
            case('f')
                c = char(12)
            case('v')
                c = char(11)
            case('\')
                c = '\'
            case('"')
                c = '"'
            case('''')
                c = ''''
            case('x')
                ! Read two hex digits
                if (L%pos <= L%buffer_len .and. is_hexdigit(L%buffer(L%pos:L%pos))) then
                    if (L%pos+1 <= L%buffer_len .and. is_hexdigit(L%buffer(L%pos+1:L%pos+1))) then
                        read(L%buffer(L%pos:L%pos+1), '(Z2)') val
                        c = char(val)
                        L%pos = L%pos + 2
                        L%col = L%col + 2
                    else
                        write(*,*) 'Warning: Incomplete hex escape at ', L%line, ':', L%col
                        c = 'x'
                    end if
                else
                    write(*,*) 'Warning: Invalid hex escape at ', L%line, ':', L%col
                    c = 'x'
                end if
            case default
                write(*,*) 'Warning: Unknown escape sequence \', next_c, ' at ', L%line, ':', L%col
                c = next_c
        end select
    end function lex_escape

    subroutine lex_ident(L)
        type(lexer_t), intent(inout) :: L
        character(len=MAX_TOKEN_TEXT) :: text
        integer :: len
        character :: c
        
        len = 0
        do while (.not. at_end(L))
            c = peekc(L)
            if (is_alnum(c) .or. c == '_') then
                len = len + 1
                if (len <= MAX_TOKEN_TEXT) then
                    text(len:len) = c
                end if
                c = getc_(L)
            else
                exit
            end if
        end do
        
        call emit(L, TOK_IDENT, text, len)
    end subroutine lex_ident

    subroutine lex_number(L)
        type(lexer_t), intent(inout) :: L
        character(len=MAX_TOKEN_TEXT) :: text
        integer :: len
        character :: c
        logical :: is_float
        integer(kind=1) :: int_val
        real(kind=8) :: float_val
        integer :: io_stat
        
        len = 0
        is_float = .false.
        
        c = peekc(L)
        
        ! Check for hex number
        if (c == '0' .and. peekc2(L) == 'x' .or. peekc2(L) == 'X') then
            c = getc_(L)  ! consume '0'
            c = getc_(L)  ! consume 'x' or 'X'
            len = 2
            
            do while (.not. at_end(L))
                c = peekc(L)
                if (is_hexdigit(c)) then
                    len = len + 1
                    if (len <= MAX_TOKEN_TEXT) then
                        text(len:len) = c
                    end if
                    c = getc_(L)
                else
                    exit
                end if
            end do
            
            ! Parse hex value
            read(text(3:len), '(Z16)', iostat=io_stat) int_val
            if (io_stat == 0) then
                L%tok%ival = int_val
            else
                call emit_error(L, 'Invalid hexadecimal number')
                return
            end if
            call emit(L, TOK_INT, text, len)
            return
        end if
        
        ! Regular decimal number
        do while (.not. at_end(L))
            c = peekc(L)
            if (is_digit(c)) then
                len = len + 1
                if (len <= MAX_TOKEN_TEXT) then
                    text(len:len) = c
                end if
                c = getc_(L)
            else if (c == '.' .and. .not. is_float) then
                ! Check if next char is also a dot (ellipsis)
                if (peekc2(L) == '.') then
                    exit
                end if
                is_float = .true.
                len = len + 1
                if (len <= MAX_TOKEN_TEXT) then
                    text(len:len) = c
                end if
                c = getc_(L)
            else if ((c == 'e' .or. c == 'E') .and. is_float) then
                len = len + 1
                if (len <= MAX_TOKEN_TEXT) then
                    text(len:len) = c
                end if
                c = getc_(L)
                
                ! Optional sign
                if (peekc(L) == '+' .or. peekc(L) == '-') then
                    len = len + 1
                    if (len <= MAX_TOKEN_TEXT) then
                        text(len:len) = peekc(L)
                    end if
                    c = getc_(L)
                end if
                
                ! Exponent digits
                do while (.not. at_end(L))
                    c = peekc(L)
                    if (is_digit(c)) then
                        len = len + 1
                        if (len <= MAX_TOKEN_TEXT) then
                            text(len:len) = c
                        end if
                        c = getc_(L)
                    else
                        exit
                    end if
                end do
                exit
            else
                exit
            end if
        end do
        
        if (is_float) then
            read(text(1:len), *, iostat=io_stat) float_val
            if (io_stat == 0) then
                L%tok%fval = float_val
            else
                call emit_error(L, 'Invalid floating point number')
                return
            end if
            call emit(L, TOK_FLOAT, text, len)
        else
            read(text(1:len), *, iostat=io_stat) int_val
            if (io_stat == 0) then
                L%tok%ival = int_val
            else
                call emit_error(L, 'Invalid integer number')
                return
            end if
            call emit(L, TOK_INT, text, len)
        end if
    end subroutine lex_number

    subroutine lex_string(L)
        type(lexer_t), intent(inout) :: L
        character(len=MAX_TOKEN_TEXT) :: text
        integer :: len
        character :: c
        
        c = getc_(L)  ! consume opening quote
        len = 0
        
        do while (.not. at_end(L))
            c = getc_(L)
            if (c == '"') then
                exit
            else if (c == char(10)) then
                call emit_error(L, 'Newline in string literal')
                return
            else if (c == '\') then
                c = lex_escape(L)
                len = len + 1
                if (len <= MAX_TOKEN_TEXT) then
                    text(len:len) = c
                end if
            else
                len = len + 1
                if (len <= MAX_TOKEN_TEXT) then
                    text(len:len) = c
                end if
            end if
        end do
        
        if (c /= '"') then
            call emit_error(L, 'Unterminated string literal')
            return
        end if
        
        call emit(L, TOK_STRING, text, len)
    end subroutine lex_string

    subroutine lex_charlit(L)
        type(lexer_t), intent(inout) :: L
        character(len=MAX_TOKEN_TEXT) :: text
        integer :: len
        character :: c
        character :: char_val
        
        c = getc_(L)  ! consume opening quote
        len = 0
        
        if (at_end(L)) then
            call emit_error(L, 'Unterminated character literal')
            return
        end if
        
        c = getc_(L)
        if (c == '\') then
            char_val = lex_escape(L)
        else
            char_val = c
        end if
        
        len = 1
        text(1:1) = char_val
        
        if (at_end(L) .or. getc_(L) /= '''') then
            call emit_error(L, 'Unterminated character literal')
            return
        end if
        
        L%tok%ival = ichar(char_val)
        call emit(L, TOK_CHAR, text, len)
    end subroutine lex_charlit

    subroutine skip_line_comment(L)
        type(lexer_t), intent(inout) :: L
        character :: c
        do while (.not. at_end(L))
            c = getc_(L)
            if (c == char(10)) then
                exit
            end if
        end do
    end subroutine skip_line_comment

    subroutine skip_block_comment(L)
        type(lexer_t), intent(inout) :: L
        character :: c
        do while (.not. at_end(L))
            c = getc_(L)
            if (c == '*' .and. peekc(L) == '/') then
                c = getc_(L)  ! consume '/'
                return
            end if
        end do
        call emit_error(L, 'Unterminated block comment')
    end subroutine skip_block_comment

    subroutine do_lex(L)
        type(lexer_t), intent(inout) :: L
        character :: c
        character :: next_c
        integer :: start_line, start_col
        
        ! Skip whitespace
        do while (.not. at_end(L))
            c = peekc(L)
            if (is_whitespace(c)) then
                c = getc_(L)
            else
                exit
            end if
        end do
        
        if (at_end(L)) then
            call emit(L, TOK_EOF, '', 0)
            return
        end if
        
        start_line = L%line
        start_col = L%col
        c = getc_(L)
        
        ! Dispatch on first character
        select case(c)
            case('A':'Z', 'a':'z', '_')
                L%pos = L%pos - 1  ! back up to start of identifier
                L%line = start_line
                L%col = start_col
                call lex_ident(L)
                
            case('0':'9')
                L%pos = L%pos - 1  ! back up to start of number
                L%line = start_line
                L%col = start_col
                call lex_number(L)
                
            case('"')
                L%pos = L%pos - 1  ! back up to quote
                L%line = start_line
                L%col = start_col
                call lex_string(L)
                
            case('''')
                L%pos = L%pos - 1  ! back up to quote
                L%line = start_line
                L%col = start_col
                call lex_charlit(L)
                
            case('.')
                if (peekc(L) == '.') then
                    if (peekc2(L) == '.') then
                        call getc_(L)  ! consume second dot
                        call getc_(L)  ! consume third dot
                        call emit(L, TOK_ELLIPSIS, '...', 3)
                    else
                        call emit(L, TOK_DOT, '.', 1)
                    end if
                else if (is_digit(peekc(L))) then
                    L%pos = L%pos - 1  ! back up to dot
                    L%line = start_line
                    L%col = start_col
                    call lex_number(L)
                else
                    call emit(L, TOK_DOT, '.', 1)
                end if
                
            case('/')
                next_c = peekc(L)
                if (next_c == '/') then
                    call skip_line_comment(L)
                    call do_lex(L)  ! re-call after skipping comment
                    return
                else if (next_c == '*') then
                    call skip_block_comment(L)
                    call do_lex(L)  ! re-call after skipping comment
                    return
                else if (next_c == '=') then
                    call getc_(L)  ! consume '='
                    call emit(L, TOK_SLASH_EQ, '/=', 2)
                else
                    call emit(L, TOK_SLASH, '/', 1)
                end if
                
            case('+')
                next_c = peekc(L)
                if (next_c == '=') then
                    call getc_(L)  ! consume '='
                    call emit(L, TOK_PLUS_EQ, '+=', 2)
                else
                    call emit(L, TOK_PLUS, '+', 1)
                end if
                
            case('-')
                next_c = peekc(L)
                if (next_c == '=') then
                    call getc_(L)  ! consume '='
                    call emit(L, TOK_MINUS_EQ, '-=', 2)
                else if (next_c == '>') then
                    call getc_(L)  ! consume '>'
                    call emit(L, TOK_ARROW, '->', 2)
                else
                    call emit(L, TOK_MINUS, '-', 1)
                end if
                
            case('*')
                next_c = peekc(L)
                if (next_c == '=') then
                    call getc_(L)  ! consume '='
                    call emit(L, TOK_STAR_EQ, '*=', 2)
                else
                    call emit(L, TOK_STAR, '*', 1)
                end if
                
            case('=')
                next_c = peekc(L)
                if (next_c == '=') then
                    call getc_(L)  ! consume '='
                    call emit(L, TOK_EQ, '==', 2)
                else
                    call emit(L, TOK_ASSIGN, '=', 1)
                end if
                
            case('!')
                next_c = peekc(L)
                if (next_c == '=') then
                    call getc_(L)  ! consume '='
                    call emit(L, TOK_NE, '!=', 2)
                else
                    call emit(L, TOK_BANG, '!', 1)
                end if
                
            case('<')
                next_c = peekc(L)
                if (next_c == '=') then
                    call getc_(L)  ! consume '='
                    call emit(L, TOK_LE, '<=', 2)
                else if (next_c == '<') then
                    call getc_(L)  ! consume '<'
                    call emit(L, TOK_LSHIFT, '<<', 2)
                else
                    call emit(L, TOK_LT, '<', 1)
                end if
                
            case('>')
                next_c = peekc(L)
                if (next_c == '=') then
                    call getc_(L)  ! consume '='
                    call emit(L, TOK_GE, '>=', 2)
                else if (next_c == '>') then
                    call getc_(L)  ! consume '>'
                    call emit(L, TOK_RSHIFT, '>>', 2)
                else
                    call emit(L, TOK_GT, '>', 1)
                end if
                
            case('&')
                next_c = peekc(L)
                if (next_c == '&') then
                    call getc_(L)  ! consume '&'
                    call emit(L, TOK_LOGICAL_AND, '&&', 2)
                else
                    call emit(L, TOK_AMP, '&', 1)
                end if
                
            case('|')
                next_c = peekc(L)
                if (next_c == '|') then
                    call getc_(L)  ! consume '|'
                    call emit(L, TOK_LOGICAL_OR, '||', 2)
                else
                    call emit(L, TOK_PIPE, '|', 1)
                end if
                
            case(':')
                next_c = peekc(L)
                if (next_c == ':') then
                    call getc_(L)  ! consume ':'
                    call emit(L, TOK_DOUBLE_COLON, '::', 2)
                else
                    call emit(L, TOK_COLON, ':', 1)
                end if
                
            case('('); call emit(L, TOK_LPAREN, '(', 1)
            case(')'); call emit(L, TOK_RPAREN, ')', 1)
            case('{'); call emit(L, TOK_LBRACE, '{', 1)
            case('}'); call emit(L, TOK_RBRACE, '}', 1)
            case('['); call emit(L, TOK_LBRACKET, '[', 1)
            case(']'); call emit(L, TOK_RBRACKET, ']', 1)
            case(';'); call emit(L, TOK_SEMICOLON, ';', 1)
            case(','); call emit(L, TOK_COMMA, ',', 1)
            case('~'); call emit(L, TOK_TILDE, '~', 1)
            case('?'); call emit(L, TOK_QUESTION, '?', 1)
            case('@'); call emit(L, TOK_AT, '@', 1)
            case('#'); call emit(L, TOK_HASH, '#', 1)
            case('^'); call emit(L, TOK_CARET, '^', 1)
            case('%'); call emit(L, TOK_PERCENT, '%', 1)
            
            case default
                call emit_error(L, 'Unknown character: ' // c)
        end select
    end subroutine do_lex

    subroutine lex_init(L, src)
        type(lexer_t), intent(out) :: L
        character(len=*), intent(in) :: src
        integer :: len_src
        len_src = min(len_trim(src), MAX_BUFFER_SIZE)
        L%buffer = repeat(' ', MAX_BUFFER_SIZE)
        L%buffer(1:len_src) = src(1:len_src)
        L%buffer_len = len_src
        L%pos = 1
        L%line = 1
        L%col = 1
    end subroutine lex_init

    function lex_next(L) result(kind)
        type(lexer_t), intent(inout) :: L
        integer :: kind
        call do_lex(L)
        kind = L%tok%kind
    end function lex_next

    function tok_name(kind) result(name)
        integer, intent(in) :: kind
        character(len=20) :: name
        name = repeat(' ', 20)
        
        select case(kind)
            case(TOK_EOF); name(1:3) = 'EOF'
            case(TOK_ERROR); name(1:5) = 'ERROR'
            case(TOK_IDENT); name(1:5) = 'IDENT'
            case(TOK_INT); name(1:3) = 'INT'
            case(TOK_FLOAT); name(1:5) = 'FLOAT'
            case(TOK_STRING); name(1:6) = 'STRING'
            case(TOK_CHAR); name(1:4) = 'CHAR'
            case(TOK_LPAREN); name(1:7) = 'LPAREN'
            case(TOK_RPAREN); name(1:7) = 'RPAREN'
            case(TOK_LBRACE); name(1:7) = 'LBRACE'
            case(TOK_RBRACE); name(1:7) = 'RBRACE'
            case(TOK_LBRACKET); name(1:8) = 'LBRACKET'
            case(TOK_RBRACKET); name(1:8) = 'RBRACKET'
            case(TOK_SEMICOLON); name(1:9) = 'SEMICOLON'
            case(TOK_COMMA); name(1:5) = 'COMMA'
            case(TOK_DOT); name(1:3) = 'DOT'
            case(TOK_TILDE); name(1:5) = 'TILDE'
            case(TOK_QUESTION); name(1:8) = 'QUESTION'
            case(TOK_AT); name(1:2) = 'AT'
            case(TOK_HASH); name(1:4) = 'HASH'
            case(TOK_CARET); name(1:5) = 'CARET'
            case(TOK_PERCENT); name(1:7) = 'PERCENT'
            case(TOK_PLUS); name(1:4) = 'PLUS'
            case(TOK_PLUS_EQ); name(1:6) = 'PLUS_EQ'
            case(TOK_MINUS); name(1:5) = 'MINUS'
            case(TOK_MINUS_EQ); name(1:8) = 'MINUS_EQ'
            case(TOK_ARROW); name(1:5) = 'ARROW'
            case(TOK_STAR); name(1:4) = 'STAR'
            case(TOK_STAR_EQ); name(1:7) = 'STAR_EQ'
            case(TOK_SLASH); name(1:5) = 'SLASH'
            case(TOK_SLASH_EQ); name(1:8) = 'SLASH_EQ'
            case(TOK_ASSIGN); name(1:6) = 'ASSIGN'
            case(TOK_EQ); name(1:2) = 'EQ'
            case(TOK_BANG); name(1:4) = 'BANG'
            case(TOK_NE); name(1:2) = 'NE'
            case(TOK_LT); name(1:2) = 'LT'
            case(TOK_LE); name(1:2) = 'LE'
            case(TOK_LSHIFT); name(1:6) = 'LSHIFT'
            case(TOK_GT); name(1:2) = 'GT'
            case(TOK_GE); name(1:2) = 'GE'
            case(TOK_RSHIFT); name(1:6) = 'RSHIFT'
            case(TOK_AMP); name(1:3) = 'AMP'
            case(TOK_LOGICAL_AND); name(1:11) = 'LOGICAL_AND'
            case(TOK_PIPE); name(1:4) = 'PIPE'
            case(TOK_LOGICAL_OR); name(1:10) = 'LOGICAL_OR'
            case(TOK_COLON); name(1:5) = 'COLON'
            case(TOK_DOUBLE_COLON); name(1:12) = 'DOUBLE_COLON'
            case(TOK_ELLIPSIS); name(1:8) = 'ELLIPSIS'
            case default; name(1:7) = 'UNKNOWN'
        end select
    end function tok_name

end module lexer_mod
