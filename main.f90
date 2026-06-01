program test_lexer
    use lexer_mod
    implicit none
    
    type(lexer_t) :: L
    integer :: kind
    character(len=20) :: name
    
    call lex_init(L, 'int x = 42; float y = 3.14; char c = ''a''; string s = "hello"; if (x > 0) { return x + y; } /* comment */ // line comment 0xFF 1.5e-2 ...')
    
    do
        kind = lex_next(L)
        name = tok_name(kind)
        write(*,'(I0,":",I0,"  ",A,"  '",A,"'")') L%tok%line, L%tok%col, trim(name), L%tok%text(1:L%tok%text_len)
        
        if (kind == TOK_EOF .or. kind == TOK_ERROR) then
            exit
        end if
    end do
end program test_lexer
