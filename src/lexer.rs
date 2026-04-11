// LL(1) lexer

#![allow(unused)]

use std::collections::HashMap;
use std::ops::Deref;
use std::sync::OnceLock;

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
    STRING(&'a str),
    FMTSTRING(&'a str),
    
    INT(i32),
    FLOAT(f64),

    BINOP(&'a str),
    UNARY(&'a str),
    PAREN(&'a str),
    AND,

    DECLARE,
    CLASS,
    IDENT(&'a str),

    FN,
    RETURN,
    PUNCT(&'a str),
    ARROW,
    LEFTARROW,
    END,

    // Control flow
    IF,
    ELSE,
    ELSEIF,
    IN,

    WHILE,
    FOR,
    DO,

    PUB,
    IMPORT,
    USE,

    STATIC,
    DYNAMIC,

    WHEN,
    AS,

    MUTABLE,

    SELF,
    SELFTYPE,

    OPERATOR,
}

static KEYWORDS: OnceLock<HashMap<&'static str, Token<'static>>> = OnceLock::new();

// Initialize keywords map
fn keywords() -> &'static HashMap<&'static str, Token<'static>> {
    KEYWORDS.get_or_init(|| {
        let mut map = HashMap::new();
        map.insert("local", Token::DECLARE);
        map.insert("function", Token::FN);
        map.insert("return", Token::RETURN);
        map.insert("class", Token::CLASS);
        map.insert("if", Token::IF);
        map.insert("else", Token::ELSE);
        map.insert("elseif", Token::ELSEIF);
        map.insert("while", Token::WHILE);
        map.insert("for", Token::FOR);
        map.insert("do", Token::DO);
        map.insert("end", Token::END);
        map.insert("in", Token::IN);
        map.insert("public", Token::PUB);
        map.insert("import", Token::IMPORT);
        map.insert("dynamic", Token::DYNAMIC);
        map.insert("static", Token::STATIC);
        map.insert("use", Token::USE);
        map.insert("when", Token::WHEN);
        map.insert("mut", Token::MUTABLE);
        map.insert("operator", Token::OPERATOR);
        map.insert("as", Token::AS);
        map.insert("self", Token::SELF);
        map.insert("Self", Token::SELFTYPE);
        map
    })
}

pub fn tokenize(src: String) -> Vec<Token<'static>> {
    let mut tokens: Vec<Token> = vec![];
    let mut chars = src.chars().peekable();

    let keywords = keywords();
    
    while let Some(mut char) = chars.next() {
        let prev = tokens.last().cloned();
        if char == '"' || char == '\'' {
            let quote_char = char;
            let mut string_lit = String::new();

            while let Some(c) = chars.next() {
                if c == '\\' {
                    // Handle escape sequences
                    if let Some(esc) = chars.next() {
                        match esc {
                            'n' => string_lit.push('\n'),
                            't' => string_lit.push('\t'),
                            'r' => string_lit.push('\r'),
                            '\\' => string_lit.push('\\'),
                            '"' => string_lit.push('"'),
                            '\'' => string_lit.push('\''),
                            other => string_lit.push(other),
                        }
                    }
                } else if c == quote_char {
                    // End of string
                    break;
                } else {
                    string_lit.push(c);
                }
            }

            tokens.push(Token::STRING(Box::leak(string_lit.into_boxed_str())));
            continue;
        }
        if char.is_alphabetic() {
            let mut identifier = String::from(char);

            while let Some(&next_char) = chars.peek() {
                if next_char.is_alphanumeric() || next_char == '_' {
                    identifier.push(chars.next().unwrap());
                } else {
                    break;
                }
            }

            // Check if the identifier is a keyword
            if let Some(keyword_token) = keywords.get(identifier.as_str()) {
                tokens.push(keyword_token.clone());
            } else {
                tokens.push(Token::IDENT(Box::leak(identifier.into_boxed_str())));
            }
            continue;
        }
        if char.is_numeric() 
        || (char == '-' && chars.peek().map_or(false, |c| c.is_numeric())) 
        {
            let mut number = String::new();
            if char == '-' {
                number.push(char);
                char = chars.next().unwrap();
            }
            number.push(char);
            
            let mut is_float = false;
            while let Some(&next_char) = chars.peek() {
                if next_char.is_numeric() {
                    number.push(chars.next().unwrap());
                } else if next_char == '.' && !is_float {
                    is_float = true;
                    number.push(chars.next().unwrap());
                } else {
                    break;
                }
            }

            if is_float {
                tokens.push(Token::FLOAT(number.parse().unwrap()));
            } else {
                tokens.push(Token::INT(number.parse().unwrap()));
            }
            continue;
        }
        
        match char {
            '`' => {
                let mut s = String::new();
                let mut brace_depth = 0usize;

                while let Some(&ch) = chars.peek() {
                    chars.next();
                    match ch {
                        '{' => { brace_depth += 1; s.push(ch); }
                        '}' => {
                            if brace_depth > 0 { brace_depth -= 1; }
                            s.push(ch);
                        }
                        '`' if brace_depth == 0 => break, // only close at depth 0
                        '`' => s.push(ch),                // nested backtick inside { }, keep it
                        _ => s.push(ch),
                    }
                }

                tokens.push(Token::FMTSTRING(Box::leak(s.into_boxed_str())));
            }
            '+' => {
                if chars.peek().map_or(false, |c| c == &'=') {
                    tokens.push(Token::BINOP("+="));
                    chars.next();
                } else {
                    tokens.push(Token::BINOP("+"));
                }
            }
            '-' => {
                if chars.peek().map_or(false, |c| c == &'-') {
                    chars.next(); // consume second '-'

                    // Block comment: --[[ ... ]]
                    if chars.peek().map_or(false, |c| c == &'[') {
                        chars.next(); // consume '['

                        if chars.peek().map_or(false, |c| c == &'[') {
                            chars.next(); // consume second '['

                            // consume until ]]
                            while let Some(_) = chars.next() {
                                if let Some(']') = chars.peek() {
                                    chars.next();
                                    if chars.peek().map_or(false, |c| c == &']') {
                                        chars.next();
                                        break;
                                    }
                                }
                            }

                            continue;
                        }
                    }

                    // Line comment: -- ...
                    while let Some(c) = chars.next() {
                        if c == '\n' {
                            break;
                        }
                    }

                    continue;
                }
                
                if chars.peek().map_or(false, |c| c == &'>') {
                    tokens.push(Token::ARROW);
                    chars.next();
                } else if chars.peek().map_or(false, |c| c == &'=') {
                    tokens.push(Token::BINOP("-="));
                    chars.next();
                } else {
                    let unary = match &prev {
                        None => true,
                        Some(Token::BINOP(_))
                        | Some(Token::BINOP("="))
                        | Some(Token::DECLARE)
                        | Some(Token::PAREN("("))
                        | Some(Token::PAREN("{"))
                        | Some(Token::PUNCT(",")) => true,
                        _ => false,
                    };

                    if unary {
                        tokens.push(Token::UNARY("-"));
                    } else {
                        tokens.push(Token::BINOP("-"));
                    }
                }
            },
            '!' => {
                if chars.peek().map_or(false, |c| c == &'=') {
                    tokens.push(Token::BINOP("!="));
                    chars.next();
                } else {
                    let unary = match &prev {
                        None => true,
                        Some(Token::BINOP(_))
                        | Some(Token::BINOP("="))
                        | Some(Token::DECLARE)
                        | Some(Token::PAREN("("))
                        | Some(Token::PAREN("{"))
                        | Some(Token::PUNCT(",")) => true,
                        _ => false,
                    };
                    if unary {
                        tokens.push(Token::UNARY("!"));
                    } 
                    // else {
                    //     tokens.push(Token::BINOP("!"));
                    // }
                }
            }
            '*' => {
                if chars.peek().map_or(false, |c| c == &'=') {
                    tokens.push(Token::BINOP("*="));
                    chars.next();
                }
                else if chars.peek().map_or(false, |c| c == &'>') {
                    tokens.push(Token::BINOP("*>"));
                    chars.next();
                }
                else {
                    tokens.push(Token::BINOP("*"));
                }
            }
            '/' => {
                if chars.peek().map_or(false, |c| c == &'=') {
                    tokens.push(Token::BINOP("/="));
                    chars.next();
                } else {
                    tokens.push(Token::BINOP("/"));
                }
            }
            '^' => {
                if chars.peek().map_or(false, |c| c == &'=') {
                    tokens.push(Token::BINOP("^="));
                    chars.next();
                } else {
                    tokens.push(Token::BINOP("^"));
                }
            }
            '%' => {
                if chars.peek().map_or(false, |c| c == &'=') {
                    tokens.push(Token::BINOP("%="));
                    chars.next();
                } else {
                    tokens.push(Token::BINOP("%"));
                }
            }
            '.' => {
                if chars.peek().map_or(false, |c| c == &'.') {
                    tokens.push(Token::BINOP(".."));
                    chars.next();
                    continue;
                }
                else {
                    tokens.push(Token::PUNCT("."))
                }
            },
            
            ',' => tokens.push(Token::PUNCT(",")),
            ':' => {
                if chars.peek().map_or(false, |c| c == &':') {
                    tokens.push(Token::BINOP("::"));
                    chars.next();
                    continue;
                }
                else {
                    tokens.push(Token::PUNCT(":"))
                }
            }
            
            '=' => {
                if chars.peek().map_or(false, |c| c == &'=') {
                    tokens.push(Token::BINOP("=="));
                    chars.next();
                    continue;
                }
                else {
                    tokens.push(Token::BINOP("="))
                }
            },
            
            '(' => tokens.push(Token::PAREN("(")),
            ')' => tokens.push(Token::PAREN(")")),
            '{' => tokens.push(Token::PAREN("{")),
            '}' => tokens.push(Token::PAREN("}")),
            '[' => tokens.push(Token::PAREN("[")),
            ']' => tokens.push(Token::PAREN("]")),

            '<' => {
                if chars.peek().map_or(false, |c| c == &'<') {
                    chars.next();
                    if chars.peek().map_or(false, |c| c == &'=') {
                        tokens.push(Token::BINOP("<<="));
                        chars.next();
                    } else {
                        tokens.push(Token::BINOP("<<"));
                    }
                    continue;
                }
                if chars.peek().map_or(false, |c| c == &'=') {
                    tokens.push(Token::BINOP("<="));
                    chars.next();
                    continue;
                }
                else if chars.peek().map_or(false, |c| c == &'-') {
                    tokens.push(Token::LEFTARROW);
                    chars.next();
                    continue;
                }
                else {
                    tokens.push(Token::BINOP("<"))
                }
            }
            '>' => {
                if chars.peek().map_or(false, |c| c == &'>') {
                    chars.next();
                    if chars.peek().map_or(false, |c| c == &'=') {
                        tokens.push(Token::BINOP(">>="));
                        chars.next();
                    } else {
                        tokens.push(Token::BINOP(">>"));
                    }
                    continue;
                }
                else
                if chars.peek().map_or(false, |c| c == &'=') {
                    tokens.push(Token::BINOP(">="));
                    chars.next();
                    continue;
                }
                else {
                    tokens.push(Token::BINOP(">"))
                }
            }
            '&' => {
                if chars.peek().map_or(false, |c| c == &'&') {
                    chars.next();
                    if chars.peek().map_or(false, |c| c == &'=') {
                        tokens.push(Token::BINOP("&&="));
                        chars.next();
                    } else {
                        tokens.push(Token::BINOP("&&"));
                    }
                    continue;
                } else if chars.peek().map_or(false, |c| c == &'=') {
                    tokens.push(Token::BINOP("&="));
                    chars.next();
                } else {
                    tokens.push(Token::AND);
                }
            }
            '|' => {
                if chars.peek().map_or(false, |c| c == &'|') {
                    chars.next();
                    if chars.peek().map_or(false, |c| c == &'=') {
                        tokens.push(Token::BINOP("||="));
                        chars.next();
                    } else {
                        tokens.push(Token::BINOP("||"));
                    }
                    continue;
                }
                else if chars.peek().map_or(false, |c| c == &'=') {
                    tokens.push(Token::BINOP("|="));
                    chars.next();
                }
                else {
                    tokens.push(Token::BINOP("|"))
                }
            }
            _ => {}
        }
    }

    tokens
}