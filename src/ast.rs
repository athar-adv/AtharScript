#![allow(unused)]

//ast.rs

use crate::lexer::Token;
use crate::v_type::{VType, vty_from_str, is_vtype};
use core::option::Option::None;
use std::collections::HashMap;
use std::iter::Peekable;
use std::str::FromStr;
use std::vec::IntoIter;

#[derive(Debug, Clone)]
pub struct Parameter {
    pub ident: String,
    pub v_type: VType
}

#[derive(Debug, Clone)]
pub enum AstNode {
    Byte(i8),
    UByte(u8),
    Short(i16),
    UShort(u16),
    Integer(i32),
    UInteger(u32),
    Float(f32),
    Double(f64),
    String(String),

    // Variables
    Identifier(String),
    FieldKey(String),
    Declaration {
        name: Box<AstNode>,
        value: Box<AstNode>,
        v_type: VType,
    },
    Assignment {
        assignee: Box<AstNode>,
        value: Box<AstNode>,
    },

    Export {
        item: Box<AstNode>, // wraps Function, StructDecl, or Declaration
    },
    Import {
        alias: String, // "module" in: import module "./path.luc"
        path: String, // "./module.luc"
    },
    Use {
        module_alias: String, // "module" in: use module::(...)
        items: Vec<(String, String)>, // (ExportName, Alias)
    },
    When {
        subject: Box<AstNode>, // the expression being matched
        arms: Vec<WhenArm>,
    },
    FmtString {
        parts: Vec<FmtPart>,
    },

    BinaryOp {
        op: String,
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    UnaryOp {
        op: String,
        value: Box<AstNode>
    },

    Function {
        name: String,
        params: Vec<Parameter>,
        body: Vec<Box<AstNode>>,
        return_type: VType,
        generics: Vec<String>,
    },
    Call {
        callee: Box<AstNode>,
        args: Vec<Box<AstNode>>,
        generics: Vec<VType>,
    },
    Index {
        target: Box<AstNode>,
        index: Box<AstNode>,
    },
    TypeCast {
        value: Box<AstNode>,
        ty: VType,
    },
    Return {
        args: Vec<AstNode>
    },

    StructDecl {
        name: String,
        fields: Vec<Parameter>,
        struct_type: VType,
        methods: Vec<(String, AstNode)>,
        generics: Vec<String>
    },
    Implement {
        name: String,
        methods: Vec<AstNode>,
    },
    StructLiteral {
        name: String,
        fields: Vec<(String, AstNode)>,
        generics: Vec<VType>
    },
    Namespace {
        name: String,
    },
    ArrayLiteral {
        exprs: Vec<Box<AstNode>>
    },
    WhileLoop {
        condition: Box<AstNode>,
        body: Vec<Box<AstNode>>
    },
    ForLoop {
        iteratee: Box<AstNode>,
        params: Vec<Parameter>,
        body: Vec<Box<AstNode>>
    },
    ConditionalBranch {
        condition: Option<Box<AstNode>>, // condition: None means the branch is unconditional
        body: Vec<Box<AstNode>>,
        next: Option<Box<AstNode>>
    },
    DoBlock(Vec<Box<AstNode>>),
    Program(Vec<AstNode>),
}

#[derive(Debug, Clone)]
pub enum FmtPart {
    Literal(String),
    Interpolated(Box<AstNode>),
}

#[derive(Debug, Clone)]
pub struct WhenArm {
    pub patterns: Vec<AstNode>,
    pub body: Vec<Box<AstNode>>,
    pub is_else: bool,
    pub binding: Option<String>,
}

pub struct Parser {
    pub defined_struct_types: HashMap<String, VType>,
    tokens: Peekable<IntoIter<Token<'static>>>,
    generic_stack: Vec<Vec<String>>,
}

type PeekIter<T> = Peekable<IntoIter<T>>;

fn consume(tokens: &mut PeekIter<Token<'static>>) -> Option<Token<'static>>
{
    tokens.next()
}

fn expect(tokens: &mut PeekIter<Token<'static>>, expected: Token<'static>) -> Token<'static>
{
    let tok = consume(tokens)
        .unwrap_or_else(|| {
            panic!("No more tokens")
        });
    if tok != expected {
        panic!("Expected Token {:?}, got Token {:?}", expected, tok);
    }
    tok
}

fn peek<'a>(tokens: &'a mut PeekIter<Token<'static>>) -> Option<&'a Token<'static>>
{
    tokens.peek()
}

impl Parser {
    pub fn new(tokens: Vec<Token<'static>>) -> Self {
        Parser {
            defined_struct_types: HashMap::new(),
            tokens: tokens.into_iter().peekable(),
            generic_stack: vec![]
        }
    }

    fn is_generic(&self, name: &str) -> bool {
        for scope in self.generic_stack.iter().rev() {
            if scope.contains(&name.to_string()) {
                return true;
            }
        }
        false
    }
    
    fn resolve_implement_blocks(&mut self, nodes: &mut Vec<AstNode>) -> Result<(), String> {
        let mut struct_map: HashMap<String, usize> = HashMap::new();
        for (i, node) in nodes.iter().enumerate() {
            if let AstNode::StructDecl { name, .. } = node {
                struct_map.insert(name.clone(), i);
            }
        }

        let mut new_nodes = vec![];

        for node in nodes.drain(..) {
            match node {
                AstNode::Implement { name, methods } => {
                    if let Some(&idx) = struct_map.get(&name) {
                        if let AstNode::StructDecl { methods: struct_methods, .. } = &mut new_nodes[idx] {
                            for func in methods {
                                if let AstNode::Function { name: fname, .. } = &func {
                                    struct_methods.push((fname.clone(), func));
                                }
                            }
                        }
                    } else {
                        return Err(format!("Implement for unknown struct '{}'", name));
                    }
                }
                other => new_nodes.push(other),
            }
        }

        *nodes = new_nodes;
        Ok(())
    }

    pub fn parse(&mut self) -> Result<AstNode, String> {
        let mut program = vec![];

        while peek(&mut self.tokens).is_some() {
            program.push(self.parse_statement()?);
        }

        self.resolve_implement_blocks(&mut program)?;

        Ok(AstNode::Program(program))
    }

    fn parse_statement(&mut self) -> Result<AstNode, String> {
        match peek(&mut self.tokens) {
            // In parse_statement, add at the top of the match:
            Some(Token::PUB) => {
                consume(&mut self.tokens);
                self.parse_export()
            }
            Some(Token::IMPORT) => {
                consume(&mut self.tokens);
                self.parse_import()
            }
            Some(Token::USE) => {
                consume(&mut self.tokens);
                self.parse_use()
            }
            Some(Token::DECLARE) => {
                consume(&mut self.tokens);
                self.parse_declaration()
            }
            Some(Token::FN) => {
                consume(&mut self.tokens);
                self.parse_function()
            }
            Some(Token::STRUCT) => {
                consume(&mut self.tokens);
                self.parse_struct_declaration()
            }
            Some(Token::IMPL) => {
                consume(&mut self.tokens);
                self.parse_implement()
            }
            Some(Token::WHILE) => {
                consume(&mut self.tokens);
                self.parse_while_loop()
            }
            Some(Token::FOR) => {
                consume(&mut self.tokens);
                self.parse_for_loop()
            }
            Some(Token::DO) => {
                consume(&mut self.tokens);
                let r = self.parse_body();
                if let Ok(body) = r {
                    Ok(AstNode::DoBlock(body))
                }
                else {
                    Err(format!("error while parsing do block: {:?}", r).into())
                }
            }
            Some(Token::IF) => {
                consume(&mut self.tokens);
                self.parse_conditional_branch()
            }
            Some(Token::WHEN) => {
                consume(&mut self.tokens);
                self.parse_when_stmt()
            }
            _ => self.parse_expression(),
        }
    }

    fn parse_when_stmt(&mut self) -> Result<AstNode, String> {
        expect(&mut self.tokens, Token::PAREN("("));
        let subject = self.parse_expression()?;
        expect(&mut self.tokens, Token::PAREN(")"));
        expect(&mut self.tokens, Token::PAREN("{"));

        let mut arms = vec![];

        while peek(&mut self.tokens) != Some(&Token::PAREN("}")) {
            let mut patterns = vec![];
            let mut is_else = false;
            let mut binding = None;

            // Parse one or more pattern groups separated by |
            loop {
                expect(&mut self.tokens, Token::PAREN("("));

                match peek(&mut self.tokens).cloned() {
                    Some(Token::IDENT(name)) => {
                        // lookahead: if ident followed by `)` it's a catch-all binding
                        let mut lookahead = self.tokens.clone();
                        lookahead.next(); // skip ident
                        let is_binding = matches!(lookahead.next(), Some(Token::PAREN(")")));

                        if is_binding {
                            consume(&mut self.tokens); // consume ident
                            expect(&mut self.tokens, Token::PAREN(")"));
                            is_else = true;
                            binding = Some(name.to_string());
                        } else {
                            patterns.push(self.parse_expression()?);
                            expect(&mut self.tokens, Token::PAREN(")"));
                        }
                    }
                    _ => {
                        patterns.push(self.parse_expression()?);
                        expect(&mut self.tokens, Token::PAREN(")"));
                    }
                }

                // Continue if next token is |
                if peek(&mut self.tokens) == Some(&Token::BINOP("|")) {
                    consume(&mut self.tokens);
                } else {
                    break;
                }
            }

            let body = if peek(&mut self.tokens) == Some(&Token::PAREN("{")) {
                self.parse_body()?
            } else {
                return Err("when arm body must be wrapped in { }".into());
            };

            arms.push(WhenArm { patterns, body, is_else, binding });
        }

        expect(&mut self.tokens, Token::PAREN("}"));
        Ok(AstNode::When { subject: Box::new(subject), arms })
    }

    fn parse_export(&mut self) -> Result<AstNode, String> {
        let item = match peek(&mut self.tokens) {
            Some(Token::FN) => {
                consume(&mut self.tokens);
                self.parse_function()?
            }
            Some(Token::STRUCT) => {
                consume(&mut self.tokens);
                self.parse_struct_declaration()?
            }
            Some(Token::DECLARE) => {
                consume(&mut self.tokens);
                self.parse_declaration()?
            }
            // Bare pub MyVar: u8 = 10, treat like a declaration without let
            Some(Token::IDENT(_)) => {
                self.parse_declaration()?
            }
            _ => return Err("Expected 'fn', 'struct', 'let', or identifier after 'pub'".into()),
        };

        Ok(AstNode::Export {
            item: Box::new(item),
        })
    }

    fn parse_import(&mut self) -> Result<AstNode, String> {
        let alias = match consume(&mut self.tokens) {
            Some(Token::IDENT(name)) => name.to_string(),
            _ => return Err("Expected module alias after 'import'".into()),
        };

        let path = match consume(&mut self.tokens) {
            Some(Token::STRING(s)) => s.to_string(),
            _ => return Err("Expected string path after module alias in 'import'".into()),
        };

        Ok(AstNode::Import { alias, path })
    }

    fn parse_use(&mut self) -> Result<AstNode, String> {
        let module_alias = match consume(&mut self.tokens) {
            Some(Token::IDENT(name)) => name.to_string(),
            _ => return Err("Expected module alias after 'use'".into()),
        };

        expect(&mut self.tokens, Token::PAREN("{"));

        let mut items = Vec::new();
        loop {
            match peek(&mut self.tokens) {
                Some(Token::PAREN("}")) => {
                    consume(&mut self.tokens);
                    break;
                }
                Some(Token::IDENT(_)) => {
                    let name = match consume(&mut self.tokens) {
                        Some(Token::IDENT(n)) => n.to_string(),
                        _ => unreachable!(),
                    };
                    let alias = match peek(&mut self.tokens) {
                        Some(Token::AS) => {
                            consume(&mut self.tokens);
                            match consume(&mut self.tokens) {
                                Some(Token::IDENT(name)) => name,
                                _ => panic!("Identifier must follow realias")
                            }
                        }
                        _ => &name.to_string()
                    };
                    items.push((name, alias.to_string()));

                    match peek(&mut self.tokens) {
                        Some(Token::PUNCT(",")) => { consume(&mut self.tokens); }
                        Some(Token::PAREN("}")) => {}
                        _ => return Err("Expected ',' or ')' in use item list".into()),
                    }
                }
                _ => return Err("Unexpected token in 'use' item list".into()),
            }
        }

        Ok(AstNode::Use { module_alias, items })
    }

    fn parse_conditional_branch(&mut self) -> Result<AstNode, String> {
        expect(&mut self.tokens, Token::PAREN("("));
        
        let condition = self.parse_expression()?;

        expect(&mut self.tokens, Token::PAREN(")"));
        
        let body = self.parse_body()?;
        let mut next = None;
        match peek(&mut self.tokens) {
            Some(Token::ELSEIF) => {
                consume(&mut self.tokens);
                next = Some(Box::new(self.parse_conditional_branch()?));
            }
            Some(Token::ELSE) => {
                consume(&mut self.tokens);
                let body = self.parse_body()?;
                next = Some(Box::new(AstNode::ConditionalBranch { condition: None, body, next: None}));
            }
            _ => {}
        }

        Ok(AstNode::ConditionalBranch {
            condition: Some(Box::new(condition)),
            body,
            next
        })
    }

    fn parse_for_loop(&mut self) -> Result<AstNode, String> {
        expect(&mut self.tokens, Token::PAREN("("));

        let binding_name = match consume(&mut self.tokens) {
            Some(Token::IDENT(name)) => name.to_string(),
            _ => return Err("Expected binding identifier after 'for'".into()),
        };
        
        let ty =
            if let Some(Token::PUNCT(":")) = peek(&mut self.tokens)
            {
                consume(&mut self.tokens);
                self.parse_type()?
            }
            else
            {
                VType::Auto
            };

        expect(&mut self.tokens, Token::IN);
        let iteratee = self.parse_expression()?;

        expect(&mut self.tokens, Token::PAREN(")"));

        let body = self.parse_body()?;

        Ok(AstNode::ForLoop {
            iteratee: Box::new(iteratee),
            params: vec![Parameter { ident: binding_name, v_type: ty }],
            body,
        })
    }

    fn parse_while_loop(&mut self) -> Result<AstNode, String> {
        expect(&mut self.tokens, Token::PAREN("("));

        let condition = self.parse_expression()?;

        expect(&mut self.tokens, Token::PAREN(")"));
        
        let body = self.parse_body()?;
        Ok(AstNode::WhileLoop {
            condition: Box::new(condition),
            body
        })
    }
    
    fn parse_implement(&mut self) -> Result<AstNode, String> {
        let name = match consume(&mut self.tokens) {
            Some(Token::IDENT(name)) => name.to_string(),
            _ => return Err("Expected struct name after 'implement'".into()),
        };

        expect(&mut self.tokens, Token::PAREN("{"));

        let mut methods = vec![];

        while peek(&mut self.tokens) != Some(&Token::PAREN("}")) {
            match peek(&mut self.tokens) {
                Some(Token::FN) => {
                    consume(&mut self.tokens);
                    let func = self.parse_function()?;
                    methods.push(func);
                }
                _ => return Err("Only functions are allowed inside implement block".into()),
            }
        }

        expect(&mut self.tokens, Token::PAREN("}"));

        Ok(AstNode::Implement { name, methods })
    }
    
    fn parse_struct_declaration(&mut self) -> Result<AstNode, String> {
        let name = match consume(&mut self.tokens) {
            Some(Token::IDENT(name)) => name.to_string(),
            _ => return Err("Expected struct name".to_string()),
        };

        let mut generics = Vec::new();

        if let Some(Token::BINOP("<")) = peek(&mut self.tokens) {
            consume(&mut self.tokens);

            loop {
                match consume(&mut self.tokens) {
                    Some(Token::IDENT(name)) => generics.push(name.to_string()),
                    Some(Token::BINOP(">")) => break,
                    Some(Token::PUNCT(",")) => continue,
                    _ => return Err("Invalid generic list".into())
                }
            }
        }

        self.generic_stack.push(generics.clone());

        expect(&mut self.tokens, Token::PAREN("{"));

        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let struct_type = VType::Struct(name.clone(), vec![]);

        self.defined_struct_types.insert(name.clone(), struct_type.clone());
        
        loop {
            match peek(&mut self.tokens) {
                Some(Token::PAREN("}")) => {
                    consume(&mut self.tokens);
                    break;
                }
                Some(Token::IDENT(field_name)) => {
                    let field_name = field_name.to_string();
                    consume(&mut self.tokens);

                    expect(&mut self.tokens, Token::PUNCT(":"));

                    let v_type = self.parse_type()?;
                    fields.push(Parameter { ident: field_name, v_type });
                    match peek(&mut self.tokens) {
                        Some(Token::PUNCT(",")) => { consume(&mut self.tokens); }
                        Some(Token::PAREN("}")) => {}
                        _ => return Err("Expected ',' or '}' after struct field".to_string()),
                    }
                }
                _ => return Err("Unexpected token in struct declaration".to_string()),
            }
        }

        self.generic_stack.pop();

        Ok(AstNode::StructDecl { name, fields, struct_type, generics, methods })
    }

    fn parse_generic_args(&mut self) -> Result<Vec<VType>, String> {
        let mut generics = Vec::new();

        expect(&mut self.tokens, Token::BINOP("<"));

        loop {
            match peek(&mut self.tokens) {
                Some(Token::BINOP(">")) => {
                    consume(&mut self.tokens);
                    break;
                }
                _ => {
                    let ty = self.parse_type()?;
                    generics.push(ty);

                    match peek(&mut self.tokens) {
                        Some(Token::PUNCT(",")) => {
                            consume(&mut self.tokens);
                        }
                        Some(Token::BINOP(">")) => {}
                        _ => return Err("Expected ',' or '>' in generic args".into()),
                    }
                }
            }
        }

        Ok(generics)
    }

    fn parse_struct_literal(
        &mut self,
        name: String,
        generics: Vec<VType>
    ) -> Result<AstNode, String>
    {
        expect(&mut self.tokens, Token::PAREN("{"));

        let mut fields = Vec::new();
        loop {
            match peek(&mut self.tokens) {
                Some(Token::PAREN("}")) => {
                    consume(&mut self.tokens);
                    break;
                }
                Some(Token::IDENT(field_name)) => {
                    let field_name = field_name.to_string();
                    consume(&mut self.tokens);
                    expect(
                        &mut self.tokens,
                        Token::PUNCT(":")
                    );
                    
                    let value = self.parse_expression()?;
                    fields.push((field_name, value));

                    match peek(&mut self.tokens) {
                        Some(Token::PUNCT(",")) => { consume(&mut self.tokens); }
                        Some(Token::PAREN("}")) => {}
                        _ => return Err("Expected ',' or '}' after struct field value".to_string()),
                    }
                }
                _ => return Err("Unexpected token in struct literal".to_string()),
            }
        }

        Ok(AstNode::StructLiteral { name, fields, generics })
    }

    fn parse_array_literal(&mut self) -> Result<AstNode, String> {
        let mut exprs = vec![];

        loop {
            match peek(&mut self.tokens) {
                Some(Token::PAREN("}")) => {
                    consume(&mut self.tokens);
                    break;
                }
                Some(_) => {
                    let expr = self.parse_expression()?;
                    exprs.push(Box::new(expr));

                    match peek(&mut self.tokens) {
                        Some(Token::PUNCT(",")) => {
                            consume(&mut self.tokens);
                        }
                        Some(Token::PAREN("}")) => {}
                        _ => return Err("Expected ',' or '}' in array literal".into()),
                    }
                }
                None => return Err("array literal ended prematurely".into()),
            }
        }

        Ok(AstNode::ArrayLiteral { exprs })
    }

    fn parse_function(&mut self) -> Result<AstNode, String> {
        let name = match consume(&mut self.tokens) {
            Some(Token::IDENT(name)) => name,
            _ => return Err("Expected function name".to_string())
        };

        let mut generics = vec![];
        if let Some(Token::BINOP("<")) = peek(&mut self.tokens) {
            consume(&mut self.tokens);
            loop {
                match consume(&mut self.tokens) {
                    Some(Token::IDENT(name)) => {
                        generics.push(name.to_string());
                        self.generic_stack.last_mut()
                            .map(|s| s.push(name.to_string()));
                    }
                    Some(Token::BINOP(">")) => break,
                    Some(Token::PUNCT(",")) => continue,
                    _ => return Err("Invalid generic list in function".into()),
                }
            }
        }

        expect(&mut self.tokens, Token::PAREN("("));

        self.generic_stack.push(generics.clone());

        let mut params: Vec<Parameter> = Vec::new();
        loop {
            let token_opt = peek(&mut self.tokens).cloned();
            match token_opt {
                Some(Token::PAREN(")")) => {
                    consume(&mut self.tokens);
                    break;
                }
                Some(Token::IDENT(name)) => {
                    consume(&mut self.tokens);
                    expect(&mut self.tokens, Token::PUNCT(":"));

                    let v_type = self.parse_type()?;
                    params.push(Parameter { ident: name.to_string(), v_type });

                    let after_param = peek(&mut self.tokens).cloned();
                    match after_param {
                        Some(Token::PAREN(")")) => continue,
                        Some(Token::PUNCT(",")) => { consume(&mut self.tokens); }
                        _ => return Err("Expected ',' or ')' after parameter".to_string()),
                    }
                }
                _ => return Err("Expected identifier or ')' in parameter list".to_string()),
            }
        }

        expect(&mut self.tokens, Token::ARROW);
        
        let return_type = self.parse_type()?;

        let body = self.parse_body()?;
        
        self.generic_stack.pop();

        Ok(AstNode::Function { name: name.to_string(), params, body, return_type, generics })
    }

    fn parse_type(&mut self) -> Result<VType, String> {
        match consume(&mut self.tokens) {
            Some(Token::IDENT(type_name)) => {
                if self.is_generic(type_name) {
                    return Ok(VType::Generic(type_name.to_string()));
                }
                else if self.defined_struct_types.contains_key(type_name)
                {
                    let mut generics = vec![];
                    if let Some(Token::BINOP("<")) = peek(&mut self.tokens)
                    {
                        consume(&mut self.tokens);

                        loop {
                            match peek(&mut self.tokens) {
                                Some(Token::BINOP(">")) => {
                                    consume(&mut self.tokens);
                                    break;
                                }
                                _ => {
                                    let ty = self.parse_type()?;
                                    generics.push(ty);

                                    if let Some(Token::PUNCT(",")) = peek(&mut self.tokens) {
                                        consume(&mut self.tokens);
                                    }
                                }
                            }
                        }
                    }
                    Ok(VType::Struct(type_name.to_string(), generics))
                }
                else if is_vtype(type_name)
                {

                    let mut base = match type_name {
                        "u8" => VType::U8,
                        "i8" => VType::I8,
                        "u16" => VType::U16,
                        "i16" => VType::I16,
                        "u32" => VType::U32,
                        "i32" | "int" => VType::I32,
                        "u64" => VType::U64,
                        "i64" => VType::I64,
                        "f32" => VType::F32,
                        "f64" => VType::F64,
                        "empty" => VType::Empty,
                        "boolean" => VType::Bool,
                        "string" => VType::String,
                        "usize" => VType::USize,
                        "auto" => VType::Auto,
                        "array" => VType::Array(Box::new(VType::Empty)),
                        _ => return Err(format!("Unknown builtin type '{}'", type_name)),
                    };

                    // handle generics
                    if let Some(Token::BINOP("<")) = peek(&mut self.tokens) {
                        consume(&mut self.tokens);

                        let inner = self.parse_type()?;

                        match peek(&mut self.tokens) {
                            Some(Token::BINOP(">")) => { consume(&mut self.tokens); }
                            _ => return Err("Expected '>' after generic".into()),
                        }

                        base = match base {
                            VType::Array(_) => VType::Array(Box::new(inner)),
                            _ => return Err(format!("Type '{}' does not take generics", type_name)),
                        };
                    }

                    Ok(base)
                } 
                else
                {
                    Ok(VType::Unresolved(type_name.to_string()))
                }
            }
            Some(Token::BINOP("&")) => {
                if true {
                    return Err("& reference type not yet supported".into())
                }
                let t = self.parse_type();
                if let Ok(inner) = t {
                    Ok(VType::Ref(Box::new(inner)))
                }
                else {
                    t
                }
            }
            _ => Err("Expected type name in struct field".to_string()),
        }
    }

    fn parse_body(&mut self) -> Result<Vec<Box<AstNode>>, String> {
        expect(&mut self.tokens, Token::PAREN("{"));

        let mut body = Vec::new();
        loop {
            match peek(&mut self.tokens) {
                Some(Token::PAREN("}"))/*Some(Token::END)*/ => {
                    consume(&mut self.tokens);
                    break;
                }
                None => return Err("Unexpected end of input in code block".to_string()),
                _ => {
                    let mut stmt = self.parse_statement()?;

                    // merge chained -> (...) from following lines
                    loop {
                        match peek(&mut self.tokens) {
                            Some(Token::ARROW) => {
                                consume(&mut self.tokens);

                                if let Some(Token::PAREN("(")) = peek(&mut self.tokens) {
                                    consume(&mut self.tokens);
                                    stmt = self.finish_parse_call(stmt)?;
                                } else {
                                    return Err("Expected '(' after '->'".into());
                                }
                            }
                            _ => break,
                        }
                    }

                    body.push(Box::new(stmt));
                }
            }
        }
        Ok(body)
    }

    fn parse_declaration(&mut self) -> Result<AstNode, String> {
        let name = match consume(&mut self.tokens) {
            Some(Token::IDENT(ident)) => AstNode::Identifier(ident.to_string()),
            _ => return Err("Expected identifier after 'let'".to_string()),
        };

        let v_type = match peek(&mut self.tokens) {
            Some(Token::PUNCT(":")) => {
                consume(&mut self.tokens);
                self.parse_type()?
            }
            _ => {
                VType::Inferred
            }
        };

        if peek(&mut self.tokens) != Some(&Token::BINOP("=")) {
            return Ok(AstNode::Declaration {
                name: Box::new(name),
                value: Box::new(AstNode::Identifier("empty".to_string())),
                v_type,
            });
        }

        expect(&mut self.tokens, Token::BINOP("="));
        let value = self.parse_expression()?;

        Ok(AstNode::Declaration {
            name: Box::new(name),
            value: Box::new(value),
            v_type,
        })
    }

    fn parse_expression(&mut self) -> Result<AstNode, String> {
        self.parse_binary_expression(0)
    }

    fn parse_binary_expression(&mut self, min_precedence: i32) -> Result<AstNode, String> {
        let mut left = self.parse_unary_expression()?; 
        
        loop {
            let token = match peek(&mut self.tokens) {
                Some(t) => t,
                None => break,
            }.clone();

            // Check if token is a binary operator
            let (is_binop, op_str) = match token {
                Token::BINOP(op) => (true, op),
                _ => (false, ""),
            };

            if !is_binop {
                break;
            }

            let precedence = get_operator_precedence(op_str);

            if precedence < min_precedence {
                break;
            }

            let is_assignment_op = matches!(op_str, "=" | "+=" | "-=" | "*=" | "/=" | "%=");
            if is_assignment_op {
                consume(&mut self.tokens);
                let right = self.parse_binary_expression(1)?;

                if op_str == "=" {
                    return Ok(AstNode::Assignment {
                        assignee: Box::new(left),
                        value: Box::new(right),
                    });
                }
                else {
                    match &left {
                        AstNode::Identifier(name) => {
                            let base_op = op_str.chars().nth(0).unwrap().to_string();
                            return Ok(AstNode::Assignment {
                                assignee: Box::new(AstNode::Identifier(name.clone())),
                                value: Box::new(AstNode::BinaryOp {
                                    op: base_op,
                                    left: Box::new(left),
                                    right: Box::new(right)
                                })
                            });
                        }
                        _ => {
                            return Err(format!("Cannot apply compound assignment to: '{:#?}'", left).into());
                        }
                    }
                }
            }
            consume(&mut self.tokens);
            let right = self.parse_binary_expression(precedence + 1)?;

            left = AstNode::BinaryOp {
                op: op_str.to_string(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary_expression(&mut self) -> Result<AstNode, String> {
        let token_opt = peek(&mut self.tokens).cloned();

        if let Some(Token::UNARY(op)) = token_opt {
            if op == "-" || op == "!" || op == "~" {
                consume(&mut self.tokens);
                let expr = self.parse_postfix()?;
                return Ok(AstNode::UnaryOp {
                    op: op.to_string(),
                    value: Box::new(expr),
                });
            }
        }

        self.parse_postfix()
    }

    fn parse_fmt_string(&mut self, raw: String) -> Result<AstNode, String> {
        let mut parts = Vec::new();
        let mut chars = raw.chars().peekable();
        let mut literal_buf = String::new();

        while let Some(ch) = chars.next() {
            if ch == '{' {
                if !literal_buf.is_empty() {
                    parts.push(FmtPart::Literal(literal_buf.clone()));
                    literal_buf.clear();
                }

                let mut depth = 1usize;
                let mut inner = String::new();
                for ch2 in chars.by_ref() {
                    match ch2 {
                        '{' => { depth += 1; inner.push(ch2); }
                        '}' => {
                            depth -= 1;
                            if depth == 0 { break; }
                            inner.push(ch2);
                        }
                        _ => inner.push(ch2),
                    }
                }

                // Re-lex and re-parse the interpolation, inheriting struct types
                let inner_tokens = crate::lexer::tokenize(inner);

                let mut inner_parser = Parser::new(inner_tokens);
                inner_parser.defined_struct_types = self.defined_struct_types.clone();
                inner_parser.generic_stack = self.generic_stack.clone();

                let expr = inner_parser.parse_expression()?;
                parts.push(FmtPart::Interpolated(Box::new(expr)));
            } else {
                literal_buf.push(ch);
            }
        }

        if !literal_buf.is_empty() {
            parts.push(FmtPart::Literal(literal_buf));
        }

        Ok(AstNode::FmtString { parts })
    }

    fn parse_postfix(&mut self) -> Result<AstNode, String> {
        let mut expr = self.parse_primary()?;

        loop {
            match peek(&mut self.tokens) {
                Some(Token::PAREN("(")) => {
                    consume(&mut self.tokens);
                    expr = self.finish_parse_call(expr)?;
                }
                Some(Token::PAREN("[")) => {
                    consume(&mut self.tokens);
                    expr = self.finish_parse_index(expr)?;
                }
                Some(Token::PUNCT(".")) | Some(Token::BINOP("::")) => {
                    let op = match consume(&mut self.tokens) {
                        Some(Token::PUNCT(".")) => ".",
                        Some(Token::BINOP("::")) => "::",
                        _ => unreachable!(),
                    };

                    let field = match consume(&mut self.tokens) {
                        Some(Token::IDENT(name)) => name.to_string(),
                        _ => return Err("Expected identifier after access operator".into()),
                    };

                    // Parse generic args if present
                    let mut generics = vec![];
                    if let Some(Token::BINOP("<")) = peek(&mut self.tokens) {
                        let mut lookahead = self.tokens.clone();
                        lookahead.next(); // skip 
                        let mut found_closing = false;
                        while let Some(tok) = lookahead.next() {
                            match tok {
                                Token::BINOP(">") => { found_closing = true; break; }
                                Token::PUNCT(",") | Token::IDENT(_) => continue,
                                _ => break,
                            }
                        }
                        if found_closing {
                            generics = self.parse_generic_args()?;
                        }
                    }

                    let access = AstNode::BinaryOp {
                        op: op.to_string(),
                        left: Box::new(expr),
                        right: Box::new(AstNode::Identifier(field)),
                    };

                    // If generics and ( follow, finish the call immediately
                    if !generics.is_empty() {
                        if let Some(Token::PAREN("(")) = peek(&mut self.tokens) {
                            consume(&mut self.tokens);
                            expr = self.finish_parse_call(AstNode::Call {
                                callee: Box::new(access),
                                generics,
                                args: vec![],
                            })?;
                            continue;
                        }
                    }

                    expr = access;
                }
                Some(Token::AS) => {
                    consume(&mut self.tokens);
                    let ty = self.parse_type()?;
                    expr = AstNode::TypeCast {
                        value: Box::new(expr),
                        ty,
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<AstNode, String> {
        match consume(&mut self.tokens) {
            Some(Token::PAREN("(")) => {
                let expr = self.parse_expression()?;
                expect(&mut self.tokens, Token::PAREN(")"));
                Ok(expr)
            }
            Some(Token::INT(n)) => Ok(AstNode::Integer(n)),
            Some(Token::FLOAT(f)) => Ok(AstNode::Double(f)),
            Some(Token::STRING(s)) => Ok(AstNode::String(s.to_string())),
            Some(Token::IDENT(name)) => {
                let name = name.to_string();

                let mut generics = Vec::new();
                if let Some(Token::BINOP("<")) = peek(&mut self.tokens) {
                    let mut clone_iter = self.tokens.clone();
                    clone_iter.next(); // consume <
                    let mut found_closing = false;
                    while let Some(tok) = clone_iter.next() {
                        match tok {
                            Token::BINOP(">") => { found_closing = true; break; }
                            Token::PUNCT(",") | Token::IDENT(_) => continue,
                            _ => break,
                        }
                    }

                    if found_closing {
                        // Only treat as generic if a closing > is found
                        generics = self.parse_generic_args()?;
                    }
                }

                if let Some(Token::PAREN("{")) = peek(&mut self.tokens) {
                    return self.parse_struct_literal(name, generics);
                }

                if generics.is_empty() {
                    Ok(AstNode::Identifier(name))
                } else {
                    Ok(AstNode::Call {
                        callee: Box::new(AstNode::Identifier(name)),
                        generics,
                        args: vec![],
                    })
                }
            }
            Some(Token::PAREN("{")) => {
                // Lookahead: if next token is IDENT + ':' → struct literal
                let mut clone = self.tokens.clone();

                let is_struct = match clone.next() {
                    Some(Token::IDENT(_)) => {
                        matches!(clone.next(), Some(Token::PUNCT(":")))
                    }
                    _ => false,
                };

                if is_struct {
                    return Err("Anonymous struct literals not supported".into());
                }

                self.parse_array_literal()
            }
            Some(Token::RETURN) => {
                let args = match peek(&mut self.tokens) {
                    // These tokens indicate end of statement or block - no return value
                    Some(Token::PUNCT(";")) | 
                    Some(Token::PAREN("}")) |
                    None => {
                        // Empty return
                        vec![]
                    }
                    // Any other token could potentially start an expression
                    _ => {
                        vec![self.parse_expression()?]
                    }
                };
                Ok(AstNode::Return { args })
            }
            Some(Token::FMTSTRING(raw)) => {
                self.parse_fmt_string(raw.to_string())
            }
            token => Err(format!("Unexpected token in expression: {:?}", token)),
        }
    }

    fn finish_parse_call(&mut self, callee: AstNode) -> Result<AstNode, String> {
        let mut args = Vec::new();

        if let Some(Token::PAREN(")")) = peek(&mut self.tokens) {
            consume(&mut self.tokens);
        } else {
            loop {
                let arg = self.parse_expression()?;
                args.push(Box::new(arg));

                match peek(&mut self.tokens) {
                    Some(Token::PUNCT(",")) => { consume(&mut self.tokens); }
                    Some(Token::PAREN(")")) => { consume(&mut self.tokens); break; }
                    _ => return Err("Expected ',' or ')' in call args".into()),
                }
            }
        }

        match callee {
            AstNode::Call { callee: inner, generics, args: existing_args } => {
                if existing_args.is_empty() {
                    // This is a generic call waiting for args
                    Ok(AstNode::Call {
                        callee: inner,
                        generics,
                        args,
                    })
                } else {
                    // This is a chained call
                    Ok(AstNode::Call {
                        callee: Box::new(AstNode::Call {
                            callee: inner,
                            generics,
                            args: existing_args,
                        }),
                        generics: vec![],
                        args,
                    })
                }
            }
            other => {
                Ok(AstNode::Call {
                    callee: Box::new(other),
                    generics: vec![],
                    args,
                })
            }
        }
    }

    fn finish_parse_index(&mut self, target: AstNode) -> Result<AstNode, String> {
        let index_expr = self.parse_expression()?;

        expect(&mut self.tokens, Token::PAREN("]"));

        Ok(AstNode::Index { target: Box::new(target), index: Box::new(index_expr) })
    }
}

fn get_operator_precedence(op: &str) -> i32 {
    match op {
        ".." => 16,
        "." => 15,
        "::" => 14,
        "*" | "/" | "%" => 12,
        "+" | "-" => 11,
        "<<" | ">>" => 10,
        "<" | "<=" | ">" | ">=" => 9,
        "==" | "!=" => 8,
        "&" => 7,
        "^" => 6,
        "|" => 5,
        "&&" => 4,
        "||" => 3,
        "=" | "+=" | "-=" | "*=" | "/=" | "%=" => 2,
        _ => 0,
    }
}

pub fn parse(tokens: Vec<Token<'static>>) -> Result<AstNode, String> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}
