#![allow(unused)]

use std::iter::Peekable;
use std::vec::IntoIter;

use crate::lexer::{Token};
use crate::operator::Operator;

#[derive(Clone)]
struct ParsingContext {
    pub no_struct_literals: bool,
    pub no_fn_body:         bool,
    pub current_class:      Option<String>,
}

impl ParsingContext {
    fn new() -> Self {
        Self {
            no_struct_literals: false,
            no_fn_body:         false,
            current_class:      None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeNode {
    NominalType {
        name:     String,
        generics: Vec<TypeNode>,
    },
    ArrayType {
        elem_type: Box<TypeNode>,
    },
    Qualified {
        inner:    Box<TypeNode>,
        mutable:  bool,
        borrowed: bool,
        moved:    bool,
    },
    Inferred,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BindingNode {
    IdentifierBinding { name: String, ty: TypeNode },
    OrderedBinding   { bindings: Vec<BindingNode> },
    UnorderedBinding { bindings: Vec<BindingNode> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassMember {
    Field {
        name:      String,
        ty:        TypeNode,
        is_public: bool,
    },
    Method {
        name:        String,
        type_params: Vec<(TypeNode, Option<TypeNode>)>,
        
        has_self:    bool,
        params:      Vec<BindingNode>,
        return_type: TypeNode,
        body:        Vec<AstNode>,
        is_public:   bool,
    },
    OperatorOverload {
        op:           Operator,
        params:       Vec<BindingNode>,
        return_type:  TypeNode,
        body:         Vec<AstNode>
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Identifier(String),

    IntLiteral(i32),
    FloatLiteral(f64),
    StringLiteral(String),
    SelfExpr,

    VarDeclaration {
        binding:    BindingNode,
        init_value: Option<Box<AstNode>>,
    },
    Assignment {
        left:  Box<AstNode>,
        right: Box<AstNode>,
    },

    FunctionDeclaration {
        name:        String,
        type_params: Vec<(TypeNode, Option<TypeNode>)>,
        params:      Vec<BindingNode>,
        return_type: TypeNode,
        body:        Vec<AstNode>,
    },
    ReturnStmt { value: Option<Box<AstNode>> },

    StaticImportStmt {
        namespace_alias: String,
        path:            String,
    },
    DynamicImportStmt {
        namespace_alias: String,
        path:            String,
    },
    UseStmt {
        base_path: Box<AstNode>,
        used:      Vec<(String, String)>,
    },
    Public(Box<AstNode>),
    Borrowed(Box<AstNode>),
    Moved(Box<AstNode>),

    BinaryOperation {
        op:    Operator,
        left:  Box<AstNode>,
        right: Box<AstNode>,
    },
    UnaryOperation {
        op:    Operator,
        right: Box<AstNode>,
    },
    ComputedIndex {
        indexee: Box<AstNode>,
        index:   Box<AstNode>,
    },
    DotIndex {
        indexee: Box<AstNode>,
        index:   Box<AstNode>,
    },
    NamespaceIndex {
        indexee: Box<AstNode>,
        index:   Box<AstNode>,
    },
    FunctionCall {
        callee: Box<AstNode>,
        args:   Vec<AstNode>,
    },
    ClassLiteral {
        ty:     Box<AstNode>,
        fields: Vec<(String, AstNode)>,
    },
    TypeInstantiation {
        callee:    Box<AstNode>,
        type_args: Vec<TypeNode>,
    },
    ConditionalBranch {
        next:      Option<Box<AstNode>>,
        condition: Option<Box<AstNode>>,
    },
    WhileLoop {
        condition: Box<AstNode>,
        body:      Vec<AstNode>,
    },
    ForLoop {
        params:   Vec<BindingNode>,
        iterator: Box<AstNode>,
        body:     Vec<AstNode>,
    },
    TypeCast {
        left: Box<AstNode>,
        right: TypeNode,
    },

    ClassDefinition {
        name:    String,
        members: Vec<ClassMember>,
    },

    Program(Vec<AstNode>),
}

type PeekIter<T> = Peekable<IntoIter<T>>;

pub struct LucyParser<'token_life> {
    tokens: PeekIter<Token<'token_life>>,
}

impl<'token_life> LucyParser<'token_life> {
    pub fn new(tokens: Vec<Token<'token_life>>) -> Self {
        Self { tokens: tokens.into_iter().peekable() }
    }
}

impl<'token_life> LucyParser<'token_life> {
    fn consume(&mut self) -> Option<Token<'token_life>> {
        self.tokens.next()
    }
    fn peek(&mut self) -> Option<&Token<'token_life>> {
        self.tokens.peek()
    }
    fn peek_some(&mut self) -> Token<'token_life> {
        self.tokens.peek()
            .unwrap_or_else(|| panic!("Unexpected end of input"))
            .clone()
    }
    fn expect_any_some(&mut self) -> Token<'token_life> {
        self.tokens.next()
            .unwrap_or_else(|| panic!("Unexpected end of input"))
    }
    fn expect_some(&mut self, expected: Token<'token_life>, msg: &str) -> Token<'token_life> {
        let got = self.tokens.next()
            .unwrap_or_else(|| panic!("Expected {:?} but got end of input: {}", expected, msg));
        if got != expected {
            panic!("Expected {:?}, got {:?}: {}", expected, got, msg);
        }
        got
    }
}

impl<'token_life> LucyParser<'token_life> {
    fn parse_body(&mut self, ctx: &ParsingContext) -> Vec<AstNode> {
        self.expect_some(Token::DO, "Expected 'do' to start body");

        let mut body = Vec::new();
        loop {
            match self.peek_some() {
                Token::END => { self.consume(); break; }
                _ => body.push(self.parse_stmt(ctx)),
            }
        }

        body
    }

    fn parse_fn_body(&mut self, ctx: &ParsingContext) -> Vec<AstNode> {
        let mut body = Vec::new();

        loop {
            match self.peek_some() {
                Token::END => { self.consume(); break; }
                _ => body.push(self.parse_stmt(ctx)),
            }
        }

        body
    }

    fn parse_binding(&mut self, ctx: &ParsingContext) -> BindingNode {
        match self.peek_some() {
            Token::IDENT(_) | Token::SELF => {
                // Allow `self` as a binding name (for method first-param)
                let name = match self.consume().unwrap() {
                    Token::IDENT(s)  => s.to_string(),
                    Token::SELF   => "self".to_string(),
                    _ => unreachable!(),
                };
                let ty = if let Token::PUNCT(":") = self.peek_some() {
                    self.consume();
                    self.parse_type(ctx)
                } else {
                    TypeNode::Inferred
                };
                BindingNode::IdentifierBinding { name, ty }
            }
            Token::PAREN("(") => {
                self.consume();
                let mut bindings = Vec::new();
                loop {
                    match self.peek_some() {
                        Token::PAREN(")") => { self.consume(); break; }
                        Token::PUNCT(",") => { self.consume(); }
                        _ => bindings.push(self.parse_binding(ctx)),
                    }
                }
                BindingNode::OrderedBinding { bindings }
            }
            Token::PAREN("{") => {
                self.consume();
                let mut bindings = Vec::new();
                loop {
                    match self.peek_some() {
                        Token::PAREN("}") => { self.consume(); break; }
                        Token::PUNCT(",") => { self.consume(); }
                        _ => bindings.push(self.parse_binding(ctx)),
                    }
                }
                BindingNode::UnorderedBinding { bindings }
            }
            other => panic!("Unknown binding initializer: {:?}", other),
        }
    }

    fn parse_type(&mut self, ctx: &ParsingContext) -> TypeNode {
        let mut is_mutable  = false;
        let mut is_borrowed = false;
        let mut is_moved    = false;

        loop {
            match self.peek_some() {
                Token::MUTABLE  => { self.consume(); is_mutable  = true; }
                Token::AND => { self.consume(); is_borrowed = true; }
                Token::ARROW    => { self.consume(); is_moved    = true; }
                _ => break,
            }
        }

        let base = match self.expect_any_some() {
            Token::IDENT(name) => {
                let name = name.to_string();
                let mut generics = Vec::new();
                if let Token::BINOP("<") = self.peek_some() {
                    self.consume();
                    loop {
                        match self.peek_some() {
                            Token::BINOP(">") => { self.consume(); break; }
                            Token::PUNCT(",")  => { self.consume(); }
                            _                  => generics.push(self.parse_type(ctx)),
                        }
                    }
                }
                TypeNode::NominalType { name, generics }
            }
            // `Self` as a type inside a class body
            Token::SELFTYPE => {
                let name = ctx.current_class.clone()
                    .unwrap_or_else(|| panic!("'Self' used outside of a class body"));
                TypeNode::NominalType { name, generics: vec![] }
            }
            Token::PAREN("{") => {
                let elem_type = self.parse_type(ctx);
                self.expect_some(Token::PAREN("}"), "Expected '}' after array element type");
                TypeNode::ArrayType { elem_type: Box::new(elem_type) }
            }
            other => panic!("Unhandled type token: {:?}", other),
        };

        if is_mutable || is_borrowed || is_moved {
            TypeNode::Qualified {
                inner:    Box::new(base),
                mutable:  is_mutable,
                borrowed: is_borrowed,
                moved:    is_moved,
            }
        } else {
            base
        }
    }

    fn parse_call_args(&mut self, ctx: &ParsingContext) -> Vec<AstNode> {
        let mut args = Vec::new();
        loop {
            match self.peek_some() {
                Token::PAREN(")") => { self.consume(); break; }
                Token::PUNCT(",")  => { self.consume(); }
                _                  => args.push(self.parse_expr(ctx)),
            }
        }
        args
    }
}

impl<'token_life> LucyParser<'token_life> {
    pub fn parse_file_source(&mut self) -> AstNode {
        let mut stmts = Vec::new();
        let ctx = ParsingContext::new();
        while self.peek().is_some() {
            stmts.push(self.parse_stmt(&ctx));
        }
        AstNode::Program(stmts)
    }

    fn parse_stmt(&mut self, ctx: &ParsingContext) -> AstNode {
        match self.peek_some() {
            Token::FN      => { self.consume(); self.parse_fun_declaration(ctx) }
            Token::DECLARE => { self.consume(); self.parse_var_declaration(ctx) }
            Token::FOR     => { self.consume(); self.parse_for_loop(ctx) }
            Token::RETURN  => { self.consume(); self.parse_ret(ctx) }
            Token::IMPORT  => { self.consume(); self.parse_static_import(ctx) }
            Token::USE     => { self.consume(); self.parse_use(ctx) }
            Token::PUB     => { self.consume(); self.parse_public(ctx) }
            Token::CLASS   => { self.consume(); self.parse_class_definition(ctx) }
            _              => self.parse_expr(ctx),
        }
    }
    
    fn parse_class_definition(&mut self, ctx: &ParsingContext) -> AstNode {
        let name = match self.expect_any_some() {
            Token::IDENT(s) => s.to_string(),
            other => panic!("Expected class name, got {:?}", other),
        };

        // Build a context that knows `Self` refers to this class
        let mut class_ctx = ctx.clone();
        class_ctx.current_class = Some(name.clone());

        let mut members = Vec::new();

        loop {
            match self.peek_some() {
                Token::END => { self.consume(); break; }
                
                Token::PUB => {
                    self.consume();
                    match self.peek_some() {
                        Token::FN => {
                            self.consume();
                            members.push(self.parse_class_method(&class_ctx, &name, true));
                        }
                        Token::IDENT(_) => {
                            members.push(self.parse_class_field(&class_ctx, true));
                        }
                        other => panic!("Expected fn or field after pub in class, got {:?}", other),
                    }
                }

                Token::OPERATOR => {
                    self.consume();
                    
                    members.push(self.parse_operator_overload(&class_ctx));
                }

                Token::FN => {
                    self.consume();
                    members.push(self.parse_class_method(&class_ctx, &name, false));
                }

                Token::IDENT(_) => {
                    members.push(self.parse_class_field(&class_ctx, false));
                }

                other => panic!("Unexpected token in class body: {:?}", other),
            }
        }

        AstNode::ClassDefinition { name, members }
    }

    fn parse_operator_overload(&mut self, ctx: &ParsingContext) -> ClassMember {
        let op = match self.expect_any_some()
        {
            Token::BINOP(s) => s,
            Token::UNARY(s) => s,
            other => panic!("Unknown operator {:?}", other)
        };

        self.expect_some(Token::PAREN("("), "Expected '(' after method name");
        
        let mut params = Vec::new();
        loop {
            match self.peek_some() {
                Token::PAREN(")") => { self.consume(); break; }
                Token::PUNCT(",")  => { self.consume(); }
                _                  => params.push(self.parse_binding(ctx)),
            }
        }

        let return_type = if let Token::PUNCT(":") = self.peek_some() {
            self.consume();
            self.parse_type(ctx)
        } else {
            TypeNode::Inferred
        };

        let body = self.parse_fn_body(ctx);
        let operator = match op {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "/" => Operator::Div,
            "*" => Operator::Mul,
            other => panic!("Unknown operator '{}'", other)
        };

        ClassMember::OperatorOverload { op: operator, params, return_type, body }
    }

    fn parse_class_field(&mut self, ctx: &ParsingContext, is_public: bool) -> ClassMember {
        let name = match self.expect_any_some() {
            Token::IDENT(s) => s.to_string(),
            other => panic!("Expected field name, got {:?}", other),
        };
        self.expect_some(Token::PUNCT(":"), "Expected ':' after field name");
        let ty = self.parse_type(ctx);
        ClassMember::Field { name, ty, is_public }
    }

    fn parse_class_method(
        &mut self,
        ctx:       &ParsingContext,
        class_name: &str,
        is_public: bool,
    ) -> ClassMember {
        let method_name = match self.expect_any_some() {
            Token::IDENT(s) => s.to_string(),
            other => panic!("Expected method name, got {:?}", other),
        };
        
        let mut type_params = Vec::new();
        if let Token::BINOP("<") = self.peek_some() {
            self.consume();
            loop {
                match self.peek_some() {
                    Token::BINOP(">") => { self.consume(); break; }
                    Token::PUNCT(",") => { self.consume(); }
                    _ => {
                        let node = self.parse_type(ctx);
                        let constraint = if let Token::PUNCT(":") = self.peek_some() {
                            self.consume();
                            Some(self.parse_type(ctx))
                        } else { None };
                        type_params.push((node, constraint));
                    }
                }
            }
        }

        self.expect_some(Token::PAREN("("), "Expected '(' after method name");

        let has_self = matches!(self.peek_some(), Token::SELF);
        if has_self {
            self.consume();
            if let Token::PUNCT(",") = self.peek_some() { self.consume(); }
        }

        let mut params = Vec::new();
        loop {
            match self.peek_some() {
                Token::PAREN(")") => { self.consume(); break; }
                Token::PUNCT(",")  => { self.consume(); }
                _                  => params.push(self.parse_binding(ctx)),
            }
        }

        let return_type = if let Token::PUNCT(":") = self.peek_some() {
            self.consume();
            self.parse_type(ctx)
        } else {
            TypeNode::Inferred
        };

        let body = self.parse_fn_body(ctx);

        ClassMember::Method {
            name: method_name,
            type_params,
            has_self,
            params,
            return_type,
            body,
            is_public,
        }
    }

    fn parse_public(&mut self, ctx: &ParsingContext) -> AstNode {
        match self.peek_some() {
            Token::CLASS => {
                self.consume();
                AstNode::Public(Box::new(self.parse_class_definition(ctx)))
            }
            Token::IDENT(..) => AstNode::Public(Box::new(self.parse_var_declaration(ctx))),
            Token::FN => {
                self.consume();
                AstNode::Public(Box::new(self.parse_fun_declaration(ctx)))
            }
            other => panic!("Cannot export this statement as public: {:?}", other),
        }
    }

    fn parse_static_import(&mut self, ctx: &ParsingContext) -> AstNode {
        let ident = match self.expect_any_some() {
            Token::IDENT(s) => s.to_string(),
            other => panic!("Expected identifier after 'import', got {:?}", other),
        };
        let path = match self.expect_any_some() {
            Token::STRING(s) => s,
            other => panic!("Expected string after import alias, got {:?}", other),
        };
        AstNode::StaticImportStmt { namespace_alias: ident, path: path.to_string() }
    }

    fn parse_use(&mut self, ctx: &ParsingContext) -> AstNode {
        let mut base_path = match self.expect_any_some() {
            Token::IDENT(s) => AstNode::Identifier(s.to_string()),
            other => panic!("Expected identifier at start of use path, got {:?}", other),
        };

        let used = loop {
            match self.peek_some() {
                Token::BINOP("::") => {
                    self.consume();
                    match self.peek_some() {
                        Token::PAREN("{") => {
                            self.consume();
                            let mut used = Vec::new();
                            loop {
                                match self.peek_some() {
                                    Token::PAREN("}") => { self.consume(); break; }
                                    Token::PUNCT(",")  => { self.consume(); }
                                    Token::IDENT(_) => {
                                        let actual = match self.expect_any_some() {
                                            Token::IDENT(s) => s.to_string(),
                                            _ => unreachable!(),
                                        };
                                        let alias = if let Token::AS = self.peek_some() {
                                            self.consume();
                                            match self.expect_any_some() {
                                                Token::IDENT(s) => s.to_string(),
                                                other => panic!("Expected ident after 'token_lifes', got {:?}", other),
                                            }
                                        } else {
                                            actual.clone()
                                        };
                                        used.push((actual, alias));
                                    }
                                    other => panic!("Expected ident or '}}' in use list, got {:?}", other),
                                }
                            }
                            break used;
                        }
                        Token::IDENT(_) => {
                            let name = match self.expect_any_some() {
                                Token::IDENT(s) => s.to_string(),
                                _ => unreachable!(),
                            };
                            match self.peek_some() {
                                Token::BINOP("::") => {
                                    base_path = AstNode::NamespaceIndex {
                                        indexee: Box::new(base_path),
                                        index:   Box::new(AstNode::Identifier(name)),
                                    };
                                }
                                Token::AS => {
                                    self.consume();
                                    let alias = match self.expect_any_some() {
                                        Token::IDENT(s) => s.to_string(),
                                        other => panic!("Expected ident after 'token_lifes', got {:?}", other),
                                    };
                                    break vec![(name, alias)];
                                }
                                _ => break vec![(name.clone(), name)],
                            }
                        }
                        other => panic!("Expected ident or '{{' after '::', got {:?}", other),
                    }
                }
                other => panic!("Expected '::' in use statement, got {:?}", other),
            }
        };

        AstNode::UseStmt { base_path: Box::new(base_path), used }
    }

    fn parse_ret(&mut self, ctx: &ParsingContext) -> AstNode {
        let value = match self.peek_some() {
            Token::PAREN("}") => None,
            _ => Some(Box::new(self.parse_expr(ctx))),
        };
        AstNode::ReturnStmt { value }
    }

    fn parse_for_loop(&mut self, ctx: &ParsingContext) -> AstNode {
        let param = self.parse_binding(ctx);
        self.expect_some(Token::IN, "Expected 'in' after for-loop binding");
        let mut no_struct_ctx = ctx.clone();
        no_struct_ctx.no_struct_literals = true;
        let iterator = self.parse_expr(&no_struct_ctx);
        let body     = self.parse_body(ctx);
        AstNode::ForLoop { params: vec![param], iterator: Box::new(iterator), body }
    }

    fn parse_var_declaration(&mut self, ctx: &ParsingContext) -> AstNode {
        let binding = self.parse_binding(ctx);
        let init_value = if let Token::BINOP("=") = self.peek_some() {
            self.consume();
            Some(Box::new(self.parse_expr(ctx)))
        } else {
            None
        };
        AstNode::VarDeclaration { binding, init_value }
    }

    fn parse_fun_declaration(&mut self, ctx: &ParsingContext) -> AstNode {
        let name = match self.consume() {
            Some(Token::IDENT(s)) => s.to_string(),
            other => panic!("Expected function name, got {:?}", other),
        };

        let mut type_params = Vec::new();
        if let Token::BINOP("<") = self.peek_some() {
            self.consume();
            loop {
                match self.peek_some() {
                    Token::BINOP(">") => { self.consume(); break; }
                    Token::PUNCT(",") => { self.consume(); }
                    _ => {
                        let node = self.parse_type(ctx);
                        let constraint = if let Token::PUNCT(":") = self.peek_some() {
                            self.consume(); Some(self.parse_type(ctx))
                        } else { None };
                        type_params.push((node, constraint));
                    }
                }
            }
        }

        self.expect_some(Token::PAREN("("), "Expected '(' after function name");
        let mut params = Vec::new();
        loop {
            match self.peek_some() {
                Token::PAREN(")") => { self.consume(); break; }
                Token::PUNCT(",")  => { self.consume(); }
                _                  => params.push(self.parse_binding(ctx)),
            }
        }

        let return_type = if let Token::PUNCT(":") = self.peek_some() {
            self.consume();
            self.parse_type(ctx)
        } else {
            TypeNode::Inferred
        };

        let body = if ctx.no_fn_body { Vec::new() } else { self.parse_fn_body(ctx) };

        AstNode::FunctionDeclaration { name, params, type_params, return_type, body }
    }
}

impl<'token_life> LucyParser<'token_life> {
    fn parse_expr(&mut self, ctx: &ParsingContext) -> AstNode {
        self.parse_expr_bp(ctx, 0)
    }

    fn get_bp(op: &Token) -> Option<u8> {
        match op {
            Token::BINOP("||")                          => Some(2),
            Token::BINOP("&&")                          => Some(3),
            Token::BINOP("|")                           => Some(4),
            Token::AND                                  => Some(5), 
            Token::BINOP("==") | Token::BINOP("!=") | Token::BINOP("<") | Token::BINOP(">") | Token::BINOP("<=") | Token::BINOP(">=") => Some(6),
            Token::BINOP("<")  | Token::BINOP(">")
            | Token::BINOP("<=")| Token::BINOP(">=")   => Some(7),
            Token::BINOP("<<") | Token::BINOP(">>")     => Some(8),
            Token::BINOP("+")  | Token::BINOP("-")      => Some(10),
            Token::BINOP("*")  | Token::BINOP("/")
            | Token::BINOP("%")                         => Some(20),
            Token::BINOP("^")                           => Some(25), // right-assoc pow
            Token::AS                                   => Some(30),
            _ => None,
        }
    }

    fn parse_expr_bp(&mut self, ctx: &ParsingContext, min_bp: u8) -> AstNode {
        let mut left = self.parse_primary(ctx);
        left = self.parse_postfix(ctx, left);

        loop {
            let bp = match self.peek() {
                Some(op) => match Self::get_bp(op) {
                    Some(bp) if bp > min_bp => bp,
                    _ => break,
                },
                None => break,
            };
            let op_token = self.consume().unwrap();
            match op_token {
                Token::AS => {
                    let ty = self.parse_type(ctx);
                    left = AstNode::TypeCast { left: Box::new(left), right: ty };
                }
                _ => {
                    let op = match op_token {
                        Token::BINOP("+")  => Operator::Add,
                        Token::BINOP("-")  => Operator::Sub,
                        Token::BINOP("*")  => Operator::Mul,
                        Token::BINOP("/")  => Operator::Div,
                        Token::BINOP("%")  => Operator::Mod,
                        Token::BINOP("^")  => Operator::Pow,
                        Token::BINOP("<<") => Operator::BLShift,
                        Token::BINOP(">>") => Operator::BRShift,
                        Token::BINOP("&&") => Operator::LAnd,
                        Token::BINOP("||") => Operator::LOr,
                        Token::AND         => Operator::BAnd,
                        Token::BINOP("|")  => Operator::BOr,
                        Token::BINOP("==") => Operator::Eq,
                        Token::BINOP("!=") => Operator::NEq,
                        Token::BINOP("<")  => Operator::Lt,
                        Token::BINOP(">")  => Operator::Gt,
                        Token::BINOP("<=") => Operator::Le,
                        Token::BINOP(">=") => Operator::Ge,
                        _ => unreachable!(),
                    };
                    let right = self.parse_expr_bp(ctx, bp);
                    left = self.parse_postfix(ctx, AstNode::BinaryOperation {
                        op,
                        left:  Box::new(left),
                        right: Box::new(right),
                    });
                }
            }
        }
        left
    }

    fn parse_postfix(&mut self, ctx: &ParsingContext, mut left: AstNode) -> AstNode {
        loop {
            left = match self.peek() {
                Some(Token::PAREN("(")) => {
                    self.consume();
                    let args = self.parse_call_args(ctx);
                    AstNode::FunctionCall { callee: Box::new(left), args }
                }
                Some(Token::PAREN("[")) => {
                    self.consume();
                    let index = self.parse_expr(ctx);
                    self.expect_some(Token::PAREN("]"), "Expected ']'");
                    AstNode::ComputedIndex { indexee: Box::new(left), index: Box::new(index) }
                }
                Some(Token::PUNCT(".")) => {
                    self.consume();
                    let field = match self.expect_any_some() {
                        Token::IDENT(s) => AstNode::Identifier(s.to_string()),
                        other => panic!("Expected field name after '.', got {:?}", other),
                    };
                    AstNode::DotIndex { indexee: Box::new(left), index: Box::new(field) }
                }
                Some(Token::BINOP("::")) => {
                    self.consume();
                    match self.peek_some() {
                        Token::BINOP("<") => {
                            self.consume();
                            let mut type_args = Vec::new();
                            loop {
                                match self.peek_some() {
                                    Token::BINOP(">") => { self.consume(); break; }
                                    Token::PUNCT(",")  => { self.consume(); }
                                    _                  => type_args.push(self.parse_type(ctx)),
                                }
                            }
                            AstNode::TypeInstantiation { callee: Box::new(left), type_args }
                        }
                        Token::IDENT(_) => {
                            let seg = match self.expect_any_some() {
                                Token::IDENT(s) => AstNode::Identifier(s.to_string()),
                                _ => unreachable!(),
                            };
                            AstNode::NamespaceIndex { indexee: Box::new(left), index: Box::new(seg) }
                        }
                        other => panic!("Expected ident or '<' after '::', got {:?}", other),
                    }
                }
                Some(Token::PAREN("{")) if !ctx.no_struct_literals => {
                    self.parse_class_literal(ctx, left)
                }
                _ => break,
            };
        }
        left
    }

    fn parse_class_literal(&mut self, ctx: &ParsingContext, ty: AstNode) -> AstNode {
        self.expect_some(Token::PAREN("{"), "Expected '{'");
        let mut fields = Vec::new();
        loop {
            match self.peek_some() {
                Token::PAREN("}") => { self.consume(); break; }
                Token::PUNCT(",")  => { self.consume(); }
                Token::IDENT(_) => {
                    let name = match self.expect_any_some() {
                        Token::IDENT(s) => s.to_string(),
                        _ => unreachable!(),
                    };
                    self.expect_some(Token::BINOP("="), "Expected '=' after field name in struct literal");
                    let value = self.parse_expr(ctx);
                    fields.push((name, value));
                }
                other => panic!("Expected field or '}}' in struct literal, got {:?}", other),
            }
        }
        AstNode::ClassLiteral { ty: Box::new(ty), fields }
    }

    fn parse_primary(&mut self, ctx: &ParsingContext) -> AstNode {
        match self.expect_any_some() {
            Token::INT(n)     => AstNode::IntLiteral(n),
            Token::FLOAT(f)   => AstNode::FloatLiteral(f),
            Token::STRING(s)  => AstNode::StringLiteral(s.to_string()),
            Token::IDENT(s)   => AstNode::Identifier(s.to_string()),
            Token::SELF       => AstNode::SelfExpr,
            Token::SELFTYPE   => {
                let class_name = ctx.current_class.clone()
                    .unwrap_or_else(|| panic!("'Self' used outside a class body"));
                AstNode::Identifier(class_name)
            }
            Token::AND => {
                let operand = self.parse_primary(ctx);
                AstNode::Borrowed(Box::new(operand))
            }
            Token::UNARY("-") => {
                let operand = self.parse_primary(ctx);
                AstNode::UnaryOperation { op: Operator::Neg, right: Box::new(operand) }
            }
            Token::UNARY("!") => {
                let operand = self.parse_primary(ctx);
                AstNode::UnaryOperation { op: Operator::LNot, right: Box::new(operand) }
            }
            Token::UNARY("~") => {
                let operand = self.parse_primary(ctx);
                AstNode::UnaryOperation { op: Operator::BNot, right: Box::new(operand) }
            }
            
            Token::ARROW => {
                let operand = self.parse_primary(ctx);
                AstNode::Moved(Box::new(operand))
            }

            Token::PAREN("(") => {
                let mut inner_ctx = ctx.clone();
                inner_ctx.no_struct_literals = false;
                let expr = self.parse_expr(&inner_ctx);
                self.expect_some(Token::PAREN(")"), "Expected ')'");
                expr
            }
            other => panic!("Unexpected token in expression: {:?}", other),
        }
    }
}