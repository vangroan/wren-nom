//! Abstract-syntax-tree types.

// TODO: Remove when all are implemented
#![allow(dead_code)]

use smol_str::SmolStr;

/// Reserved language-defined identifiers.
///
/// Because there's no explicit lexical analysis step,
/// the keywords live with the syntactic domain.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum KeywordType {
    Break,
    Class,
    Construct,
    Continue,
    False,
    For,
    Foreign,
    If,
    Import,
    Is,
    Return,
    Static,
    Super,
    This,
    True,
    Var,
    While,
}

impl KeywordType {
    /// A string representation of the keyword as it would
    /// appear in source code.
    #[rustfmt::skip]
    pub fn as_str(&self) -> &str {
        match self {
            Self::Break     => "break",
            Self::Class     => "class",
            Self::Construct => "construct",
            Self::Continue  => "continue",
            Self::False     => "false",
            Self::For       => "for",
            Self::Foreign   => "foreign",
            Self::If        => "if",
            Self::Import    => "import",
            Self::Is        => "is",
            Self::Return    => "return",
            Self::Static    => "static",
            Self::Super     => "super",
            Self::This      => "this",
            Self::True      => "true",
            Self::Var       => "var",
            Self::While     => "while",
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub stmts: Vec<DefStmt>,
}

/// Definition statement.
///
/// These are statements that declare bindings like `var` and
/// `class`, which may only appear at the top level of curly
/// braced blocks.
///
/// They are not allowed in places like `if` conditional
/// statement branches that do not have curly braces.
///
/// The following is *valid*:
///
/// ```wren
/// if (true) {
///   var x = 1
/// }
/// ```
///
/// The following is *invalid*:
///
/// ```wren
/// if (true) var x = 1
/// if (true) { var x = 1 }
/// if (true)
///   var x = 1
/// ```
#[derive(Debug)]
pub enum DefStmt {
    Class(ClassDef),
    // Foreign,
    // Import,
    AssignDef(AssignDef),
    // Simple(SimpleStmt),
    /// Failed to parse definition statement.
    Error,
    Empty,
}

/// Simple statement.
///
/// Definition statements can only appear at the top level of
/// the curly braces.
///
/// Simple statements however exclude variable binding statements
/// like `var` and `class`, because these are not allowed in places
/// like the branches of `if` conditional statement that don't
/// have curly braces.
#[derive(Debug)]
pub enum SimpleStmt {
    // Break,
// Continue,
// For,
// If,
// Return,
// Comment(Comment),
// Expression statement is an expression that appears
// on its own line.
// Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Ident(Ident),
    Lit(Literal),
    Error,
}

/// User defined identifier.
#[derive(Debug)]
pub struct Ident {
    pub name: SmolStr,
}

#[derive(Debug)]
pub enum Literal {
    Number(SmolStr),
}

#[derive(Debug)]
pub struct ClassDef {
    pub name: Ident,
    pub parent: Option<Ident>,
    pub body: ClassBody,
}

#[derive(Debug)]
pub struct ClassBody {}

/// Variable definition with assignment.
#[derive(Debug)]
pub struct AssignDef {
    pub keyword: Ident,
    /// Name of the new variable to be declared.
    pub name: Ident,
    // pub op: BinaryOp,
    /// Right-hand side expression.
    pub expr: Expr,
}
