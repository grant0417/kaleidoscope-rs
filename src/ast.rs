#[derive(Debug, PartialEq, Clone)]
pub enum ExprAst {
    Number {
        value: f64,
    },
    Variable {
        name: String,
    },
    VarExpr {
        var_names: Vec<(String, ExprAst)>,
        body: Box<ExprAst>,
    },
    UnaryExpr {
        op: char,
        operand: Box<ExprAst>,
    },
    BinaryExpr {
        op: char,
        lhs: Box<ExprAst>,
        rhs: Box<ExprAst>,
    },
    CallExpr {
        callee: String,
        args: Vec<ExprAst>,
    },
    IfExpr {
        cond: Box<ExprAst>,
        then: Box<ExprAst>,
        r#else: Box<ExprAst>,
    },
    ForExpr {
        var_name: String,
        start: Box<ExprAst>,
        end: Box<ExprAst>,
        step: Option<Box<ExprAst>>,
        body: Box<ExprAst>,
    },
}

impl ExprAst {
    pub fn new_number(value: f64) -> Self {
        ExprAst::Number { value }
    }

    pub fn new_variable(name: String) -> Self {
        ExprAst::Variable { name }
    }

    pub fn new_var_expr(var_names: Vec<(String, ExprAst)>, body: ExprAst) -> Self {
        ExprAst::VarExpr {
            var_names,
            body: Box::new(body),
        }
    }

    pub fn new_unary_expr(op: char, operand: ExprAst) -> Self {
        ExprAst::UnaryExpr {
            op,
            operand: Box::new(operand),
        }
    }

    pub fn new_binary_expr(op: char, lhs: ExprAst, rhs: ExprAst) -> Self {
        ExprAst::BinaryExpr {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn new_call_expr(callee: String, args: Vec<ExprAst>) -> Self {
        ExprAst::CallExpr { callee, args }
    }

    pub fn new_if_expr(cond: ExprAst, then: ExprAst, r#else: ExprAst) -> Self {
        ExprAst::IfExpr {
            cond: Box::new(cond),
            then: Box::new(then),
            r#else: Box::new(r#else),
        }
    }

    pub fn new_for_expr(
        var_name: String,
        start: ExprAst,
        end: ExprAst,
        step: Option<ExprAst>,
        body: ExprAst,
    ) -> Self {
        ExprAst::ForExpr {
            var_name,
            start: Box::new(start),
            end: Box::new(end),
            step: step.map(Box::new),
            body: Box::new(body),
        }
    }
}

impl std::fmt::Display for ExprAst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprAst::Number { value } => write!(f, "{value}"),
            ExprAst::Variable { name } => write!(f, "{name}"),
            ExprAst::UnaryExpr { op, operand } => write!(f, "({op} {operand})"),
            ExprAst::BinaryExpr { op, lhs, rhs } => {
                write!(f, "({op} {lhs} {rhs})")
            }
            ExprAst::CallExpr { callee, args } => {
                write!(f, "{callee}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            ExprAst::IfExpr { cond, then, r#else } => {
                write!(f, "(if ({cond}) then ({then}) else ({else})")
            }
            ExprAst::ForExpr {
                var_name,
                start,
                end,
                step,
                body,
            } => {
                write!(f, "(for {var_name} = ({start}), ({end}), (",)?;
                if let Some(step) = step {
                    write!(f, "{step}")?;
                } else {
                    write!(f, "1")?;
                }
                write!(f, ") in ({body})")
            }
            ExprAst::VarExpr { var_names, body } => {
                write!(f, "(var ")?;
                for (i, (var_name, init)) in var_names.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{var_name} = {init}")?;
                }
                write!(f, " in {body})")
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum OperatorType {
    Binary,
    Unary,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Operator {
    pub character: char,
    pub precedence: i32,
    pub operator_type: OperatorType,
}

impl Operator {
    pub fn new_binary(character: char, precedence: i32) -> Self {
        Operator {
            character,
            precedence,
            operator_type: OperatorType::Binary,
        }
    }

    pub fn new_unary(character: char) -> Self {
        Operator {
            character,
            // This is meaningless for unary operators
            precedence: 0,
            operator_type: OperatorType::Unary,
        }
    }

    pub fn is_binary(&self) -> bool {
        match self.operator_type {
            OperatorType::Binary => true,
            OperatorType::Unary => false,
        }
    }

    pub fn is_unary(&self) -> bool {
        match self.operator_type {
            OperatorType::Binary => false,
            OperatorType::Unary => true,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrototypeAst {
    pub name: String,
    pub args: Vec<String>,
    pub operator: Option<Operator>,
}

impl PrototypeAst {
    pub fn new(name: String, args: Vec<String>) -> Self {
        PrototypeAst {
            name,
            args,
            operator: None,
        }
    }

    pub fn new_operator(name: String, args: Vec<String>, operator: Option<Operator>) -> Self {
        PrototypeAst {
            name,
            args,
            operator,
        }
    }
}

impl std::fmt::Display for PrototypeAst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{name}(", name = self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{arg}")?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionAst {
    pub prototype: PrototypeAst,
    pub body: ExprAst,
    pub is_anonymous: bool,
}

impl FunctionAst {
    pub fn new(prototype: PrototypeAst, body: ExprAst, is_anonymous: bool) -> Self {
        FunctionAst {
            prototype,
            body,
            is_anonymous,
        }
    }
}

impl std::fmt::Display for FunctionAst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.prototype, self.body)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstNode {
    Expr(ExprAst),
    Prototype(PrototypeAst),
    Function(FunctionAst),
}

impl AstNode {
    pub fn new_expr(expr: ExprAst) -> Self {
        AstNode::Expr(expr)
    }

    pub fn new_prototype(prototype: PrototypeAst) -> Self {
        AstNode::Prototype(prototype)
    }

    pub fn new_function(function: FunctionAst) -> Self {
        AstNode::Function(function)
    }
}

impl std::fmt::Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstNode::Expr(expr) => write!(f, "{}", expr),
            AstNode::Prototype(prototype) => write!(f, "{}", prototype),
            AstNode::Function(function) => write!(f, "{}", function),
        }
    }
}
