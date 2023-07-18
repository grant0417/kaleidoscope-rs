use std::{cell::RefCell, collections::HashMap, rc::Rc};

use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::{Linkage, Module},
    passes::PassManager,
    types::BasicMetadataTypeEnum,
    values::{AnyValue, BasicMetadataValueEnum, FloatValue, FunctionValue, PointerValue},
    FloatPredicate,
};

use crate::ast::{AstNode, ExprAst, FunctionAst, PrototypeAst};

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub named_values: HashMap<String, PointerValue<'ctx>>,
    pub pass_manager: PassManager<FunctionValue<'ctx>>,
    pub execution_engine: ExecutionEngine<'ctx>,
    pub binary_op_precedence: Rc<RefCell<HashMap<char, i32>>>,
}

impl<'ctx> CodeGen<'ctx> {
    fn create_entry_block_alloca(&self, _f: FunctionValue<'ctx>, name: &str) -> PointerValue<'ctx> {
        self.builder.build_alloca(self.context.f64_type(), name)
    }
}

impl<'ctx> ExprAst {
    pub fn codegen(&self, codegen: &mut CodeGen<'ctx>) -> Result<FloatValue<'ctx>, String> {
        match self {
            ExprAst::Number { value } => Ok(codegen.context.f64_type().const_float(*value)),
            ExprAst::Variable { name } => codegen
                .named_values
                .get(name)
                .map(|val| {
                    codegen
                        .builder
                        .build_load(codegen.context.f64_type(), *val, "loadtmp")
                        .into_float_value()
                })
                .ok_or_else(|| format!("No value found for {name}")),
            ExprAst::UnaryExpr { op, operand } => {
                let operand_value = operand.codegen(codegen)?;

                let callee = codegen
                    .module
                    .get_function(&format!("unary{op}"))
                    .ok_or_else(|| format!("Unary operator {op:?} not found"))?;

                let args = [operand_value].map(BasicMetadataValueEnum::FloatValue);

                Ok(codegen
                    .builder
                    .build_call(callee, &args, "unop")
                    .as_any_value_enum()
                    .into_float_value())
            }
            ExprAst::BinaryExpr { op, lhs, rhs } => {
                if op == &'=' {
                    let var_name = match lhs.as_ref() {
                        ExprAst::Variable { name } => name,
                        _ => return Err("Destination of '=' must be a variable".into()),
                    };

                    let val = rhs.codegen(codegen)?;
                    let var = codegen
                        .named_values
                        .get(var_name)
                        .ok_or_else(|| format!("Unknown variable name: {var_name}"))?;

                    codegen.builder.build_store(*var, val);
                    return Ok(val);
                }

                let lhs = lhs.codegen(codegen)?;
                let rhs = rhs.codegen(codegen)?;

                match op {
                    '+' => Ok(codegen.builder.build_float_add(lhs, rhs, "addtmp")),
                    '-' => Ok(codegen.builder.build_float_sub(lhs, rhs, "subtmp")),
                    '*' => Ok(codegen.builder.build_float_mul(lhs, rhs, "multmp")),
                    '<' => {
                        let int = codegen.builder.build_float_compare(
                            FloatPredicate::ULT,
                            lhs,
                            rhs,
                            "cmptmp",
                        );
                        Ok(codegen.builder.build_unsigned_int_to_float(
                            int,
                            codegen.context.f64_type(),
                            "booltmp",
                        ))
                    }
                    _ => {
                        // See if we have user-defined binary operator.
                        let callee = codegen
                            .module
                            .get_function(&format!("binary{op}"))
                            .ok_or_else(|| format!("Binary operator {op:?} not found"))?;

                        let args = [lhs, rhs].map(BasicMetadataValueEnum::FloatValue);

                        Ok(codegen
                            .builder
                            .build_call(callee, &args, "binop")
                            .as_any_value_enum()
                            .into_float_value())
                    }
                }
            }
            ExprAst::CallExpr { callee, args } => {
                let callee_function = codegen
                    .module
                    .get_function(callee)
                    .ok_or_else(|| format!("Unknown function referenced: {callee}"))?;

                if callee_function.count_params() as usize != args.len() {
                    return Err("Incorrect # of arguments passed".into());
                }

                let args = args
                    .iter()
                    .flat_map(|a| a.codegen(codegen))
                    .map(BasicMetadataValueEnum::FloatValue)
                    .collect::<Vec<_>>();

                Ok(codegen
                    .builder
                    .build_call(callee_function, &args, "calltmp")
                    .as_any_value_enum()
                    .into_float_value())
            }
            ExprAst::IfExpr { cond, then, r#else } => {
                let cond_value = cond.codegen(codegen)?;
                let cmp_value = codegen.builder.build_float_compare(
                    FloatPredicate::ONE,
                    cond_value,
                    codegen.context.f64_type().const_float(0.0),
                    "ifcond",
                );

                let callee_function = codegen
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();

                let then_bb = codegen.context.append_basic_block(callee_function, "then");
                let else_bb = codegen.context.append_basic_block(callee_function, "else");
                let merge_bb = codegen
                    .context
                    .append_basic_block(callee_function, "ifcont");

                codegen
                    .builder
                    .build_conditional_branch(cmp_value, then_bb, else_bb);

                codegen.builder.position_at_end(then_bb);
                let then_codegen = then.codegen(codegen)?;
                codegen.builder.build_unconditional_branch(merge_bb);
                let then_bb = codegen.builder.get_insert_block().unwrap();

                codegen.builder.position_at_end(else_bb);
                let else_codegen = r#else.codegen(codegen)?;
                codegen.builder.build_unconditional_branch(merge_bb);
                let else_bb = codegen.builder.get_insert_block().unwrap();

                codegen.builder.position_at_end(merge_bb);
                let phi = codegen
                    .builder
                    .build_phi(codegen.context.f64_type(), "iftmp");
                phi.add_incoming(&[(&then_codegen, then_bb), (&else_codegen, else_bb)]);

                Ok(phi.as_basic_value().into_float_value())
            }
            ExprAst::ForExpr {
                var_name,
                start,
                end,
                step,
                body,
            } => {
                let callee_function = codegen
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();

                let alloc = codegen.create_entry_block_alloca(callee_function, var_name);
                let start_value = start.codegen(codegen)?;

                codegen.builder.build_store(alloc, start_value);

                let loop_bb = codegen.context.append_basic_block(callee_function, "loop");
                codegen.builder.build_unconditional_branch(loop_bb);
                codegen.builder.position_at_end(loop_bb);

                let old_var = codegen.named_values.get(var_name).copied();
                codegen.named_values.insert(var_name.clone(), alloc);

                body.codegen(codegen)?;

                let step_value = match step {
                    Some(step) => step.codegen(codegen)?,
                    None => codegen.context.f64_type().const_float(1.0),
                };

                let end_cond = end.codegen(codegen)?;

                let curr_val = codegen
                    .builder
                    .build_load(codegen.context.f64_type(), alloc, var_name)
                    .into_float_value();
                let next_val = codegen
                    .builder
                    .build_float_add(curr_val, step_value, "nextvar");
                codegen.builder.build_store(alloc, next_val);

                let end_cmp = codegen.builder.build_float_compare(
                    FloatPredicate::ONE,
                    end_cond,
                    codegen.context.f64_type().const_float(0.0),
                    "loopcond",
                );

                let after_bb = codegen
                    .context
                    .append_basic_block(callee_function, "afterloop");

                codegen
                    .builder
                    .build_conditional_branch(end_cmp, loop_bb, after_bb);

                codegen.builder.position_at_end(after_bb);

                match old_var {
                    Some(old_var) => {
                        codegen.named_values.insert(var_name.clone(), old_var);
                    }
                    None => {
                        codegen.named_values.remove(var_name);
                    }
                }

                Ok(codegen.context.f64_type().const_float(0.0))
            }
            ExprAst::VarExpr { var_names, body } => {
                let callee_function = codegen
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();

                let mut old_values = Vec::new();

                for (name, val) in var_names {
                    let alloc = codegen.create_entry_block_alloca(callee_function, name);
                    let val = val.codegen(codegen)?;
                    codegen.builder.build_store(alloc, val);
                    if let Some(old_val) = codegen.named_values.insert(name.clone(), alloc) {
                        old_values.push((name.clone(), old_val));
                    }
                }

                let body_codegen = body.codegen(codegen)?;

                for (name, old_val) in old_values {
                    codegen.named_values.insert(name, old_val);
                }

                Ok(body_codegen)
            }
        }
    }
}

impl<'ctx> PrototypeAst {
    pub fn codegen(&self, codegen: &mut CodeGen<'ctx>) -> Result<FunctionValue<'ctx>, String> {
        let f64_type = codegen.context.f64_type();
        let arg_types = vec![BasicMetadataTypeEnum::FloatType(f64_type); self.args.len()];
        let function_type = codegen.context.f64_type().fn_type(&arg_types, false);

        let function_value =
            codegen
                .module
                .add_function(&self.name, function_type, Some(Linkage::External));

        for (i, arg) in function_value.get_param_iter().enumerate() {
            arg.into_float_value().set_name(&self.args[i]);
        }

        Ok(function_value)
    }
}

impl<'ctx> FunctionAst {
    pub fn codegen(&self, codegen: &mut CodeGen<'ctx>) -> Result<FunctionValue<'ctx>, String> {
        let function = match codegen.module.get_function(&self.prototype.name) {
            Some(function) => function,
            None => self.prototype.codegen(codegen)?,
        };

        if function.count_basic_blocks() > 0 {
            return Err("Function cannot be redefined".into());
        }

        if let Some(operator) = &self.prototype.operator {
            if operator.is_binary() {
                codegen
                    .binary_op_precedence
                    .borrow_mut()
                    .insert(operator.character, operator.precedence);
            }
        }

        let bb = codegen.context.append_basic_block(function, "entry");
        codegen.builder.position_at_end(bb);

        // codegen.named_values.clear();
        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = &self.prototype.args[i];
            let alloc = codegen.create_entry_block_alloca(function, arg_name);
            codegen.builder.build_store(alloc, arg);
            codegen.named_values.insert(arg_name.clone(), alloc);
        }

        match self.body.codegen(codegen) {
            Ok(ret) => {
                codegen.builder.build_return(Some(&ret));
                if function.verify(true) {
                    codegen.pass_manager.run_on(&function);
                    Ok(function)
                } else {
                    Err(format!("Failed to verify function: {function}"))
                }
            }
            Err(err) => Err(format!("Failed to gen body: {err}")),
        }
    }
}

impl<'ctx> AstNode {
    pub fn codegen(&self, codegen: &mut CodeGen<'ctx>, print_ir: bool) -> Result<(), String> {
        match self {
            AstNode::Expr(expr) => {
                let val = expr.codegen(codegen)?;
                if print_ir {
                    // eprintln!("IR:");
                    val.print_to_stderr();
                }
            }
            AstNode::Prototype(proto) => {
                let val = proto.codegen(codegen)?;
                if print_ir {
                    // eprintln!("IR:");
                    val.print_to_stderr();
                }
            }
            AstNode::Function(func) => {
                let func_val = func.codegen(codegen)?;

                if print_ir {
                    // eprintln!("IR:");
                    func_val.print_to_stderr();
                }

                if func.is_anonymous {
                    codegen
                        .execution_engine
                        .remove_module(&codegen.module)
                        .unwrap();
                    codegen
                        .execution_engine
                        .add_module(&codegen.module)
                        .unwrap();

                    let maybe_fn = unsafe {
                        codegen
                            .execution_engine
                            .get_function::<unsafe extern "C" fn() -> f64>(&func.prototype.name)
                    };

                    match maybe_fn {
                        Ok(compiled_fn) => unsafe {
                            println!("=> {}", compiled_fn.call());
                        },
                        Err(err) => {
                            println!("!> Error during execution: {:?}", err);
                        }
                    };

                    // Safe if `func_val` is not used after this call.
                    // unsafe { func_val.delete() };
                }
            }
        }

        Ok(())
    }
}
