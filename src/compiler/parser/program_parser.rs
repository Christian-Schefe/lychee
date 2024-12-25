use crate::compiler::lexer;
use crate::compiler::lexer::lexer_error::LocationError;
use crate::compiler::lexer::location::Src;
use crate::compiler::lexer::token::{Keyword, StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::lexer::SrcToken;
use crate::compiler::parser::binop_expr_parser::parse_binop_expression;
use crate::compiler::parser::parsed_expression::{
    ParsedEnumDefinition, ParsedExpression, ParsedExpressionKind, ParsedFunction, ParsedImport,
    ParsedModule, ParsedStructDefinition, ParsedTypeAlias,
};
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::primary_expr_parser::{parse_block_expression, parse_generic_params};
use crate::compiler::parser::type_parser::{parse_import_id, parse_type};
use crate::compiler::parser::{ModuleIdentifier, ModulePath};
use anyhow::Context;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

pub fn pop_expected(tokens: &mut TokenStack, expected: Token) -> ParseResult<SrcToken> {
    let token = tokens.shift().clone();
    if token.value != expected {
        Err(LocationError::new(
            format!(
                "Expected token '{}', found token '{}'",
                expected, token.value
            ),
            token.location,
        ))?
    } else {
        Ok(token)
    }
}

pub fn pop_matching<F, T, F2>(
    tokens: &mut TokenStack,
    expected: F,
    error: F2,
) -> ParseResult<Src<T>>
where
    F: Fn(&Token) -> Option<T>,
    F2: Fn(Token) -> String,
{
    let token = tokens.shift().clone();
    if let Some(result) = expected(&token.value) {
        Ok(Src {
            value: result,
            location: token.location,
        })
    } else {
        Err(LocationError::new(error(token.value), token.location))?
    }
}

pub fn parse_identifier(tokens: &mut TokenStack) -> ParseResult<Src<String>> {
    pop_matching(
        tokens,
        |t| {
            if let Token::Identifier(name) = t {
                Some(name.clone())
            } else {
                None
            }
        },
        |t| format!("Expected Identifier, found '{}'", t),
    )
}

pub fn parse_module(
    visited_paths: &mut HashSet<PathBuf>,
    module_tree: &mut HashMap<ModuleIdentifier, ParsedModule>,
    module_path: ModulePath,
) -> ParseResult<()> {
    let path = &module_path.file;
    if !path.try_exists()? {
        return Err(anyhow::anyhow!(
            "Module file '{}' does not exist.",
            path.to_str().unwrap()
        ))?;
    }
    if !visited_paths.insert(path.clone()) {
        return Err(anyhow::anyhow!(
            "Cyclic module dependency in file '{}'.",
            path.to_str().unwrap()
        ))?;
    }

    let tokens = lexer::lex(&module_path)?;
    let mut tokens = TokenStack::new(tokens);

    let mut functions = Vec::new();
    let mut struct_definitions = Vec::new();
    let mut submodule_declarations = Vec::new();
    let mut imports = Vec::new();
    let mut type_aliases = Vec::new();
    let mut enum_definitions = Vec::new();

    while tokens.peek().value != Token::EOF {
        let token = tokens.peek().clone();
        match token.value {
            Token::Keyword(Keyword::Struct) => {
                let struct_def = parse_struct_definition(&mut tokens).with_context(|| {
                    format!("Failed to parse struct definition at {}.", token.location)
                })?;
                struct_definitions.push(struct_def);
            }
            Token::Keyword(Keyword::Module) => {
                let submodule = parse_module_declaration(&mut tokens)
                    .with_context(|| format!("Failed to parse submodule at {}.", token.location))?;
                submodule_declarations.push((token.location, submodule));
            }
            Token::Keyword(Keyword::Import) => {
                let module = parse_import(&mut tokens)
                    .with_context(|| format!("Failed to parse import at {}.", token.location))?;
                imports.push(Src {
                    value: module,
                    location: token.location.clone(),
                });
            }
            Token::Keyword(Keyword::Alias) => {
                let alias = parse_alias(&mut tokens).with_context(|| {
                    format!("Failed to parse type alias at {}.", token.location)
                })?;
                type_aliases.push(alias);
            }
            Token::Keyword(Keyword::Enum) => {
                let enum_def = parse_enum_definition(&mut tokens).with_context(|| {
                    format!("Failed to parse enum definition at {}.", token.location)
                })?;
                enum_definitions.push(enum_def);
            }
            _ => {
                let func = parse_function(&mut tokens)
                    .with_context(|| format!("Failed to parse function at {}.", token.location))?;
                functions.push(func);
            }
        }
    }

    let mut submodules = Vec::new();

    for (location, submodule) in submodule_declarations {
        let child_module = module_path.get_submodule_path(&submodule);
        parse_module(visited_paths, module_tree, child_module).with_context(|| {
            format!("Failed to parse submodule '{}' at {}.", submodule, location)
        })?;
        submodules.push(submodule);
    }

    let module = ParsedModule {
        module_path: module_path.id.clone(),
        functions,
        struct_definitions,
        type_aliases,
        imports,
        enums: enum_definitions,
    };

    module_tree.insert(module_path.id, module);
    Ok(())
}

pub fn parse_import(tokens: &mut TokenStack) -> ParseResult<ParsedImport> {
    pop_expected(tokens, Token::Keyword(Keyword::Import))?;
    let token = tokens.shift().clone();
    let current_module = &token.location.file.as_ref().unwrap().id;
    let parsed_import = match &token.value {
        Token::Identifier(module_name) => {
            parse_import_id(tokens, module_name.clone(), false, current_module)?
        }
        Token::Static(StaticToken::DoubleColon) => {
            let first_id = parse_identifier(tokens)?;
            parse_import_id(tokens, first_id.value, true, current_module)?
        }
        _ => Err(LocationError::new(
            format!("Expected module name or '::', found '{}'", token.value),
            token.location,
        ))?,
    };
    pop_expected(tokens, Token::Static(StaticToken::Semicolon))?;
    Ok(parsed_import)
}

pub fn parse_module_declaration(tokens: &mut TokenStack) -> ParseResult<String> {
    pop_expected(tokens, Token::Keyword(Keyword::Module))?;
    let module_name = parse_identifier(tokens)?.value;
    pop_expected(tokens, Token::Static(StaticToken::Semicolon))?;
    Ok(module_name)
}

pub fn parse_enum_definition(tokens: &mut TokenStack) -> ParseResult<Src<ParsedEnumDefinition>> {
    let location = pop_expected(tokens, Token::Keyword(Keyword::Enum))?.location;
    let enum_name = parse_identifier(tokens)?.value;
    pop_expected(tokens, Token::Static(StaticToken::OpenBrace))?;

    let mut variants = Vec::new();
    let mut used_variant_names = HashSet::new();
    while tokens.peek().value != Token::Static(StaticToken::CloseBrace) {
        let location = tokens.peek().location.clone();
        let variant_name = parse_identifier(tokens)?.value;
        if !used_variant_names.insert(variant_name.clone()) {
            Err(anyhow::anyhow!(
                "Duplicate variant name '{}' at {}",
                variant_name,
                location
            ))?;
        }
        if tokens.peek().value == Token::Static(StaticToken::Assign) {
            tokens.shift();
            let value = parse_expression(tokens)
                .with_context(|| format!("Failed to parse variant value at {}.", location))?;
            if let ParsedExpressionKind::Literal(lit) = &value.value {
                variants.push((variant_name, Some(lit.clone())));
            } else {
                Err(anyhow::anyhow!(
                    "Expected literal value for variant at {location}."
                ))?;
            }
        } else {
            variants.push((variant_name, None));
        }

        match tokens.peek().value {
            Token::Static(StaticToken::Comma) => {
                tokens.shift();
            }
            Token::Static(StaticToken::CloseBrace) => {}
            _ => Err(LocationError::new(
                format!("Expected ',' or ')', found '{}'", tokens.peek().value),
                tokens.location().clone(),
            ))?,
        }
    }
    pop_expected(tokens, Token::Static(StaticToken::CloseBrace))?;
    if variants.is_empty() {
        Err(anyhow::anyhow!(
            "Enums must have at least one variant at {location}."
        ))?
    } else {
        Ok(Src::new(
            ParsedEnumDefinition {
                enum_name,
                variants,
            },
            location,
        ))
    }
}

pub fn parse_struct_definition(
    tokens: &mut TokenStack,
) -> ParseResult<Src<ParsedStructDefinition>> {
    let location = pop_expected(tokens, Token::Keyword(Keyword::Struct))?.location;
    let struct_name = parse_identifier(tokens)?.value;
    let generics = parse_generic_params(tokens)?;
    pop_expected(tokens, Token::Static(StaticToken::OpenBrace))?;

    let mut fields = Vec::new();
    let mut used_field_names = HashSet::new();
    while tokens.peek().value != Token::Static(StaticToken::CloseBrace) {
        let location = tokens.peek().location.clone();
        let field_type =
            parse_type(tokens).with_context(|| format!("Failed to parse type at {}.", location))?;
        let field_name = parse_identifier(tokens)?.value;
        pop_expected(tokens, Token::Static(StaticToken::Semicolon))?;
        if !used_field_names.insert(field_name.clone()) {
            Err(LocationError::new(
                format!("Duplicate field name '{}'", field_name),
                location,
            ))?;
        }
        fields.push((field_name, field_type));
    }
    pop_expected(tokens, Token::Static(StaticToken::CloseBrace))?;
    if fields.is_empty() {
        Err(anyhow::anyhow!(
            "Structs must have at least one field at {location}."
        ))?;
    }

    Ok(Src::new(
        ParsedStructDefinition {
            struct_name,
            fields,
            generics,
        },
        location,
    ))
}

pub fn parse_function(tokens: &mut TokenStack) -> ParseResult<Src<ParsedFunction>> {
    let location = tokens.location().clone();
    let return_type = parse_type(tokens)
        .with_context(|| format!("Failed to parse return type at {}.", location))?;
    let function_name = parse_identifier(tokens)?.value;
    let generics = parse_generic_params(tokens)?;
    pop_expected(tokens, Token::Static(StaticToken::OpenParen))?;

    let mut args = Vec::new();
    let mut used_arg_names = HashSet::new();
    while tokens.peek().value != Token::Static(StaticToken::CloseParen) {
        let location = tokens.location().clone();
        let arg_type = parse_type(tokens)
            .with_context(|| format!("Failed to parse argument type at {}.", location))?;
        let arg_name = parse_identifier(tokens)?.value;
        if !used_arg_names.insert(arg_name.clone()) {
            Err(LocationError::new(
                format!("Duplicate argument name '{}'", arg_name),
                location,
            ))?;
        }
        args.push((arg_type, arg_name));
        match tokens.peek().value {
            Token::Static(StaticToken::Comma) => {
                tokens.shift();
            }
            Token::Static(StaticToken::CloseParen) => {}
            _ => Err(LocationError::new(
                format!("Expected ',' or ')', found '{}'", tokens.peek().value),
                tokens.location().clone(),
            ))?,
        }
    }
    pop_expected(tokens, Token::Static(StaticToken::CloseParen))?;

    let body_location = tokens.location().clone();
    let body = parse_block_expression(tokens)
        .with_context(|| format!("Failed to parse function body at {}.", body_location))?;

    Ok(Src::new(
        ParsedFunction {
            function_name,
            return_type,
            generic_params: generics,
            params: args,
            body,
        },
        location,
    ))
}

pub fn parse_expression(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    parse_binop_expression(tokens)
}

fn parse_alias(tokens: &mut TokenStack) -> ParseResult<Src<ParsedTypeAlias>> {
    let location = pop_expected(tokens, Token::Keyword(Keyword::Alias))?.location;
    let alias = parse_identifier(tokens)?.value;
    pop_expected(tokens, Token::Static(StaticToken::Assign))?;
    let aliased_type = parse_type(tokens)
        .with_context(|| format!("Failed to parse aliased type at {}.", location))?;
    pop_expected(tokens, Token::Static(StaticToken::Semicolon))?;
    Ok(Src::new(
        ParsedTypeAlias {
            alias,
            aliased_type,
        },
        location,
    ))
}
