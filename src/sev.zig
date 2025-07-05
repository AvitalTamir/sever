const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const SirsParser = @import("sirs.zig");
const Expression = SirsParser.Expression;
const Statement = SirsParser.Statement;
const Type = SirsParser.Type;
const Function = SirsParser.Function;

/// Ultra-compact SEV (Sever) format parser
/// Optimized for minimum token usage by LLMs
pub const SevParser = struct {
    allocator: Allocator,
    input: []const u8,
    pos: usize,

    const ParseError = error{
        UnexpectedToken,
        UnexpectedEof,
        InvalidSyntax,
        OutOfMemory,
    };

    pub fn init(allocator: Allocator, input: []const u8) SevParser {
        return SevParser{
            .allocator = allocator,
            .input = input,
            .pos = 0,
        };
    }

    pub fn deinit(self: *SevParser) void {
        // Parser cleanup - the Program owns the allocated memory
        _ = self;
    }

    pub fn parse(self: *SevParser) !SirsParser.Program {
        // SEV format: P<entry>|<functions>
        if (!self.consume('P')) return ParseError.InvalidSyntax;
        
        const entry = try self.parseIdentifier();
        if (!self.consume('|')) return ParseError.InvalidSyntax;
        
        var functions = StringHashMap(Function).init(self.allocator);
        errdefer functions.deinit();
        
        // Parse all functions
        while (self.pos < self.input.len and self.peek() == 'D') {
            const func_entry = try self.parseFunctionWithName();
            try functions.put(func_entry.name, func_entry.function);
        }
        
        return SirsParser.Program{
            .entry = entry,
            .entry_allocated = false,
            .functions = functions,
            .types = StringHashMap(SirsParser.Type).init(self.allocator),
            .generic_types = StringHashMap(SirsParser.GenericType).init(self.allocator),
            .interfaces = StringHashMap(SirsParser.Interface).init(self.allocator),
            .trait_impls = ArrayList(SirsParser.TraitImpl).init(self.allocator),
            .constants = StringHashMap(SirsParser.Constant).init(self.allocator),
            .allocator = self.allocator,
        };
    }

    const FunctionEntry = struct {
        name: []const u8,
        function: Function,
    };

    fn parseFunctionWithName(self: *SevParser) !FunctionEntry {
        // Expected: Dmain[]I;La:I=10;Lb:I=20;Lsum:I=(a+b);Lproduct:I=(a*b);R(sum+product)
        if (!self.consume('D')) return ParseError.InvalidSyntax;
        
        // Parse function name and make a copy
        const func_name_slice = try self.parseIdentifier();
        const func_name = try self.allocator.dupe(u8, func_name_slice);
        
        const func = try self.parseFunctionBody();
        
        return FunctionEntry{
            .name = func_name,
            .function = func,
        };
    }

    fn parseFunction(self: *SevParser) !Function {
        // Expected: Dmain[]I;La:I=10;Lb:I=20;Lsum:I=(a+b);Lproduct:I=(a*b);R(sum+product)
        if (!self.consume('D')) return ParseError.InvalidSyntax;
        
        // Skip function name (we already have it)
        _ = try self.parseIdentifier();
        
        return try self.parseFunctionBody();
    }

    fn parseFunctionBody(self: *SevParser) !Function {
        // Parse args 
        if (!self.consume('[')) return ParseError.InvalidSyntax;
        
        var args = ArrayList(SirsParser.Parameter).init(self.allocator);
        errdefer args.deinit();
        
        while (self.peek() != ']') {
            const arg_name_slice = try self.parseIdentifier();
            const arg_name = try self.allocator.dupe(u8, arg_name_slice);
            if (!self.consume(':')) return ParseError.InvalidSyntax;
            const arg_type = try self.parseType();
            
            try args.append(SirsParser.Parameter{
                .name = arg_name,
                .type = arg_type,
            });
            
            // Check for comma (more args) or closing bracket
            if (self.peek() == ',') {
                _ = self.advance(); // consume comma
            } else if (self.peek() != ']') {
                return ParseError.InvalidSyntax;
            }
        }
        
        if (!self.consume(']')) return ParseError.InvalidSyntax;
        
        // Parse return type
        const return_type = try self.parseType();
        
        if (!self.consume(';')) return ParseError.InvalidSyntax;
        
        // Parse statements until we hit the next function (D) or end of input
        var body = ArrayList(Statement).init(self.allocator);
        errdefer body.deinit();
        
        while (self.pos < self.input.len and self.peek() != 'D') {
            const stmt = try self.parseStatement();
            try body.append(stmt);
            
            // Consume semicolon if present
            if (self.pos < self.input.len and self.peek() == ';') {
                _ = self.advance();
            }
        }
        
        return Function{
            .args = args,
            .@"return" = return_type,
            .body = body,
        };
    }

    fn parseStatement(self: *SevParser) ParseError!Statement {
        const ch = self.peek();
        
        switch (ch) {
            'L' => return try self.parseLetStatement(),
            'M' => return try self.parseMutableLetStatement(),
            'R' => return try self.parseReturnStatement(),
            'E' => return try self.parseExpressionStatement(),
            'A' => return try self.parseAssignStatement(),
            'I' => return try self.parseIfStatement(),
            'W' => return try self.parseWhileStatement(),
            'F' => return try self.parseForStatement(),
            'B' => return try self.parseBreakStatement(),
            'N' => return try self.parseContinueStatement(),
            'T' => return try self.parseTryStatement(),
            '0' => {
                // Skip placeholder statements
                _ = self.advance();
                return Statement{ .expression = Expression{ .literal = SirsParser.Literal{ .integer = 0 } } };
            },
            else => return ParseError.InvalidSyntax,
        }
    }

    fn parseLetStatement(self: *SevParser) ParseError!Statement {
        // Expected: La:I=10 or Lsum:I=(a+b)
        if (!self.consume('L')) return ParseError.InvalidSyntax;
        
        const name_slice = try self.parseIdentifier();
        const name = try self.allocator.dupe(u8, name_slice);
        if (!self.consume(':')) return ParseError.InvalidSyntax;
        const var_type = try self.parseType();
        if (!self.consume('=')) return ParseError.InvalidSyntax;
        const value = try self.parseExpression();
        
        return Statement{
            .let = .{
                .name = name,
                .type = var_type,
                .mutable = false,
                .value = value,
            },
        };
    }

    fn parseMutableLetStatement(self: *SevParser) ParseError!Statement {
        // Expected: Ma:I=10 or Msum:I=(a+b) - same as let but mutable
        if (!self.consume('M')) return ParseError.InvalidSyntax;
        
        const name_slice = try self.parseIdentifier();
        const name = try self.allocator.dupe(u8, name_slice);
        if (!self.consume(':')) return ParseError.InvalidSyntax;
        const var_type = try self.parseType();
        if (!self.consume('=')) return ParseError.InvalidSyntax;
        const value = try self.parseExpression();
        
        return Statement{
            .let = .{
                .name = name,
                .type = var_type,
                .mutable = true,
                .value = value,
            },
        };
    }

    fn parseExpressionStatement(self: *SevParser) ParseError!Statement {
        // Expected: E followed by expression (function call)
        if (!self.consume('E')) return ParseError.InvalidSyntax;
        
        const expr = try self.parseExpression();
        return Statement{
            .expression = expr,
        };
    }

    fn parseAssignStatement(self: *SevParser) ParseError!Statement {
        // Expected: Avariable=value
        if (!self.consume('A')) return ParseError.InvalidSyntax;
        
        const var_name_slice = try self.parseIdentifier();
        const var_name = try self.allocator.dupe(u8, var_name_slice);
        if (!self.consume('=')) return ParseError.InvalidSyntax;
        const value = try self.parseExpression();
        
        return Statement{
            .assign = .{
                .target = SirsParser.LValue{ .variable = var_name },
                .value = value,
            },
        };
    }

    fn parseReturnStatement(self: *SevParser) ParseError!Statement {
        // Expected: R(sum+product)
        if (!self.consume('R')) return ParseError.InvalidSyntax;
        
        const value = try self.parseExpression();
        return Statement{
            .@"return" = value,
        };
    }

    fn parseIfStatement(self: *SevParser) ParseError!Statement {
        // Expected: I<condition>?<then_statements>:<else_statements>|
        if (!self.consume('I')) return ParseError.InvalidSyntax;
        
        const condition = try self.parseExpression();
        if (!self.consume('?')) return ParseError.InvalidSyntax;
        
        // Parse then block statements until we hit ':' or '|'
        var then_statements = ArrayList(Statement).init(self.allocator);
        errdefer then_statements.deinit();
        
        while (self.pos < self.input.len and self.peek() != ':' and self.peek() != '|') {
            const stmt = try self.parseStatement();
            try then_statements.append(stmt);
        }
        
        // Parse optional else block
        var else_statements: ?ArrayList(Statement) = null;
        if (self.pos < self.input.len and self.peek() == ':') {
            _ = self.advance(); // consume ':'
            
            var else_stmts = ArrayList(Statement).init(self.allocator);
            errdefer else_stmts.deinit();
            
            while (self.pos < self.input.len and self.peek() != '|') {
                const stmt = try self.parseStatement();
                try else_stmts.append(stmt);
            }
            
            else_statements = else_stmts;
        }
        
        // Consume end marker
        if (!self.consume('|')) return ParseError.InvalidSyntax;
        
        return Statement{
            .@"if" = .{
                .condition = condition,
                .then = then_statements,
                .@"else" = else_statements,
            },
        };
    }

    fn parseWhileStatement(self: *SevParser) ParseError!Statement {
        // Expected: W<condition>?<body_statements>|
        if (!self.consume('W')) return ParseError.InvalidSyntax;
        
        const condition = try self.parseExpression();
        if (!self.consume('?')) return ParseError.InvalidSyntax;
        
        // Parse body statements until we hit '|'
        var body_statements = ArrayList(Statement).init(self.allocator);
        errdefer body_statements.deinit();
        
        while (self.pos < self.input.len and self.peek() != '|') {
            const stmt = try self.parseStatement();
            try body_statements.append(stmt);
            
            // Consume semicolon if present
            if (self.pos < self.input.len and self.peek() == ';') {
                _ = self.advance();
            }
        }
        
        // Consume end marker
        if (!self.consume('|')) return ParseError.InvalidSyntax;
        
        return Statement{
            .@"while" = .{
                .condition = condition,
                .body = body_statements,
            },
        };
    }

    fn parseForStatement(self: *SevParser) ParseError!Statement {
        // Expected: F<variable>@<iterable>?<body_statements>|
        if (!self.consume('F')) return ParseError.InvalidSyntax;
        
        const var_name_slice = try self.parseIdentifier();
        const var_name = try self.allocator.dupe(u8, var_name_slice);
        
        if (!self.consume('@')) return ParseError.InvalidSyntax;
        
        const iterable = try self.parseExpression();
        if (!self.consume('?')) return ParseError.InvalidSyntax;
        
        // Parse body statements until we hit '|'
        var body_statements = ArrayList(Statement).init(self.allocator);
        errdefer body_statements.deinit();
        
        while (self.pos < self.input.len and self.peek() != '|') {
            const stmt = try self.parseStatement();
            try body_statements.append(stmt);
            
            // Consume semicolon if present
            if (self.pos < self.input.len and self.peek() == ';') {
                _ = self.advance();
            }
        }
        
        // Consume end marker
        if (!self.consume('|')) return ParseError.InvalidSyntax;
        
        return Statement{
            .@"for" = .{
                .variable = var_name,
                .iterable = iterable,
                .body = body_statements,
            },
        };
    }

    fn parseBreakStatement(self: *SevParser) ParseError!Statement {
        // Expected: B
        if (!self.consume('B')) return ParseError.InvalidSyntax;
        
        return Statement{
            .@"break" = {},
        };
    }

    fn parseContinueStatement(self: *SevParser) ParseError!Statement {
        // Expected: N
        if (!self.consume('N')) return ParseError.InvalidSyntax;
        
        return Statement{
            .@"continue" = {},
        };
    }

    fn parseTryStatement(self: *SevParser) ParseError!Statement {
        // Format: T<try_body>C<catch_variable>:<catch_body>F<finally_body>
        if (!self.consume('T')) return ParseError.InvalidSyntax;
        
        var try_body = ArrayList(Statement).init(self.allocator);
        
        // Parse try block until we hit 'C' for catch
        while (self.pos < self.input.len and self.peek() != 'C' and self.peek() != 'F') {
            const stmt = try self.parseStatement();
            try try_body.append(stmt);
            
            if (self.peek() == ';') {
                _ = self.advance();
            }
        }
        
        var catch_clauses = ArrayList(SirsParser.CatchClause).init(self.allocator);
        
        if (self.consume('C')) {
            const variable_name = try self.parseIdentifier();
            if (!self.consume(':')) return ParseError.InvalidSyntax;
            
            var catch_stmts = ArrayList(Statement).init(self.allocator);
            while (self.pos < self.input.len and self.peek() != 'F') {
                const stmt = try self.parseStatement();
                try catch_stmts.append(stmt);
                
                if (self.peek() == ';') {
                    _ = self.advance();
                }
            }
            
            try catch_clauses.append(SirsParser.CatchClause{
                .exception_type = null, // catch all for simplicity
                .variable_name = variable_name,
                .body = catch_stmts,
            });
        }
        
        var finally_body: ?ArrayList(Statement) = null;
        if (self.consume('F')) {
            var finally_stmts = ArrayList(Statement).init(self.allocator);
            while (self.pos < self.input.len and self.peek() != 'D') {
                const stmt = try self.parseStatement();
                try finally_stmts.append(stmt);
                
                if (self.peek() == ';') {
                    _ = self.advance();
                }
            }
            finally_body = finally_stmts;
        }
        
        return Statement{
            .@"try" = .{
                .body = try_body,
                .catch_clauses = catch_clauses,
                .finally_body = finally_body,
            },
        };
    }

    fn parseMatchStatement(self: *SevParser) ParseError!Statement {
        // Format: M<expr>(<pattern1>:<body1>;<pattern2>:<body2>;...)
        if (!self.consume('M')) return ParseError.InvalidSyntax;
        
        const expr = try self.parseExpression();
        if (!self.consume('(')) return ParseError.InvalidSyntax;
        
        var arms = ArrayList(SirsParser.MatchArm).init(self.allocator);
        
        while (self.peek() != ')') {
            const pattern = try self.parseExpression(); // Simplified pattern parsing
            if (!self.consume(':')) return ParseError.InvalidSyntax;
            
            var body = ArrayList(Statement).init(self.allocator);
            while (self.pos < self.input.len and self.peek() != ';' and self.peek() != ')') {
                const stmt = try self.parseStatement();
                try body.append(stmt);
            }
            
            try arms.append(SirsParser.MatchArm{
                .pattern = pattern,
                .guard = null,
                .body = body.toOwnedSlice() catch unreachable,
            });
            
            if (self.peek() == ';') {
                _ = self.advance();
            }
        }
        
        if (!self.consume(')')) return ParseError.InvalidSyntax;
        
        return Statement{
            .match_stmt = .{
                .expr = expr,
                .arms = arms.toOwnedSlice() catch unreachable,
            },
        };
    }

    fn parseExpression(self: *SevParser) ParseError!Expression {
        const ch = self.peek();
        
        switch (ch) {
            '(' => return try self.parseParenthesizedExpression(),
            '0'...'9' => return try self.parseNumber(),
            'a'...'z', 'A'...'Z', '_' => return try self.parseVariableOrCall(),
            '"' => return try self.parseString(),
            '[' => return try self.parseArrayLiteral(),
            else => return ParseError.UnexpectedToken,
        }
    }

    fn parseParenthesizedExpression(self: *SevParser) ParseError!Expression {
        // Expected: (expr op expr) or nested like ((a+b)/c)
        if (!self.consume('(')) return ParseError.InvalidSyntax;
        
        // Check for nested parentheses like ((a+b)/c)
        if (self.peek() == '(') {
            // Parse inner parentheses first
            _ = self.advance(); // consume second '('
            const inner_left = try self.parseSimpleExpression();
            const inner_op = self.advance();
            const inner_right = try self.parseSimpleExpression();
            if (!self.consume(')')) return ParseError.InvalidSyntax;
            
            // Parse outer operator
            const outer_op = self.advance();
            const outer_right = try self.parseSimpleExpression();
            if (!self.consume(')')) return ParseError.InvalidSyntax;
            
            // Create nested structure: ((inner_left inner_op inner_right) outer_op outer_right)
            var inner_args = ArrayList(Expression).init(self.allocator);
            errdefer inner_args.deinit();
            try inner_args.append(inner_left);
            try inner_args.append(inner_right);
            
            const inner_expr = Expression{
                .op = .{
                    .kind = switch (inner_op) {
                        '+' => SirsParser.OpKind.add,
                        '-' => SirsParser.OpKind.sub,
                        '*' => SirsParser.OpKind.mul,
                        '/' => SirsParser.OpKind.div,
                        else => SirsParser.OpKind.add,
                    },
                    .args = inner_args,
                },
            };
            
            var outer_args = ArrayList(Expression).init(self.allocator);
            errdefer outer_args.deinit();
            try outer_args.append(inner_expr);
            try outer_args.append(outer_right);
            
            return Expression{
                .op = .{
                    .kind = switch (outer_op) {
                        '+' => SirsParser.OpKind.add,
                        '-' => SirsParser.OpKind.sub,
                        '*' => SirsParser.OpKind.mul,
                        '/' => SirsParser.OpKind.div,
                        else => SirsParser.OpKind.add,
                    },
                    .args = outer_args,
                },
            };
        } else {
            // Parse chained operations: (a op b op c op d ...)
            const left = try self.parseSimpleExpression();
            
            // Check if there's at least one operator
            if (self.pos < self.input.len and self.isOperator(self.peek())) {
                // Parse the first operation
                const current_op = self.advance();
                const right = try self.parseSimpleExpression();
                
                // Create the first binary operation
                const current_op_kind = switch (current_op) {
                    '+' => SirsParser.OpKind.add,
                    '-' => SirsParser.OpKind.sub,
                    '*' => SirsParser.OpKind.mul,
                    '/' => SirsParser.OpKind.div,
                    '>' => SirsParser.OpKind.gt,
                    '<' => SirsParser.OpKind.lt,
                    '=' => SirsParser.OpKind.eq,
                    else => return ParseError.InvalidSyntax,
                };
                
                var current_args = ArrayList(Expression).init(self.allocator);
                errdefer current_args.deinit();
                try current_args.append(left);
                try current_args.append(right);
                
                var current_expr = Expression{
                    .op = .{
                        .kind = current_op_kind,
                        .args = current_args,
                    },
                };
                
                // Chain additional operations left-associatively: ((a+b)+c)+d
                while (self.pos < self.input.len and self.isOperator(self.peek()) and self.peek() != ')') {
                    const next_op = self.advance();
                    const next_right = try self.parseSimpleExpression();
                    
                    const next_op_kind = switch (next_op) {
                        '+' => SirsParser.OpKind.add,
                        '-' => SirsParser.OpKind.sub,
                        '*' => SirsParser.OpKind.mul,
                        '/' => SirsParser.OpKind.div,
                        '>' => SirsParser.OpKind.gt,
                        '<' => SirsParser.OpKind.lt,
                        '=' => SirsParser.OpKind.eq,
                        else => return ParseError.InvalidSyntax,
                    };
                    
                    var next_args = ArrayList(Expression).init(self.allocator);
                    errdefer next_args.deinit();
                    try next_args.append(current_expr);
                    try next_args.append(next_right);
                    
                    current_expr = Expression{
                        .op = .{
                            .kind = next_op_kind,
                            .args = next_args,
                        },
                    };
                }
                
                if (!self.consume(')')) return ParseError.InvalidSyntax;
                return current_expr;
            } else {
                if (!self.consume(')')) return ParseError.InvalidSyntax;
                return left;
            }
        }
    }

    fn isOperator(self: *SevParser, ch: u8) bool {
        _ = self;
        return ch == '+' or ch == '-' or ch == '*' or ch == '/' or ch == '>' or ch == '<' or ch == '=';
    }

    fn parseNumber(self: *SevParser) ParseError!Expression {
        const start = self.pos;
        
        // Parse digits
        while (self.pos < self.input.len and std.ascii.isDigit(self.input[self.pos])) {
            self.pos += 1;
        }
        
        // Check for decimal point
        if (self.pos < self.input.len and self.input[self.pos] == '.') {
            self.pos += 1;
            while (self.pos < self.input.len and std.ascii.isDigit(self.input[self.pos])) {
                self.pos += 1;
            }
        }
        
        const num_str = self.input[start..self.pos];
        
        // Try to parse as float first, then integer
        if (std.mem.indexOf(u8, num_str, ".")) |_| {
            const value = std.fmt.parseFloat(f64, num_str) catch return ParseError.InvalidSyntax;
            return Expression{
                .literal = SirsParser.Literal{ .float = value },
            };
        } else {
            const value = std.fmt.parseInt(i32, num_str, 10) catch return ParseError.InvalidSyntax;
            return Expression{
                .literal = SirsParser.Literal{ .integer = value },
            };
        }
    }

    fn parseVariable(self: *SevParser) ParseError!Expression {
        const name_slice = try self.parseIdentifier();
        const name = try self.allocator.dupe(u8, name_slice);
        return Expression{ .variable = name };
    }

    fn parseVariableOrCall(self: *SevParser) ParseError!Expression {
        const name_slice = try self.parseIdentifier();
        
        // Remove 'C' prefix from function calls if present
        const clean_name_slice = if (name_slice.len > 0 and name_slice[0] == 'C') name_slice[1..] else name_slice;
        
        // Check if this is a function call (followed by parentheses)
        if (self.peek() == '(') {
            const name = try self.allocator.dupe(u8, clean_name_slice);
            return try self.parseFunctionCall(name);
        } else {
            const name = try self.allocator.dupe(u8, clean_name_slice);
            return Expression{ .variable = name };
        }
    }

    fn parseFunctionCall(self: *SevParser, function_name: []const u8) ParseError!Expression {
        // Expected: function_name(arg1,arg2,...)
        if (!self.consume('(')) return ParseError.InvalidSyntax;
        
        var args = ArrayList(Expression).init(self.allocator);
        errdefer args.deinit();
        
        // Parse arguments
        while (self.peek() != ')') {
            const arg = try self.parseSimpleExpression();
            try args.append(arg);
            
            // Check for comma (more args) or closing paren
            if (self.peek() == ',') {
                _ = self.advance(); // consume comma
            } else if (self.peek() != ')') {
                return ParseError.InvalidSyntax;
            }
        }
        
        if (!self.consume(')')) return ParseError.InvalidSyntax;
        
        return Expression{
            .call = .{
                .function = function_name,
                .args = args,
            },
        };
    }

    fn parseSimpleExpression(self: *SevParser) ParseError!Expression {
        const ch = self.peek();
        
        switch (ch) {
            '0'...'9' => return try self.parseNumber(),
            '"' => return try self.parseString(),
            'a'...'z', 'A'...'Z', '_' => return try self.parseVariableOrCall(),
            else => return ParseError.UnexpectedToken,
        }
    }

    fn parseString(self: *SevParser) ParseError!Expression {
        // Expected: "string content"
        if (!self.consume('"')) return ParseError.InvalidSyntax;
        
        const start = self.pos;
        while (self.pos < self.input.len and self.input[self.pos] != '"') {
            self.pos += 1;
        }
        
        if (self.pos >= self.input.len) return ParseError.UnexpectedEof;
        
        const content_slice = self.input[start..self.pos];
        const content = try self.allocator.dupe(u8, content_slice);
        if (!self.consume('"')) return ParseError.InvalidSyntax;
        
        return Expression{
            .literal = SirsParser.Literal{ .string = content },
        };
    }

    fn parseArrayLiteral(self: *SevParser) ParseError!Expression {
        // Expected: [<elements>]
        if (!self.consume('[')) return ParseError.InvalidSyntax;
        
        var elements = ArrayList(Expression).init(self.allocator);
        errdefer elements.deinit();
        
        // Parse elements
        while (self.peek() != ']') {
            const elem = try self.parseSimpleExpression();
            try elements.append(elem);
            
            // Check for comma (more elements) or closing bracket
            if (self.peek() == ',') {
                _ = self.advance(); // consume comma
            } else if (self.peek() != ']') {
                return ParseError.InvalidSyntax;
            }
        }
        
        if (!self.consume(']')) return ParseError.InvalidSyntax;
        
        return Expression{
            .array = elements,
        };
    }

    fn parseType(self: *SevParser) !Type {
        const ch = self.advance();
        return switch (ch) {
            'I' => Type.i32,
            'F' => Type.f64,
            'B' => Type.bool,
            'S' => Type.str,
            else => ParseError.InvalidSyntax,
        };
    }

    fn parseIdentifier(self: *SevParser) ![]const u8 {
        const start = self.pos;
        
        while (self.pos < self.input.len and 
               (std.ascii.isAlphanumeric(self.input[self.pos]) or self.input[self.pos] == '_')) {
            self.pos += 1;
        }
        
        if (start == self.pos) return ParseError.InvalidSyntax;
        
        return self.input[start..self.pos];
    }

    fn peek(self: *SevParser) u8 {
        if (self.pos >= self.input.len) return 0;
        return self.input[self.pos];
    }

    fn peekNext(self: *SevParser) u8 {
        if (self.pos + 1 >= self.input.len) return 0;
        return self.input[self.pos + 1];
    }

    fn advance(self: *SevParser) u8 {
        if (self.pos >= self.input.len) return 0;
        const ch = self.input[self.pos];
        self.pos += 1;
        return ch;
    }

    fn consume(self: *SevParser, expected: u8) bool {
        if (self.peek() == expected) {
            _ = self.advance();
            return true;
        }
        return false;
    }
};