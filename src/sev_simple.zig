const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const SirsParser = @import("sirs.zig");
const Expression = SirsParser.Expression;
const Statement = SirsParser.Statement;
const Type = SirsParser.Type;
const Function = SirsParser.Function;
const Program = SirsParser.Program;
const Parameter = SirsParser.Parameter;
const Literal = SirsParser.Literal;
const OpKind = SirsParser.OpKind;

/// Simple SEV parser that works with our generated format
pub const SevSimpleParser = struct {
    allocator: Allocator,
    input: []const u8,
    pos: usize,

    const ParseError = error{
        UnexpectedToken,
        UnexpectedEof,
        InvalidSyntax,
        OutOfMemory,
    };

    pub fn init(allocator: Allocator, input: []const u8) SevSimpleParser {
        return SevSimpleParser{
            .allocator = allocator,
            .input = input,
            .pos = 0,
        };
    }

    pub fn parse(self: *SevSimpleParser) !Program {
        // Expected format: Pmain|Dfunction1[...]...Dfunction2[...]...
        
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
        
        return Program{
            .entry = entry,
            .entry_allocated = false,
            .functions = functions,
            .types = StringHashMap(Type).init(self.allocator),
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

    fn parseFunctionWithName(self: *SevSimpleParser) !FunctionEntry {
        // Expected: Dmain[]I;La:I=10;Lb:I=20;Lsum:I=(a+b);Lproduct:I=(a*b);R(sum+product)
        if (!self.consume('D')) return ParseError.InvalidSyntax;
        
        // Parse function name and make a copy
        const func_name_slice = try self.parseIdentifier();
        const func_name = try self.allocator.dupe(u8, func_name_slice);
        errdefer self.allocator.free(func_name);
        
        const func = try self.parseFunctionBody();
        
        return FunctionEntry{
            .name = func_name,
            .function = func,
        };
    }

    fn parseFunction(self: *SevSimpleParser) !Function {
        // Expected: Dmain[]I;La:I=10;Lb:I=20;Lsum:I=(a+b);Lproduct:I=(a*b);R(sum+product)
        if (!self.consume('D')) return ParseError.InvalidSyntax;
        
        // Skip function name (we already have it)
        _ = try self.parseIdentifier();
        
        return try self.parseFunctionBody();
    }

    fn parseFunctionBody(self: *SevSimpleParser) !Function {
        // Parse args 
        if (!self.consume('[')) return ParseError.InvalidSyntax;
        
        var args = ArrayList(Parameter).init(self.allocator);
        errdefer args.deinit();
        
        while (self.peek() != ']') {
            const arg_name_slice = try self.parseIdentifier();
            const arg_name = try self.allocator.dupe(u8, arg_name_slice);
            errdefer self.allocator.free(arg_name);
            if (!self.consume(':')) return ParseError.InvalidSyntax;
            const arg_type = try self.parseType();
            
            try args.append(Parameter{
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

    fn parseStatement(self: *SevSimpleParser) !Statement {
        const ch = self.peek();
        
        switch (ch) {
            'L' => return try self.parseLetStatement(),
            'R' => return try self.parseReturnStatement(),
            'E' => return try self.parseExpressionStatement(),
            'A' => return try self.parseAssignStatement(),
            'I' => return try self.parseIfStatement(),
            '0' => {
                // Skip placeholder statements
                _ = self.advance();
                return Statement{ .expression = Expression{ .literal = Literal{ .integer = 0 } } };
            },
            else => return ParseError.InvalidSyntax,
        }
    }

    fn parseLetStatement(self: *SevSimpleParser) !Statement {
        // Expected: La:I=10 or Lsum:I=(a+b)
        if (!self.consume('L')) return ParseError.InvalidSyntax;
        
        const name_slice = try self.parseIdentifier();
        const name = try self.allocator.dupe(u8, name_slice);
        errdefer self.allocator.free(name);
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

    fn parseReturnStatement(self: *SevSimpleParser) !Statement {
        // Expected: R(sum+product)
        if (!self.consume('R')) return ParseError.InvalidSyntax;
        
        const value = try self.parseExpression();
        return Statement{
            .@"return" = value,
        };
    }

    fn parseExpressionStatement(self: *SevSimpleParser) !Statement {
        // Expected: E followed by expression (function call)
        if (!self.consume('E')) return ParseError.InvalidSyntax;
        
        const expr = try self.parseExpression();
        return Statement{
            .expression = expr,
        };
    }

    fn parseExpression(self: *SevSimpleParser) !Expression {
        const ch = self.peek();
        
        switch (ch) {
            '(' => return try self.parseParenthesizedExpression(),
            '0'...'9' => return try self.parseNumber(),
            'a'...'z', 'A'...'Z', '_' => return try self.parseVariableOrCall(),
            '"' => return try self.parseString(),
            else => return ParseError.UnexpectedToken,
        }
    }

    fn parseParenthesizedExpression(self: *SevSimpleParser) !Expression {
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
                        '+' => OpKind.add,
                        '-' => OpKind.sub,
                        '*' => OpKind.mul,
                        '/' => OpKind.div,
                        else => OpKind.add,
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
                        '+' => OpKind.add,
                        '-' => OpKind.sub,
                        '*' => OpKind.mul,
                        '/' => OpKind.div,
                        else => OpKind.add,
                    },
                    .args = outer_args,
                },
            };
        } else {
            // Simple case: (a op b)
            const left = try self.parseSimpleExpression();
            
            if (self.pos < self.input.len and self.isOperator(self.peek())) {
                const op_char = self.advance();
                const right = try self.parseSimpleExpression();
                
                if (!self.consume(')')) return ParseError.InvalidSyntax;
                
                const op_kind = switch (op_char) {
                    '+' => OpKind.add,
                    '-' => OpKind.sub,
                    '*' => OpKind.mul,
                    '/' => OpKind.div,
                    '>' => OpKind.gt,
                    '<' => OpKind.lt,
                    else => return ParseError.InvalidSyntax,
                };
                
                var args = ArrayList(Expression).init(self.allocator);
                errdefer args.deinit();
                try args.append(left);
                try args.append(right);
                
                return Expression{
                    .op = .{
                        .kind = op_kind,
                        .args = args,
                    },
                };
            } else {
                if (!self.consume(')')) return ParseError.InvalidSyntax;
                return left;
            }
        }
    }

    fn isOperator(self: *SevSimpleParser, ch: u8) bool {
        _ = self;
        return ch == '+' or ch == '-' or ch == '*' or ch == '/' or ch == '>' or ch == '<' or ch == '=';
    }

    fn parseNumber(self: *SevSimpleParser) !Expression {
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
                .literal = Literal{ .float = value },
            };
        } else {
            const value = std.fmt.parseInt(i32, num_str, 10) catch return ParseError.InvalidSyntax;
            return Expression{
                .literal = Literal{ .integer = value },
            };
        }
    }

    fn parseVariable(self: *SevSimpleParser) !Expression {
        const name_slice = try self.parseIdentifier();
        const name = try self.allocator.dupe(u8, name_slice);
        return Expression{ .variable = name };
    }

    fn parseVariableOrCall(self: *SevSimpleParser) !Expression {
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

    fn parseFunctionCall(self: *SevSimpleParser, function_name: []const u8) !Expression {
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

    fn parseSimpleExpression(self: *SevSimpleParser) !Expression {
        const ch = self.peek();
        
        switch (ch) {
            '0'...'9' => return try self.parseNumber(),
            '"' => return try self.parseString(),
            'a'...'z', 'A'...'Z', '_' => return try self.parseVariable(),
            else => return ParseError.UnexpectedToken,
        }
    }

    fn parseString(self: *SevSimpleParser) !Expression {
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
            .literal = Literal{ .string = content },
        };
    }

    fn parseType(self: *SevSimpleParser) !Type {
        const ch = self.advance();
        return switch (ch) {
            'I' => Type.i32,
            'F' => Type.f64,
            'B' => Type.bool,
            'S' => Type.str,
            else => ParseError.InvalidSyntax,
        };
    }

    fn parseIdentifier(self: *SevSimpleParser) ![]const u8 {
        const start = self.pos;
        
        while (self.pos < self.input.len and 
               (std.ascii.isAlphanumeric(self.input[self.pos]) or self.input[self.pos] == '_')) {
            self.pos += 1;
        }
        
        if (start == self.pos) return ParseError.InvalidSyntax;
        
        return self.input[start..self.pos];
    }

    fn peek(self: *SevSimpleParser) u8 {
        if (self.pos >= self.input.len) return 0;
        return self.input[self.pos];
    }

    fn advance(self: *SevSimpleParser) u8 {
        if (self.pos >= self.input.len) return 0;
        const ch = self.input[self.pos];
        self.pos += 1;
        return ch;
    }

    fn consume(self: *SevSimpleParser, expected: u8) bool {
        if (self.peek() == expected) {
            _ = self.advance();
            return true;
        }
        return false;
    }

    fn parseAssignStatement(self: *SevSimpleParser) !Statement {
        // Expected: Avariable=value
        if (!self.consume('A')) return ParseError.InvalidSyntax;
        
        const var_name_slice = try self.parseIdentifier();
        const var_name = try self.allocator.dupe(u8, var_name_slice);
        errdefer self.allocator.free(var_name);
        if (!self.consume('=')) return ParseError.InvalidSyntax;
        const value = try self.parseExpression();
        
        return Statement{
            .assign = .{
                .target = SirsParser.LValue{ .variable = var_name },
                .value = value,
            },
        };
    }

    fn parseIfStatement(self: *SevSimpleParser) !Statement {
        // Expected: Icondition?then:else (simplified)
        if (!self.consume('I')) return ParseError.InvalidSyntax;
        
        const condition = try self.parseExpression();
        if (!self.consume('?')) return ParseError.InvalidSyntax;
        
        // For now, just skip the then/else parts and return a minimal if statement
        while (self.pos < self.input.len and self.peek() != ';' and self.peek() != 'L' and self.peek() != 'R' and self.peek() != 'E') {
            _ = self.advance();
        }
        
        var then_statements = ArrayList(Statement).init(self.allocator);
        errdefer then_statements.deinit();
        
        return Statement{
            .@"if" = .{
                .condition = condition,
                .then = then_statements,
                .@"else" = null,
            },
        };
    }
};