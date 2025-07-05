const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const SirsParser = @import("sirs.zig");
const Expression = SirsParser.Expression;
const Statement = SirsParser.Statement;
const Type = SirsParser.Type;
const Function = SirsParser.Function;

/// Ultra-compact SEV (Sever) format generator
/// Generates minimal token representation from AST
pub const SevGenerator = struct {
    allocator: Allocator,
    output: ArrayList(u8),

    pub fn init(allocator: Allocator) SevGenerator {
        return SevGenerator{
            .allocator = allocator,
            .output = ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *SevGenerator) void {
        self.output.deinit();
    }

    pub fn generate(self: *SevGenerator, program: SirsParser.Program) ![]const u8 {
        // Format: P<entry>|<functions>
        try self.output.append('P');
        try self.output.appendSlice(program.entry);
        try self.output.append('|');
        
        var iter = program.functions.iterator();
        while (iter.next()) |entry| {
            try self.generateFunction(entry.key_ptr.*, entry.value_ptr.*);
        }
        
        return self.output.toOwnedSlice();
    }

    fn generateFunction(self: *SevGenerator, func_name: []const u8, func: Function) !void {
        // Format: D<name>[<args>]<return>;<body>
        try self.output.append('D');
        try self.output.appendSlice(func_name);
        try self.output.append('[');
        
        for (func.args.items, 0..) |arg, i| {
            if (i > 0) try self.output.append(',');
            try self.output.appendSlice(arg.name);
            try self.output.append(':');
            try self.generateType(arg.type);
        }
        
        try self.output.append(']');
        try self.generateType(func.@"return");
        try self.output.append(';');
        
        for (func.body.items, 0..) |stmt, i| {
            if (i > 0) try self.output.append(';');
            try self.generateStatement(stmt);
        }
    }

    fn generateStatement(self: *SevGenerator, stmt: Statement) !void {
        switch (stmt) {
            .let => {
                // Format: L<name>:<type>=<value>
                try self.output.append('L');
                try self.output.appendSlice(stmt.let.name);
                try self.output.append(':');
                if (stmt.let.type) |let_type| {
                    try self.generateType(let_type);
                } else {
                    try self.output.append('I'); // Default to i32 if no type specified
                }
                try self.output.append('=');
                try self.generateExpression(stmt.let.value);
            },
            .@"return" => {
                // Format: R<expr>
                try self.output.append('R');
                try self.generateExpression(stmt.@"return");
            },
            .expression => {
                // Format: E<expr>
                try self.output.append('E');
                try self.generateExpression(stmt.expression);
            },
            .@"if" => {
                // Format: I<condition>?<then>:<else>
                try self.output.append('I');
                try self.generateExpression(stmt.@"if".condition);
                try self.output.append('?');
                // TODO: Handle statement blocks properly
                try self.output.append('0'); // Placeholder
                
                if (stmt.@"if".@"else") |_| {
                    try self.output.append(':');
                    try self.output.append('0'); // Placeholder
                }
            },
            else => {
                // For now, just output a placeholder for unsupported statements
                try self.output.append('0');
            },
        }
    }

    fn generateExpression(self: *SevGenerator, expr: Expression) !void {
        switch (expr) {
            .literal => |lit| {
                switch (lit) {
                    .integer => |val| {
                        const str = try std.fmt.allocPrint(self.allocator, "{}", .{val});
                        defer self.allocator.free(str);
                        try self.output.appendSlice(str);
                    },
                    .float => |val| {
                        const str = try std.fmt.allocPrint(self.allocator, "{d}", .{val});
                        defer self.allocator.free(str);
                        try self.output.appendSlice(str);
                    },
                    .boolean => |val| {
                        if (val) {
                            try self.output.append('1');
                        } else {
                            try self.output.append('0');
                        }
                    },
                    .string => |val| {
                        // String with quotes for proper parsing
                        try self.output.append('"');
                        try self.output.appendSlice(val);
                        try self.output.append('"');
                    },
                    .null => {
                        try self.output.append('0');
                    },
                }
            },
            .variable => |name| {
                try self.output.appendSlice(name);
            },
            .op => |operation| {
                // Handle operations
                try self.output.append('(');
                
                if (operation.args.items.len >= 1) {
                    try self.generateExpression(operation.args.items[0]);
                }
                
                // Convert OpKind to operator symbol
                const op_str = switch (operation.kind) {
                    .add => "+",
                    .sub => "-", 
                    .mul => "*",
                    .div => "/",
                    .eq => "==",
                    .lt => "<",
                    .gt => ">",
                    else => "+", // Default
                };
                try self.output.appendSlice(op_str);
                
                if (operation.args.items.len >= 2) {
                    try self.generateExpression(operation.args.items[1]);
                }
                
                try self.output.append(')');
            },
            .call => |call_expr| {
                // Format: C<name>(<args>)
                try self.output.append('C');
                try self.output.appendSlice(call_expr.function);
                try self.output.append('(');
                
                for (call_expr.args.items, 0..) |arg, i| {
                    if (i > 0) try self.output.append(',');
                    try self.generateExpression(arg);
                }
                
                try self.output.append(')');
            },
            .array => |arr| {
                // Format: [<elements>]
                try self.output.append('[');
                
                for (arr.items, 0..) |elem, i| {
                    if (i > 0) try self.output.append(',');
                    try self.generateExpression(elem);
                }
                
                try self.output.append(']');
            },
            .index => |idx| {
                // Format: <array>[<index>]
                try self.generateExpression(idx.array.*);
                try self.output.append('[');
                try self.generateExpression(idx.index.*);
                try self.output.append(']');
            },
            else => {
                // Placeholder for unsupported expressions
                try self.output.append('0');
            },
        }
    }

    fn generateType(self: *SevGenerator, type_info: Type) !void {
        switch (type_info) {
            .i32 => try self.output.append('I'),
            .f64 => try self.output.append('F'),
            .bool => try self.output.append('B'),
            .str => try self.output.append('S'),
            .array => |elem_type| {
                try self.output.append('[');
                try self.generateType(elem_type.element.*);
                try self.output.append(']');
            },
            else => {
                // Default for unsupported types
                try self.output.append('I');
            },
        }
    }

    fn needsParentheses(self: *SevGenerator, expr: Expression) bool {
        _ = self;
        return switch (expr) {
            .binary => true,
            .unary => true,
            else => false,
        };
    }
};

/// Convenience function to generate SEV from program
pub fn generateSev(allocator: Allocator, program: SirsParser.Program) ![]const u8 {
    var generator = SevGenerator.init(allocator);
    defer generator.deinit();
    return try generator.generate(program);
}