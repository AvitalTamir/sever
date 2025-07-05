const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

const SirsParser = @import("sirs.zig");
const Type = SirsParser.Type;
const Expression = SirsParser.Expression;
const Statement = SirsParser.Statement;
const Program = SirsParser.Program;
const Function = SirsParser.Function;

pub const TypeCheckError = error{
    UndefinedVariable,
    UndefinedFunction,
    TypeMismatch,
    InvalidOperation,
    ReturnTypeMismatch,
    ArgumentCountMismatch,
    ArgumentTypeMismatch,
    InvalidSample,
    InvalidObservation,
    MissingReturn,
    InvalidContext,
    OutOfMemory,
};

pub const TypeChecker = struct {
    allocator: Allocator,
    // Symbol table for current scope
    variables: StringHashMap(Type),
    // Current function context
    current_function: ?*Function,
    // Arena for type allocations
    type_ptrs: ArrayList(*Type),
    // Loop nesting depth for break/continue validation
    loop_depth: u32,
    
    pub fn init(allocator: Allocator) TypeChecker {
        return TypeChecker{
            .allocator = allocator,
            .variables = StringHashMap(Type).init(allocator),
            .current_function = null,
            .type_ptrs = ArrayList(*Type).init(allocator),
            .loop_depth = 0,
        };
    }
    
    pub fn deinit(self: *TypeChecker) void {
        self.variables.deinit();
        // Free all type pointers
        for (self.type_ptrs.items) |type_ptr| {
            self.allocator.destroy(type_ptr);
        }
        self.type_ptrs.deinit();
    }
    
    pub fn check(self: *TypeChecker, program: *Program) TypeCheckError!void {
        // Check that entry function exists
        if (!program.functions.contains(program.entry)) {
            std.debug.print("Error: Entry function '{s}' not found\n", .{program.entry});
            return TypeCheckError.UndefinedFunction;
        }
        
        // Type check all functions
        var func_iter = program.functions.iterator();
        while (func_iter.next()) |entry| {
            const func_name = entry.key_ptr.*;
            const function = entry.value_ptr;
            
            try self.checkFunction(func_name, function, program);
        }
        
        // Type check all trait implementations
        for (program.trait_impls.items) |*trait_impl| {
            try self.checkTraitImpl(trait_impl, program);
        }
    }
    
    fn checkFunction(self: *TypeChecker, name: []const u8, function: *Function, program: *Program) TypeCheckError!void {
        _ = name;
        
        // Clear variable scope for new function
        self.variables.clearRetainingCapacity();
        self.current_function = function;
        
        // Add function parameters to scope
        for (function.args.items) |param| {
            self.variables.put(param.name, param.type) catch return TypeCheckError.OutOfMemory;
        }
        
        // Check function body
        var has_return = false;
        for (function.body.items) |*stmt| {
            try self.checkStatement(stmt, program);
            if (self.statementHasReturn(stmt)) {
                has_return = true;
            }
        }
        
        // Check that non-void functions have a return statement
        if (function.@"return" != .void and !has_return) {
            return TypeCheckError.MissingReturn;
        }
    }
    
    fn checkStatement(self: *TypeChecker, stmt: *Statement, program: *Program) TypeCheckError!void {
        switch (stmt.*) {
            .let => |*let_stmt| {
                const value_type = try self.checkExpression(&let_stmt.value, program);
                
                // If type is explicitly specified, check compatibility
                if (let_stmt.type) |expected_type| {
                    if (!self.typesCompatible(value_type, expected_type)) {
                        return TypeCheckError.TypeMismatch;
                    }
                    // Store the explicit type
                    self.variables.put(let_stmt.name, expected_type) catch return TypeCheckError.OutOfMemory;
                } else {
                    // Infer type from value
                    self.variables.put(let_stmt.name, value_type) catch return TypeCheckError.OutOfMemory;
                }
            },
            
            .assign => |*assign_stmt| {
                const target_type = try self.checkLValue(&assign_stmt.target, program);
                const value_type = try self.checkExpression(&assign_stmt.value, program);
                
                if (!self.typesCompatible(target_type, value_type)) {
                    return TypeCheckError.TypeMismatch;
                }
            },
            
            .@"if" => |*if_stmt| {
                const condition_type = try self.checkExpression(&if_stmt.condition, program);
                if (condition_type != .bool) {
                    return TypeCheckError.TypeMismatch;
                }
                
                // Check then branch
                for (if_stmt.then.items) |*then_stmt| {
                    try self.checkStatement(then_stmt, program);
                }
                
                // Check else branch if present
                if (if_stmt.@"else") |*else_stmts| {
                    for (else_stmts.items) |*else_stmt| {
                        try self.checkStatement(else_stmt, program);
                    }
                }
            },
            
            .@"while" => |*while_stmt| {
                const condition_type = try self.checkExpression(&while_stmt.condition, program);
                if (condition_type != .bool) {
                    return TypeCheckError.TypeMismatch;
                }
                
                // Enter loop context
                self.loop_depth += 1;
                for (while_stmt.body.items) |*body_stmt| {
                    try self.checkStatement(body_stmt, program);
                }
                self.loop_depth -= 1;
            },
            
            .@"for" => |*for_stmt| {
                const iterable_type = try self.checkExpression(&for_stmt.iterable, program);
                
                // Determine element type from iterable
                const element_type = switch (iterable_type) {
                    .array => |arr| arr.element.*,
                    .slice => |slice| slice.element.*,
                    else => {
                        // For now, support iterating over basic types as single elements
                        // This can be extended later for more complex iterables
                        return TypeCheckError.TypeMismatch;
                    },
                };
                
                // Add loop variable to scope
                const prev_value = self.variables.get(for_stmt.variable);
                try self.variables.put(for_stmt.variable, element_type);
                
                // Enter loop context
                self.loop_depth += 1;
                // Check body statements
                for (for_stmt.body.items) |*body_stmt| {
                    try self.checkStatement(body_stmt, program);
                }
                self.loop_depth -= 1;
                
                // Restore previous variable binding (if any)
                if (prev_value) |prev| {
                    try self.variables.put(for_stmt.variable, prev);
                } else {
                    _ = self.variables.remove(for_stmt.variable);
                }
            },
            
            .@"return" => |*return_expr| {
                const return_type = try self.checkExpression(return_expr, program);
                
                if (self.current_function) |func| {
                    if (!self.typesCompatible(return_type, func.@"return")) {
                        return TypeCheckError.ReturnTypeMismatch;
                    }
                }
            },
            
            .observe => |*observe_stmt| {
                // Check that parameters match distribution requirements
                for (observe_stmt.params.items) |*param| {
                    _ = try self.checkExpression(param, program);
                }
                
                _ = try self.checkExpression(&observe_stmt.value, program);
            },
            
            .prob_assert => |*assert_stmt| {
                const condition_type = try self.checkExpression(&assert_stmt.condition, program);
                if (condition_type != .bool) {
                    return TypeCheckError.TypeMismatch;
                }
                
                // Confidence should be between 0 and 1
                if (assert_stmt.confidence < 0.0 or assert_stmt.confidence > 1.0) {
                    return TypeCheckError.InvalidOperation;
                }
            },
            
            .expression => |*expr| {
                _ = try self.checkExpression(expr, program);
            },
            
            .model => |*model_stmt| {
                // Type check graphical model components
                for (model_stmt.nodes.items) |*node_expr| {
                    _ = try self.checkExpression(node_expr, program);
                }
                for (model_stmt.plates.items) |*plate_expr| {
                    _ = try self.checkExpression(plate_expr, program);
                }
                for (model_stmt.factors.items) |*factor_expr| {
                    _ = try self.checkExpression(factor_expr, program);
                }
            },
            
            .match => |*match_stmt| {
                const value_type = try self.checkExpression(&match_stmt.value, program);
                
                // Check all patterns are compatible with the value type
                for (match_stmt.cases.items) |*case| {
                    try self.checkPattern(&case.pattern, value_type, program);
                    
                    // Check statements in this case
                    for (case.body.items) |*body_stmt| {
                        try self.checkStatement(body_stmt, program);
                    }
                }
            },
            
            .@"try" => |*try_stmt| {
                // Check try body
                for (try_stmt.body.items) |*body_stmt| {
                    try self.checkStatement(body_stmt, program);
                }
                
                // Check catch clauses
                for (try_stmt.catch_clauses.items) |*catch_clause| {
                    // If exception type is specified, validate it exists
                    if (catch_clause.exception_type) |exception_type| {
                        // Could add validation that exception_type is an error type
                        _ = exception_type;
                    }
                    
                    // If variable name is specified, add it to scope for catch body
                    if (catch_clause.variable_name) |var_name| {
                        const exception_type = catch_clause.exception_type orelse Type.str; // Default to string
                        try self.variables.put(var_name, exception_type);
                    }
                    
                    // Check catch body
                    for (catch_clause.body.items) |*catch_stmt| {
                        try self.checkStatement(catch_stmt, program);
                    }
                    
                    // Remove exception variable from scope
                    if (catch_clause.variable_name) |var_name| {
                        _ = self.variables.remove(var_name);
                    }
                }
                
                // Check finally body
                if (try_stmt.finally_body) |*finally_stmts| {
                    for (finally_stmts.items) |*finally_stmt| {
                        try self.checkStatement(finally_stmt, program);
                    }
                }
            },
            
            .@"throw" => |*throw_expr| {
                const exception_type = try self.checkExpression(throw_expr, program);
                // Could add validation that throw_expr is an error type or throwable
                _ = exception_type;
            },
            
            .@"break", .@"continue" => {
                // Validate that break/continue are used within a loop
                if (self.loop_depth == 0) {
                    return TypeCheckError.InvalidContext;
                }
            },
        }
    }
    
    fn checkExpression(self: *TypeChecker, expr: *Expression, program: *Program) TypeCheckError!Type {
        switch (expr.*) {
            .literal => |literal| {
                return switch (literal) {
                    .integer => Type.i32, // Use i32 for integer literals by default
                    .float => Type.f64,
                    .string => Type.str,
                    .boolean => Type.bool,
                    .null => Type.void, // or optional type
                };
            },
            
            .variable => |var_name| {
                return self.variables.get(var_name) orelse {
                    std.debug.print("Error: Undefined variable '{s}'\n", .{var_name});
                    return TypeCheckError.UndefinedVariable;
                };
            },
            
            .call => |*call_expr| {
                // Check for built-in functions first
                if (self.isBuiltinFunction(call_expr.function)) {
                    return try self.checkBuiltinFunction(call_expr, program);
                }
                
                const function = program.functions.get(call_expr.function) orelse {
                    std.debug.print("Error: Undefined function '{s}'\n", .{call_expr.function});
                    return TypeCheckError.UndefinedFunction;
                };
                
                // Check argument count
                if (call_expr.args.items.len != function.args.items.len) {
                    return TypeCheckError.ArgumentCountMismatch;
                }
                
                // Check argument types
                for (call_expr.args.items, 0..) |*arg, i| {
                    const arg_type = try self.checkExpression(arg, program);
                    const expected_type = function.args.items[i].type;
                    
                    if (!self.typesCompatible(arg_type, expected_type)) {
                        return TypeCheckError.ArgumentTypeMismatch;
                    }
                }
                
                // If the function is async, return a future type
                if (function.@"async") {
                    const return_type_ptr = try self.createType(function.@"return");
                    return Type{ .future = return_type_ptr };
                } else {
                    return function.@"return";
                }
            },
            
            .op => |*op_expr| {
                return try self.checkOperation(op_expr, program);
            },
            
            .sample => |*sample_expr| {
                // Check if it's a built-in distribution
                if (self.getDistributionKind(sample_expr.distribution)) |dist_kind| {
                    // Handle built-in distributions
                    const expected_param_count = self.getDistributionParamCount(dist_kind);
                    if (sample_expr.params.items.len != expected_param_count) {
                        return TypeCheckError.InvalidSample;
                    }
                    
                    // Check parameter types
                    for (sample_expr.params.items) |*param| {
                        const param_type = try self.checkExpression(param, program);
                        // For now, assume all distribution parameters are numeric
                        if (!self.isNumericType(param_type)) {
                            return TypeCheckError.InvalidSample;
                        }
                    }
                    
                    return self.getDistributionSampleType(dist_kind);
                } else {
                    // Check if it's a custom distribution (e.g., mixture model name)
                    // For now, allow any distribution name and assume it returns f64
                    // In a more complete implementation, we'd track custom distributions in the symbol table
                    
                    // Check parameter types
                    for (sample_expr.params.items) |*param| {
                        const param_type = try self.checkExpression(param, program);
                        // For now, assume all distribution parameters are numeric
                        if (!self.isNumericType(param_type)) {
                            return TypeCheckError.InvalidSample;
                        }
                    }
                    
                    return Type.f64; // Assume custom distributions return f64
                }
            },
            
            .index => |*index_expr| {
                const array_type = try self.checkExpression(index_expr.array, program);
                const index_type = try self.checkExpression(index_expr.index, program);
                
                if (!self.isIntegerType(index_type)) {
                    return TypeCheckError.TypeMismatch;
                }
                
                return switch (array_type) {
                    .array => |arr| arr.element.*,
                    .slice => |slice| slice.element.*,
                    else => TypeCheckError.InvalidOperation,
                };
            },
            
            .field => |*field_expr| {
                const object_type = try self.checkExpression(field_expr.object, program);
                
                return switch (object_type) {
                    .@"struct" => |struct_fields| {
                        if (struct_fields.get(field_expr.field)) |field_type_ptr| {
                            return field_type_ptr.*;
                        } else {
                            return TypeCheckError.UndefinedVariable;
                        }
                    },
                    else => TypeCheckError.InvalidOperation,
                };
            },
            
            .@"struct" => |*struct_expr| {
                // For struct literals, we need to infer the struct type from its fields
                // This is a simplified version - in a full implementation we'd need 
                // proper struct type definitions and validation
                var field_types = std.StringHashMap(*Type).init(self.allocator);
                
                var field_iter = struct_expr.iterator();
                while (field_iter.next()) |entry| {
                    const field_name = entry.key_ptr.*;
                    const field_expr = entry.value_ptr;
                    
                    const field_type = try self.checkExpression(@constCast(field_expr), program);
                    const field_type_ptr = try self.createType(field_type);
                    
                    const field_name_copy = self.allocator.dupe(u8, field_name) catch return TypeCheckError.OutOfMemory;
                    field_types.put(field_name_copy, field_type_ptr) catch return TypeCheckError.OutOfMemory;
                }
                
                return Type{ .@"struct" = field_types };
            },
            
            .array => |*array_expr| {
                // For array literals, infer type from first element
                if (array_expr.items.len == 0) {
                    // Empty array - we'd need type annotations in a full implementation
                    return Type.void; // Placeholder
                }
                
                const first_elem_type = try self.checkExpression(@constCast(&array_expr.items[0]), program);
                
                // Check that all elements have the same type
                for (array_expr.items[1..]) |*elem| {
                    const elem_type = try self.checkExpression(@constCast(elem), program);
                    if (!self.typesCompatible(first_elem_type, elem_type)) {
                        return TypeCheckError.TypeMismatch;
                    }
                }
                
                // Return slice type for now (dynamic arrays)
                const elem_type_ptr = try self.createType(first_elem_type);
                
                return Type{ .slice = .{ .element = elem_type_ptr } };
            },
            
            .enum_constructor => |*enum_expr| {
                // Find the enum type in the program's type definitions
                const enum_type = program.types.get(enum_expr.enum_type) orelse {
                    return TypeCheckError.UndefinedVariable; // Enum type not found
                };
                
                switch (enum_type) {
                    .@"enum" => |enum_def| {
                        // Check if the variant exists in the enum
                        if (enum_def.variants.get(enum_expr.variant)) |variant_type| {
                            // If there's an associated value, check its type
                            if (enum_expr.value) |value_expr| {
                                if (variant_type) |expected_type| {
                                    const actual_type = try self.checkExpression(value_expr, program);
                                    if (!self.typesCompatible(actual_type, expected_type.*)) {
                                        return TypeCheckError.TypeMismatch;
                                    }
                                } else {
                                    // Variant doesn't expect a value but one was provided
                                    return TypeCheckError.TypeMismatch;
                                }
                            } else if (variant_type != null) {
                                // Variant expects a value but none was provided
                                return TypeCheckError.TypeMismatch;
                            }
                            
                            // Return the enum type
                            return enum_type;
                        } else {
                            // Variant doesn't exist in the enum
                            return TypeCheckError.TypeMismatch;
                        }
                    },
                    else => {
                        // Not an enum type
                        return TypeCheckError.TypeMismatch;
                    },
                }
            },
            
            .hashmap => |*hashmap_expr| {
                // For hashmap literals, infer types from key-value pairs
                if (hashmap_expr.count() == 0) {
                    // Empty hashmap - would need type annotations in full implementation
                    return Type.void; // Placeholder
                }
                
                var map_iter = hashmap_expr.iterator();
                var key_type: ?Type = null;
                var value_type: ?Type = null;
                
                while (map_iter.next()) |entry| {
                    // For now, assume string keys (could be extended for other key types)
                    const current_key_type = Type.str;
                    const current_value_type = try self.checkExpression(@constCast(entry.value_ptr), program);
                    
                    if (key_type == null) {
                        key_type = current_key_type;
                        value_type = current_value_type;
                    } else {
                        // Check type consistency
                        if (!self.typesCompatible(key_type.?, current_key_type)) {
                            return TypeCheckError.TypeMismatch;
                        }
                        if (!self.typesCompatible(value_type.?, current_value_type)) {
                            return TypeCheckError.TypeMismatch;
                        }
                    }
                }
                
                const key_type_ptr = try self.createType(key_type.?);
                const value_type_ptr = try self.createType(value_type.?);
                
                return Type{ .hashmap = .{ .key = key_type_ptr, .value = value_type_ptr } };
            },
            
            .set => |*set_expr| {
                // For set literals, infer type from elements
                if (set_expr.items.len == 0) {
                    // Empty set - would need type annotations in full implementation
                    return Type.void; // Placeholder
                }
                
                const first_elem_type = try self.checkExpression(@constCast(&set_expr.items[0]), program);
                
                // Check that all elements have the same type
                for (set_expr.items[1..]) |*elem| {
                    const elem_type = try self.checkExpression(@constCast(elem), program);
                    if (!self.typesCompatible(first_elem_type, elem_type)) {
                        return TypeCheckError.TypeMismatch;
                    }
                }
                
                const elem_type_ptr = try self.createType(first_elem_type);
                return Type{ .set = .{ .element = elem_type_ptr } };
            },
            
            .tuple => |*tuple_expr| {
                // For tuple literals, infer types from elements
                if (tuple_expr.items.len == 0) {
                    // Empty tuple
                    return Type{ .tuple = ArrayList(*Type).init(self.allocator) };
                }
                
                var element_types = ArrayList(*Type).init(self.allocator);
                
                for (tuple_expr.items) |*elem| {
                    const elem_type = try self.checkExpression(@constCast(elem), program);
                    const elem_type_ptr = try self.createType(elem_type);
                    try element_types.append(elem_type_ptr);
                }
                
                return Type{ .tuple = element_types };
            },
            
            .record => |*record_expr| {
                // Find the record type definition
                const record_type = program.types.get(record_expr.type_name) orelse {
                    return TypeCheckError.UndefinedVariable; // Record type not found
                };
                
                switch (record_type) {
                    .record => |record_def| {
                        // Check that all required fields are provided and have correct types
                        var provided_fields = std.StringHashMap(bool).init(self.allocator);
                        defer provided_fields.deinit();
                        
                        var field_iter = record_expr.fields.iterator();
                        while (field_iter.next()) |entry| {
                            const field_name = entry.key_ptr.*;
                            const field_expr = entry.value_ptr;
                            
                            // Check if field exists in record type
                            if (record_def.fields.get(field_name)) |expected_type| {
                                const actual_type = try self.checkExpression(@constCast(field_expr), program);
                                if (!self.typesCompatible(actual_type, expected_type.*)) {
                                    return TypeCheckError.TypeMismatch;
                                }
                                try provided_fields.put(field_name, true);
                            } else {
                                // Field doesn't exist in record type
                                return TypeCheckError.TypeMismatch;
                            }
                        }
                        
                        // Check that all required fields are provided
                        var type_field_iter = record_def.fields.iterator();
                        while (type_field_iter.next()) |entry| {
                            if (!provided_fields.contains(entry.key_ptr.*)) {
                                // Missing required field
                                return TypeCheckError.TypeMismatch;
                            }
                        }
                        
                        return record_type;
                    },
                    else => {
                        // Not a record type
                        return TypeCheckError.TypeMismatch;
                    },
                }
            },
            
            .@"await" => |await_expr| {
                // Check the inner expression
                const inner_type = try self.checkExpression(await_expr, program);
                
                // Verify that inner expression returns a future type
                switch (inner_type) {
                    .future => |future_type| {
                        // Await unwraps the future and returns the inner type
                        return future_type.*;
                    },
                    else => {
                        // Can only await future types
                        return TypeCheckError.TypeMismatch;
                    },
                }
            },
            
            .mixture => |*mixture_expr| {
                // Type check mixture model
                return try self.checkMixtureExpression(mixture_expr, program);
            },
            
            .hierarchical => |*hierarchical_expr| {
                // Type check hierarchical model
                return try self.checkHierarchicalExpression(hierarchical_expr, program);
            },
            
            .plate => |*plate_expr| {
                // Type check plate expression
                _ = try self.checkExpression(plate_expr.size, program);
                if (plate_expr.condition) |condition| {
                    _ = try self.checkExpression(condition, program);
                }
                for (plate_expr.body.items) |*stmt| {
                    try self.checkStatement(stmt, program);
                }
                return Type.void;
            },
            
            .factor => |*factor_expr| {
                // Type check factor expression
                _ = try self.checkExpression(factor_expr.function_expr, program);
                return Type.void;
            },
            
            .graphical_node => |*node_expr| {
                // Type check graphical node expression
                for (node_expr.parameters.items) |*param| {
                    _ = try self.checkExpression(param, program);
                }
                if (node_expr.observed_value) |obs_val| {
                    _ = try self.checkExpression(obs_val, program);
                }
                return Type.void;
            },
            
            else => {
                // Handle other expression types
                return Type.void;
            },
        }
    }
    
    fn checkLValue(self: *TypeChecker, lvalue: *const SirsParser.LValue, program: *Program) TypeCheckError!Type {
        switch (lvalue.*) {
            .variable => |var_name| {
                return self.variables.get(var_name) orelse TypeCheckError.UndefinedVariable;
            },
            .index => |*index_lvalue| {
                const array_type = try self.checkLValue(index_lvalue.array, program);
                const index_type = try self.checkExpression(@constCast(&index_lvalue.index), program);
                
                if (!self.isIntegerType(index_type)) {
                    return TypeCheckError.TypeMismatch;
                }
                
                return switch (array_type) {
                    .array => |arr| arr.element.*,
                    .slice => |slice| slice.element.*,
                    else => TypeCheckError.InvalidOperation,
                };
            },
            .field => |*field_lvalue| {
                const object_type = try self.checkLValue(field_lvalue.object, program);
                
                return switch (object_type) {
                    .@"struct" => |struct_fields| {
                        if (struct_fields.get(field_lvalue.field)) |field_type_ptr| {
                            return field_type_ptr.*;
                        } else {
                            return TypeCheckError.UndefinedVariable;
                        }
                    },
                    else => TypeCheckError.InvalidOperation,
                };
            },
        }
    }
    
    fn checkOperation(self: *TypeChecker, op_expr: anytype, program: *Program) TypeCheckError!Type {
        const args = &op_expr.args;
        
        switch (op_expr.kind) {
            .add => {
                if (args.items.len != 2) return TypeCheckError.InvalidOperation;
                
                const left_type = try self.checkExpression(@constCast(&args.items[0]), program);
                const right_type = try self.checkExpression(@constCast(&args.items[1]), program);
                
                // Handle string concatenation
                if (left_type == .str and right_type == .str) {
                    return Type.str;
                }
                
                // Handle numeric addition
                if (self.isNumericType(left_type) and self.isNumericType(right_type)) {
                    return self.getArithmeticResultType(left_type, right_type);
                }
                
                return TypeCheckError.TypeMismatch;
            },
            
            .sub, .mul, .div, .mod => {
                if (args.items.len != 2) return TypeCheckError.InvalidOperation;
                
                const left_type = try self.checkExpression(@constCast(&args.items[0]), program);
                const right_type = try self.checkExpression(@constCast(&args.items[1]), program);
                
                if (!self.isNumericType(left_type) or !self.isNumericType(right_type)) {
                    return TypeCheckError.TypeMismatch;
                }
                
                // Return the "wider" type
                return self.getArithmeticResultType(left_type, right_type);
            },
            
            .eq, .ne, .lt, .le, .gt, .ge => {
                if (args.items.len != 2) return TypeCheckError.InvalidOperation;
                
                const left_type = try self.checkExpression(@constCast(&args.items[0]), program);
                const right_type = try self.checkExpression(@constCast(&args.items[1]), program);
                
                if (!self.typesCompatible(left_type, right_type)) {
                    return TypeCheckError.TypeMismatch;
                }
                
                return Type.bool;
            },
            
            .@"and", .@"or" => {
                if (args.items.len != 2) return TypeCheckError.InvalidOperation;
                
                const left_type = try self.checkExpression(@constCast(&args.items[0]), program);
                const right_type = try self.checkExpression(@constCast(&args.items[1]), program);
                
                if (left_type != .bool or right_type != .bool) {
                    return TypeCheckError.TypeMismatch;
                }
                
                return Type.bool;
            },
            
            .not => {
                if (args.items.len != 1) return TypeCheckError.InvalidOperation;
                
                const operand_type = try self.checkExpression(@constCast(&args.items[0]), program);
                if (operand_type != .bool) {
                    return TypeCheckError.TypeMismatch;
                }
                
                return Type.bool;
            },
            
            else => {
                // Handle other operations
                return Type.void;
            },
        }
    }
    
    fn typesCompatible(_: *TypeChecker, type1: Type, type2: Type) bool {
        // Simple type compatibility check - can be extended
        return std.meta.eql(type1, type2);
    }
    
    fn isNumericType(_: *TypeChecker, t: Type) bool {
        return switch (t) {
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .f32, .f64 => true,
            else => false,
        };
    }
    
    fn isIntegerType(_: *TypeChecker, t: Type) bool {
        return switch (t) {
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => true,
            else => false,
        };
    }
    
    fn getArithmeticResultType(_: *TypeChecker, left: Type, right: Type) Type {
        // Simple type promotion rules
        if (left == .f64 or right == .f64) return Type.f64;
        if (left == .f32 or right == .f32) return Type.f32;
        if (left == .i64 or right == .i64) return Type.i64;
        if (left == .u64 or right == .u64) return Type.u64;
        return Type.i32;
    }
    
    fn getDistributionKind(_: *TypeChecker, dist_name: []const u8) ?SirsParser.DistributionKind {
        if (std.mem.eql(u8, dist_name, "uniform")) return .uniform;
        if (std.mem.eql(u8, dist_name, "normal")) return .normal;
        if (std.mem.eql(u8, dist_name, "categorical")) return .categorical;
        if (std.mem.eql(u8, dist_name, "bernoulli")) return .bernoulli;
        if (std.mem.eql(u8, dist_name, "exponential")) return .exponential;
        if (std.mem.eql(u8, dist_name, "gamma")) return .gamma;
        if (std.mem.eql(u8, dist_name, "beta")) return .beta;
        if (std.mem.eql(u8, dist_name, "dirichlet")) return .dirichlet;
        return null;
    }
    
    fn getDistributionParamCount(_: *TypeChecker, kind: SirsParser.DistributionKind) usize {
        return switch (kind) {
            .uniform => 2, // min, max
            .normal => 2,  // mean, std
            .categorical => 1, // probabilities array
            .bernoulli => 1,   // probability
            .exponential => 1, // rate
            .gamma => 2,       // shape, scale
            .beta => 2,        // alpha, beta
            .dirichlet => 3,   // concentration parameters (variable, but using 3 for our example)
        };
    }
    
    fn getDistributionSampleType(_: *TypeChecker, kind: SirsParser.DistributionKind) Type {
        return switch (kind) {
            .uniform, .normal, .exponential, .gamma, .beta, .dirichlet, .bernoulli => Type.f64,
            .categorical => Type.i32,
        };
    }
    
    fn isBuiltinFunction(_: *TypeChecker, function_name: []const u8) bool {
        return std.mem.eql(u8, function_name, "std_print") or
               std.mem.eql(u8, function_name, "std_print_int") or
               std.mem.eql(u8, function_name, "std_print_float") or
               std.mem.eql(u8, function_name, "debug_trace") or
               std.mem.eql(u8, function_name, "debug_breakpoint") or
               std.mem.eql(u8, function_name, "debug_variable") or
               std.mem.eql(u8, function_name, "http_get") or
               std.mem.eql(u8, function_name, "http_post") or
               std.mem.eql(u8, function_name, "http_put") or
               std.mem.eql(u8, function_name, "http_delete") or
               std.mem.eql(u8, function_name, "http_serve") or
               std.mem.eql(u8, function_name, "file_read") or
               std.mem.eql(u8, function_name, "file_write") or
               std.mem.eql(u8, function_name, "file_append") or
               std.mem.eql(u8, function_name, "file_exists") or
               std.mem.eql(u8, function_name, "file_delete") or
               std.mem.eql(u8, function_name, "file_size") or
               std.mem.eql(u8, function_name, "dir_create") or
               std.mem.eql(u8, function_name, "dir_exists") or
               std.mem.eql(u8, function_name, "dir_list") or
               std.mem.eql(u8, function_name, "json_parse") or
               std.mem.eql(u8, function_name, "json_get_string") or
               std.mem.eql(u8, function_name, "json_get_number") or
               std.mem.eql(u8, function_name, "json_get_bool") or
               std.mem.eql(u8, function_name, "json_create_object") or
               std.mem.eql(u8, function_name, "json_add_string") or
               std.mem.eql(u8, function_name, "json_add_number") or
               std.mem.eql(u8, function_name, "f64_to_string") or
               std.mem.eql(u8, function_name, "json_has_key") or
               std.mem.eql(u8, function_name, "json_stringify_object") or
               std.mem.eql(u8, function_name, "json_stringify_array") or
               std.mem.eql(u8, function_name, "str_length") or
               std.mem.eql(u8, function_name, "str_substring") or
               std.mem.eql(u8, function_name, "str_contains") or
               std.mem.eql(u8, function_name, "str_starts_with") or
               std.mem.eql(u8, function_name, "str_ends_with") or
               std.mem.eql(u8, function_name, "str_index_of") or
               std.mem.eql(u8, function_name, "str_replace") or
               std.mem.eql(u8, function_name, "str_to_upper") or
               std.mem.eql(u8, function_name, "str_to_lower") or
               std.mem.eql(u8, function_name, "str_trim") or
               std.mem.eql(u8, function_name, "str_equals") or
               std.mem.eql(u8, function_name, "datetime_now") or
               std.mem.eql(u8, function_name, "datetime_now_millis") or
               std.mem.eql(u8, function_name, "datetime_now_micros") or
               std.mem.eql(u8, function_name, "datetime_year") or
               std.mem.eql(u8, function_name, "datetime_month") or
               std.mem.eql(u8, function_name, "datetime_day") or
               std.mem.eql(u8, function_name, "datetime_hour") or
               std.mem.eql(u8, function_name, "datetime_minute") or
               std.mem.eql(u8, function_name, "datetime_second") or
               std.mem.eql(u8, function_name, "datetime_add_seconds") or
               std.mem.eql(u8, function_name, "datetime_add_minutes") or
               std.mem.eql(u8, function_name, "datetime_add_hours") or
               std.mem.eql(u8, function_name, "datetime_add_days") or
               std.mem.eql(u8, function_name, "datetime_diff_seconds") or
               std.mem.eql(u8, function_name, "sleep_seconds") or
               std.mem.eql(u8, function_name, "sleep_millis") or
               std.mem.eql(u8, function_name, "regex_match") or
               std.mem.eql(u8, function_name, "regex_find") or
               std.mem.eql(u8, function_name, "regex_replace") or
               std.mem.eql(u8, function_name, "regex_split") or
               std.mem.eql(u8, function_name, "ffi_load_library") or
               std.mem.eql(u8, function_name, "ffi_unload_library") or
               std.mem.eql(u8, function_name, "ffi_call_i32") or
               std.mem.eql(u8, function_name, "ffi_call_f64") or
               std.mem.eql(u8, function_name, "ffi_call_str") or
               std.mem.eql(u8, function_name, "ffi_call_void") or
               std.mem.eql(u8, function_name, "ffi_alloc_bytes") or
               std.mem.eql(u8, function_name, "ffi_free_bytes") or
               std.mem.eql(u8, function_name, "ffi_read_i32") or
               std.mem.eql(u8, function_name, "ffi_write_i32") or
               std.mem.eql(u8, function_name, "ffi_read_str") or
               std.mem.eql(u8, function_name, "ffi_write_str") or
               // Mathematical functions
               std.mem.eql(u8, function_name, "math_abs") or
               std.mem.eql(u8, function_name, "math_sqrt") or
               std.mem.eql(u8, function_name, "math_pow") or
               std.mem.eql(u8, function_name, "math_exp") or
               std.mem.eql(u8, function_name, "math_log") or
               std.mem.eql(u8, function_name, "math_log10") or
               std.mem.eql(u8, function_name, "math_log2") or
               std.mem.eql(u8, function_name, "math_sin") or
               std.mem.eql(u8, function_name, "math_cos") or
               std.mem.eql(u8, function_name, "math_tan") or
               std.mem.eql(u8, function_name, "math_asin") or
               std.mem.eql(u8, function_name, "math_acos") or
               std.mem.eql(u8, function_name, "math_atan") or
               std.mem.eql(u8, function_name, "math_atan2") or
               std.mem.eql(u8, function_name, "math_sinh") or
               std.mem.eql(u8, function_name, "math_cosh") or
               std.mem.eql(u8, function_name, "math_tanh") or
               std.mem.eql(u8, function_name, "math_floor") or
               std.mem.eql(u8, function_name, "math_ceil") or
               std.mem.eql(u8, function_name, "math_round") or
               std.mem.eql(u8, function_name, "math_trunc") or
               std.mem.eql(u8, function_name, "math_fmod") or
               std.mem.eql(u8, function_name, "math_remainder") or
               std.mem.eql(u8, function_name, "math_min") or
               std.mem.eql(u8, function_name, "math_max") or
               std.mem.eql(u8, function_name, "math_clamp") or
               std.mem.eql(u8, function_name, "math_lerp") or
               std.mem.eql(u8, function_name, "math_degrees") or
               std.mem.eql(u8, function_name, "math_radians") or
               std.mem.eql(u8, function_name, "math_pi") or
               std.mem.eql(u8, function_name, "math_e") or
               std.mem.eql(u8, function_name, "math_inf") or
               std.mem.eql(u8, function_name, "math_nan") or
               std.mem.eql(u8, function_name, "math_is_finite") or
               std.mem.eql(u8, function_name, "math_is_infinite") or
               std.mem.eql(u8, function_name, "math_is_nan") or
               std.mem.eql(u8, function_name, "math_sign") or
               std.mem.eql(u8, function_name, "math_copysign");
    }
    
    fn checkBuiltinFunction(self: *TypeChecker, call_expr: anytype, program: *Program) TypeCheckError!Type {
        const function_name = call_expr.function;
        
        if (std.mem.eql(u8, function_name, "std_print")) {
            // std_print(message: []const u8) void
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.void;
        } else if (std.mem.eql(u8, function_name, "std_print_int")) {
            // std_print_int(value: i32|i64) void - accepts both i32 and i64
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .i32 and arg_type != .i64) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.void;
        } else if (std.mem.eql(u8, function_name, "std_print_float")) {
            // std_print_float(value: f64) void
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .f64) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.void;
        } else if (std.mem.eql(u8, function_name, "debug_trace")) {
            // debug_trace(function_name: []const u8, value: i64) void
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const name_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const value_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (name_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            if (value_type != .i32 and value_type != .i64) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.void;
        } else if (std.mem.eql(u8, function_name, "debug_breakpoint")) {
            // debug_breakpoint(file: []const u8, line: i32, message: []const u8) void
            if (call_expr.args.items.len != 3) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const file_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const line_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            const message_type = try self.checkExpression(@constCast(&call_expr.args.items[2]), program);
            if (file_type != .str or line_type != .i32 or message_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.void;
        } else if (std.mem.eql(u8, function_name, "debug_variable")) {
            // debug_variable(name: []const u8, value: []const u8) void
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const name_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const value_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (name_type != .str or value_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.void;
        } else if (std.mem.eql(u8, function_name, "http_get")) {
            // http_get(url: []const u8) []const u8
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "http_post")) {
            // http_post(url: []const u8, body: []const u8) []const u8
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const url_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const body_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (url_type != .str or body_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "http_put")) {
            // http_put(url: []const u8, body: []const u8) []const u8
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const url_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const body_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (url_type != .str or body_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "http_delete")) {
            // http_delete(url: []const u8) []const u8
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "http_serve")) {
            // http_serve(port: i32, handler_name: str) void
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const port_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (port_type != .i32) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            const handler_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (handler_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.void;
        } else if (std.mem.eql(u8, function_name, "file_read")) {
            // file_read(path: []const u8) []const u8
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "file_write")) {
            // file_write(path: []const u8, content: []const u8) bool
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const path_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const content_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (path_type != .str or content_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        } else if (std.mem.eql(u8, function_name, "file_append")) {
            // file_append(path: []const u8, content: []const u8) bool
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const path_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const content_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (path_type != .str or content_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        } else if (std.mem.eql(u8, function_name, "file_exists")) {
            // file_exists(path: []const u8) bool
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        } else if (std.mem.eql(u8, function_name, "file_delete")) {
            // file_delete(path: []const u8) bool
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        } else if (std.mem.eql(u8, function_name, "file_size")) {
            // file_size(path: []const u8) i64
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.i64;
        } else if (std.mem.eql(u8, function_name, "dir_create")) {
            // dir_create(path: []const u8) bool
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        } else if (std.mem.eql(u8, function_name, "dir_exists")) {
            // dir_exists(path: []const u8) bool
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        } else if (std.mem.eql(u8, function_name, "dir_list")) {
            // dir_list(path: []const u8) []const u8
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "json_parse")) {
            // json_parse(json_str: []const u8) []const u8
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "json_get_string")) {
            // json_get_string(json_str: []const u8, key: []const u8) []const u8
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const json_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const key_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (json_type != .str or key_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "json_get_number")) {
            // json_get_number(json_str: []const u8, key: []const u8) f64
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const json_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const key_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (json_type != .str or key_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.f64;
        } else if (std.mem.eql(u8, function_name, "json_get_bool")) {
            // json_get_bool(json_str: []const u8, key: []const u8) bool
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const json_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const key_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (json_type != .str or key_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        } else if (std.mem.eql(u8, function_name, "json_create_object")) {
            // json_create_object() str
            if (call_expr.args.items.len != 0) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "json_add_string")) {
            // json_add_string(json_str: str, key: str, value: str) str
            if (call_expr.args.items.len != 3) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const json_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const key_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            const value_type = try self.checkExpression(@constCast(&call_expr.args.items[2]), program);
            if (json_type != .str or key_type != .str or value_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "json_add_number")) {
            // json_add_number(json_str: str, key: str, value: f64) str
            if (call_expr.args.items.len != 3) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const json_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const key_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            const value_type = try self.checkExpression(@constCast(&call_expr.args.items[2]), program);
            if (json_type != .str or key_type != .str or value_type != .f64) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "f64_to_string")) {
            // f64_to_string(value: f64) str
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const value_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (value_type != .f64) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "json_has_key")) {
            // json_has_key(json_str: []const u8, key: []const u8) bool
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const json_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const key_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (json_type != .str or key_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        } else if (std.mem.eql(u8, function_name, "json_stringify_object")) {
            // json_stringify_object(keys: []const []const u8, values: []const []const u8) []const u8
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            // For now, just check that both arguments are expressions - proper array type checking would be more complex
            _ = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            _ = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "json_stringify_array")) {
            // json_stringify_array(values: []const []const u8) []const u8
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            // For now, just check that the argument is an expression - proper array type checking would be more complex
            _ = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "str_length")) {
            // str_length(s: []const u8) i32
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.i32;
        } else if (std.mem.eql(u8, function_name, "str_substring")) {
            // str_substring(s: []const u8, start: i64, end: i64) []const u8
            if (call_expr.args.items.len != 3) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const str_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const start_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            const end_type = try self.checkExpression(@constCast(&call_expr.args.items[2]), program);
            if (str_type != .str or start_type != .i64 or end_type != .i64) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "str_contains")) {
            // str_contains(s: []const u8, needle: []const u8) bool
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const str_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const needle_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (str_type != .str or needle_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        } else if (std.mem.eql(u8, function_name, "str_starts_with")) {
            // str_starts_with(s: []const u8, prefix: []const u8) bool
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const str_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const prefix_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (str_type != .str or prefix_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        } else if (std.mem.eql(u8, function_name, "str_ends_with")) {
            // str_ends_with(s: []const u8, suffix: []const u8) bool
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const str_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const suffix_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (str_type != .str or suffix_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        } else if (std.mem.eql(u8, function_name, "str_index_of")) {
            // str_index_of(s: []const u8, needle: []const u8) i64
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const str_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const needle_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (str_type != .str or needle_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.i64;
        } else if (std.mem.eql(u8, function_name, "str_replace")) {
            // str_replace(s: []const u8, needle: []const u8, replacement: []const u8) []const u8
            if (call_expr.args.items.len != 3) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const str_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const needle_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            const replacement_type = try self.checkExpression(@constCast(&call_expr.args.items[2]), program);
            if (str_type != .str or needle_type != .str or replacement_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "str_to_upper")) {
            // str_to_upper(s: []const u8) []const u8
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "str_to_lower")) {
            // str_to_lower(s: []const u8) []const u8
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "str_trim")) {
            // str_trim(s: []const u8) []const u8
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "str_equals")) {
            // str_equals(a: []const u8, b: []const u8) bool
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const str1_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const str2_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (str1_type != .str or str2_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        } else if (std.mem.eql(u8, function_name, "datetime_now") or
                   std.mem.eql(u8, function_name, "datetime_now_millis") or
                   std.mem.eql(u8, function_name, "datetime_now_micros")) {
            // datetime_now() i64
            if (call_expr.args.items.len != 0) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            return Type.i64;
        } else if (std.mem.eql(u8, function_name, "datetime_year") or
                   std.mem.eql(u8, function_name, "datetime_month") or
                   std.mem.eql(u8, function_name, "datetime_day") or
                   std.mem.eql(u8, function_name, "datetime_hour") or
                   std.mem.eql(u8, function_name, "datetime_minute") or
                   std.mem.eql(u8, function_name, "datetime_second")) {
            // datetime_year(timestamp: i64) i32
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (arg_type != .i64) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.i32;
        } else if (std.mem.eql(u8, function_name, "datetime_add_seconds") or
                   std.mem.eql(u8, function_name, "datetime_add_minutes") or
                   std.mem.eql(u8, function_name, "datetime_add_hours") or
                   std.mem.eql(u8, function_name, "datetime_add_days")) {
            // datetime_add_seconds(timestamp: i64, amount: i32) i64
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const timestamp_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const amount_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (timestamp_type != .i64 or amount_type != .i32) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.i64;
        } else if (std.mem.eql(u8, function_name, "datetime_diff_seconds")) {
            // datetime_diff_seconds(end: i64, start: i64) i64
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const end_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const start_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (end_type != .i64 or start_type != .i64) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.i64;
        } else if (std.mem.eql(u8, function_name, "sleep_seconds") or
                   std.mem.eql(u8, function_name, "sleep_millis")) {
            // sleep_seconds(duration: i32) void
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const duration_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (duration_type != .i32) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.void;
        } else if (std.mem.eql(u8, function_name, "regex_match")) {
            // regex_match(text: str, pattern: str) bool
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const text_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const pattern_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (text_type != .str or pattern_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        } else if (std.mem.eql(u8, function_name, "regex_find") or
                   std.mem.eql(u8, function_name, "regex_split")) {
            // regex_find(text: str, pattern: str) str
            // regex_split(text: str, pattern: str) str
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const text_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const pattern_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (text_type != .str or pattern_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "regex_replace")) {
            // regex_replace(text: str, pattern: str, replacement: str) str
            if (call_expr.args.items.len != 3) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const text_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const pattern_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            const replacement_type = try self.checkExpression(@constCast(&call_expr.args.items[2]), program);
            if (text_type != .str or pattern_type != .str or replacement_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "ffi_load_library") or
                   std.mem.eql(u8, function_name, "ffi_unload_library")) {
            // ffi_load_library(path: str) bool
            // ffi_unload_library(path: str) bool
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const path_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (path_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        } else if (std.mem.eql(u8, function_name, "ffi_call_i32")) {
            // ffi_call_i32(lib_path: str, func_name: str, args: array) i32
            if (call_expr.args.items.len != 3) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const lib_path_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const func_name_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            const args_type = try self.checkExpression(@constCast(&call_expr.args.items[2]), program);
            if (lib_path_type != .str or func_name_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            // args_type should be an array, but for simplicity we'll allow any type for now
            _ = args_type;
            return Type.i32;
        } else if (std.mem.eql(u8, function_name, "ffi_call_f64")) {
            // ffi_call_f64(lib_path: str, func_name: str, args: array) f64
            if (call_expr.args.items.len != 3) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const lib_path_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const func_name_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            const args_type = try self.checkExpression(@constCast(&call_expr.args.items[2]), program);
            if (lib_path_type != .str or func_name_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            _ = args_type;
            return Type.f64;
        } else if (std.mem.eql(u8, function_name, "ffi_call_str")) {
            // ffi_call_str(lib_path: str, func_name: str, args: array) str
            if (call_expr.args.items.len != 3) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const lib_path_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const func_name_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            const args_type = try self.checkExpression(@constCast(&call_expr.args.items[2]), program);
            if (lib_path_type != .str or func_name_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            _ = args_type;
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "ffi_call_void")) {
            // ffi_call_void(lib_path: str, func_name: str, args: array) void
            if (call_expr.args.items.len != 3) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const lib_path_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const func_name_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            const args_type = try self.checkExpression(@constCast(&call_expr.args.items[2]), program);
            if (lib_path_type != .str or func_name_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            _ = args_type;
            return Type.void;
        } else if (std.mem.eql(u8, function_name, "ffi_alloc_bytes")) {
            // ffi_alloc_bytes(size: i32) i64
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const size_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (size_type != .i32) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.i64;
        } else if (std.mem.eql(u8, function_name, "ffi_free_bytes")) {
            // ffi_free_bytes(ptr: i64) void
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const ptr_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (ptr_type != .i64) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.void;
        } else if (std.mem.eql(u8, function_name, "ffi_write_i32")) {
            // ffi_write_i32(ptr: i64, value: i32) void
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const ptr_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const value_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (ptr_type != .i64 or value_type != .i32) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.void;
        } else if (std.mem.eql(u8, function_name, "ffi_read_i32")) {
            // ffi_read_i32(ptr: i64) i32
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const ptr_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (ptr_type != .i64) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.i32;
        } else if (std.mem.eql(u8, function_name, "ffi_read_str")) {
            // ffi_read_str(ptr: i64, len: i32) str
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const ptr_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const len_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (ptr_type != .i64 or len_type != .i32) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.str;
        } else if (std.mem.eql(u8, function_name, "ffi_write_str")) {
            // ffi_write_str(ptr: i64, str: str) void
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const ptr_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const str_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (ptr_type != .i64 or str_type != .str) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.void;
        
        // Mathematical functions
        } else if (std.mem.eql(u8, function_name, "math_abs")) {
            // math_abs(x: f64) f64
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (!self.isNumericType(arg_type)) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return arg_type; // Return same type as input
        } else if (std.mem.eql(u8, function_name, "math_sqrt")) {
            // math_sqrt(x: f64) f64
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (!self.isNumericType(arg_type)) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.f64;
        } else if (std.mem.eql(u8, function_name, "math_pow")) {
            // math_pow(base: f64, exponent: f64) f64
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const base_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const exp_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (!self.isNumericType(base_type) or !self.isNumericType(exp_type)) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.f64;
        } else if (std.mem.eql(u8, function_name, "math_exp") or 
                   std.mem.eql(u8, function_name, "math_log") or
                   std.mem.eql(u8, function_name, "math_log10") or
                   std.mem.eql(u8, function_name, "math_log2") or
                   std.mem.eql(u8, function_name, "math_sin") or
                   std.mem.eql(u8, function_name, "math_cos") or
                   std.mem.eql(u8, function_name, "math_tan") or
                   std.mem.eql(u8, function_name, "math_asin") or
                   std.mem.eql(u8, function_name, "math_acos") or
                   std.mem.eql(u8, function_name, "math_atan") or
                   std.mem.eql(u8, function_name, "math_sinh") or
                   std.mem.eql(u8, function_name, "math_cosh") or
                   std.mem.eql(u8, function_name, "math_tanh") or
                   std.mem.eql(u8, function_name, "math_floor") or
                   std.mem.eql(u8, function_name, "math_ceil") or
                   std.mem.eql(u8, function_name, "math_round") or
                   std.mem.eql(u8, function_name, "math_trunc") or
                   std.mem.eql(u8, function_name, "math_degrees") or
                   std.mem.eql(u8, function_name, "math_radians") or
                   std.mem.eql(u8, function_name, "math_sign")) {
            // Single argument math functions: f(x: f64) f64
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (!self.isNumericType(arg_type)) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.f64;
        } else if (std.mem.eql(u8, function_name, "math_atan2") or
                   std.mem.eql(u8, function_name, "math_fmod") or
                   std.mem.eql(u8, function_name, "math_remainder") or
                   std.mem.eql(u8, function_name, "math_min") or
                   std.mem.eql(u8, function_name, "math_max") or
                   std.mem.eql(u8, function_name, "math_copysign")) {
            // Two argument math functions: f(x: f64, y: f64) f64
            if (call_expr.args.items.len != 2) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg1_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const arg2_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            if (!self.isNumericType(arg1_type) or !self.isNumericType(arg2_type)) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.f64;
        } else if (std.mem.eql(u8, function_name, "math_clamp")) {
            // math_clamp(value: f64, min: f64, max: f64) f64
            if (call_expr.args.items.len != 3) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const value_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const min_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            const max_type = try self.checkExpression(@constCast(&call_expr.args.items[2]), program);
            if (!self.isNumericType(value_type) or !self.isNumericType(min_type) or !self.isNumericType(max_type)) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.f64;
        } else if (std.mem.eql(u8, function_name, "math_lerp")) {
            // math_lerp(a: f64, b: f64, t: f64) f64 - linear interpolation
            if (call_expr.args.items.len != 3) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const a_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            const b_type = try self.checkExpression(@constCast(&call_expr.args.items[1]), program);
            const t_type = try self.checkExpression(@constCast(&call_expr.args.items[2]), program);
            if (!self.isNumericType(a_type) or !self.isNumericType(b_type) or !self.isNumericType(t_type)) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.f64;
        } else if (std.mem.eql(u8, function_name, "math_pi") or
                   std.mem.eql(u8, function_name, "math_e") or
                   std.mem.eql(u8, function_name, "math_inf") or
                   std.mem.eql(u8, function_name, "math_nan")) {
            // Mathematical constants: f() f64
            if (call_expr.args.items.len != 0) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            return Type.f64;
        } else if (std.mem.eql(u8, function_name, "math_is_finite") or
                   std.mem.eql(u8, function_name, "math_is_infinite") or
                   std.mem.eql(u8, function_name, "math_is_nan")) {
            // Mathematical predicates: f(x: f64) bool
            if (call_expr.args.items.len != 1) {
                return TypeCheckError.ArgumentCountMismatch;
            }
            const arg_type = try self.checkExpression(@constCast(&call_expr.args.items[0]), program);
            if (!self.isNumericType(arg_type)) {
                return TypeCheckError.ArgumentTypeMismatch;
            }
            return Type.bool;
        }
        
        return TypeCheckError.UndefinedFunction;
    }
    
    fn createType(self: *TypeChecker, type_value: Type) TypeCheckError!*Type {
        const type_ptr = self.allocator.create(Type) catch return TypeCheckError.OutOfMemory;
        type_ptr.* = type_value;
        self.type_ptrs.append(type_ptr) catch return TypeCheckError.OutOfMemory;
        return type_ptr;
    }
    
    fn statementHasReturn(self: *TypeChecker, stmt: *Statement) bool {
        return switch (stmt.*) {
            .@"return" => true,
            .@"if" => |*if_stmt| {
                // Check if both branches have returns
                var then_has_return = false;
                for (if_stmt.then.items) |*then_stmt| {
                    if (self.statementHasReturn(then_stmt)) {
                        then_has_return = true;
                        break;
                    }
                }
                
                if (if_stmt.@"else") |*else_stmts| {
                    var else_has_return = false;
                    for (else_stmts.items) |*else_stmt| {
                        if (self.statementHasReturn(else_stmt)) {
                            else_has_return = true;
                            break;
                        }
                    }
                    return then_has_return and else_has_return;
                } else {
                    return false; // if without else can't guarantee return
                }
            },
            .match => |*match_stmt| {
                // Check if all match cases have returns
                for (match_stmt.cases.items) |*case| {
                    var case_has_return = false;
                    for (case.body.items) |*case_stmt| {
                        if (self.statementHasReturn(case_stmt)) {
                            case_has_return = true;
                            break;
                        }
                    }
                    if (!case_has_return) {
                        return false; // if any case doesn't have return, match doesn't guarantee return
                    }
                }
                return match_stmt.cases.items.len > 0; // all cases have returns
            },
            .@"while" => |*while_stmt| {
                // While loops don't guarantee execution, so can't guarantee return
                _ = while_stmt;
                return false;
            },
            .@"for" => |*for_stmt| {
                // For loops don't guarantee execution (iterable could be empty), so can't guarantee return
                _ = for_stmt;
                return false;
            },
            else => false,
        };
    }
    
    fn checkPattern(self: *TypeChecker, pattern: *SirsParser.Pattern, expected_type: Type, program: *Program) TypeCheckError!void {
        switch (pattern.*) {
            .literal => |literal| {
                // Check that literal type matches expected type
                const literal_type = switch (literal) {
                    .integer => Type{ .i32 = {} },
                    .float => Type{ .f64 = {} },
                    .string => Type{ .str = {} },
                    .boolean => Type{ .bool = {} },
                    .null => Type{ .void = {} }, // Simplified - should check specific optional type
                };
                
                if (!self.typesCompatible(literal_type, expected_type)) {
                    return TypeCheckError.TypeMismatch;
                }
            },
            
            .variable => |var_name| {
                // Variable patterns bind the value to a new variable
                // Add it to the symbol table with the expected type
                self.variables.put(var_name, expected_type) catch return TypeCheckError.OutOfMemory;
            },
            
            .wildcard => {
                // Wildcard matches anything, no type checking needed
            },
            
            .@"struct" => |*struct_patterns| {
                // Expected type must be a struct
                switch (expected_type) {
                    .@"struct" => |expected_struct_fields| {
                        // Check each field pattern
                        var pattern_iter = struct_patterns.iterator();
                        while (pattern_iter.next()) |pattern_entry| {
                            const field_name = pattern_entry.key_ptr.*;
                            const field_pattern = pattern_entry.value_ptr;
                            
                            // Find the expected field type
                            if (expected_struct_fields.get(field_name)) |expected_field_type| {
                                try self.checkPattern(field_pattern, expected_field_type.*, program);
                            } else {
                                // Field doesn't exist in the expected struct type
                                return TypeCheckError.TypeMismatch;
                            }
                        }
                    },
                    else => {
                        // Cannot match struct pattern against non-struct type
                        return TypeCheckError.TypeMismatch;
                    },
                }
            },
            
            .@"enum" => |*enum_pattern| {
                // Expected type must be the same enum
                switch (expected_type) {
                    .@"enum" => |expected_enum| {
                        // Check enum type name matches
                        if (!std.mem.eql(u8, enum_pattern.enum_type, expected_enum.name)) {
                            return TypeCheckError.TypeMismatch;
                        }
                        
                        // Check variant exists in the enum
                        if (expected_enum.variants.get(enum_pattern.variant)) |variant_type| {
                            // If there's an associated value, check the pattern matches the type
                            if (enum_pattern.value_pattern) |value_pattern| {
                                if (variant_type) |vtype| {
                                    try self.checkPattern(value_pattern, vtype.*, program);
                                } else {
                                    // Pattern expects a value but variant has none
                                    return TypeCheckError.TypeMismatch;
                                }
                            }
                        } else {
                            // Variant doesn't exist in the enum
                            return TypeCheckError.TypeMismatch;
                        }
                    },
                    else => {
                        // Cannot match enum pattern against non-enum type
                        return TypeCheckError.TypeMismatch;
                    },
                }
            },
        }
    }
    
    fn typesEqual(self: *TypeChecker, type1: Type, type2: Type) bool {
        
        switch (type1) {
            .void, .bool, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .f32, .f64, .str => {
                return std.meta.activeTag(type1) == std.meta.activeTag(type2);
            },
            .array => |array1| {
                if (type2 != .array) return false;
                const array2 = type2.array;
                return array1.size == array2.size and self.typesEqual(array1.element.*, array2.element.*);
            },
            .slice => |slice1| {
                if (type2 != .slice) return false;
                const slice2 = type2.slice;
                return self.typesEqual(slice1.element.*, slice2.element.*);
            },
            .optional => |opt1| {
                if (type2 != .optional) return false;
                const opt2 = type2.optional;
                return self.typesEqual(opt1.*, opt2.*);
            },
            .discriminated_union => |union1| {
                if (type2 != .discriminated_union) return false;
                const union2 = type2.discriminated_union;
                
                // Check if names match
                if (!std.mem.eql(u8, union1.name, union2.name)) return false;
                
                // Check if variant counts match
                if (union1.variants.items.len != union2.variants.items.len) return false;
                
                // Check if all variants match
                for (union1.variants.items, union2.variants.items) |var1, var2| {
                    if (!self.typesEqual(var1.*, var2.*)) return false;
                }
                
                return true;
            },
            else => {
                // For complex types, just compare tags for now
                return std.meta.activeTag(type1) == std.meta.activeTag(type2);
            },
        }
    }
    
    fn checkTraitImpl(self: *TypeChecker, trait_impl: *SirsParser.TraitImpl, program: *Program) TypeCheckError!void {
        // Check that the trait/interface exists
        if (!program.interfaces.contains(trait_impl.trait_name)) {
            std.debug.print("Error: Interface '{s}' not found\n", .{trait_impl.trait_name});
            return TypeCheckError.UndefinedFunction; // Reusing error type
        }
        
        const interface = program.interfaces.get(trait_impl.trait_name).?;
        
        // Check that all required methods are implemented
        var method_iter = interface.methods.iterator();
        while (method_iter.next()) |method_entry| {
            const method_name = method_entry.key_ptr.*;
            const required_signature = method_entry.value_ptr;
            
            if (!trait_impl.methods.contains(method_name)) {
                std.debug.print("Error: Method '{s}' required by interface '{s}' not implemented\n", .{ method_name, trait_impl.trait_name });
                return TypeCheckError.UndefinedFunction;
            }
            
            const implemented_function = trait_impl.methods.get(method_name).?;
            
            // Check that method signature matches (implementation includes 'self' parameter)
            if (implemented_function.args.items.len != required_signature.args.items.len + 1) {
                std.debug.print("Error: Method '{s}' has incorrect number of parameters\n", .{method_name});
                return TypeCheckError.ArgumentCountMismatch;
            }
            
            // Check argument types (skip first argument which is 'self')
            for (implemented_function.args.items[1..], required_signature.args.items) |impl_param, required_type| {
                if (!self.typesEqual(impl_param.type, required_type)) {
                    std.debug.print("Error: Method '{s}' parameter type mismatch\n", .{method_name});
                    return TypeCheckError.ArgumentTypeMismatch;
                }
            }
            
            // Check return type
            if (!self.typesEqual(implemented_function.@"return", required_signature.@"return")) {
                std.debug.print("Error: Method '{s}' return type mismatch\n", .{method_name});
                return TypeCheckError.ReturnTypeMismatch;
            }
            
            // Type check the method implementation
            try self.checkFunction(method_name, @constCast(&implemented_function), program);
        }
    }
    
    fn checkMixtureExpression(self: *TypeChecker, mixture_expr: anytype, program: *Program) TypeCheckError!Type {
        // Type check each component
        var total_weight: f64 = 0.0;
        
        for (mixture_expr.components.items) |*component| {
            // Check component weight
            const weight_type = try self.checkExpression(@constCast(component.weight), program);
            if (!self.isNumericType(weight_type)) {
                return TypeCheckError.TypeMismatch;
            }
            
            // Extract weight value if it's a literal
            if (component.weight.* == .literal and component.weight.literal == .float) {
                total_weight += component.weight.literal.float;
            }
            
            // Check that the distribution exists
            // For now, we check if it's a known built-in distribution
            const known_distributions = [_][]const u8{ "normal", "uniform", "gamma", "beta", "exponential", "bernoulli", "categorical", "dirichlet" };
            var found_distribution = false;
            for (known_distributions) |known_dist| {
                if (std.mem.eql(u8, component.distribution, known_dist)) {
                    found_distribution = true;
                    break;
                }
            }
            
            if (!found_distribution) {
                // Check if it's a custom distribution in the program
                if (!program.types.contains(component.distribution)) {
                    return TypeCheckError.UndefinedVariable;
                }
            }
            
            // Type check component parameters
            for (component.parameters.items) |param| {
                const param_type = try self.checkExpression(@constCast(param), program);
                if (!self.isNumericType(param_type)) {
                    return TypeCheckError.ArgumentTypeMismatch;
                }
            }
        }
        
        // Check weight prior if provided
        if (mixture_expr.weight_prior) |prior| {
            const prior_type = try self.checkExpression(@constCast(prior), program);
            _ = prior_type; // Use the type to avoid unused variable warning
            // Weight prior should be a sampling expression
            if (prior.* != .sample) {
                return TypeCheckError.TypeMismatch;
            }
        }
        
        // Mixture models return a distribution type
        return Type{
            .distribution = .{
                .kind = .categorical, // Mixtures are categorical over components
                .param_types = ArrayList(Type).init(self.allocator),
            },
        };
    }
    
    fn checkHierarchicalExpression(self: *TypeChecker, hierarchical_expr: anytype, program: *Program) TypeCheckError!Type {
        // Type check each hierarchical level
        for (hierarchical_expr.levels.items) |*level| {
            // Check that group variable is accessible
            // For now, we assume group variables are valid
            
            // Type check parameters at this level
            var param_iter = level.parameters.iterator();
            while (param_iter.next()) |entry| {
                const param_spec = entry.value_ptr;
                
                // Check that distribution family is known
                const known_distributions = [_][]const u8{ "normal", "uniform", "gamma", "beta", "exponential", "bernoulli", "categorical", "dirichlet" };
                var found_distribution = false;
                for (known_distributions) |known_dist| {
                    if (std.mem.eql(u8, param_spec.distribution, known_dist)) {
                        found_distribution = true;
                        break;
                    }
                }
                
                if (!found_distribution) {
                    return TypeCheckError.UndefinedVariable;
                }
                
                // Check that hyperparameters are defined
                for (param_spec.hyperparameters.items) |hyperparam_name| {
                    if (!level.hyperpriors.contains(hyperparam_name)) {
                        // Hyperparameter not defined in hyperpriors
                        return TypeCheckError.UndefinedVariable;
                    }
                }
            }
            
            // Type check hyperprior expressions
            var hyperprior_iter = level.hyperpriors.iterator();
            while (hyperprior_iter.next()) |entry| {
                const prior_expr = entry.value_ptr;
                const prior_type = try self.checkExpression(@constCast(prior_expr.*), program);
                
                // Hyperpriors should typically be sampling expressions or numeric literals
                switch (prior_expr.*.*) {
                    .sample => {
                        // Valid: sampling expression like sample(normal(0, 1))
                    },
                    .literal => {
                        // Valid: numeric literal
                    },
                    else => {
                        // TODO: Consider warning or erroring for non-sample/literal hyperpriors
                        // Other expression types (variables, function calls) may be valid in some contexts
                        // but typically hyperpriors should be explicit distributions or constants
                    },
                }
                
                _ = prior_type; // Use the type to avoid unused variable warning
            }
        }
        
        // Type check observation model
        const obs_model = &hierarchical_expr.observation_model;
        
        // Check that likelihood distribution is known
        const known_distributions = [_][]const u8{ "normal", "uniform", "gamma", "beta", "exponential", "bernoulli", "categorical" };
        var found_likelihood = false;
        for (known_distributions) |known_dist| {
            if (std.mem.eql(u8, obs_model.likelihood, known_dist)) {
                found_likelihood = true;
                break;
            }
        }
        
        if (!found_likelihood) {
            return TypeCheckError.UndefinedVariable;
        }
        
        // Check that all observation model parameters exist in hierarchical levels
        for (obs_model.parameters.items) |param_name| {
            var found_param = false;
            
            for (hierarchical_expr.levels.items) |*level| {
                if (level.parameters.contains(param_name)) {
                    found_param = true;
                    break;
                }
            }
            
            if (!found_param) {
                return TypeCheckError.UndefinedVariable;
            }
        }
        
        // Hierarchical models return a specialized hierarchical distribution type
        return Type{
            .distribution = .{
                .kind = .normal, // Default to normal for now
                .param_types = ArrayList(Type).init(self.allocator),
            },
        };
    }
};