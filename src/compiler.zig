const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const SirsParser = @import("sirs.zig");
const SevSimpleParser = @import("sev_simple.zig").SevSimpleParser;
const TypeChecker = @import("typechecker.zig").TypeChecker;
const CodeGen = @import("codegen.zig").CodeGen;
const McpServer = @import("mcp.zig").McpServer;
const ErrorReporter = @import("error_reporter.zig").ErrorReporter;
const CirLowering = @import("cir.zig").CirLowering;
const OptimizationManager = @import("optimization.zig").OptimizationManager;

pub const CompilerError = error{
    FileNotFound,
    ParseError,
    TypeCheckError,
    CirLoweringError,
    CodeGenError,
    IoError,
};

pub const SeverCompiler = struct {
    allocator: Allocator,
    parser: SirsParser.Parser,
    type_checker: TypeChecker,
    code_gen: CodeGen,
    error_reporter: ErrorReporter,
    optimization_manager: OptimizationManager,
    
    pub fn init(allocator: Allocator) SeverCompiler {
        return SeverCompiler{
            .allocator = allocator,
            .parser = SirsParser.Parser.init(allocator),
            .type_checker = TypeChecker.init(allocator),
            .code_gen = CodeGen.init(allocator),
            .error_reporter = ErrorReporter.init(allocator),
            .optimization_manager = OptimizationManager.init(allocator),
        };
    }
    
    pub fn deinit(self: *SeverCompiler) void {
        self.type_checker.deinit();
        self.code_gen.deinit();
        self.error_reporter.deinit();
        self.optimization_manager.deinit();
    }
    
    pub fn compile(self: *SeverCompiler, input_file: []const u8) !void {
        // Clear any previous errors
        self.error_reporter.clear();
        try self.error_reporter.setCurrentFile(input_file);
        
        print("Building Sever program: {s}\n", .{input_file});
        
        print("Phase 1: Reading SIRS file...\n", .{});
        const content = self.readFile(input_file) catch |err| switch (err) {
            error.FileNotFound => {
                try self.error_reporter.reportErrorWithHint(
                    null,
                    "File '{s}' not found",
                    .{input_file},
                    "Make sure the file path is correct and the file exists",
                    .{}
                );
                self.error_reporter.printAllErrors();
                return CompilerError.FileNotFound;
            },
            else => {
                try self.error_reporter.reportError(
                    null,
                    "Failed to read file '{s}': {s}",
                    .{ input_file, @errorName(err) }
                );
                self.error_reporter.printAllErrors();
                return CompilerError.IoError;
            },
        };
        defer self.allocator.free(content);
        
        print("Phase 2: Parsing program...\n", .{});
        var program = self.parseProgram(input_file, content) catch |err| {
            try self.error_reporter.reportErrorWithHint(
                null,
                "Failed to parse program file: {s}",
                .{@errorName(err)},
                "Check that the file syntax is valid and follows the specification",
                .{}
            );
            self.error_reporter.printAllErrors();
            return CompilerError.ParseError;
        };
        defer program.deinit();
        
        print("Phase 3: Type checking...\n", .{});
        self.type_checker.check(&program) catch |err| {
            try self.error_reporter.reportErrorWithHint(
                null,
                "Type checking failed: {s}",
                .{@errorName(err)},
                "Check variable types, function signatures, and expression compatibility",
                .{}
            );
            self.error_reporter.printAllErrors();
            return CompilerError.TypeCheckError;
        };
        
        print("Phase 4: Lowering to CIR...\n", .{});
        var cir_lowering = CirLowering.init(self.allocator, &self.error_reporter, "main");
        defer cir_lowering.deinit();
        
        var cir_module = cir_lowering.lower(&program) catch |err| {
            try self.error_reporter.reportErrorWithHint(
                null,
                "CIR lowering failed: {s}",
                .{@errorName(err)},
                "Check that all language constructs are supported in CIR",
                .{}
            );
            self.error_reporter.printAllErrors();
            return CompilerError.CirLoweringError;
        };
        
        print("Phase 5: Optimizing code...\n", .{});
        self.optimization_manager.optimize(&cir_module) catch |err| {
            try self.error_reporter.reportErrorWithHint(
                null,
                "Optimization failed: {s}",
                .{@errorName(err)},
                "Check optimization passes for errors",
                .{}
            );
            self.error_reporter.printAllErrors();
            return CompilerError.CodeGenError;
        };
        
        print("Phase 6: Generating code...\n", .{});
        const output_file = try self.getOutputFilename(input_file);
        defer self.allocator.free(output_file);
        
        self.code_gen.generate(&program, output_file) catch |err| {
            try self.error_reporter.reportErrorWithHint(
                null,
                "Code generation failed: {s}",
                .{@errorName(err)},
                "Check that all expressions and statements are supported",
                .{}
            );
            self.error_reporter.printAllErrors();
            return CompilerError.CodeGenError;
        };
        
        const error_count = self.error_reporter.getErrorCount();
        const warning_count = self.error_reporter.getWarningCount();
        
        if (error_count > 0) {
            print("Compilation failed with {} error(s)", .{error_count});
            if (warning_count > 0) {
                print(" and {} warning(s)", .{warning_count});
            }
            print("\n", .{});
            self.error_reporter.printAllErrors();
            return CompilerError.CodeGenError;
        }
        
        print("Compilation complete: {s}", .{output_file});
        if (warning_count > 0) {
            print(" (with {} warning(s))", .{warning_count});
        }
        print("\n", .{});
        
        if (warning_count > 0) {
            self.error_reporter.printAllErrors();
        }
    }
    
    pub fn test_program(self: *SeverCompiler, input_file: []const u8) !void {
        // Clear any previous errors
        self.error_reporter.clear();
        try self.error_reporter.setCurrentFile(input_file);
        
        print("Testing Sever program: {s}\n", .{input_file});
        
        // First compile the program
        print("Phase 1: Compiling program...\n", .{});
        self.compile(input_file) catch |err| {
            try self.error_reporter.reportError(
                null,
                "Compilation failed during testing: {s}",
                .{@errorName(err)}
            );
            self.error_reporter.printAllErrors();
            return err;
        };
        
        // Get the output executable name
        const output_file = try self.getOutputFilename(input_file);
        defer self.allocator.free(output_file);
        
        // Check if the file exists before trying to execute it (output_file already includes dist/ path)
        const file_stat = std.fs.cwd().statFile(output_file) catch |err| {
            try self.error_reporter.reportErrorWithHint(
                null,
                "Compiled executable '{s}' not found: {s}",
                .{ output_file, @errorName(err) },
                "Make sure the compilation succeeded and the file was created",
                .{}
            );
            self.error_reporter.printAllErrors();
            return CompilerError.CodeGenError;
        };
        _ = file_stat;
        
        // Run the compiled program and capture output
        print("Phase 2: Executing test cases...\n", .{});
        const result = std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = &[_][]const u8{output_file},
        }) catch |err| {
            try self.error_reporter.reportErrorWithHint(
                null,
                "Failed to execute compiled program: {s}",
                .{@errorName(err)},
                "Make sure the program compiled successfully and is executable",
                .{}
            );
            self.error_reporter.printAllErrors();
            return CompilerError.CodeGenError;
        };
        defer self.allocator.free(result.stdout);
        defer self.allocator.free(result.stderr);
        
        // Check if the program executed successfully
        if (result.term.Exited != 0) {
            try self.error_reporter.reportErrorWithHint(
                null,
                "Program exited with non-zero status: {}",
                .{result.term.Exited},
                "Check the program logic and ensure all assertions pass",
                .{}
            );
            
            if (result.stderr.len > 0) {
                print("Error output:\n{s}\n", .{result.stderr});
            }
            
            self.error_reporter.printAllErrors();
            return CompilerError.CodeGenError;
        }
        
        // Parse and validate test output
        try self.validateTestOutput(result.stdout);
        
        const error_count = self.error_reporter.getErrorCount();
        const warning_count = self.error_reporter.getWarningCount();
        
        if (error_count > 0) {
            print("Tests failed with {} error(s)", .{error_count});
            if (warning_count > 0) {
                print(" and {} warning(s)", .{warning_count});
            }
            print("\n", .{});
            self.error_reporter.printAllErrors();
            return CompilerError.CodeGenError;
        }
        
        print("All tests passed successfully!", .{});
        if (warning_count > 0) {
            print(" (with {} warning(s))", .{warning_count});
        }
        print("\n", .{});
        
        if (warning_count > 0) {
            self.error_reporter.printAllErrors();
        }
    }
    
    pub fn serve(self: *SeverCompiler) !void {
        print("Starting MCP server...\n", .{});
        
        var mcp_server = McpServer.init(self.allocator);
        defer mcp_server.deinit();
        
        try mcp_server.start();
    }
    
    /// Parse a program from either SIRS JSON or SEV format based on file extension
    fn parseProgram(self: *SeverCompiler, filename: []const u8, content: []const u8) !SirsParser.Program {
        if (std.mem.endsWith(u8, filename, ".sev")) {
            // Parse SEV format
            var sev_parser = SevSimpleParser.init(self.allocator, content);
            return try sev_parser.parse();
        } else {
            // Default to SIRS JSON format
            return try self.parser.parse(content);
        }
    }
    
    fn readFile(self: *SeverCompiler, path: []const u8) ![]u8 {
        const file = std.fs.cwd().openFile(path, .{}) catch return error.FileNotFound;
        defer file.close();
        
        const file_size = try file.getEndPos();
        const content = try self.allocator.alloc(u8, file_size);
        _ = try file.readAll(content);
        
        return content;
    }
    
    fn getOutputFilename(self: *SeverCompiler, input_file: []const u8) ![]u8 {
        // Ensure dist/ directory exists
        std.fs.cwd().makeDir("dist") catch |err| switch (err) {
            error.PathAlreadyExists => {}, // Directory exists, that's fine
            else => return err,
        };
        
        // Convert input.sirs.json or input.sev to input or input.exe
        var base_name: []const u8 = input_file;
        
        if (std.mem.endsWith(u8, input_file, ".sirs.json")) {
            base_name = input_file[0..input_file.len - 10]; // Remove .sirs.json
        } else if (std.mem.endsWith(u8, input_file, ".sev")) {
            base_name = input_file[0..input_file.len - 4]; // Remove .sev
        }
        
        // Extract just the basename (remove any path components)
        const filename = std.fs.path.basename(base_name);
        
        // Add platform-specific extension if needed and put in dist/
        if (@import("builtin").os.tag == .windows) {
            return try std.fmt.allocPrint(self.allocator, "dist/{s}.exe", .{filename});
        }
        
        return try std.fmt.allocPrint(self.allocator, "dist/{s}", .{filename});
    }
    
    pub fn generate_docs(self: *SeverCompiler, input_file: []const u8) !void {
        // Clear any previous errors
        self.error_reporter.clear();
        try self.error_reporter.setCurrentFile(input_file);
        
        print("Generating documentation for: {s}\n", .{input_file});
        
        print("Phase 1: Reading SIRS file...\n", .{});
        const content = self.readFile(input_file) catch |err| switch (err) {
            error.FileNotFound => {
                try self.error_reporter.reportErrorWithHint(
                    null,
                    "File '{s}' not found",
                    .{input_file},
                    "Make sure the file path is correct and the file exists",
                    .{}
                );
                self.error_reporter.printAllErrors();
                return CompilerError.FileNotFound;
            },
            else => return err,
        };
        defer self.allocator.free(content);
        
        print("Phase 2: Parsing SIRS...\n", .{});
        var program = self.parser.parse(content) catch |err| {
            try self.error_reporter.reportErrorWithHint(
                null,
                "Failed to parse SIRS program: {s}",
                .{@errorName(err)},
                "Check the JSON syntax and SIRS program structure",
                .{}
            );
            self.error_reporter.printAllErrors();
            return CompilerError.ParseError;
        };
        defer program.deinit();
        
        print("Phase 3: Generating documentation...\n", .{});
        try self.generateDocumentation(&program, input_file);
        
        print("Documentation generated successfully\n", .{});
    }
    
    fn generateDocumentation(self: *SeverCompiler, program: *SirsParser.Program, input_file: []const u8) !void {
        const output_filename = try self.getDocOutputFilename(input_file);
        defer self.allocator.free(output_filename);
        
        const file = std.fs.cwd().createFile(output_filename, .{}) catch |err| {
            try self.error_reporter.reportErrorWithHint(
                null,
                "Failed to create documentation file '{s}': {s}",
                .{ output_filename, @errorName(err) },
                "Check file permissions and available disk space",
                .{}
            );
            return CompilerError.IoError;
        };
        defer file.close();
        
        var writer = file.writer();
        
        // Generate markdown documentation
        try writer.print("# Documentation for {s}\n\n", .{std.fs.path.basename(input_file)});
        try writer.print("Generated automatically by Sever compiler\n\n", .{});
        
        // Document program structure
        try writer.print("## Program Overview\n\n", .{});
        try writer.print("- **Entry point**: `{s}()`\n", .{program.entry});
        try writer.print("- **Functions**: {} defined\n", .{program.functions.count()});
        try writer.print("- **Types**: {} defined\n", .{program.types.count()});
        
        if (program.interfaces.count() > 0) {
            try writer.print("- **Interfaces**: {} defined\n", .{program.interfaces.count()});
        }
        
        if (program.trait_impls.items.len > 0) {
            try writer.print("- **Trait implementations**: {} defined\n", .{program.trait_impls.items.len});
        }
        
        if (program.constants.count() > 0) {
            try writer.print("- **Constants**: {} defined\n", .{program.constants.count()});
        }
        
        try writer.print("\n", .{});
        
        // Document functions
        try writer.print("## Functions\n\n", .{});
        var func_iter = program.functions.iterator();
        while (func_iter.next()) |entry| {
            const func_name = entry.key_ptr.*;
            const function = entry.value_ptr;
            
            try writer.print("### `{s}(", .{func_name});
            
            // Document function parameters
            for (function.args.items, 0..) |param, i| {
                if (i > 0) try writer.print(", ", .{});
                try writer.print("{s}: {s}", .{ param.name, self.typeToString(param.type) });
            }
            
            try writer.print(") -> {s}`\n\n", .{self.typeToString(function.@"return")});
            
            // Add function type parameters if any
            if (function.type_params) |type_params| {
                if (type_params.items.len > 0) {
                    try writer.print("**Type parameters**: ", .{});
                    for (type_params.items, 0..) |param, i| {
                        if (i > 0) try writer.print(", ", .{});
                        try writer.print("`{s}`", .{param});
                    }
                    try writer.print("\n\n", .{});
                }
            }
            
            try writer.print("Function with {} statement(s)\n\n", .{function.body.items.len});
        }
        
        // Document types
        if (program.types.count() > 0) {
            try writer.print("## Types\n\n", .{});
            var type_iter = program.types.iterator();
            while (type_iter.next()) |entry| {
                const type_name = entry.key_ptr.*;
                const type_def = entry.value_ptr;
                
                try writer.print("### `{s}`\n\n", .{type_name});
                try writer.print("Type: {s}\n\n", .{self.typeToString(type_def.*)});
            }
        }
        
        // Document interfaces
        if (program.interfaces.count() > 0) {
            try writer.print("## Interfaces\n\n", .{});
            var interface_iter = program.interfaces.iterator();
            while (interface_iter.next()) |entry| {
                const interface_name = entry.key_ptr.*;
                const interface_def = entry.value_ptr;
                
                try writer.print("### `{s}`\n\n", .{interface_name});
                
                if (interface_def.type_params) |type_params| {
                    if (type_params.items.len > 0) {
                        try writer.print("**Type parameters**: ", .{});
                        for (type_params.items, 0..) |param, i| {
                            if (i > 0) try writer.print(", ", .{});
                            try writer.print("`{s}`", .{param});
                        }
                        try writer.print("\n\n", .{});
                    }
                }
                
                try writer.print("Methods:\n", .{});
                var method_iter = interface_def.methods.iterator();
                while (method_iter.next()) |method_entry| {
                    const method_name = method_entry.key_ptr.*;
                    const method_sig = method_entry.value_ptr;
                    
                    try writer.print("- `{s}(", .{method_name});
                    for (method_sig.args.items, 0..) |arg_type, i| {
                        if (i > 0) try writer.print(", ", .{});
                        try writer.print("{s}", .{self.typeToString(arg_type)});
                    }
                    try writer.print(") -> {s}`\n", .{self.typeToString(method_sig.@"return")});
                }
                try writer.print("\n", .{});
            }
        }
        
        print("Documentation written to: {s}\n", .{output_filename});
    }
    
    fn getDocOutputFilename(self: *SeverCompiler, input_file: []const u8) ![]u8 {
        // Get the directory of the input file
        const dirname = std.fs.path.dirname(input_file) orelse ".";
        
        // Get the basename and remove extension
        const basename = std.fs.path.basename(input_file);
        const extension_start = std.mem.lastIndexOf(u8, basename, ".") orelse basename.len;
        const base_name = basename[0..extension_start];
        
        // Create the .md file in the same directory as the input file
        return try std.fmt.allocPrint(self.allocator, "{s}/{s}.md", .{ dirname, base_name });
    }

    pub fn repl(self: *SeverCompiler) !void {
        const stdin = std.io.getStdIn().reader();
        const stdout = std.io.getStdOut().writer();
        var buffer: [4096]u8 = undefined;
        var repl_counter: u32 = 0;
        
        try stdout.writeAll("Sever REPL - Interactive Mode\n");
        try stdout.writeAll("Type SIRS JSON expressions or 'help' for assistance\n");
        try stdout.writeAll("Use 'exit' to quit\n\n");
        
        while (true) {
            // Print prompt
            try stdout.writeAll("sever> ");
            
            // Read input
            if (try stdin.readUntilDelimiterOrEof(buffer[0..], '\n')) |input| {
                const trimmed_input = std.mem.trim(u8, input, " \t\r\n");
                
                // Check for exit commands
                if (std.mem.eql(u8, trimmed_input, "exit") or 
                   std.mem.eql(u8, trimmed_input, "quit")) {
                    try stdout.writeAll("Goodbye!\n");
                    break;
                }
                
                // Skip empty lines
                if (trimmed_input.len == 0) {
                    continue;
                }
                
                // Check for help command
                if (std.mem.eql(u8, trimmed_input, "help")) {
                    try self.printReplHelp();
                    continue;
                }
                
                // Try to evaluate the input as a SIRS expression
                try self.evalReplInput(trimmed_input, repl_counter);
                repl_counter += 1;
            } else {
                // EOF reached (Ctrl+D)
                try stdout.writeAll("\nGoodbye!\n");
                break;
            }
        }
    }
    
    fn printReplHelp(self: *SeverCompiler) !void {
        _ = self;
        const stdout = std.io.getStdOut().writer();
        try stdout.writeAll("\nSever REPL Help:\n");
        try stdout.writeAll("- Type SIRS JSON expressions to evaluate them\n");
        try stdout.writeAll("- Examples:\n");
        try stdout.writeAll("  42\n");
        try stdout.writeAll("  {\"literal\": 100}\n");
        try stdout.writeAll("  {\"op\": {\"kind\": \"add\", \"args\": [{\"literal\": 10}, {\"literal\": 20}]}}\n");
        try stdout.writeAll("  {\"call\": {\"function\": \"std_print\", \"args\": [{\"literal\": \"Hello!\"}]}}\n");
        try stdout.writeAll("- Commands:\n");
        try stdout.writeAll("  help - Show this help\n");
        try stdout.writeAll("  exit, quit - Exit REPL\n");
        try stdout.writeAll("\n");
    }
    
    fn evalReplInput(self: *SeverCompiler, input: []const u8, counter: u32) !void {
        // Clear any previous errors
        self.error_reporter.clear();
        
        // For simple literals, wrap them appropriately
        var expression_json: []u8 = undefined;
        var should_free_expression = false;
        
        if (std.mem.startsWith(u8, input, "{")) {
            // Already JSON object
            expression_json = @constCast(input);
        } else {
            // Simple literal, wrap as {"literal": value}
            expression_json = try std.fmt.allocPrint(self.allocator, "{{\"literal\": {s}}}", .{input});
            should_free_expression = true;
        }
        defer if (should_free_expression) self.allocator.free(expression_json);
        
        // Create a temporary SIRS program with the expression
        const repl_program_template =
            \\{{"program":{{"entry":"main","functions":{{"main":{{"args":[],"return":"i32","body":[{{"expression":{s}}},{{"return":{{"literal":0}}}}]}}}}}}}}
        ;
        
        const repl_program_json = try std.fmt.allocPrint(
            self.allocator, 
            repl_program_template, 
            .{expression_json}
        );
        defer self.allocator.free(repl_program_json);
        
        // Parse and execute the temporary program
        var program = self.parser.parse(repl_program_json) catch |err| {
            print("Parse error: {s}\n", .{@errorName(err)});
            return;
        };
        defer program.deinit();
        
        // Type check
        self.type_checker.check(&program) catch |err| {
            print("Type error: {s}\n", .{@errorName(err)});
            return;
        };
        
        // Generate and compile
        const temp_filename = try std.fmt.allocPrint(self.allocator, "dist/repl_temp_{d}", .{counter});
        defer self.allocator.free(temp_filename);
        
        self.code_gen.generate(&program, temp_filename) catch |err| {
            print("Codegen error: {s}\n", .{@errorName(err)});
            return;
        };
        
        // Execute the compiled program and show output
        const result = std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = &[_][]const u8{temp_filename},
        }) catch |err| {
            print("Execution error: {s}\n", .{@errorName(err)});
            return;
        };
        defer self.allocator.free(result.stdout);
        defer self.allocator.free(result.stderr);
        
        // Print any output
        if (result.stdout.len > 0) {
            print("{s}", .{result.stdout});
        }
        if (result.stderr.len > 0) {
            print("stderr: {s}", .{result.stderr});
        }
        
        // Clean up temporary executable
        std.fs.cwd().deleteFile(temp_filename) catch {};
    }
    
    fn typeToString(self: *SeverCompiler, type_info: SirsParser.Type) []const u8 {
        _ = self;
        return switch (type_info) {
            .void => "void",
            .bool => "bool",
            .i8, .i16, .i32, .i64 => "int",
            .u8, .u16, .u32, .u64 => "uint",
            .f32, .f64 => "float",
            .str => "str",
            .array => "Array",
            .slice => "Slice",
            .@"struct" => "Struct",
            .@"union" => "Union",
            .discriminated_union => |d| d.name,
            .@"enum" => |e| e.name,
            .@"error" => |e| e.name,
            .hashmap => "HashMap",
            .set => "Set",
            .tuple => "Tuple",
            .record => |r| r.name,
            .optional => "Optional",
            .function => "Function",
            .future => "Future",
            .distribution => "Distribution",
            .type_parameter => |tp| tp,
            .generic_instance => |g| g.base_type,
            .@"interface" => |i| i.name,
            .trait_object => |t| t.trait_name,
            .result => "Result",
        };
    }

    fn validateTestOutput(self: *SeverCompiler, output: []const u8) !void {
        // For now, we'll implement a simple test validation:
        // - If the program outputs anything to stdout, we consider it a test result
        // - Empty output means the test passed silently
        // - In the future, we could implement a more sophisticated test format
        
        print("Program output {d} bytes:\n", .{output.len});
        if (output.len > 0) {
            print("{s}\n", .{output});
        } else {
            print("no output\n", .{});
        }
        
        // Check for common test patterns or error indicators
        if (std.mem.indexOf(u8, output, "error") != null or 
           std.mem.indexOf(u8, output, "failed") != null or
           std.mem.indexOf(u8, output, "FAIL") != null) {
            try self.error_reporter.reportWarning(
                null,
                "Program output contains potential error indicators",
                .{}
            );
        }
        
        // In a more advanced implementation, we could:
        // 1. Parse structured test output (JSON/TAP format)
        // 2. Look for specific test assertions
        // 3. Count passed/failed test cases
        // 4. Generate test reports
    }
};