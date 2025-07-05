const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const SirsParser = @import("sirs.zig");
const Program = SirsParser.Program;
const Function = SirsParser.Function;
const Statement = SirsParser.Statement;
const Expression = SirsParser.Expression;
const Type = SirsParser.Type;

pub const CodeGenError = error{
    UnsupportedType,
    UnsupportedExpression,
    UnsupportedStatement,
    IoError,
    CompilationError,
    OutOfMemory,
};

pub const CodeGen = struct {
    allocator: Allocator,
    output: ArrayList(u8),
    indent_level: u32,
    current_function_name: ?[]const u8,
    current_function: ?*Function,
    
    pub fn init(allocator: Allocator) CodeGen {
        return CodeGen{
            .allocator = allocator,
            .output = ArrayList(u8).init(allocator),
            .indent_level = 0,
            .current_function_name = null,
            .current_function = null,
        };
    }
    
    pub fn deinit(self: *CodeGen) void {
        self.output.deinit();
    }
    
    pub fn generate(self: *CodeGen, program: *Program, output_file: []const u8) CodeGenError!void {
        // Clear output buffer
        self.output.clearRetainingCapacity();
        self.indent_level = 0;
        
        // Generate Zig code for the program
        try self.generateProgram(program);
        
        // Write to temporary Zig file
        const temp_zig_file = std.fmt.allocPrint(self.allocator, "{s}.zig", .{output_file}) catch return CodeGenError.OutOfMemory;
        defer self.allocator.free(temp_zig_file);
        
        const file = std.fs.cwd().createFile(temp_zig_file, .{}) catch return CodeGenError.IoError;
        defer file.close();
        
        file.writeAll(self.output.items) catch return CodeGenError.IoError;
        
        // Compile the Zig file to native binary
        try self.compileZigFile(temp_zig_file, output_file);
        
        // Clean up temporary file
        std.fs.cwd().deleteFile(temp_zig_file) catch {};
    }
    
    pub fn generateProgram(self: *CodeGen, program: *Program) CodeGenError!void {
        // Generate standard library imports
        try self.writeLine("const std = @import(\"std\");");
        try self.writeLine("const debug_print = std.debug.print;");
        try self.writeLine("const Allocator = std.mem.Allocator;");
        try self.writeLine("const math = std.math;");
        try self.writeLine("const time = std.time;");
        try self.writeLine("const Thread = std.Thread;");
        try self.writeLine("const Mutex = std.Thread.Mutex;");
        try self.writeLine("const ArrayList = std.ArrayList;");
        try self.writeLine("");
        
        // Generate custom type definitions (enums and errors)
        try self.generateCustomTypes(program);
        
        // Generate embedded runtime functions
        try self.writeLine("// Embedded Sever Runtime Functions");
        try self.writeLine("var gpa = std.heap.GeneralPurposeAllocator(.{}){};");
        try self.writeLine("var allocator = gpa.allocator();");
        try self.writeLine("var prng = std.Random.DefaultPrng.init(0);");
        try self.writeLine("var random = prng.random();");
        try self.writeLine("var async_runtime: ?AsyncRuntime = null;");
        try self.writeLine("");
        
        // Generate async runtime types inline
        try self.generateAsyncRuntime();
        
        try self.writeLine("fn sever_runtime_init(seed: ?u64) void {");
        try self.writeLine("    const actual_seed = seed orelse @as(u64, @intCast(time.timestamp()));");
        try self.writeLine("    prng = std.Random.DefaultPrng.init(actual_seed);");
        try self.writeLine("    random = prng.random();");
        try self.writeLine("    // Initialize async runtime");
        try self.writeLine("    async_runtime = AsyncRuntime.init(allocator) catch null;");
        try self.writeLine("}");
        try self.writeLine("");
        try self.writeLine("fn sample(distribution: []const u8, params: []const f64) f64 {");
        try self.writeLine("    if (std.mem.eql(u8, distribution, \"uniform\")) {");
        try self.writeLine("        const min = params[0];");
        try self.writeLine("        const max = params[1];");
        try self.writeLine("        return min + random.float(f64) * (max - min);");
        try self.writeLine("    } else if (std.mem.eql(u8, distribution, \"normal\")) {");
        try self.writeLine("        const mean = params[0];");
        try self.writeLine("        const std_dev = params[1];");
        try self.writeLine("        const rand1 = random.float(f64);");
        try self.writeLine("        const rand2 = random.float(f64);");
        try self.writeLine("        const z0 = math.sqrt(-2.0 * math.log(f64, math.e, rand1)) * math.cos(2.0 * math.pi * rand2);");
        try self.writeLine("        return mean + std_dev * z0;");
        try self.writeLine("    }");
        try self.writeLine("    return 0.0; // Default case");
        try self.writeLine("}");
        try self.writeLine("");
        try self.writeLine("fn observe(distribution: []const u8, params: []const f64, value: f64) void {");
        try self.writeLine("    _ = distribution; _ = params; _ = value; // TODO: Implement");
        try self.writeLine("}");
        try self.writeLine("");
        try self.writeLine("fn prob_assert(condition: bool, confidence: f64) void {");
        try self.writeLine("    _ = confidence;");
        try self.writeLine("    if (!condition) @panic(\"Probabilistic assertion failed\");");
        try self.writeLine("}");
        try self.writeLine("");
        
        // Generate standard library helper functions
        try self.writeLine("fn std_print(message: []const u8) void {");
        try self.writeLine("    debug_print(\"{s}\\n\", .{message});");
        try self.writeLine("}");
        try self.writeLine("");
        try self.writeLine("fn std_print_int(value: i64) void {");
        try self.writeLine("    debug_print(\"{d}\\n\", .{value});");
        try self.writeLine("}");
        try self.writeLine("");
        try self.writeLine("fn std_print_float(value: f64) void {");
        try self.writeLine("    debug_print(\"{d}\\n\", .{value});");
        try self.writeLine("}");
        try self.writeLine("");
        
        // Debug functions
        try self.writeLine("fn debug_trace(function_name: []const u8, value: i64) void {");
        try self.writeLine("    debug_print(\"DEBUG TRACE: {s}() - value: {d}\\n\", .{function_name, value});");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn debug_breakpoint(file: []const u8, line: i32, message: []const u8) void {");
        try self.writeLine("    debug_print(\"BREAKPOINT: {s}:{d} - {s}\\n\", .{file, line, message});");
        try self.writeLine("    // In a full implementation, this would interact with the debugger");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn debug_variable(name: []const u8, value: []const u8) void {");
        try self.writeLine("    debug_print(\"VAR: {s} = {s}\\n\", .{name, value});");
        try self.writeLine("}");
        try self.writeLine("");
        // Add string concatenation function that handles null termination
        try self.writeLine("fn string_concat_z(a: [:0]const u8, b: [:0]const u8) [:0]const u8 {");
        try self.writeLine("    const result = std.fmt.allocPrintZ(allocator, \"{s}{s}\", .{a, b}) catch return \"<concat_error>\";");
        try self.writeLine("    return result;");
        try self.writeLine("}");
        try self.writeLine("");
        
        // Add async I/O functions
        try self.writeLine("// Async I/O Functions");
        try self.writeLine("fn async_http_get(url: []const u8) []const u8 {");
        try self.writeLine("    // Simulate async HTTP GET");
        try self.writeLine("    std.time.sleep(100_000_000); // 100ms delay");
        try self.writeLine("    return http_get(url);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn async_file_read(path: []const u8) []const u8 {");
        try self.writeLine("    // Simulate async file read");
        try self.writeLine("    std.time.sleep(50_000_000); // 50ms delay");
        try self.writeLine("    return file_read(path);");
        try self.writeLine("}");
        try self.writeLine("");
        
        // Add date/time functions
        try self.writeLine("// Date/Time Functions");
        try self.writeLine("fn datetime_now() i64 {");
        try self.writeLine("    return time.timestamp();");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_now_millis() i64 {");
        try self.writeLine("    return time.milliTimestamp();");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_now_micros() i64 {");
        try self.writeLine("    return time.microTimestamp();");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_format(timestamp: i64, format: []const u8) []const u8 {");
        try self.writeLine("    _ = format; // Simplified for now");
        try self.writeLine("    const result = std.fmt.allocPrint(allocator, \"{d}\", .{timestamp}) catch return \"<format_error>\";");
        try self.writeLine("    return result;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_parse(date_str: []const u8, format: []const u8) i64 {");
        try self.writeLine("    _ = format; // Simplified for now");
        try self.writeLine("    const parsed = std.fmt.parseInt(i64, date_str, 10) catch return 0;");
        try self.writeLine("    return parsed;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_add_seconds(timestamp: i64, seconds: i64) i64 {");
        try self.writeLine("    return timestamp + seconds;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_add_minutes(timestamp: i64, minutes: i64) i64 {");
        try self.writeLine("    return timestamp + (minutes * 60);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_add_hours(timestamp: i64, hours: i64) i64 {");
        try self.writeLine("    return timestamp + (hours * 3600);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_add_days(timestamp: i64, days: i64) i64 {");
        try self.writeLine("    return timestamp + (days * 86400);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_diff_seconds(timestamp1: i64, timestamp2: i64) i64 {");
        try self.writeLine("    return timestamp1 - timestamp2;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_year(timestamp: i64) i32 {");
        try self.writeLine("    // Simplified calculation - Unix timestamp to year");
        try self.writeLine("    const seconds_per_year: i64 = 365 * 24 * 3600;");
        try self.writeLine("    const years_since_1970 = @divTrunc(timestamp, seconds_per_year);");
        try self.writeLine("    return @intCast(1970 + years_since_1970);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_month(timestamp: i64) i32 {");
        try self.writeLine("    // Simplified calculation - returns month estimate");
        try self.writeLine("    const seconds_per_month: i64 = 30 * 24 * 3600; // Approximate");
        try self.writeLine("    const year_start = (datetime_year(timestamp) - 1970) * 365 * 24 * 3600;");
        try self.writeLine("    const month_offset = @divTrunc(timestamp - year_start, seconds_per_month);");
        try self.writeLine("    return @intCast(@mod(month_offset, 12) + 1);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_day(timestamp: i64) i32 {");
        try self.writeLine("    // Simplified calculation - returns day estimate");
        try self.writeLine("    const seconds_per_day: i64 = 24 * 3600;");
        try self.writeLine("    const day_offset = @divTrunc(timestamp, seconds_per_day);");
        try self.writeLine("    return @intCast(@mod(day_offset, 31) + 1);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_hour(timestamp: i64) i32 {");
        try self.writeLine("    const seconds_per_hour: i64 = 3600;");
        try self.writeLine("    const hour_offset = @divTrunc(timestamp, seconds_per_hour);");
        try self.writeLine("    return @intCast(@mod(hour_offset, 24));");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_minute(timestamp: i64) i32 {");
        try self.writeLine("    const seconds_per_minute: i64 = 60;");
        try self.writeLine("    const minute_offset = @divTrunc(timestamp, seconds_per_minute);");
        try self.writeLine("    return @intCast(@mod(minute_offset, 60));");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn datetime_second(timestamp: i64) i32 {");
        try self.writeLine("    return @intCast(@mod(timestamp, 60));");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn sleep_seconds(seconds: i64) void {");
        try self.writeLine("    const nanoseconds = @as(u64, @intCast(seconds * 1_000_000_000));");
        try self.writeLine("    std.time.sleep(nanoseconds);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn sleep_millis(millis: i64) void {");
        try self.writeLine("    const nanoseconds = @as(u64, @intCast(millis * 1_000_000));");
        try self.writeLine("    std.time.sleep(nanoseconds);");
        try self.writeLine("}");
        try self.writeLine("");
        
        // Regular expression functions
        try self.writeLine("// Regular Expression Functions");
        try self.writeLine("fn regex_match(text: []const u8, pattern: []const u8) bool {");
        try self.writeLine("    // Simple pattern matching - supports basic patterns:");
        try self.writeLine("    // \\\\d+ - one or more digits");
        try self.writeLine("    // \\\\w+ - one or more word characters");
        try self.writeLine("    // .* - any characters");
        try self.writeLine("    // literal strings");
        try self.writeLine("    ");
        try self.writeLine("    if (std.mem.eql(u8, pattern, \"\\\\d+\")) {");
        try self.writeLine("        return regex_match_digits(text);");
        try self.writeLine("    } else if (std.mem.eql(u8, pattern, \"\\\\w+\")) {");
        try self.writeLine("        return regex_match_word(text);");
        try self.writeLine("    } else if (std.mem.eql(u8, pattern, \".*\")) {");
        try self.writeLine("        return true; // .* matches everything");
        try self.writeLine("    } else {");
        try self.writeLine("        return std.mem.indexOf(u8, text, pattern) != null;");
        try self.writeLine("    }");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn regex_find(text: []const u8, pattern: []const u8) []const u8 {");
        try self.writeLine("    if (std.mem.eql(u8, pattern, \"\\\\d+\")) {");
        try self.writeLine("        return regex_extract_digits(text);");
        try self.writeLine("    } else if (std.mem.eql(u8, pattern, \"\\\\w+\")) {");
        try self.writeLine("        return regex_extract_word(text);");
        try self.writeLine("    } else if (std.mem.eql(u8, pattern, \".*\")) {");
        try self.writeLine("        return text; // .* matches entire text");
        try self.writeLine("    } else {");
        try self.writeLine("        if (std.mem.indexOf(u8, text, pattern)) |index| {");
        try self.writeLine("            const end = index + pattern.len;");
        try self.writeLine("            return allocator.dupe(u8, text[index..end]) catch return \"<error>\";");
        try self.writeLine("        }");
        try self.writeLine("        return \"\"; // No match");
        try self.writeLine("    }");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn regex_replace(text: []const u8, pattern: []const u8, replacement: []const u8) []const u8 {");
        try self.writeLine("    if (std.mem.eql(u8, pattern, \"\\\\d+\")) {");
        try self.writeLine("        return regex_replace_digits(text, replacement);");
        try self.writeLine("    } else if (std.mem.eql(u8, pattern, \"\\\\w+\")) {");
        try self.writeLine("        return regex_replace_word(text, replacement);");
        try self.writeLine("    } else {");
        try self.writeLine("        return std.mem.replaceOwned(u8, allocator, text, pattern, replacement) catch return \"<error>\";");
        try self.writeLine("    }");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn regex_split(text: []const u8, pattern: []const u8) []const u8 {");
        try self.writeLine("    // Simple split implementation - returns first part before match");
        try self.writeLine("    if (std.mem.indexOf(u8, text, pattern)) |index| {");
        try self.writeLine("        return allocator.dupe(u8, text[0..index]) catch return \"<error>\";");
        try self.writeLine("    }");
        try self.writeLine("    return allocator.dupe(u8, text) catch return \"<error>\";");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("// Helper functions for pattern matching");
        try self.writeLine("fn regex_match_digits(text: []const u8) bool {");
        try self.writeLine("    if (text.len == 0) return false;");
        try self.writeLine("    for (text) |char| {");
        try self.writeLine("        if (!std.ascii.isDigit(char)) return false;");
        try self.writeLine("    }");
        try self.writeLine("    return true;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn regex_match_word(text: []const u8) bool {");
        try self.writeLine("    if (text.len == 0) return false;");
        try self.writeLine("    for (text) |char| {");
        try self.writeLine("        if (!std.ascii.isAlphanumeric(char) and char != '_') return false;");
        try self.writeLine("    }");
        try self.writeLine("    return true;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn regex_extract_digits(text: []const u8) []const u8 {");
        try self.writeLine("    var start: ?usize = null;");
        try self.writeLine("    var end: usize = 0;");
        try self.writeLine("    ");
        try self.writeLine("    for (text, 0..) |char, i| {");
        try self.writeLine("        if (std.ascii.isDigit(char)) {");
        try self.writeLine("            if (start == null) start = i;");
        try self.writeLine("            end = i + 1;");
        try self.writeLine("        } else if (start != null) {");
        try self.writeLine("            break; // Found end of digit sequence");
        try self.writeLine("        }");
        try self.writeLine("    }");
        try self.writeLine("    ");
        try self.writeLine("    if (start) |s| {");
        try self.writeLine("        return allocator.dupe(u8, text[s..end]) catch return \"<error>\";");
        try self.writeLine("    }");
        try self.writeLine("    return \"\"; // No digits found");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn regex_extract_word(text: []const u8) []const u8 {");
        try self.writeLine("    var start: ?usize = null;");
        try self.writeLine("    var end: usize = 0;");
        try self.writeLine("    ");
        try self.writeLine("    for (text, 0..) |char, i| {");
        try self.writeLine("        if (std.ascii.isAlphanumeric(char) or char == '_') {");
        try self.writeLine("            if (start == null) start = i;");
        try self.writeLine("            end = i + 1;");
        try self.writeLine("        } else if (start != null) {");
        try self.writeLine("            break; // Found end of word");
        try self.writeLine("        }");
        try self.writeLine("    }");
        try self.writeLine("    ");
        try self.writeLine("    if (start) |s| {");
        try self.writeLine("        return allocator.dupe(u8, text[s..end]) catch return \"<error>\";");
        try self.writeLine("    }");
        try self.writeLine("    return \"\"; // No word found");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn regex_replace_digits(text: []const u8, replacement: []const u8) []const u8 {");
        try self.writeLine("    var result = std.ArrayList(u8).init(allocator);");
        try self.writeLine("    var i: usize = 0;");
        try self.writeLine("    ");
        try self.writeLine("    while (i < text.len) {");
        try self.writeLine("        if (std.ascii.isDigit(text[i])) {");
        try self.writeLine("            // Skip all consecutive digits");
        try self.writeLine("            while (i < text.len and std.ascii.isDigit(text[i])) {");
        try self.writeLine("                i += 1;");
        try self.writeLine("            }");
        try self.writeLine("            // Add replacement");
        try self.writeLine("            result.appendSlice(replacement) catch return \"<error>\";");
        try self.writeLine("        } else {");
        try self.writeLine("            result.append(text[i]) catch return \"<error>\";");
        try self.writeLine("            i += 1;");
        try self.writeLine("        }");
        try self.writeLine("    }");
        try self.writeLine("    ");
        try self.writeLine("    return result.toOwnedSlice() catch return \"<error>\";");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn regex_replace_word(text: []const u8, replacement: []const u8) []const u8 {");
        try self.writeLine("    var result = std.ArrayList(u8).init(allocator);");
        try self.writeLine("    var i: usize = 0;");
        try self.writeLine("    ");
        try self.writeLine("    while (i < text.len) {");
        try self.writeLine("        if (std.ascii.isAlphanumeric(text[i]) or text[i] == '_') {");
        try self.writeLine("            // Skip all consecutive word characters");
        try self.writeLine("            while (i < text.len and (std.ascii.isAlphanumeric(text[i]) or text[i] == '_')) {");
        try self.writeLine("                i += 1;");
        try self.writeLine("            }");
        try self.writeLine("            // Add replacement");
        try self.writeLine("            result.appendSlice(replacement) catch return \"<error>\";");
        try self.writeLine("        } else {");
        try self.writeLine("            result.append(text[i]) catch return \"<error>\";");
        try self.writeLine("            i += 1;");
        try self.writeLine("        }");
        try self.writeLine("    }");
        try self.writeLine("    ");
        try self.writeLine("    return result.toOwnedSlice() catch return \"<error>\";");
        try self.writeLine("}");
        try self.writeLine("");
        
        // FFI (Foreign Function Interface) support
        try self.writeLine("// FFI (Foreign Function Interface) Functions");
        try self.writeLine("const DynamicLibrary = std.DynLib;");
        try self.writeLine("var loaded_libraries = std.HashMap([]const u8, DynamicLibrary, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator);");
        try self.writeLine("");
        
        try self.writeLine("fn ffi_load_library(path: []const u8) bool {");
        try self.writeLine("    const lib = DynamicLibrary.open(path) catch return false;");
        try self.writeLine("    const path_copy = allocator.dupe(u8, path) catch return false;");
        try self.writeLine("    loaded_libraries.put(path_copy, lib) catch return false;");
        try self.writeLine("    return true;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn ffi_unload_library(path: []const u8) bool {");
        try self.writeLine("    if (loaded_libraries.fetchRemove(path)) |entry| {");
        try self.writeLine("        entry.value.close();");
        try self.writeLine("        allocator.free(entry.key);");
        try self.writeLine("        return true;");
        try self.writeLine("    }");
        try self.writeLine("    return false;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn ffi_call_i32(lib_path: []const u8, func_name: []const u8, args: []const i32) i32 {");
        try self.writeLine("    const lib = loaded_libraries.get(lib_path) orelse return -1;");
        try self.writeLine("    ");
        try self.writeLine("    // Create null-terminated function name");
        try self.writeLine("    const func_name_z = allocator.dupeZ(u8, func_name) catch return -1;");
        try self.writeLine("    defer allocator.free(func_name_z);");
        try self.writeLine("    ");
        try self.writeLine("    // Lookup the function symbol");
        try self.writeLine("    const func_ptr = lib.lookup(*const fn() callconv(.C) i32, func_name_z) orelse return -1;");
        try self.writeLine("    ");
        try self.writeLine("    // Call function based on argument count");
        try self.writeLine("    return switch (args.len) {");
        try self.writeLine("        0 => @as(*const fn() callconv(.C) i32, func_ptr)(),");
        try self.writeLine("        1 => @as(*const fn(i32) callconv(.C) i32, func_ptr)(args[0]),");
        try self.writeLine("        2 => @as(*const fn(i32, i32) callconv(.C) i32, func_ptr)(args[0], args[1]),");
        try self.writeLine("        3 => @as(*const fn(i32, i32, i32) callconv(.C) i32, func_ptr)(args[0], args[1], args[2]),");
        try self.writeLine("        else => -1, // Unsupported argument count");
        try self.writeLine("    };");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn ffi_call_f64(lib_path: []const u8, func_name: []const u8, args: []const f64) f64 {");
        try self.writeLine("    const lib = loaded_libraries.get(lib_path) orelse return -1.0;");
        try self.writeLine("    ");
        try self.writeLine("    const func_name_z = allocator.dupeZ(u8, func_name) catch return -1.0;");
        try self.writeLine("    defer allocator.free(func_name_z);");
        try self.writeLine("    ");
        try self.writeLine("    const func_ptr = lib.lookup(*const fn() callconv(.C) f64, func_name_z) orelse return -1.0;");
        try self.writeLine("    ");
        try self.writeLine("    return switch (args.len) {");
        try self.writeLine("        0 => @as(*const fn() callconv(.C) f64, func_ptr)(),");
        try self.writeLine("        1 => @as(*const fn(f64) callconv(.C) f64, func_ptr)(args[0]),");
        try self.writeLine("        2 => @as(*const fn(f64, f64) callconv(.C) f64, func_ptr)(args[0], args[1]),");
        try self.writeLine("        3 => @as(*const fn(f64, f64, f64) callconv(.C) f64, func_ptr)(args[0], args[1], args[2]),");
        try self.writeLine("        else => -1.0,");
        try self.writeLine("    };");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn ffi_call_str(lib_path: []const u8, func_name: []const u8, args: []const []const u8) []const u8 {");
        try self.writeLine("    const lib = loaded_libraries.get(lib_path) orelse return \"<error: library not found>\";");
        try self.writeLine("    ");
        try self.writeLine("    const func_name_z = allocator.dupeZ(u8, func_name) catch return \"<error: out of memory>\";");
        try self.writeLine("    defer allocator.free(func_name_z);");
        try self.writeLine("    ");
        try self.writeLine("    const func_ptr = lib.lookup(*const fn() callconv(.C) [*:0]const u8, func_name_z) orelse return \"<error: function not found>\";");
        try self.writeLine("    ");
        try self.writeLine("    // Convert string arguments to null-terminated");
        try self.writeLine("    var c_args = allocator.alloc([*:0]const u8, args.len) catch return \"<error: out of memory>\";");
        try self.writeLine("    defer allocator.free(c_args);");
        try self.writeLine("    ");
        try self.writeLine("    for (args, 0..) |arg, i| {");
        try self.writeLine("        c_args[i] = allocator.dupeZ(u8, arg) catch return \"<error: out of memory>\";");
        try self.writeLine("    }");
        try self.writeLine("    ");
        try self.writeLine("    defer for (c_args) |c_arg| {");
        try self.writeLine("        allocator.free(std.mem.span(c_arg));");
        try self.writeLine("    };");
        try self.writeLine("    ");
        try self.writeLine("    const result = switch (args.len) {");
        try self.writeLine("        0 => @as(*const fn() callconv(.C) [*:0]const u8, func_ptr)(),");
        try self.writeLine("        1 => @as(*const fn([*:0]const u8) callconv(.C) [*:0]const u8, func_ptr)(c_args[0]),");
        try self.writeLine("        2 => @as(*const fn([*:0]const u8, [*:0]const u8) callconv(.C) [*:0]const u8, func_ptr)(c_args[0], c_args[1]),");
        try self.writeLine("        3 => @as(*const fn([*:0]const u8, [*:0]const u8, [*:0]const u8) callconv(.C) [*:0]const u8, func_ptr)(c_args[0], c_args[1], c_args[2]),");
        try self.writeLine("        else => return \"<error: too many arguments>\",");
        try self.writeLine("    };");
        try self.writeLine("    ");
        try self.writeLine("    // Convert result back to Zig string");
        try self.writeLine("    const result_span = std.mem.span(result);");
        try self.writeLine("    return allocator.dupe(u8, result_span) catch return \"<error: out of memory>\";");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn ffi_call_void(lib_path: []const u8, func_name: []const u8, args: []const i32) void {");
        try self.writeLine("    const lib = loaded_libraries.get(lib_path) orelse return;");
        try self.writeLine("    ");
        try self.writeLine("    const func_name_z = allocator.dupeZ(u8, func_name) catch return;");
        try self.writeLine("    defer allocator.free(func_name_z);");
        try self.writeLine("    ");
        try self.writeLine("    const func_ptr = lib.lookup(*const fn() callconv(.C) void, func_name_z) orelse return;");
        try self.writeLine("    ");
        try self.writeLine("    switch (args.len) {");
        try self.writeLine("        0 => @as(*const fn() callconv(.C) void, func_ptr)(),");
        try self.writeLine("        1 => @as(*const fn(i32) callconv(.C) void, func_ptr)(args[0]),");
        try self.writeLine("        2 => @as(*const fn(i32, i32) callconv(.C) void, func_ptr)(args[0], args[1]),");
        try self.writeLine("        3 => @as(*const fn(i32, i32, i32) callconv(.C) void, func_ptr)(args[0], args[1], args[2]),");
        try self.writeLine("        else => {},");
        try self.writeLine("    }");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn ffi_alloc_bytes(size: i32) i64 {");
        try self.writeLine("    const memory = allocator.alloc(u8, @intCast(size)) catch return 0;");
        try self.writeLine("    return @intCast(@intFromPtr(memory.ptr));");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn ffi_free_bytes(ptr: i64) void {");
        try self.writeLine("    if (ptr == 0) return;");
        try self.writeLine("    // Note: This is simplified - in practice we'd need to track allocation sizes");
        try self.writeLine("    // For now, this is a placeholder for proper memory management");
        try self.writeLine("    // In a real implementation, we would free the actual memory");
        try self.writeLine("    // TODO: Implement proper memory tracking and freeing");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn ffi_read_i32(ptr: i64) i32 {");
        try self.writeLine("    if (ptr == 0) return 0;");
        try self.writeLine("    const memory: *i32 = @ptrFromInt(@as(usize, @intCast(ptr)));");
        try self.writeLine("    return memory.*;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn ffi_write_i32(ptr: i64, value: i32) void {");
        try self.writeLine("    if (ptr == 0) return;");
        try self.writeLine("    const memory: *i32 = @ptrFromInt(@as(usize, @intCast(ptr)));");
        try self.writeLine("    memory.* = value;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn ffi_read_str(ptr: i64, len: i32) []const u8 {");
        try self.writeLine("    if (ptr == 0 or len <= 0) return \"\";");
        try self.writeLine("    const memory: [*]const u8 = @ptrFromInt(@as(usize, @intCast(ptr)));");
        try self.writeLine("    const slice = memory[0..@intCast(len)];");
        try self.writeLine("    return allocator.dupe(u8, slice) catch return \"<error: out of memory>\";");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn ffi_write_str(ptr: i64, str: []const u8) void {");
        try self.writeLine("    if (ptr == 0) return;");
        try self.writeLine("    const memory: [*]u8 = @ptrFromInt(@as(usize, @intCast(ptr)));");
        try self.writeLine("    for (str, 0..) |char, i| {");
        try self.writeLine("        memory[i] = char;");
        try self.writeLine("    }");
        try self.writeLine("}");
        try self.writeLine("");
        
        // Add simplified add function with proper type handling
        try self.writeLine("fn sever_add(a: anytype, b: anytype) @TypeOf(a, b) {");
        try self.writeLine("    const T = @TypeOf(a, b);");
        try self.writeLine("    const info = @typeInfo(T);");
        try self.writeLine("    return switch (info) {");
        try self.writeLine("        .pointer => |ptr_info| if (ptr_info.child == u8) string_concat_z(a, b) else @compileError(\"Unsupported pointer type: \" ++ @typeName(T)),");
        try self.writeLine("        .int, .float, .comptime_int, .comptime_float => a + b,");
        try self.writeLine("        else => @compileError(\"Unsupported type for addition: \" ++ @typeName(T)),");
        try self.writeLine("    };");
        try self.writeLine("}");
        try self.writeLine("");
        
        // HTTP standard library functions
        try self.writeLine("// HTTP Client Functions");
        try self.writeLine("fn http_get(url: []const u8) []const u8 {");
        try self.writeLine("    var client = std.http.Client{ .allocator = allocator };");
        try self.writeLine("    defer client.deinit();");
        try self.writeLine("");
        try self.writeLine("    const uri = std.Uri.parse(url) catch return \"<error: invalid URL>\";");
        try self.writeLine("    var server_header_buffer: [16384]u8 = undefined;");
        try self.writeLine("    var req = client.open(.GET, uri, .{");
        try self.writeLine("        .server_header_buffer = &server_header_buffer,");
        try self.writeLine("    }) catch return \"<error: connection failed>\";");
        try self.writeLine("    defer req.deinit();");
        try self.writeLine("");
        try self.writeLine("    req.send() catch return \"<error: send failed>\";");
        try self.writeLine("    req.finish() catch return \"<error: finish failed>\";");
        try self.writeLine("    req.wait() catch return \"<error: wait failed>\";");
        try self.writeLine("");
        try self.writeLine("    if (req.response.status != .ok) {");
        try self.writeLine("        return \"<error: HTTP error>\";");
        try self.writeLine("    }");
        try self.writeLine("");
        try self.writeLine("    const body = req.reader().readAllAlloc(allocator, 1024 * 1024) catch return \"<error: read failed>\";");
        try self.writeLine("    return body;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn http_post(url: []const u8, body: []const u8) []const u8 {");
        try self.writeLine("    var client = std.http.Client{ .allocator = allocator };");
        try self.writeLine("    defer client.deinit();");
        try self.writeLine("");
        try self.writeLine("    const uri = std.Uri.parse(url) catch return \"<error: invalid URL>\";");
        try self.writeLine("    var server_header_buffer: [16384]u8 = undefined;");
        try self.writeLine("    var req = client.open(.POST, uri, .{");
        try self.writeLine("        .server_header_buffer = &server_header_buffer,");
        try self.writeLine("        .extra_headers = &.{");
        try self.writeLine("            .{ .name = \"content-type\", .value = \"application/json\" },");
        try self.writeLine("        },");
        try self.writeLine("    }) catch return \"<error: connection failed>\";");
        try self.writeLine("    defer req.deinit();");
        try self.writeLine("");
        try self.writeLine("    req.transfer_encoding = .{ .content_length = body.len };");
        try self.writeLine("    req.send() catch return \"<error: send failed>\";");
        try self.writeLine("    _ = req.writer().writeAll(body) catch return \"<error: write failed>\";");
        try self.writeLine("    req.finish() catch return \"<error: finish failed>\";");
        try self.writeLine("    req.wait() catch return \"<error: wait failed>\";");
        try self.writeLine("");
        try self.writeLine("    if (req.response.status != .ok and req.response.status != .created) {");
        try self.writeLine("        return \"<error: HTTP error>\";");
        try self.writeLine("    }");
        try self.writeLine("");
        try self.writeLine("    const response_body = req.reader().readAllAlloc(allocator, 1024 * 1024) catch return \"<error: read failed>\";");
        try self.writeLine("    return response_body;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn http_put(url: []const u8, body: []const u8) []const u8 {");
        try self.writeLine("    var client = std.http.Client{ .allocator = allocator };");
        try self.writeLine("    defer client.deinit();");
        try self.writeLine("");
        try self.writeLine("    const uri = std.Uri.parse(url) catch return \"<error: invalid URL>\";");
        try self.writeLine("    var server_header_buffer: [16384]u8 = undefined;");
        try self.writeLine("    var req = client.open(.PUT, uri, .{");
        try self.writeLine("        .server_header_buffer = &server_header_buffer,");
        try self.writeLine("        .extra_headers = &.{");
        try self.writeLine("            .{ .name = \"content-type\", .value = \"application/json\" },");
        try self.writeLine("        },");
        try self.writeLine("    }) catch return \"<error: connection failed>\";");
        try self.writeLine("    defer req.deinit();");
        try self.writeLine("");
        try self.writeLine("    req.transfer_encoding = .{ .content_length = body.len };");
        try self.writeLine("    req.send() catch return \"<error: send failed>\";");
        try self.writeLine("    _ = req.writer().writeAll(body) catch return \"<error: write failed>\";");
        try self.writeLine("    req.finish() catch return \"<error: finish failed>\";");
        try self.writeLine("    req.wait() catch return \"<error: wait failed>\";");
        try self.writeLine("");
        try self.writeLine("    const response_body = req.reader().readAllAlloc(allocator, 1024 * 1024) catch return \"<error: read failed>\";");
        try self.writeLine("    return response_body;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn http_delete(url: []const u8) []const u8 {");
        try self.writeLine("    var client = std.http.Client{ .allocator = allocator };");
        try self.writeLine("    defer client.deinit();");
        try self.writeLine("");
        try self.writeLine("    const uri = std.Uri.parse(url) catch return \"<error: invalid URL>\";");
        try self.writeLine("    var server_header_buffer: [16384]u8 = undefined;");
        try self.writeLine("    var req = client.open(.DELETE, uri, .{");
        try self.writeLine("        .server_header_buffer = &server_header_buffer,");
        try self.writeLine("    }) catch return \"<error: connection failed>\";");
        try self.writeLine("    defer req.deinit();");
        try self.writeLine("");
        try self.writeLine("    req.send() catch return \"<error: send failed>\";");
        try self.writeLine("    req.finish() catch return \"<error: finish failed>\";");
        try self.writeLine("    req.wait() catch return \"<error: wait failed>\";");
        try self.writeLine("");
        try self.writeLine("    const response_body = req.reader().readAllAlloc(allocator, 1024 * 1024) catch return \"<error: read failed>\";");
        try self.writeLine("    return response_body;");
        try self.writeLine("}");
        try self.writeLine("");
        
        // HTTP Server Functions
        try self.writeLine("// HTTP Server Functions");
        try self.writeLine("const HttpHandler = *const fn(method: []const u8, path: []const u8, body: []const u8) []const u8;");
        try self.writeLine("");
        try self.writeLine("fn http_serve(port: i32, handler_name: []const u8) void {");
        try self.writeLine("    const socket = std.posix.socket(std.posix.AF.INET, std.posix.SOCK.STREAM, 0) catch {");
        try self.writeLine("        std.debug.print(\"Failed to create socket\\n\", .{});");
        try self.writeLine("        return;");
        try self.writeLine("    };");
        try self.writeLine("    defer std.posix.close(socket);");
        try self.writeLine("");
        try self.writeLine("    // Enable SO_REUSEADDR to allow port reuse");
        try self.writeLine("    const enable: c_int = 1;");
        try self.writeLine("    _ = std.posix.setsockopt(socket, std.posix.SOL.SOCKET, std.posix.SO.REUSEADDR, std.mem.asBytes(&enable)) catch {};");
        try self.writeLine("");
        try self.writeLine("    const sockaddr = std.posix.sockaddr.in{ .family = std.posix.AF.INET, .port = std.mem.nativeToBig(u16, @intCast(port)), .addr = 0 };");
        try self.writeLine("    std.posix.bind(socket, @ptrCast(&sockaddr), @sizeOf(@TypeOf(sockaddr))) catch {");
        try self.writeLine("        std.debug.print(\"Failed to bind to port {}\\n\", .{port});");
        try self.writeLine("        return;");
        try self.writeLine("    };");
        try self.writeLine("");
        try self.writeLine("    std.posix.listen(socket, 5) catch {");
        try self.writeLine("        std.debug.print(\"Failed to listen on socket\\n\", .{});");
        try self.writeLine("        return;");
        try self.writeLine("    };");
        try self.writeLine("");
        try self.writeLine("    std.debug.print(\"HTTP server listening on http://127.0.0.1:{}\\n\", .{port});");
        try self.writeLine("    std.debug.print(\"Server ready to accept connections (handler: {s})\\n\", .{handler_name});");
        try self.writeLine("");
        try self.writeLine("    // Main server loop");
        try self.writeLine("    while (true) {");
        try self.writeLine("        var client_addr: std.posix.sockaddr = undefined;");
        try self.writeLine("        var client_addr_len: std.posix.socklen_t = @sizeOf(@TypeOf(client_addr));");
        try self.writeLine("");
        try self.writeLine("        const client_socket = std.posix.accept(socket, &client_addr, &client_addr_len, 0) catch |err| {");
        try self.writeLine("            std.debug.print(\"Failed to accept connection: {}\\n\", .{err});");
        try self.writeLine("            continue;");
        try self.writeLine("        };");
        try self.writeLine("");
        try self.writeLine("        // Handle request in blocking mode (simplified for now)");
        try self.writeLine("        handleHttpRequest(client_socket, handler_name) catch |err| {");
        try self.writeLine("            std.debug.print(\"Error handling request: {}\\n\", .{err});");
        try self.writeLine("        };");
        try self.writeLine("");
        try self.writeLine("        std.posix.close(client_socket);");
        try self.writeLine("    }");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn handleHttpRequest(client_socket: std.posix.socket_t, handler_name: []const u8) !void {");
        try self.writeLine("    var buffer: [4096]u8 = undefined;");
        try self.writeLine("    const bytes_read = std.posix.recv(client_socket, &buffer, 0) catch |err| {");
        try self.writeLine("        std.debug.print(\"Failed to read from socket: {}\\n\", .{err});");
        try self.writeLine("        return;");
        try self.writeLine("    };");
        try self.writeLine("");
        try self.writeLine("    if (bytes_read == 0) return; // Client closed connection");
        try self.writeLine("");
        try self.writeLine("    const request = buffer[0..bytes_read];");
        try self.writeLine("    std.debug.print(\"Received {} bytes\\n\", .{bytes_read});");
        try self.writeLine("");
        try self.writeLine("    // Parse HTTP request line");
        try self.writeLine("    var lines = std.mem.splitSequence(u8, request, \"\\r\\n\");");
        try self.writeLine("    const first_line = lines.next() orelse return;");
        try self.writeLine("");
        try self.writeLine("    var request_parts = std.mem.splitSequence(u8, first_line, \" \");");
        try self.writeLine("    const method = request_parts.next() orelse \"GET\";");
        try self.writeLine("    const path = request_parts.next() orelse \"/\";");
        try self.writeLine("    const version = request_parts.next() orelse \"HTTP/1.1\";");
        try self.writeLine("    _ = version;");
        try self.writeLine("");
        try self.writeLine("    // Skip headers and find body");
        try self.writeLine("    var body: []const u8 = \"\";");
        try self.writeLine("    var found_empty_line = false;");
        try self.writeLine("    while (lines.next()) |line| {");
        try self.writeLine("        if (line.len == 0) {");
        try self.writeLine("            found_empty_line = true;");
        try self.writeLine("            break;");
        try self.writeLine("        }");
        try self.writeLine("    }");
        try self.writeLine("");
        try self.writeLine("    if (found_empty_line) {");
        try self.writeLine("        // Get the remaining data as body");
        try self.writeLine("        const header_end_pos = std.mem.indexOf(u8, request, \"\\r\\n\\r\\n\");");
        try self.writeLine("        if (header_end_pos) |pos| {");
        try self.writeLine("            const body_start = pos + 4;");
        try self.writeLine("            if (body_start < request.len) {");
        try self.writeLine("                body = request[body_start..];");
        try self.writeLine("            }");
        try self.writeLine("        }");
        try self.writeLine("    }");
        try self.writeLine("");
        try self.writeLine("    std.debug.print(\"Request: {s} {s} (body: {} bytes)\\n\", .{method, path, body.len});");
        try self.writeLine("");
        try self.writeLine("    // Call the appropriate handler function");
        try self.writeLine("    var response: []const u8 = \"\";");
        try self.writeLine("    ");
        try self.writeLine("    // Dynamic handler dispatch - this will be replaced with proper function pointer logic");
        try self.writeLine("    response = callHandler(handler_name, method, path, body);");
        try self.writeLine("");
        try self.writeLine("    // Send HTTP response");
        try self.writeLine("    const http_response = std.fmt.allocPrint(allocator, ");
        try self.writeLine("        \"HTTP/1.1 200 OK\\r\\n\" ++");
        try self.writeLine("        \"Content-Type: application/json\\r\\n\" ++");
        try self.writeLine("        \"Content-Length: {}\\r\\n\" ++");
        try self.writeLine("        \"Access-Control-Allow-Origin: *\\r\\n\" ++");
        try self.writeLine("        \"Access-Control-Allow-Methods: GET, POST, PUT, DELETE\\r\\n\" ++");
        try self.writeLine("        \"Access-Control-Allow-Headers: Content-Type\\r\\n\" ++");
        try self.writeLine("        \"\\r\\n\" ++");
        try self.writeLine("        \"{s}\",");
        try self.writeLine("        .{response.len, response}) catch {");
        try self.writeLine("        std.debug.print(\"Failed to format HTTP response\\n\", .{});");
        try self.writeLine("        return;");
        try self.writeLine("    };");
        try self.writeLine("    defer allocator.free(http_response);");
        try self.writeLine("");
        try self.writeLine("    _ = std.posix.send(client_socket, http_response, 0) catch |err| {");
        try self.writeLine("        std.debug.print(\"Failed to send response: {}\\n\", .{err});");
        try self.writeLine("    };");
        try self.writeLine("}");
        try self.writeLine("");
        
        
        // File I/O standard library functions
        try self.writeLine("// File I/O Functions");
        try self.writeLine("fn file_read(path: []const u8) []const u8 {");
        try self.writeLine("    const file = std.fs.cwd().openFile(path, .{}) catch return \"<error: file not found>\";");
        try self.writeLine("    defer file.close();");
        try self.writeLine("");
        try self.writeLine("    const file_length = file.getEndPos() catch return \"<error: cannot get file size>\";");
        try self.writeLine("    if (file_length > 10 * 1024 * 1024) return \"<error: file too large>\"; // 10MB limit");
        try self.writeLine("");
        try self.writeLine("    const contents = file.readToEndAlloc(allocator, file_length) catch return \"<error: read failed>\";");
        try self.writeLine("    return contents;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn file_write(path: []const u8, content: []const u8) bool {");
        try self.writeLine("    const file = std.fs.cwd().createFile(path, .{}) catch return false;");
        try self.writeLine("    defer file.close();");
        try self.writeLine("");
        try self.writeLine("    file.writeAll(content) catch return false;");
        try self.writeLine("    return true;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn file_append(path: []const u8, content: []const u8) bool {");
        try self.writeLine("    const file = std.fs.cwd().openFile(path, .{ .mode = .write_only }) catch {");
        try self.writeLine("        // If file doesn't exist, create it");
        try self.writeLine("        const new_file = std.fs.cwd().createFile(path, .{}) catch return false;");
        try self.writeLine("        defer new_file.close();");
        try self.writeLine("        new_file.writeAll(content) catch return false;");
        try self.writeLine("        return true;");
        try self.writeLine("    };");
        try self.writeLine("    defer file.close();");
        try self.writeLine("");
        try self.writeLine("    file.seekFromEnd(0) catch return false;");
        try self.writeLine("    file.writeAll(content) catch return false;");
        try self.writeLine("    return true;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn file_exists(path: []const u8) bool {");
        try self.writeLine("    std.fs.cwd().access(path, .{}) catch return false;");
        try self.writeLine("    return true;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn file_delete(path: []const u8) bool {");
        try self.writeLine("    std.fs.cwd().deleteFile(path) catch return false;");
        try self.writeLine("    return true;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn file_size(path: []const u8) i64 {");
        try self.writeLine("    const file = std.fs.cwd().openFile(path, .{}) catch return -1;");
        try self.writeLine("    defer file.close();");
        try self.writeLine("");
        try self.writeLine("    const size = file.getEndPos() catch return -1;");
        try self.writeLine("    return @intCast(size);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn dir_create(path: []const u8) bool {");
        try self.writeLine("    std.fs.cwd().makeDir(path) catch return false;");
        try self.writeLine("    return true;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn dir_exists(path: []const u8) bool {");
        try self.writeLine("    var dir = std.fs.cwd().openDir(path, .{}) catch return false;");
        try self.writeLine("    dir.close();");
        try self.writeLine("    return true;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn dir_list(path: []const u8) []const u8 {");
        try self.writeLine("    var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch return \"<error: cannot open directory>\";");
        try self.writeLine("    defer dir.close();");
        try self.writeLine("");
        try self.writeLine("    var result = std.ArrayList(u8).init(allocator);");
        try self.writeLine("    var iterator = dir.iterate();");
        try self.writeLine("");
        try self.writeLine("    while (iterator.next() catch return \"<error: iteration failed>\") |entry| {");
        try self.writeLine("        result.appendSlice(entry.name) catch return \"<error: out of memory>\";");
        try self.writeLine("        if (entry.kind == .directory) {");
        try self.writeLine("            result.appendSlice(\"/\") catch return \"<error: out of memory>\";");
        try self.writeLine("        }");
        try self.writeLine("        result.appendSlice(\"\\n\") catch return \"<error: out of memory>\";");
        try self.writeLine("    }");
        try self.writeLine("");
        try self.writeLine("    return result.toOwnedSlice() catch return \"<error: out of memory>\";");
        try self.writeLine("}");
        try self.writeLine("");
        
        // JSON standard library functions
        try self.writeLine("// JSON Functions");
        try self.writeLine("fn json_parse(json_str: []const u8) []const u8 {");
        try self.writeLine("    var parsed = std.json.parseFromSlice(std.json.Value, allocator, json_str, .{}) catch return \"<error: invalid JSON>\";");
        try self.writeLine("    defer parsed.deinit();");
        try self.writeLine("");
        try self.writeLine("    // For now, just return a formatted representation");
        try self.writeLine("    const formatted = std.json.stringifyAlloc(allocator, parsed.value, .{ .whitespace = .indent_2 }) catch return \"<error: stringify failed>\";");
        try self.writeLine("    return formatted;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn json_get_string(json_str: []const u8, key: []const u8) []const u8 {");
        try self.writeLine("    var parsed = std.json.parseFromSlice(std.json.Value, allocator, json_str, .{}) catch return \"<error: invalid JSON>\";");
        try self.writeLine("    defer parsed.deinit();");
        try self.writeLine("");
        try self.writeLine("    if (parsed.value != .object) return \"<error: not an object>\";");
        try self.writeLine("");
        try self.writeLine("    const value = parsed.value.object.get(key) orelse return \"<error: key not found>\";");
        try self.writeLine("    if (value != .string) return \"<error: value is not a string>\";");
        try self.writeLine("");
        try self.writeLine("    return allocator.dupe(u8, value.string) catch return \"<error: out of memory>\";");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn json_get_number(json_str: []const u8, key: []const u8) f64 {");
        try self.writeLine("    var parsed = std.json.parseFromSlice(std.json.Value, allocator, json_str, .{}) catch return -1.0;");
        try self.writeLine("    defer parsed.deinit();");
        try self.writeLine("");
        try self.writeLine("    if (parsed.value != .object) return -1.0;");
        try self.writeLine("");
        try self.writeLine("    const value = parsed.value.object.get(key) orelse return -1.0;");
        try self.writeLine("    return switch (value) {");
        try self.writeLine("        .integer => |i| @floatFromInt(i),");
        try self.writeLine("        .float => |f| f,");
        try self.writeLine("        .number_string => |s| std.fmt.parseFloat(f64, s) catch -1.0,");
        try self.writeLine("        else => -1.0,");
        try self.writeLine("    };");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn json_get_bool(json_str: []const u8, key: []const u8) bool {");
        try self.writeLine("    var parsed = std.json.parseFromSlice(std.json.Value, allocator, json_str, .{}) catch return false;");
        try self.writeLine("    defer parsed.deinit();");
        try self.writeLine("");
        try self.writeLine("    if (parsed.value != .object) return false;");
        try self.writeLine("");
        try self.writeLine("    const value = parsed.value.object.get(key) orelse return false;");
        try self.writeLine("    if (value != .bool) return false;");
        try self.writeLine("");
        try self.writeLine("    return value.bool;");
        try self.writeLine("}");
        try self.writeLine("");
        
        // Additional JSON utility functions for HTTP APIs
        try self.writeLine("fn json_create_object() []const u8 {");
        try self.writeLine("    return \"{}\";");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn json_add_string(json_str: []const u8, key: []const u8, value: []const u8) []const u8 {");
        try self.writeLine("    // Simple JSON string addition - removes closing brace and adds key-value");
        try self.writeLine("    if (std.mem.eql(u8, json_str, \"{}\")) {");
        try self.writeLine("        return std.fmt.allocPrint(allocator, \"{{\\\"{s}\\\": \\\"{s}\\\"}}\", .{key, value}) catch return json_str;");
        try self.writeLine("    } else {");
        try self.writeLine("        const len = json_str.len;");
        try self.writeLine("        if (len > 0 and json_str[len-1] == '}') {");
        try self.writeLine("            const without_brace = json_str[0..len-1];");
        try self.writeLine("            return std.fmt.allocPrint(allocator, \"{s}, \\\"{s}\\\": \\\"{s}\\\"}}\", .{without_brace, key, value}) catch return json_str;");
        try self.writeLine("        }");
        try self.writeLine("    }");
        try self.writeLine("    return json_str;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn json_add_number(json_str: []const u8, key: []const u8, value: f64) []const u8 {");
        try self.writeLine("    // Simple JSON number addition");
        try self.writeLine("    if (std.mem.eql(u8, json_str, \"{}\")) {");
        try self.writeLine("        return std.fmt.allocPrint(allocator, \"{{\\\"{s}\\\": {}}}\", .{key, value}) catch return json_str;");
        try self.writeLine("    } else {");
        try self.writeLine("        const len = json_str.len;");
        try self.writeLine("        if (len > 0 and json_str[len-1] == '}') {");
        try self.writeLine("            const without_brace = json_str[0..len-1];");
        try self.writeLine("            return std.fmt.allocPrint(allocator, \"{s}, \\\"{s}\\\": {}}}\", .{without_brace, key, value}) catch return json_str;");
        try self.writeLine("        }");
        try self.writeLine("    }");
        try self.writeLine("    return json_str;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn f64_to_string(value: f64) []const u8 {");
        try self.writeLine("    return std.fmt.allocPrint(allocator, \"{d}\", .{value}) catch \"0.0\";");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn json_has_key(json_str: []const u8, key: []const u8) bool {");
        try self.writeLine("    var parsed = std.json.parseFromSlice(std.json.Value, allocator, json_str, .{}) catch return false;");
        try self.writeLine("    defer parsed.deinit();");
        try self.writeLine("");
        try self.writeLine("    if (parsed.value != .object) return false;");
        try self.writeLine("");
        try self.writeLine("    return parsed.value.object.contains(key);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn json_stringify_object(keys: []const []const u8, values: []const []const u8) []const u8 {");
        try self.writeLine("    if (keys.len != values.len) return \"<error: mismatched arrays>\";");
        try self.writeLine("");
        try self.writeLine("    var result = std.ArrayList(u8).init(allocator);");
        try self.writeLine("    result.appendSlice(\"{\") catch return \"<error: out of memory>\";");
        try self.writeLine("");
        try self.writeLine("    for (keys, values, 0..) |key, value, i| {");
        try self.writeLine("        if (i > 0) result.appendSlice(\",\") catch return \"<error: out of memory>\";");
        try self.writeLine("        result.appendSlice(\"\\\"\") catch return \"<error: out of memory>\";");
        try self.writeLine("        result.appendSlice(key) catch return \"<error: out of memory>\";");
        try self.writeLine("        result.appendSlice(\"\\\":\\\"\") catch return \"<error: out of memory>\";");
        try self.writeLine("        result.appendSlice(value) catch return \"<error: out of memory>\";");
        try self.writeLine("        result.appendSlice(\"\\\"\") catch return \"<error: out of memory>\";");
        try self.writeLine("    }");
        try self.writeLine("");
        try self.writeLine("    result.appendSlice(\"}\") catch return \"<error: out of memory>\";");
        try self.writeLine("    return result.toOwnedSlice() catch return \"<error: out of memory>\";");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn json_stringify_array(values: []const []const u8) []const u8 {");
        try self.writeLine("    var result = std.ArrayList(u8).init(allocator);");
        try self.writeLine("    result.appendSlice(\"[\") catch return \"<error: out of memory>\";");
        try self.writeLine("");
        try self.writeLine("    for (values, 0..) |value, i| {");
        try self.writeLine("        if (i > 0) result.appendSlice(\",\") catch return \"<error: out of memory>\";");
        try self.writeLine("        result.appendSlice(\"\\\"\") catch return \"<error: out of memory>\";");
        try self.writeLine("        result.appendSlice(value) catch return \"<error: out of memory>\";");
        try self.writeLine("        result.appendSlice(\"\\\"\") catch return \"<error: out of memory>\";");
        try self.writeLine("    }");
        try self.writeLine("");
        try self.writeLine("    result.appendSlice(\"]\") catch return \"<error: out of memory>\";");
        try self.writeLine("    return result.toOwnedSlice() catch return \"<error: out of memory>\";");
        try self.writeLine("}");
        try self.writeLine("");
        
        // String manipulation standard library functions
        try self.writeLine("// String Functions");
        try self.writeLine("fn str_length(s: []const u8) i32 {");
        try self.writeLine("    return @intCast(s.len);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn str_substring(s: []const u8, start: i64, end: i64) []const u8 {");
        try self.writeLine("    if (start < 0 or end < 0 or start >= s.len or end > s.len or start >= end) {");
        try self.writeLine("        return \"<error: invalid range>\";");
        try self.writeLine("    }");
        try self.writeLine("    const start_idx: usize = @intCast(start);");
        try self.writeLine("    const end_idx: usize = @intCast(end);");
        try self.writeLine("    return allocator.dupe(u8, s[start_idx..end_idx]) catch return \"<error: out of memory>\";");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn str_contains(s: []const u8, needle: []const u8) bool {");
        try self.writeLine("    return std.mem.indexOf(u8, s, needle) != null;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn str_starts_with(s: []const u8, prefix: []const u8) bool {");
        try self.writeLine("    return std.mem.startsWith(u8, s, prefix);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn str_ends_with(s: []const u8, suffix: []const u8) bool {");
        try self.writeLine("    return std.mem.endsWith(u8, s, suffix);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn str_index_of(s: []const u8, needle: []const u8) i64 {");
        try self.writeLine("    if (std.mem.indexOf(u8, s, needle)) |index| {");
        try self.writeLine("        return @intCast(index);");
        try self.writeLine("    }");
        try self.writeLine("    return -1;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn str_replace(s: []const u8, needle: []const u8, replacement: []const u8) []const u8 {");
        try self.writeLine("    return std.mem.replaceOwned(u8, allocator, s, needle, replacement) catch return \"<error: replace failed>\";");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn str_to_upper(s: []const u8) []const u8 {");
        try self.writeLine("    var result = allocator.alloc(u8, s.len) catch return \"<error: out of memory>\";");
        try self.writeLine("    for (s, 0..) |char, i| {");
        try self.writeLine("        result[i] = std.ascii.toUpper(char);");
        try self.writeLine("    }");
        try self.writeLine("    return result;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn str_to_lower(s: []const u8) []const u8 {");
        try self.writeLine("    var result = allocator.alloc(u8, s.len) catch return \"<error: out of memory>\";");
        try self.writeLine("    for (s, 0..) |char, i| {");
        try self.writeLine("        result[i] = std.ascii.toLower(char);");
        try self.writeLine("    }");
        try self.writeLine("    return result;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn str_trim(s: []const u8) []const u8 {");
        try self.writeLine("    const trimmed = std.mem.trim(u8, s, \" \\t\\n\\r\");");
        try self.writeLine("    return allocator.dupe(u8, trimmed) catch return \"<error: out of memory>\";");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn str_equals(a: []const u8, b: []const u8) bool {");
        try self.writeLine("    return std.mem.eql(u8, a, b);");
        try self.writeLine("}");
        try self.writeLine("");
        
        // Mathematical Functions
        try self.writeLine("// Mathematical Functions");
        try self.writeLine("fn math_abs(x: f64) f64 {");
        try self.writeLine("    return @abs(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_sqrt(x: f64) f64 {");
        try self.writeLine("    return math.sqrt(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_pow(base: f64, exp: f64) f64 {");
        try self.writeLine("    return math.pow(f64, base, exp);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_exp(x: f64) f64 {");
        try self.writeLine("    return math.exp(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_log(x: f64) f64 {");
        try self.writeLine("    return math.log(f64, math.e, x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_log10(x: f64) f64 {");
        try self.writeLine("    return math.log10(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_log2(x: f64) f64 {");
        try self.writeLine("    return math.log2(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_sin(x: f64) f64 {");
        try self.writeLine("    return math.sin(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_cos(x: f64) f64 {");
        try self.writeLine("    return math.cos(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_tan(x: f64) f64 {");
        try self.writeLine("    return math.tan(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_asin(x: f64) f64 {");
        try self.writeLine("    return math.asin(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_acos(x: f64) f64 {");
        try self.writeLine("    return math.acos(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_atan(x: f64) f64 {");
        try self.writeLine("    return math.atan(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_atan2(y: f64, x: f64) f64 {");
        try self.writeLine("    return math.atan2(f64, y, x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_sinh(x: f64) f64 {");
        try self.writeLine("    return math.sinh(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_cosh(x: f64) f64 {");
        try self.writeLine("    return math.cosh(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_tanh(x: f64) f64 {");
        try self.writeLine("    return math.tanh(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_floor(x: f64) f64 {");
        try self.writeLine("    return math.floor(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_ceil(x: f64) f64 {");
        try self.writeLine("    return math.ceil(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_round(x: f64) f64 {");
        try self.writeLine("    return math.round(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_trunc(x: f64) f64 {");
        try self.writeLine("    return math.trunc(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_fmod(x: f64, y: f64) f64 {");
        try self.writeLine("    return math.fmod(f64, x, y);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_remainder(x: f64, y: f64) f64 {");
        try self.writeLine("    return math.mod(f64, x, y);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_min(x: f64, y: f64) f64 {");
        try self.writeLine("    return math.min(x, y);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_max(x: f64, y: f64) f64 {");
        try self.writeLine("    return math.max(x, y);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_clamp(value: f64, min_val: f64, max_val: f64) f64 {");
        try self.writeLine("    return math.clamp(value, min_val, max_val);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_lerp(a: f64, b: f64, t: f64) f64 {");
        try self.writeLine("    return a + t * (b - a);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_degrees(radians: f64) f64 {");
        try self.writeLine("    return radians * 180.0 / math.pi;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_radians(degrees: f64) f64 {");
        try self.writeLine("    return degrees * math.pi / 180.0;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_pi() f64 {");
        try self.writeLine("    return math.pi;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_e() f64 {");
        try self.writeLine("    return math.e;");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_inf() f64 {");
        try self.writeLine("    return math.inf(f64);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_nan() f64 {");
        try self.writeLine("    return math.nan(f64);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_is_finite(x: f64) bool {");
        try self.writeLine("    return math.isFinite(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_is_infinite(x: f64) bool {");
        try self.writeLine("    return math.isInf(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_is_nan(x: f64) bool {");
        try self.writeLine("    return math.isNan(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_sign(x: f64) f64 {");
        try self.writeLine("    return math.sign(x);");
        try self.writeLine("}");
        try self.writeLine("");
        
        try self.writeLine("fn math_copysign(magnitude: f64, sign: f64) f64 {");
        try self.writeLine("    return math.copysign(f64, magnitude, sign);");
        try self.writeLine("}");
        try self.writeLine("");
        
        // Generate all functions
        var func_iter = program.functions.iterator();
        while (func_iter.next()) |entry| {
            const func_name = entry.key_ptr.*;
            const function = entry.value_ptr;
            
            try self.generateFunction(func_name, function);
            try self.writeLine("");
        }
        
        // Generate dynamic callHandler function based on available functions
        try self.generateCallHandlerFunction(program);
        
        // Generate main function wrapper if entry point is not "main"
        if (!std.mem.eql(u8, program.entry, "main")) {
            try self.writeLine("pub fn main() !void {");
            self.indent_level += 1;
            try self.writeIndent();
            try self.writeLine("sever_runtime.init(null);");
            try self.writeIndent();
            try self.write("try ");
            try self.write(program.entry);
            try self.write("();");
            try self.writeLine("");
            self.indent_level -= 1;
            try self.writeLine("}");
        } else {
            // If main exists, add runtime initialization at the beginning
            const main_func = program.functions.get("main").?;
            if (main_func.body.items.len > 0) {
                try self.writeLine("// Runtime initialization will be added automatically");
            }
        }
    }
    
    fn generateFunction(self: *CodeGen, name: []const u8, function: *Function) CodeGenError!void {
        self.current_function_name = name;
        self.current_function = function;
        
        // Function signature
        if (std.mem.eql(u8, name, "main")) {
            try self.write("pub fn main() ");
        } else {
            try self.write("fn ");
            try self.write(name);
            try self.write("(");
            
            // Parameters
            for (function.args.items, 0..) |param, i| {
                if (i > 0) try self.write(", ");
                try self.write(param.name);
                try self.write(": ");
                try self.generateType(param.type);
            }
            
            try self.write(") ");
        }
        
        // Return type - main function should return !void and print result
        if (std.mem.eql(u8, name, "main")) {
            try self.write("!void");
        } else if (function.@"async") {
            // Async functions return futures
            try self.write("*Future(");
            if (function.@"return" == .void) {
                try self.write("void");
            } else {
                try self.generateType(function.@"return");
            }
            try self.write(")");
        } else if (function.@"return" == .void) {
            try self.write("void");
        } else {
            try self.generateType(function.@"return");
        }
        
        try self.writeLine(" {");
        self.indent_level += 1;
        
        // Add runtime initialization for main function
        if (std.mem.eql(u8, name, "main")) {
            try self.writeIndent();
            try self.writeLine("sever_runtime_init(null);");
        }
        
        // Handle async function wrapper
        if (function.@"async" and !std.mem.eql(u8, name, "main")) {
            try self.writeIndent();
            try self.writeLine("const future = allocator.create(Future([]const u8)) catch @panic(\"OOM\");");
            try self.writeIndent();
            try self.writeLine("future.* = Future([]const u8){};");
            try self.writeIndent();
            try self.writeLine("_ = Thread.spawn(.{}, struct {");
            self.indent_level += 1;
            try self.writeIndent();
            try self.writeLine("fn run(f: *Future([]const u8)) void {");
            self.indent_level += 1;
            
            // Generate async function body
            for (function.body.items) |*stmt| {
                try self.generateAsyncStatement(stmt);
            }
            
            self.indent_level -= 1;
            try self.writeIndent();
            try self.writeLine("}");
            self.indent_level -= 1;
            try self.writeIndent();
            try self.writeLine("}.run, .{future}) catch {};");
            try self.writeIndent();
            try self.writeLine("return future;");
        } else {
            // Regular function body
            for (function.body.items) |*stmt| {
                try self.generateStatement(stmt);
            }
        }
        
        self.indent_level -= 1;
        try self.writeLine("}");
    }
    
    fn generateStatement(self: *CodeGen, stmt: *Statement) CodeGenError!void {
        switch (stmt.*) {
            .let => |*let_stmt| {
                try self.writeIndent();
                if (let_stmt.mutable) {
                    try self.write("var ");
                } else {
                    try self.write("const ");
                }
                try self.write(let_stmt.name);
                
                if (let_stmt.type) |stmt_type| {
                    try self.write(": ");
                    try self.generateType(stmt_type);
                }
                
                try self.write(" = ");
                try self.generateExpression(&let_stmt.value);
                try self.writeLine(";");
                // Add unused variable suppression for mixture/hierarchical models
                if (let_stmt.value == .mixture or let_stmt.value == .hierarchical) {
                    try self.writeIndent();
                    try self.write("_ = ");
                    try self.write(let_stmt.name);
                    try self.writeLine(";");
                }
            },
            
            .assign => |*assign_stmt| {
                try self.writeIndent();
                try self.generateLValue(&assign_stmt.target);
                try self.write(" = ");
                try self.generateExpression(&assign_stmt.value);
                try self.writeLine(";");
            },
            
            .@"if" => |*if_stmt| {
                try self.writeIndent();
                try self.write("if (");
                try self.generateExpression(&if_stmt.condition);
                try self.writeLine(") {");
                
                self.indent_level += 1;
                for (if_stmt.then.items) |*then_stmt| {
                    try self.generateStatement(then_stmt);
                }
                self.indent_level -= 1;
                
                if (if_stmt.@"else") |*else_stmts| {
                    try self.writeLine("} else {");
                    self.indent_level += 1;
                    for (else_stmts.items) |*else_stmt| {
                        try self.generateStatement(else_stmt);
                    }
                    self.indent_level -= 1;
                }
                
                try self.writeLine("}");
            },
            
            .@"while" => |*while_stmt| {
                try self.writeIndent();
                try self.write("while (");
                try self.generateExpression(&while_stmt.condition);
                try self.writeLine(") {");
                
                self.indent_level += 1;
                for (while_stmt.body.items) |*body_stmt| {
                    try self.generateStatement(body_stmt);
                }
                self.indent_level -= 1;
                
                try self.writeLine("}");
            },
            
            .@"for" => |*for_stmt| {
                // Generate simplified for loop - for now just iterate once
                // TODO: Implement proper iteration based on iterable type
                try self.writeIndent();
                try self.write("// For loop iteration (simplified)");
                try self.writeLine("");
                
                try self.writeIndent();
                try self.write("{{");
                try self.writeLine("");
                
                self.indent_level += 1;
                
                // Bind loop variable to iterable value
                try self.writeIndent();
                try self.write("const ");
                try self.write(for_stmt.variable);
                try self.write(" = ");
                try self.generateExpression(&for_stmt.iterable);
                try self.writeLine(";");
                
                // Generate body
                for (for_stmt.body.items) |*body_stmt| {
                    try self.generateStatement(body_stmt);
                }
                
                self.indent_level -= 1;
                try self.writeLine("}");
            },
            
            .@"return" => |*return_expr| {
                try self.writeIndent();
                // Check if we're in main function and need to print result instead of returning it
                if (self.current_function) |func| {
                    if (func.@"return" != .void and self.current_function_name != null and std.mem.eql(u8, self.current_function_name.?, "main")) {
                        // For main function, print the result and return void
                        try self.write("const _sever_main_result = ");
                        try self.generateExpression(return_expr);
                        try self.writeLine(";");
                        try self.writeIndent();
                        try self.writeLine("std_print_int(_sever_main_result);");
                        try self.writeIndent();
                        try self.writeLine("return;");
                    } else {
                        try self.write("return ");
                        try self.generateExpression(return_expr);
                        try self.writeLine(";");
                    }
                } else {
                    try self.write("return ");
                    try self.generateExpression(return_expr);
                    try self.writeLine(";");
                }
            },
            
            .@"break" => {
                try self.writeIndent();
                try self.writeLine("break;");
            },
            
            .@"continue" => {
                try self.writeIndent();
                try self.writeLine("continue;");
            },
            
            .observe => |*observe_stmt| {
                try self.writeIndent();
                try self.write("observe(\"");
                try self.write(observe_stmt.distribution);
                try self.write("\", &[_]f64{");
                
                for (observe_stmt.params.items, 0..) |*param, i| {
                    if (i > 0) try self.write(", ");
                    try self.generateExpression(param);
                }
                
                try self.write("}, ");
                try self.generateExpression(&observe_stmt.value);
                try self.writeLine(");");
            },
            
            .prob_assert => |*assert_stmt| {
                try self.writeIndent();
                try self.write("prob_assert(");
                try self.generateExpression(&assert_stmt.condition);
                try self.write(", ");
                const confidence_str = std.fmt.allocPrint(self.allocator, "{d}", .{assert_stmt.confidence}) catch return CodeGenError.OutOfMemory;
                defer self.allocator.free(confidence_str);
                try self.write(confidence_str);
                try self.writeLine(");");
            },
            
            .match => |*match_stmt| {
                return try self.generateMatchStatement(match_stmt);
            },
            
            .@"try" => |*try_stmt| {
                return try self.generateTryStatement(try_stmt);
            },
            
            .@"throw" => |*throw_expr| {
                try self.writeIndent();
                // For now, generate a simple panic with the message
                try self.write("std.debug.panic(");
                try self.generateExpression(throw_expr);
                try self.writeLine(", .{});");
            },
            
            .expression => |*expr| {
                try self.writeIndent();
                try self.generateExpression(expr);
                try self.writeLine(";");
            },
            
            .model => |*model_stmt| {
                // Generate graphical model as a struct definition
                try self.writeIndent();
                try self.write("const ");
                try self.write(model_stmt.name);
                try self.writeLine(" = struct {");
                self.indent_level += 1;
                
                // Generate model components as comments for now
                try self.writeIndent();
                try self.write("// Graphical model with ");
                const node_count = std.fmt.allocPrint(self.allocator, "{d}", .{model_stmt.nodes.items.len}) catch return CodeGenError.OutOfMemory;
                defer self.allocator.free(node_count);
                try self.write(node_count);
                try self.write(" nodes, ");
                const plate_count = std.fmt.allocPrint(self.allocator, "{d}", .{model_stmt.plates.items.len}) catch return CodeGenError.OutOfMemory;
                defer self.allocator.free(plate_count);
                try self.write(plate_count);
                try self.write(" plates, ");
                const factor_count = std.fmt.allocPrint(self.allocator, "{d}", .{model_stmt.factors.items.len}) catch return CodeGenError.OutOfMemory;
                defer self.allocator.free(factor_count);
                try self.write(factor_count);
                try self.writeLine(" factors");
                
                self.indent_level -= 1;
                try self.writeLine("};");
            },
            
        }
    }
    
    fn generateExpression(self: *CodeGen, expr: *Expression) CodeGenError!void {
        switch (expr.*) {
            .literal => |literal| {
                switch (literal) {
                    .integer => |i| {
                        const str = std.fmt.allocPrint(self.allocator, "{d}", .{i}) catch return CodeGenError.OutOfMemory;
                        defer self.allocator.free(str);
                        try self.write(str);
                    },
                    .float => |f| {
                        const str = std.fmt.allocPrint(self.allocator, "{d}", .{f}) catch return CodeGenError.OutOfMemory;
                        defer self.allocator.free(str);
                        try self.write(str);
                    },
                    .string => |s| {
                        try self.write("\"");
                        try self.write(s);
                        try self.write("\"");
                    },
                    .boolean => |b| try self.write(if (b) "true" else "false"),
                    .null => try self.write("null"),
                }
            },
            
            .variable => |var_name| {
                try self.write(var_name);
            },
            
            .call => |*call_expr| {
                try self.write(call_expr.function);
                try self.write("(");
                
                for (call_expr.args.items, 0..) |*arg, i| {
                    if (i > 0) try self.write(", ");
                    try self.generateExpression(arg);
                }
                
                try self.write(")");
            },
            
            .op => |*op_expr| {
                try self.generateOperation(op_expr);
            },
            
            .index => |*index_expr| {
                try self.generateExpression(index_expr.array);
                try self.write("[");
                try self.generateExpression(index_expr.index);
                try self.write("]");
            },
            
            .field => |*field_expr| {
                try self.generateExpression(field_expr.object);
                try self.write(".");
                try self.write(field_expr.field);
            },
            
            .array => |*array_expr| {
                // Use Zig's automatic type inference for arrays
                if (array_expr.items.len > 0) {
                    try self.write("[_]@TypeOf(");
                    try self.generateExpression(@constCast(&array_expr.items[0]));
                    try self.write("){");
                    
                    for (array_expr.items, 0..) |*elem, i| {
                        if (i > 0) try self.write(", ");
                        try self.generateExpression(elem);
                    }
                    
                    try self.write("}");
                } else {
                    // Empty array - need explicit type, for now use void
                    try self.write("[_]void{}");
                }
            },
            
            .sample => |*sample_expr| {
                try self.write("sample(\"");
                try self.write(sample_expr.distribution);
                try self.write("\", &[_]f64{");
                
                for (sample_expr.params.items, 0..) |*param, i| {
                    if (i > 0) try self.write(", ");
                    try self.generateExpression(param);
                }
                
                try self.write("})");
            },
            
            .cast => |*cast_expr| {
                try self.write("@as(");
                try self.generateType(cast_expr.type);
                try self.write(", ");
                try self.generateExpression(cast_expr.value);
                try self.write(")");
            },
            
            .@"struct" => |*struct_expr| {
                // Generate anonymous struct literal
                try self.write(".{");
                
                var field_iter = struct_expr.iterator();
                var first = true;
                while (field_iter.next()) |entry| {
                    if (!first) try self.write(", ");
                    first = false;
                    
                    const field_name = entry.key_ptr.*;
                    const field_value = entry.value_ptr;
                    
                    try self.write(".");
                    try self.write(field_name);
                    try self.write(" = ");
                    try self.generateExpression(@constCast(field_value));
                }
                
                try self.write("}");
            },
            
            .enum_constructor => |*enum_expr| {
                // Generate enum constructor call
                try self.write(enum_expr.enum_type);
                try self.write(".");
                try self.write(enum_expr.variant);
                
                // If there's an associated value, include it
                if (enum_expr.value) |value_expr| {
                    try self.write("(");
                    try self.generateExpression(value_expr);
                    try self.write(")");
                }
            },
            
            .hashmap => |*hashmap_expr| {
                // Generate hashmap literal using Zig's StringHashMap for string keys
                try self.write("blk: {\n");
                self.indent_level += 1;
                try self.writeIndent();
                try self.write("var map = std.StringHashMap(i32).init(allocator);\n");
                
                // Add all entries
                var map_iter = hashmap_expr.iterator();
                while (map_iter.next()) |entry| {
                    try self.writeIndent();
                    try self.write("try map.put(\"");
                    try self.write(entry.key_ptr.*);
                    try self.write("\", ");
                    try self.generateExpression(@constCast(entry.value_ptr));
                    try self.write(");\n");
                }
                
                try self.writeIndent();
                try self.write("break :blk map;\n");
                self.indent_level -= 1;
                try self.writeIndent();
                try self.write("}");
            },
            
            .set => |*set_expr| {
                // Generate set literal using HashMap with void values
                try self.write("blk: {\n");
                self.indent_level += 1;
                try self.writeIndent();
                try self.write("var set = std.HashMap(i32, void, std.hash_map.AutoContext(i32), std.hash_map.default_max_load_percentage).init(allocator);\n");
                
                for (set_expr.items) |*elem| {
                    try self.writeIndent();
                    try self.write("try set.put(");
                    try self.generateExpression(@constCast(elem));
                    try self.write(", {});\n");
                }
                
                try self.writeIndent();
                try self.write("break :blk set;\n");
                self.indent_level -= 1;
                try self.writeIndent();
                try self.write("}");
            },
            
            .tuple => |*tuple_expr| {
                // Generate tuple literal as anonymous struct
                try self.write(".{");
                
                for (tuple_expr.items, 0..) |*elem, i| {
                    if (i > 0) try self.write(", ");
                    try self.generateExpression(@constCast(elem));
                }
                
                try self.write("}");
            },
            
            .record => |*record_expr| {
                // Generate record literal as named struct literal
                try self.write(record_expr.type_name);
                try self.write("{");
                
                var field_iter = record_expr.fields.iterator();
                var first = true;
                while (field_iter.next()) |entry| {
                    if (!first) try self.write(", ");
                    first = false;
                    
                    try self.write(".");
                    try self.write(entry.key_ptr.*);
                    try self.write(" = ");
                    try self.generateExpression(@constCast(entry.value_ptr));
                }
                
                try self.write("}");
            },
            
            .@"await" => |await_expr| {
                // Generate await expression - wait on the future
                try self.write("try ");
                try self.generateExpression(await_expr);
                try self.write(".wait()");
            },
            
            .mixture => {
                // For now, generate a placeholder value for mixture models
                // In a full implementation, this would generate probabilistic model code
                try self.write("0.0"); // mixture model placeholder
            },
            
            .hierarchical => {
                // For now, generate a placeholder value for hierarchical models
                // In a full implementation, this would generate hierarchical model code
                try self.write("0.0"); // hierarchical model placeholder
            },
            
            .plate => {
                // Generate plate as a for loop placeholder
                try self.write("0.0"); // plate placeholder
            },
            
            .factor => {
                // Generate factor as function call placeholder
                try self.write("0.0"); // factor placeholder
            },
            
            .graphical_node => {
                // Generate graphical node as variable placeholder
                try self.write("0.0"); // graphical node placeholder
            },
            
            else => {
                return CodeGenError.UnsupportedExpression;
            },
        }
    }
    
    fn generateOperation(self: *CodeGen, op_expr: anytype) CodeGenError!void {
        const args = &op_expr.args;
        
        switch (op_expr.kind) {
            .not => {
                try self.write("!");
                try self.generateExpression(@constCast(&args.items[0]));
            },
            
            .bitnot => {
                try self.write("~");
                try self.generateExpression(@constCast(&args.items[0]));
            },
            
            .add => {
                // For add operations, use the sever_add helper function that can handle
                // both numeric addition and string concatenation at compile time
                if (args.items.len >= 2) {
                    try self.write("sever_add(");
                    try self.generateExpression(@constCast(&args.items[0]));
                    try self.write(", ");
                    try self.generateExpression(@constCast(&args.items[1]));
                    try self.write(")");
                } else {
                    return CodeGenError.UnsupportedExpression;
                }
            },
            
            .div => {
                // For division operations, use @divTrunc for integer division in Zig
                if (args.items.len >= 2) {
                    try self.write("@divTrunc(");
                    try self.generateExpression(@constCast(&args.items[0]));
                    try self.write(", ");
                    try self.generateExpression(@constCast(&args.items[1]));
                    try self.write(")");
                } else {
                    return CodeGenError.UnsupportedExpression;
                }
            },
            
            else => {
                // Binary operations
                if (args.items.len >= 2) {
                    try self.write("(");
                    try self.generateExpression(@constCast(&args.items[0]));
                    try self.write(" ");
                    try self.write(try self.getOperatorSymbol(op_expr.kind));
                    try self.write(" ");
                    try self.generateExpression(@constCast(&args.items[1]));
                    try self.write(")");
                } else {
                    return CodeGenError.UnsupportedExpression;
                }
            },
        }
    }
    
    fn generateLValue(self: *CodeGen, lvalue: *const SirsParser.LValue) CodeGenError!void {
        switch (lvalue.*) {
            .variable => |var_name| {
                try self.write(var_name);
            },
            
            .index => |*index_lvalue| {
                try self.generateLValue(index_lvalue.array);
                try self.write("[");
                try self.generateExpression(@constCast(&index_lvalue.index));
                try self.write("]");
            },
            
            .field => |*field_lvalue| {
                try self.generateLValue(field_lvalue.object);
                try self.write(".");
                try self.write(field_lvalue.field);
            },
        }
    }
    
    fn generateCustomTypes(self: *CodeGen, program: *Program) CodeGenError!void {
        var type_iter = program.types.iterator();
        while (type_iter.next()) |entry| {
            const type_name = entry.key_ptr.*;
            const type_def = entry.value_ptr;
            
            switch (type_def.*) {
                .@"enum" => |enum_def| {
                    try self.write("const ");
                    try self.write(type_name);
                    try self.writeLine(" = enum {");
                    self.indent_level += 1;
                    
                    // Generate enum variants
                    var variant_iter = enum_def.variants.iterator();
                    while (variant_iter.next()) |variant_entry| {
                        const variant_name = variant_entry.key_ptr.*;
                        const variant_type = variant_entry.value_ptr.*;
                        
                        try self.writeIndent();
                        try self.write(variant_name);
                        
                        // If variant has associated type, we'd need to handle it differently
                        // For now, we're generating simple enums without associated values
                        if (variant_type != null) {
                            // In a full implementation, we'd generate tagged unions for enums with associated values
                            // For now, we'll just generate a comment
                            try self.write(" // TODO: associated value support");
                        }
                        
                        try self.writeLine(",");
                    }
                    
                    self.indent_level -= 1;
                    try self.writeLine("};");
                    try self.writeLine("");
                },
                .@"error" => |error_def| {
                    try self.write("const ");
                    try self.write(type_name);
                    try self.write(" = error{");
                    try self.write(error_def.name);
                    try self.writeLine("};");
                    try self.writeLine("");
                },
                .record => |record_def| {
                    try self.write("const ");
                    try self.write(type_name);
                    try self.writeLine(" = struct {");
                    self.indent_level += 1;
                    
                    // Generate record fields
                    var field_iter = record_def.fields.iterator();
                    while (field_iter.next()) |field_entry| {
                        const field_name = field_entry.key_ptr.*;
                        const field_type = field_entry.value_ptr.*;
                        
                        try self.writeIndent();
                        try self.write(field_name);
                        try self.write(": ");
                        try self.generateType(field_type.*);
                        try self.writeLine(",");
                    }
                    
                    self.indent_level -= 1;
                    try self.writeLine("};");
                    try self.writeLine("");
                },
                .discriminated_union => |union_def| {
                    try self.write("const ");
                    try self.write(type_name);
                    try self.writeLine(" = union(enum) {");
                    self.indent_level += 1;
                    
                    // Generate union variants
                    for (union_def.variants.items, 0..) |variant_type, i| {
                        try self.writeIndent();
                        try self.write("variant");
                        const variant_index_str = std.fmt.allocPrint(self.allocator, "{d}", .{i}) catch return CodeGenError.OutOfMemory;
                        defer self.allocator.free(variant_index_str);
                        try self.write(variant_index_str);
                        try self.write(": ");
                        try self.generateType(variant_type.*);
                        try self.writeLine(",");
                    }
                    
                    self.indent_level -= 1;
                    try self.writeLine("};");
                    try self.writeLine("");
                },
                else => {
                    // Handle other type definitions if needed
                },
            }
        }
        
        // Generate generic type definitions
        var generic_iter = program.generic_types.iterator();
        while (generic_iter.next()) |entry| {
            const type_name = entry.key_ptr.*;
            const generic_type = entry.value_ptr;
            
            // Generate generic type as a Zig function that returns a type
            try self.write("fn ");
            try self.write(type_name);
            try self.write("(comptime ");
            
            // Generate type parameters
            for (generic_type.type_params.items, 0..) |param_name, i| {
                if (i > 0) try self.write(", comptime ");
                try self.write(param_name);
                try self.write(": type");
            }
            
            try self.write(") type {\n");
            self.indent_level += 1;
            
            // Add parameter usage to avoid unused parameter warnings
            for (generic_type.type_params.items) |param_name| {
                try self.writeIndent();
                try self.write("_ = ");
                try self.write(param_name);
                try self.writeLine(";");
            }
            
            try self.writeIndent();
            try self.write("return ");
            
            // Generate the type definition with parameters substituted
            try self.generateType(generic_type.definition);
            try self.writeLine(";");
            
            self.indent_level -= 1;
            try self.writeLine("}");
            try self.writeLine("");
        }
        
        // Generate interface vtables
        var interface_iter = program.interfaces.iterator();
        while (interface_iter.next()) |entry| {
            const interface_name = entry.key_ptr.*;
            const interface_def = entry.value_ptr;
            
            // Generate vtable struct
            try self.write("const ");
            try self.write(interface_name);
            try self.writeLine("VTable = struct {");
            self.indent_level += 1;
            
            var method_iter = interface_def.methods.iterator();
            while (method_iter.next()) |method_entry| {
                const method_name = method_entry.key_ptr.*;
                const signature = method_entry.value_ptr;
                
                try self.writeIndent();
                try self.write(method_name);
                try self.write(": *const fn(");
                
                // Add self parameter as first argument
                try self.write("self: *anyopaque");
                
                for (signature.args.items, 0..) |arg_type, i| {
                    _ = i;
                    try self.write(", ");
                    try self.generateType(arg_type);
                }
                
                try self.write(") ");
                try self.generateType(signature.@"return");
                try self.writeLine(",");
            }
            
            self.indent_level -= 1;
            try self.writeLine("};");
            try self.writeLine("");
        }
    }
    
    fn generateType(self: *CodeGen, t: Type) CodeGenError!void {
        switch (t) {
            .void => try self.write("void"),
            .bool => try self.write("bool"),
            .i8 => try self.write("i8"),
            .i16 => try self.write("i16"),
            .i32 => try self.write("i32"),
            .i64 => try self.write("i64"),
            .u8 => try self.write("u8"),
            .u16 => try self.write("u16"),
            .u32 => try self.write("u32"),
            .u64 => try self.write("u64"),
            .f32 => try self.write("f32"),
            .f64 => try self.write("f64"),
            .str => try self.write("[]const u8"),
            
            .array => |arr| {
                try self.write("[");
                const size_str = std.fmt.allocPrint(self.allocator, "{d}", .{arr.size}) catch return CodeGenError.OutOfMemory;
                defer self.allocator.free(size_str);
                try self.write(size_str);
                try self.write("]");
                try self.generateType(arr.element.*);
            },
            
            .slice => |slice| {
                try self.write("[]");
                try self.generateType(slice.element.*);
            },
            
            .optional => |opt| {
                try self.write("?");
                try self.generateType(opt.*);
            },
            
            .@"enum" => |enum_def| {
                try self.write(enum_def.name);
            },
            
            .@"error" => |error_def| {
                try self.write(error_def.name);
            },
            
            .hashmap => |hashmap_def| {
                // Use StringHashMap for string keys, HashMap for others
                switch (hashmap_def.key.*) {
                    .str => {
                        try self.write("std.StringHashMap(");
                        try self.generateType(hashmap_def.value.*);
                        try self.write(")");
                    },
                    else => {
                        try self.write("std.HashMap(");
                        try self.generateType(hashmap_def.key.*);
                        try self.write(", ");
                        try self.generateType(hashmap_def.value.*);
                        try self.write(", std.hash_map.AutoContext(");
                        try self.generateType(hashmap_def.key.*);
                        try self.write("), std.hash_map.default_max_load_percentage)");
                    },
                }
            },
            
            .set => |set_def| {
                try self.write("std.HashMap(");
                try self.generateType(set_def.element.*);
                try self.write(", void, std.hash_map.AutoContext(");
                try self.generateType(set_def.element.*);
                try self.write("), std.hash_map.default_max_load_percentage)");
            },
            
            .tuple => |tuple_types| {
                try self.write("struct { ");
                for (tuple_types.items, 0..) |type_ptr, i| {
                    if (i > 0) try self.write(", ");
                    const field_name = std.fmt.allocPrint(self.allocator, "_{d}", .{i}) catch return CodeGenError.OutOfMemory;
                    defer self.allocator.free(field_name);
                    try self.write(field_name);
                    try self.write(": ");
                    try self.generateType(type_ptr.*);
                }
                try self.write(" }");
            },
            
            .record => |record_def| {
                try self.write(record_def.name);
            },
            
            .type_parameter => |param_name| {
                // In a monomorphized context, type parameters should be substituted
                // For now, just generate the parameter name as a placeholder
                try self.write(param_name);
            },
            
            .function => |func_type| {
                // Generate function type
                if (func_type.@"async") {
                    try self.write("async ");
                }
                try self.write("fn (");
                for (func_type.args.items, 0..) |arg_type, i| {
                    if (i > 0) try self.write(", ");
                    try self.generateType(arg_type);
                }
                try self.write(") ");
                try self.generateType(func_type.@"return".*);
            },
            
            .generic_instance => |generic_inst| {
                // Generate instantiated generic type
                try self.write(generic_inst.base_type);
                try self.write("(");
                for (generic_inst.type_args.items, 0..) |type_arg, i| {
                    if (i > 0) try self.write(", ");
                    try self.generateType(type_arg.*);
                }
                try self.write(")");
            },
            
            .@"interface" => |interface_def| {
                // Interfaces generate as pointers to vtables in Zig
                try self.write("*const ");
                try self.write(interface_def.name);
                try self.write("VTable");
            },
            
            .trait_object => |trait_obj| {
                // Trait objects generate as struct with vtable and data pointer
                try self.write("struct { vtable: *const ");
                try self.write(trait_obj.trait_name);
                try self.write("VTable, data: *anyopaque }");
            },
            
            .discriminated_union => |union_def| {
                // Discriminated unions generate as tagged unions in Zig
                try self.write(union_def.name);
            },
            
            .future => |future_type| {
                // Future types in Zig - for simplicity, we'll use the wrapped type directly
                // In a full implementation, this would be a proper async frame type
                try self.generateType(future_type.*);
            },
            
            .result => |result_def| {
                // Generate Zig error union: Error!Type
                try self.generateType(result_def.err_type.*);
                try self.write("!");
                try self.generateType(result_def.ok_type.*);
            },
            
            else => {
                return CodeGenError.UnsupportedType;
            },
        }
    }
    
    fn getOperatorSymbol(_: *CodeGen, op: SirsParser.OpKind) ![]const u8 {
        return switch (op) {
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
            .mod => "%",
            .eq => "==",
            .ne => "!=",
            .lt => "<",
            .le => "<=",
            .gt => ">",
            .ge => ">=",
            .@"and" => "and",
            .@"or" => "or",
            .bitand => "&",
            .bitor => "|",
            .bitxor => "^",
            .shl => "<<",
            .shr => ">>",
            else => return CodeGenError.UnsupportedExpression,
        };
    }
    
    fn compileZigFile(self: *CodeGen, zig_file: []const u8, output_file: []const u8) CodeGenError!void {
        // Extract just the basename for the package name  
        const basename = std.fs.path.basename(output_file);
        
        // Use the full output_file path which already includes dist/
        const cmd = std.fmt.allocPrint(self.allocator, "zig build-exe {s} -O ReleaseFast --name {s} -femit-bin={s}", .{ zig_file, basename, output_file }) catch return CodeGenError.OutOfMemory;
        defer self.allocator.free(cmd);
        
        const result = std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = &[_][]const u8{ "sh", "-c", cmd },
        }) catch return CodeGenError.IoError;
        defer self.allocator.free(result.stdout);
        defer self.allocator.free(result.stderr);
        
        if (result.term.Exited != 0) {
            print("Compilation failed:\n{s}\n", .{result.stderr});
            return CodeGenError.CompilationError;
        }
    }
    
    fn generateMatchStatement(self: *CodeGen, match_stmt: anytype) CodeGenError!void {
        // Generate a match expression using if-else chain
        try self.writeIndent();
        try self.writeLine("{");
        self.indent_level += 1;
        
        // Store the match value in a temporary variable
        try self.writeIndent();
        try self.write("const _match_value = ");
        try self.generateExpression(&match_stmt.value);
        try self.writeLine(";");
        
        // Generate if-else chain for pattern matching
        for (match_stmt.cases.items, 0..) |*case, i| {
            if (i == 0) {
                try self.writeIndent();
                try self.write("if (");
            } else {
                try self.write(" else if (");
            }
            
            // Generate pattern matching condition
            try self.generatePatternCondition(&case.pattern, "_match_value");
            try self.writeLine(") {");
            
            self.indent_level += 1;
            
            // Generate variable bindings for the pattern
            try self.generatePatternBindings(&case.pattern, "_match_value");
            
            // Generate case body
            for (case.body.items) |*stmt| {
                try self.generateStatement(stmt);
            }
            
            self.indent_level -= 1;
            try self.writeIndent();
            try self.write("}");
        }
        
        // Close the final else
        if (match_stmt.cases.items.len > 0) {
            try self.writeLine("");
        }
        
        self.indent_level -= 1;
        try self.writeIndent();
        try self.writeLine("}");
    }
    
    fn generateTryStatement(self: *CodeGen, try_stmt: anytype) CodeGenError!void {
        // In Zig, we'll generate try-catch using error unions and if-else chains
        // For now, we'll generate a simplified version that catches any error
        
        try self.writeIndent();
        try self.writeLine("{");
        self.indent_level += 1;
        
        // Generate try body with error handling
        if (try_stmt.catch_clauses.items.len > 0) {
            // If there are catch clauses, wrap in error handling
            try self.writeIndent();
            try self.writeLine("if (true) {"); // Simple wrapper for now
            self.indent_level += 1;
            
            // Generate try body
            for (try_stmt.body.items) |*stmt| {
                try self.generateStatement(stmt);
            }
            
            self.indent_level -= 1;
            try self.writeIndent();
            try self.write("}");
            
            // Generate catch blocks
            for (try_stmt.catch_clauses.items) |*catch_clause| {
                try self.write(" else {");
                try self.writeLine("");
                self.indent_level += 1;
                
                // If there's a variable binding, create it (for now, skip to avoid unused variable)
                if (catch_clause.variable_name) |var_name| {
                    // Skip variable generation for now to avoid unused variable warnings
                    _ = var_name;
                    // try self.writeIndent();
                    // try self.write("const ");
                    // // Add prefix to avoid Zig reserved keywords
                    // try self.write("sever_");
                    // try self.write(var_name);
                    // try self.writeLine(" = \"Exception occurred\";"); // Simplified
                }
                
                // Generate catch body
                for (catch_clause.body.items) |*stmt| {
                    try self.generateStatement(stmt);
                }
                
                self.indent_level -= 1;
                try self.writeIndent();
                try self.write("}");
            }
            
            try self.writeLine("");
        } else {
            // No catch clauses, just generate try body
            for (try_stmt.body.items) |*stmt| {
                try self.generateStatement(stmt);
            }
        }
        
        // Generate finally block if present
        if (try_stmt.finally_body) |*finally_stmts| {
            try self.writeIndent();
            try self.writeLine("// Finally block");
            for (finally_stmts.items) |*stmt| {
                try self.generateStatement(stmt);
            }
        }
        
        self.indent_level -= 1;
        try self.writeIndent();
        try self.writeLine("}");
    }
    
    fn generatePatternCondition(self: *CodeGen, pattern: *SirsParser.Pattern, match_var: []const u8) CodeGenError!void {
        switch (pattern.*) {
            .literal => |literal| {
                try self.write(match_var);
                try self.write(" == ");
                try self.generateLiteral(literal);
            },
            
            .variable => |_| {
                // Variable patterns always match
                try self.write("true");
            },
            
            .wildcard => {
                // Wildcard patterns always match
                try self.write("true");
            },
            
            .@"struct" => |*struct_patterns| {
                // For struct patterns, we need to check each field
                // This is simplified - a full implementation would handle this properly
                try self.write("true"); // Simplified for now
                
                // In a full implementation, we would generate:
                // match_var.field1 == pattern.field1 && match_var.field2 == pattern.field2 && ...
                _ = struct_patterns; // Avoid unused warning
            },
            
            .@"enum" => |*enum_pattern| {
                // For enum patterns, check the tag matches
                // This is simplified - a full implementation would handle associated values properly
                try self.write("@intFromEnum(");
                try self.write(match_var);
                try self.write(") == @intFromEnum(");
                try self.write(enum_pattern.enum_type);
                try self.write(".");
                try self.write(enum_pattern.variant);
                try self.write(")");
            },
        }
    }
    
    fn generatePatternBindings(self: *CodeGen, pattern: *SirsParser.Pattern, match_var: []const u8) CodeGenError!void {
        switch (pattern.*) {
            .variable => |var_name| {
                // Bind the variable to the matched value
                try self.writeIndent();
                try self.write("const ");
                try self.write(var_name);
                try self.write(" = ");
                try self.write(match_var);
                try self.writeLine(";");
            },
            
            .@"struct" => |*struct_patterns| {
                // For struct patterns, bind each field
                var iter = struct_patterns.iterator();
                while (iter.next()) |entry| {
                    const field_name = entry.key_ptr.*;
                    const field_pattern = entry.value_ptr;
                    
                    // Create field access expression
                    const field_access = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ match_var, field_name });
                    defer self.allocator.free(field_access);
                    
                    // Recursively generate bindings for the field pattern
                    try self.generatePatternBindings(field_pattern, field_access);
                }
            },
            
            .@"enum" => |*enum_pattern| {
                // For enum patterns, bind any associated values
                if (enum_pattern.value_pattern) |value_pattern| {
                    // In a full implementation, we'd extract the associated value
                    // For now, just recursively handle the value pattern
                    try self.generatePatternBindings(value_pattern, match_var); // Simplified
                }
            },
            
            .literal, .wildcard => {
                // These patterns don't create bindings
            },
        }
    }
    
    fn generateLiteral(self: *CodeGen, literal: SirsParser.Literal) CodeGenError!void {
        switch (literal) {
            .integer => |i| {
                const str = std.fmt.allocPrint(self.allocator, "{d}", .{i}) catch return CodeGenError.OutOfMemory;
                defer self.allocator.free(str);
                try self.write(str);
            },
            .float => |f| {
                const str = std.fmt.allocPrint(self.allocator, "{d}", .{f}) catch return CodeGenError.OutOfMemory;
                defer self.allocator.free(str);
                try self.write(str);
            },
            .string => |s| {
                try self.write("\"");
                // Escape backslashes and quotes in string literals
                for (s) |char| {
                    switch (char) {
                        '\\' => try self.write("\\\\"),
                        '"' => try self.write("\\\""),
                        '\n' => try self.write("\\n"),
                        '\r' => try self.write("\\r"),
                        '\t' => try self.write("\\t"),
                        else => {
                            const char_str = [_]u8{char};
                            try self.write(&char_str);
                        },
                    }
                }
                try self.write("\"");
            },
            .boolean => |b| {
                if (b) {
                    try self.write("true");
                } else {
                    try self.write("false");
                }
            },
            .null => {
                try self.write("null");
            },
        }
    }
    
    fn write(self: *CodeGen, text: []const u8) CodeGenError!void {
        self.output.appendSlice(text) catch return CodeGenError.OutOfMemory;
    }
    
    fn writeLine(self: *CodeGen, text: []const u8) CodeGenError!void {
        self.output.appendSlice(text) catch return CodeGenError.OutOfMemory;
        self.output.append('\n') catch return CodeGenError.OutOfMemory;
    }
    
    fn writeIndent(self: *CodeGen) CodeGenError!void {
        var i: u32 = 0;
        while (i < self.indent_level) : (i += 1) {
            self.output.appendSlice("    ") catch return CodeGenError.OutOfMemory;
        }
    }
    
    fn isAsyncFunction(self: *CodeGen, function_name: []const u8) bool {
        // For now, check if function name contains "fetch" or similar async patterns
        // In a full implementation, we'd track async functions from the AST
        _ = self;
        return std.mem.indexOf(u8, function_name, "fetch") != null or
               std.mem.indexOf(u8, function_name, "async") != null;
    }
    
    fn generateAsyncStatement(self: *CodeGen, stmt: *Statement) CodeGenError!void {
        switch (stmt.*) {
            .@"return" => |*return_expr| {
                try self.writeIndent();
                try self.write("f.complete(");
                try self.generateExpression(return_expr);
                try self.writeLine(");");
                try self.writeIndent();
                try self.writeLine("return;");
            },
            else => {
                // For other statements, generate normally
                try self.generateStatement(stmt);
            },
        }
    }
    
    fn generateAsyncRuntime(self: *CodeGen) CodeGenError!void {
        // Generate simplified async runtime inline
        try self.writeLine("// Async Runtime Types");
        try self.writeLine("pub const TaskStatus = enum { pending, running, completed, failed };");
        try self.writeLine("");
        
        // Generate Future type
        try self.writeLine("pub fn Future(comptime T: type) type {");
        try self.writeLine("    return struct {");
        try self.writeLine("        const Self = @This();");
        try self.writeLine("        status: TaskStatus = .pending,");
        try self.writeLine("        value: ?T = null,");
        try self.writeLine("        error_value: ?anyerror = null,");
        try self.writeLine("        mutex: Mutex = .{},");
        try self.writeLine("        ");
        try self.writeLine("        pub fn complete(self: *Self, val: T) void {");
        try self.writeLine("            self.mutex.lock();");
        try self.writeLine("            defer self.mutex.unlock();");
        try self.writeLine("            self.value = val;");
        try self.writeLine("            self.status = .completed;");
        try self.writeLine("        }");
        try self.writeLine("        ");
        try self.writeLine("        pub fn wait(self: *Self) !T {");
        try self.writeLine("            while (true) {");
        try self.writeLine("                self.mutex.lock();");
        try self.writeLine("                const status = self.status;");
        try self.writeLine("                const val = self.value;");
        try self.writeLine("                self.mutex.unlock();");
        try self.writeLine("                if (status == .completed) return val.?;");
        try self.writeLine("                if (status == .failed) return self.error_value.?;");
        try self.writeLine("                std.time.sleep(1_000_000);");
        try self.writeLine("            }");
        try self.writeLine("        }");
        try self.writeLine("    };");
        try self.writeLine("}");
        try self.writeLine("");
        
        // Generate simple async runtime
        try self.writeLine("pub const AsyncRuntime = struct {");
        try self.writeLine("    allocator: Allocator,");
        try self.writeLine("    ");
        try self.writeLine("    pub fn init(alloc: Allocator) !AsyncRuntime {");
        try self.writeLine("        return AsyncRuntime{ .allocator = alloc };");
        try self.writeLine("    }");
        try self.writeLine("    ");
        try self.writeLine("    pub fn spawn(self: *AsyncRuntime, comptime T: type, func: *const fn() T) !*Future(T) {");
        try self.writeLine("        const future = try self.allocator.create(Future(T));");
        try self.writeLine("        future.* = Future(T){};");
        try self.writeLine("        // Spawn thread to execute function");
        try self.writeLine("        const Context = struct {");
        try self.writeLine("            future: *Future(T),");
        try self.writeLine("            func: *const fn() T,");
        try self.writeLine("        };");
        try self.writeLine("        const ctx = try self.allocator.create(Context);");
        try self.writeLine("        ctx.* = .{ .future = future, .func = func };");
        try self.writeLine("        _ = try Thread.spawn(.{}, struct {");
        try self.writeLine("            fn run(context: *Context) void {");
        try self.writeLine("                const result = context.func();");
        try self.writeLine("                context.future.complete(result);");
        try self.writeLine("            }");
        try self.writeLine("        }.run, .{ctx});");
        try self.writeLine("        return future;");
        try self.writeLine("    }");
        try self.writeLine("};");
        try self.writeLine("");
        
        // Generate channel type
        try self.writeLine("pub fn Channel(comptime T: type) type {");
        try self.writeLine("    return struct {");
        try self.writeLine("        const Self = @This();");
        try self.writeLine("        buffer: ArrayList(T),");
        try self.writeLine("        mutex: Mutex = .{},");
        try self.writeLine("        ");
        try self.writeLine("        pub fn init(alloc: Allocator) Self {");
        try self.writeLine("            return Self{ .buffer = ArrayList(T).init(alloc) };");
        try self.writeLine("        }");
        try self.writeLine("        ");
        try self.writeLine("        pub fn send(self: *Self, value: T) !void {");
        try self.writeLine("            self.mutex.lock();");
        try self.writeLine("            defer self.mutex.unlock();");
        try self.writeLine("            try self.buffer.append(value);");
        try self.writeLine("        }");
        try self.writeLine("        ");
        try self.writeLine("        pub fn receive(self: *Self) ?T {");
        try self.writeLine("            self.mutex.lock();");
        try self.writeLine("            defer self.mutex.unlock();");
        try self.writeLine("            if (self.buffer.items.len > 0) {");
        try self.writeLine("                return self.buffer.orderedRemove(0);");
        try self.writeLine("            }");
        try self.writeLine("            return null;");
        try self.writeLine("        }");
        try self.writeLine("    };");
        try self.writeLine("}");
        try self.writeLine("");
    }
    
    fn generateCallHandlerFunction(self: *CodeGen, program: *Program) CodeGenError!void {
        try self.writeLine("fn callHandler(handler_name: []const u8, method: []const u8, path: []const u8, body: []const u8) []const u8 {");
        self.indent_level += 1;
        
        // Generate if-else chain for each available function
        var func_iter = program.functions.iterator();
        var first = true;
        while (func_iter.next()) |entry| {
            const func_name = entry.key_ptr.*;
            const function = entry.value_ptr;
            
            // Skip main function
            if (std.mem.eql(u8, func_name, "main")) continue;
            
            // Generate handler dispatch based on function signature
            if (first) {
                try self.writeIndent();
                try self.write("if (std.mem.eql(u8, handler_name, \"");
                try self.write(func_name);
                try self.writeLine("\")) {");
                first = false;
            } else {
                try self.writeIndent();
                try self.write("} else if (std.mem.eql(u8, handler_name, \"");
                try self.write(func_name);
                try self.writeLine("\")) {");
            }
            
            self.indent_level += 1;
            try self.writeIndent();
            try self.write("return ");
            try self.write(func_name);
            try self.write("(");
            
            // Generate parameters based on function signature
            for (function.args.items, 0..) |param, i| {
                if (i > 0) try self.write(", ");
                
                // Map standard HTTP parameters
                if (std.mem.eql(u8, param.name, "method")) {
                    try self.write("method");
                } else if (std.mem.eql(u8, param.name, "path")) {
                    try self.write("path");
                } else if (std.mem.eql(u8, param.name, "body")) {
                    try self.write("body");
                } else {
                    // For other parameters, provide defaults based on type
                    switch (param.type) {
                        .f64 => try self.write("0.0"),
                        .i32 => try self.write("0"),
                        .str => try self.write("\"\""),
                        .bool => try self.write("false"),
                        else => try self.write("undefined"),
                    }
                }
            }
            
            try self.writeLine(");");
            self.indent_level -= 1;
        }
        
        // Default case
        try self.writeIndent();
        try self.writeLine("} else {");
        self.indent_level += 1;
        try self.writeIndent();
        try self.writeLine("return \"{\\\"error\\\": \\\"Unknown handler\\\"}\";");
        self.indent_level -= 1;
        try self.writeIndent();
        try self.writeLine("}");
        
        self.indent_level -= 1;
        try self.writeLine("}");
        try self.writeLine("");
    }
};