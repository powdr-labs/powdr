use std::env;
use std::fs;
use std::path::{Path, PathBuf};

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let src_dir = PathBuf::from("src/rule_based_optimizer");
    let rules_dir = src_dir.join("rules");
    let template_file = src_dir.join("rules.rs");
    let output_file = PathBuf::from(&out_dir).join("rules_generated.rs");

    // Tell Cargo to rerun this build script if any of these change
    println!("cargo:rerun-if-changed={}", template_file.display());
    
    // Read the template file
    let template_content = fs::read_to_string(&template_file)
        .expect("Failed to read rules.rs template");

    // Check if rules directory exists
    let generated_content = if rules_dir.exists() {
        println!("cargo:rerun-if-changed={}", rules_dir.display());
        
        // Watch all .rs files in the rules directory
        if let Ok(entries) = fs::read_dir(&rules_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().and_then(|s| s.to_str()) == Some("rs") {
                    println!("cargo:rerun-if-changed={}", path.display());
                }
            }
        }

        inline_rule_files(&template_content, &rules_dir)
    } else {
        // If no rules directory exists, just use the template as-is
        template_content
    };

    // Write the generated content to the output directory
    fs::write(&output_file, generated_content)
        .expect("Failed to write generated rules file");
}

fn inline_rule_files(template: &str, rules_dir: &Path) -> String {
    let mut result = String::new();
    let mut line_number = 0;
    
    for line in template.lines() {
        line_number += 1;
        let trimmed = line.trim();
        
        // Look for lines like: // @include "filename.rs"
        if trimmed.starts_with("// @include") {
            if let Some(filename) = extract_filename(trimmed) {
                let rule_file = rules_dir.join(filename);
                
                if rule_file.exists() {
                    match fs::read_to_string(&rule_file) {
                        Ok(content) => {
                            result.push_str(&format!("    // BEGIN INCLUDE: {}\n", filename));
                            
                            // Get canonical path for better error messages
                            let canonical_path = rule_file.canonicalize()
                                .unwrap_or_else(|_| rule_file.clone());
                            
                            // Add #line directive to preserve source location
                            result.push_str(&format!("# 1 \"{}\"\n", canonical_path.display()));
                            
                            // Indent the included content appropriately
                            for include_line in content.lines() {
                                if !include_line.trim().is_empty() {
                                    result.push_str("    ");
                                    result.push_str(include_line);
                                }
                                result.push('\n');
                            }
                            
                            // Return to the template file location
                            result.push_str(&format!("# {} \"src/rule_based_optimizer/rules.rs\"\n", line_number + 1));
                            result.push_str(&format!("    // END INCLUDE: {}\n", filename));
                            continue;
                        }
                        Err(e) => {
                            panic!("Failed to read rule file {}: {}", rule_file.display(), e);
                        }
                    }
                } else {
                    panic!("Rule file not found: {}", rule_file.display());
                }
            }
        }
        
        result.push_str(line);
        result.push('\n');
    }
    
    result
}

fn extract_filename(line: &str) -> Option<&str> {
    // Extract filename from: // @include "filename.rs"
    let start = line.find('"')?;
    let end = line.rfind('"')?;
    if start < end {
        Some(&line[start + 1..end])
    } else {
        None
    }
}
