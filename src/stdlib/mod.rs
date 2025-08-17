use std::{fs, path::Path};

const STDLIB_DIR_PATH: &'static str = "./src/stdlib";

fn collect_file_paths(dir_path: &Path) -> Vec<String> {
    let mut file_paths = vec![];

    for entry in fs::read_dir(dir_path).unwrap() {
        let path = entry.unwrap().path();

        if path.is_dir() {
            file_paths.extend(collect_file_paths(&path));
        } else if path.extension().unwrap() == "vay" {
            file_paths.push(path.to_str().unwrap().to_string());
        }
    }

    file_paths
}

pub fn collect_std_file_paths() -> Vec<String> {
    collect_file_paths(Path::new(STDLIB_DIR_PATH))
}