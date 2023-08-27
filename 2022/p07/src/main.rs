use std::collections::HashMap;
use std::env;
use std::fs;

fn main() -> Result<(), String> {
    let filename = parse_args();
    let input = load_input(&filename)?;

    let sum = sum_directories(&input)?;
    println!("sum: {}", sum);
    Ok(())
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input file>", args[0]);
    }

    args.remove(1)
}

fn load_input(filename: &str) -> Result<Vec<String>, String> {
    let contents = fs::read_to_string(filename).map_err(|e| e.to_string())?;
    Ok(contents.lines().map(|line| line.to_string()).collect())
}

fn sum_directories(input: &[String]) -> Result<u64, String> {
    let commands = parse_commands(input)?;
    let file_tree = build_file_tree(&commands);
    Ok(sum_file_tree(&file_tree))
}

#[derive(Debug)]
enum Command {
    ChangeDirectory(String),
    ListDirectory(Vec<DirectoryContents>),
}

#[derive(Debug)]
enum DirectoryContents {
    Directory(String),
    File(FileInfo),
}

#[derive(Debug)]
struct FileInfo {
    name: String,
    size: u64,
}

fn parse_commands(input: &[String]) -> Result<Vec<Command>, String> {
    let mut commands = Vec::new();
    if input.is_empty() {
        return Err("Empty input".to_string());
    }

    let mut input = input.iter();
    let mut line: String = input.next().unwrap().to_string();

    loop {
        let l = match &line[..4] {
            "$ cd" => {
                let dir = line[5..].to_string();
                commands.push(Command::ChangeDirectory(dir));
                input.next()
            }
            "$ ls" => {
                let (command, l) = parse_dir_command(&mut input)?;
                commands.push(command);
                l
            }

            _ => return Err(format!("Invalid command: {}", line)),
        };

        if l.is_none() {
            return Ok(commands);
        }

        line = l.unwrap().to_string();
    }
}

fn parse_dir_command<'a>(
    input: &mut std::slice::Iter<'a, String>,
) -> Result<(Command, Option<&'a String>), String> {
    let mut dir_contents = Vec::new();

    loop {
        let l = input.next();
        if l.is_none() {
            return Ok((Command::ListDirectory(dir_contents), None));
        }

        let line = l.unwrap();
        if line.starts_with("$") {
            return Ok((Command::ListDirectory(dir_contents), Some(line)));
        }

        if line.starts_with("dir") {
            let dir = line[4..].to_string();
            dir_contents.push(DirectoryContents::Directory(dir));
        } else {
            let file = parse_file_info(&line)?;
            dir_contents.push(DirectoryContents::File(file));
        }
    }
}

fn parse_file_info(line: &str) -> Result<FileInfo, String> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.len() != 2 {
        return Err("Invalid file info".to_string());
    }

    let size = parts[0].parse::<u64>().map_err(|e| e.to_string())?;
    let name = parts[1].to_string();

    Ok(FileInfo { name, size })
}

struct FileTree {
    nodes: Vec<FileTreeNode>,
}

struct FileTreeNode {
    index: usize,
    name: String,
    size: u64,
    children: HashMap<String, usize>,
    parent: Option<usize>,
}

fn build_file_tree(commands: &[Command]) -> FileTree {
    let mut tree = FileTree { nodes: Vec::new() };
    let root = FileTreeNode {
        index: 0,
        name: "/".to_string(),
        size: 0,
        children: HashMap::new(),
        parent: None,
    };
    tree.nodes.push(root);

    let mut node_index = 0;

    for command in commands {
        match command {
            Command::ChangeDirectory(dir) => {
                node_index = change_directory(&mut tree, node_index, dir);
            }
            Command::ListDirectory(dir_contents) => {
                list_directory(&mut tree, node_index, dir_contents);
            }
        }
    }

    calculate_sizes(tree)
}

fn change_directory(tree: &mut FileTree, node_index: usize, dir: &str) -> usize {
    if dir == "/" {
        return node_index;
    }

    let node = &tree.nodes[node_index];
    if dir == ".." {
        if let Some(parent) = node.parent {
            return parent;
        } else {
            return node_index;
        }
    }

    let child = node.children.get(dir);
    if let Some(&child) = child {
        return child;
    }

    let child = FileTreeNode {
        index: tree.nodes.len(),
        name: dir.to_string(),
        size: 0,
        children: HashMap::new(),
        parent: Some(node.index),
    };

    let child_index = child.index;
    let node = &mut tree.nodes[node_index];
    node.children.insert(dir.to_string(), child_index);
    tree.nodes.push(child);

    child_index
}

fn list_directory(tree: &mut FileTree, node_index: usize, dir_contents: &[DirectoryContents]) {
    for dir_content in dir_contents {
        let node = &tree.nodes[node_index];
        let child = match dir_content {
            DirectoryContents::Directory(dir) => FileTreeNode {
                index: tree.nodes.len(),
                name: dir.to_string(),
                size: 0,
                children: HashMap::new(),
                parent: Some(node.index),
            },
            DirectoryContents::File(file) => FileTreeNode {
                index: tree.nodes.len(),
                name: file.name.to_string(),
                size: file.size,
                children: HashMap::new(),
                parent: Some(node.index),
            },
        };

        let child_index = child.index;
        let node = &mut tree.nodes[node_index];
        node.children.insert(child.name.to_string(), child_index);
        tree.nodes.push(child);
    }
}

fn calculate_sizes(mut tree: FileTree) -> FileTree {
    calculate_sizes_recur(&mut tree, 0);
    tree
}

fn calculate_sizes_recur(tree: &mut FileTree, node_index: usize) {
    let children: Vec<usize> = tree.nodes[node_index].children.values().cloned().collect();
    if children.is_empty() {
        return;
    }

    for &child in children.iter() {
        calculate_sizes_recur(tree, child);
    }

    let node = &tree.nodes[node_index];
    let size: u64 = node
        .children
        .values()
        .map(|child| tree.nodes[*child].size)
        .sum();

    let mut node = &mut tree.nodes[node_index];
    node.size = size;
}

const MAX_NODE_SIZE: u64 = 100000;

fn sum_file_tree(tree: &FileTree) -> u64 {
    tree.nodes
        .iter()
        .filter(|node| !node.children.is_empty() && node.size <= MAX_NODE_SIZE)
        .map(|node| node.size)
        .sum::<u64>()
}
