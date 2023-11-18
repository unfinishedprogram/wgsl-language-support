struct UnclosedCandidate {
    position: usize,
    depth: u32,
}

fn find_templates(src: &str) -> Vec<(usize, usize)> {
    let chars: Vec<char> = src.chars().collect();
    let mut discovered_template_lists = vec![];
    let mut pending: Vec<UnclosedCandidate> = vec![];
    let mut current_position: usize = 0;
    let mut nesting_depth: u32 = 0;

    while current_position < chars.len() {
        match chars[current_position] {
            '<' => {
                pending.push(UnclosedCandidate {
                    position: current_position,
                    depth: nesting_depth,
                });
                current_position += 1;
                if chars[current_position] == '<' || chars[current_position] == '=' {
                    pending.pop();
                    current_position += 1;
                    continue;
                }
            }
            '>' => match pending.last() {
                Some(unclosed) if unclosed.depth == nesting_depth => {
                    discovered_template_lists.push((unclosed.position, current_position));
                    pending.pop();
                    current_position += 1;
                    continue;
                }
                _ => {
                    current_position += 1;
                    if chars[current_position] == '=' {
                        current_position += 1
                    }
                    continue;
                }
            },

            '(' | '[' => {
                nesting_depth += 1;
                current_position += 1;
                continue;
            }

            ')' | ']' => {
                loop {
                    pending.pop();
                    if pending.is_empty() || pending.last().unwrap().depth < nesting_depth {
                        break;
                    }
                }
                nesting_depth = nesting_depth.saturating_sub(1);
                current_position += 1;
                continue;
            }

            '!' => {
                current_position += 1;
                if chars[current_position] == '=' {
                    current_position += 1
                }
                continue;
            }

            '=' => {
                current_position += 1;
                if chars[current_position] != '=' {
                    nesting_depth = 0;
                    pending.clear();
                }
                current_position += 1;
                continue;
            }

            ';' | '{' | ':' => {
                nesting_depth = 0;
                pending.clear();
                current_position += 1;
            }

            '&' if chars[current_position + 1] == '&' => {
                loop {
                    pending.pop();
                    if pending.is_empty() || pending.last().unwrap().depth < nesting_depth {
                        break;
                    }
                }
                current_position += 2;
            }

            '|' if chars[current_position + 1] == '|' => {
                loop {
                    pending.pop();
                    if pending.is_empty() || pending.last().unwrap().depth < nesting_depth {
                        break;
                    }
                }
                current_position += 2;
            }
            _ => current_position += 1,
        };
    }

    discovered_template_lists
}

pub fn insert_template_delimiters(src: &str) -> String {
    let templates = find_templates(src);

    // Replace the characters in the template with special chars that are unlikely to be otherwise used
    // TODO: Make this algorithm more efficient
    src.chars()
        .enumerate()
        .map(|(index, ch)| {
            for (start, end) in &templates {
                if index == *start {
                    return '⋖';
                } else if index == *end {
                    return '⋗';
                }
            }

            ch
        })
        .collect()
}
