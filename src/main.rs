use std::{env, io, process};

use log::{debug, info};

// Usage: echo <input_text> | your_program.sh -E <pattern>
fn main() {
    env_logger::init();

    if env::args().nth(1).unwrap() != "-E" {
        println!("Expected first argument to be '-E'");
        process::exit(1);
    }

    let pattern = env::args().nth(2).unwrap();

    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();

    if match_pattern(&input_line, &pattern) {
        process::exit(0)
    } else {
        process::exit(1)
    }
}

#[derive(Debug, Clone)]
struct Regexp {
    patterns: Vec<Pattern>,
    group_count: usize,
}

#[derive(Debug, Clone)]
struct RegexpTester<'r, 'i> {
    regexp: &'r Regexp,
    starting_input: &'i str,
    groups: Box<[&'i str]>,
    capture_start_stack: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Pattern {
    Exact(char),
    PositiveGroup(Vec<char>),
    NegativeGroup(Vec<char>),
    Digit,
    AlphaNumeric,
    StartAnchor,
    EndAnchor,
    OneOrMore(Box<Pattern>),
    ZeroOrOne(Box<Pattern>),
    Wildcard,
    CaptureGroupStart { post_seperator_jump: Option<usize> },
    CaptureGroupSeperator { end_jump: usize },
    CaptureGroupEnd { group_number: usize },
    Backref(usize),
}

fn match_pattern(input_line: &str, pattern: &str) -> bool {
    let regexp = Regexp::compile(pattern);

    info!("matching pattern {pattern:?} on input {input_line:?}");

    regexp.match_pattern(input_line)
}

#[derive(Debug, Clone)]
struct Compiler<'source> {
    source: &'source str,
    curr: usize,
    group_count: usize,
    patterns: Vec<Pattern>,
}

impl<'source> Compiler<'source> {
    fn new(pattern: &'source str) -> Self {
        Self {
            source: pattern,
            curr: 0,
            group_count: 0,
            patterns: Vec::new(),
        }
    }

    fn compile(mut self) -> Regexp {
        self.compile_regexp();
        Regexp {
            patterns: self.patterns,
            group_count: self.group_count,
        }
    }

    fn compile_regexp(&mut self) {
        debug!("compiling regexp: {:?}", &self.source[self.curr..]);

        if self.match_char('^') {
            self.patterns.push(Pattern::StartAnchor);
        }

        while let Some(ch) = self.advance() {
            match ch {
                '\\' => match self.advance() {
                    Some('d') => self.patterns.push(Pattern::Digit),
                    Some('w') => self.patterns.push(Pattern::AlphaNumeric),
                    Some('\\') => self.patterns.push(Pattern::Exact('\\')),
                    Some(ch) if ch.is_ascii_digit() => self
                        .patterns
                        .push(Pattern::Backref(ch.to_digit(10).unwrap() as usize)),
                    Some(ch) => panic!("Unknown escape character: {ch}"),
                    None => panic!("Expected character after '\\'"),
                },
                '.' => self.patterns.push(Pattern::Wildcard),
                '[' if self.match_char('^') => {
                    self.compile_group(true);
                }
                '[' => self.compile_group(false),
                '$' if self.peek().is_none() => self.patterns.push(Pattern::EndAnchor),
                '+' => {
                    let prev_pattern = self.patterns.pop().expect("expected a pattern before '+'");
                    self.patterns
                        .push(Pattern::OneOrMore(Box::new(prev_pattern)));
                }
                '?' => {
                    let prev_pattern = self.patterns.pop().expect("expected a pattern before '?'");
                    self.patterns
                        .push(Pattern::ZeroOrOne(Box::new(prev_pattern)));
                }
                '(' => self.compile_capture_group(),
                ch => self.patterns.push(Pattern::Exact(ch)),
            }

            if matches!(self.peek(), Some('|' | ')')) {
                break;
            }
        }
    }

    fn compile_capture_group(&mut self) {
        debug!("compiling capture group: {:?}", &self.source[self.curr..]);
        let group_number = self.group_count;

        self.group_count += 1;
        let group_start_index = self.patterns.len();
        self.patterns.push(Pattern::CaptureGroupStart {
            post_seperator_jump: None,
        });
        self.compile_regexp();

        if self.advance() == Some(')') {
            self.patterns
                .push(Pattern::CaptureGroupEnd { group_number });
            return;
        }

        self.patterns.push(Pattern::CaptureGroupSeperator {
            end_jump: 0, /* tmp */
        });
        let post_seperator_index = self.patterns.len();
        self.patterns[group_start_index] = Pattern::CaptureGroupStart {
            post_seperator_jump: Some(post_seperator_index - group_start_index - 1),
        };
        self.compile_regexp();
        self.consume(')');
        self.patterns[post_seperator_index - 1] = Pattern::CaptureGroupSeperator {
            end_jump: self.patterns.len() - post_seperator_index + 1,
        };
        self.patterns
            .push(Pattern::CaptureGroupEnd { group_number });
    }

    fn compile_group(&mut self, is_negative_group: bool) {
        debug!(
            "compiling {} group at {:?}",
            if is_negative_group {
                "negative"
            } else {
                "positive"
            },
            &self.source[self.curr..]
        );

        let group_chars: Vec<char> = self.take_while(|&ch| ch != ']').collect();
        debug!("group chars: {group_chars:?}");
        if is_negative_group {
            self.patterns.push(Pattern::NegativeGroup(group_chars));
        } else {
            self.patterns.push(Pattern::PositiveGroup(group_chars));
        }
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.source.as_bytes().get(self.curr)?;
        self.curr += 1;
        Some(*ch as char)
    }

    fn peek(&self) -> Option<char> {
        self.source
            .as_bytes()
            .get(self.curr)
            .map(|&byte| byte as char)
    }

    fn match_char(&mut self, ch: char) -> bool {
        if self.peek() != Some(ch) {
            return false;
        }

        self.advance();
        true
    }

    fn consume(&mut self, to_match: char) -> char {
        let ch = self.advance().unwrap();
        assert_eq!(ch, to_match);
        ch
    }
}

impl Iterator for Compiler<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.advance()
    }
}

impl Regexp {
    fn compile(pattern: &str) -> Self {
        info!("compiling pattern: {pattern:?}");
        let output = Compiler::new(pattern).compile();
        info!("output of compiling pattern {pattern:?} => {output:#?}");
        output
    }

    fn match_pattern(&self, input: &str) -> bool {
        let tester = RegexpTester {
            regexp: self,
            starting_input: input,
            groups: vec![input; self.group_count].into_boxed_slice(),
            capture_start_stack: Vec::new(),
        };

        tester.test()
    }
}

impl<'r, 'i> RegexpTester<'r, 'i> {
    fn test(mut self) -> bool {
        if self.regexp.patterns[0] == Pattern::StartAnchor {
            return self
                .match_here(self.starting_input, &self.regexp.patterns[1..])
                .is_some();
        }

        for i in 0..self.starting_input.len() {
            if self
                .match_here(&self.starting_input[i..], &self.regexp.patterns)
                .is_some()
            {
                return true;
            }
        }

        false
    }

    fn match_here(&mut self, input: &'i str, regexp: &[Pattern]) -> Option<&'i str> {
        debug!("matching here {input:?} with regexp {regexp:?}");

        if regexp.is_empty() {
            debug!("reached regexp end");
            return Some(input);
        }

        if regexp == [Pattern::EndAnchor] {
            debug!("found end anchor");
            return input.is_empty().then_some(input);
        }

        if let Pattern::OneOrMore(pattern) = &regexp[0] {
            return self.match_one_or_more(pattern, input, &regexp[1..]);
        }

        if let Pattern::ZeroOrOne(pattern) = &regexp[0] {
            if input.is_empty() {
                return Some(input);
            } else if self.match_char(ascii_at(input, 0), pattern) {
                return self.match_here(&input[1..], &regexp[1..]);
            } else {
                return self.match_here(input, &regexp[1..]);
            }
        }

        if let Pattern::CaptureGroupStart {
            post_seperator_jump,
        } = &regexp[0]
        {
            let capture_start = self.starting_input.len() - input.len();
            self.capture_start_stack.push(capture_start);
            debug!("capture start set to {}", capture_start);
            return self.match_capture_group(*post_seperator_jump, input, &regexp[1..]);
        }

        if let Pattern::CaptureGroupSeperator { end_jump } = &regexp[0] {
            return self.match_here(input, &regexp[*end_jump..]);
        }

        if let Pattern::CaptureGroupEnd { group_number } = &regexp[0] {
            let capture_start = self.capture_start_stack.pop().unwrap();
            let captured =
                &self.starting_input[capture_start..(self.starting_input.len() - input.len())];

            self.groups[*group_number] = captured;
            debug!("captured {captured:?} into group slot {}", group_number);
            return self.match_here(input, &regexp[1..]);
        }

        if let Pattern::Backref(group_order) = &regexp[0] {
            return self.match_backref(group_order - 1, input, &regexp[1..]);
        }

        if !input.is_empty() && self.match_char(ascii_at(input, 0), &regexp[0]) {
            return self.match_here(&input[1..], &regexp[1..]);
        }

        debug!("reached input end");
        None
    }

    fn match_one_or_more(
        &mut self,
        pattern: &Pattern,
        input: &'i str,
        regexp: &[Pattern],
    ) -> Option<&'i str> {
        debug!("matching one or more of {pattern:?} at {input:?} with regexp {regexp:?}");

        let mut t = 0;
        while t < input.len() && self.match_char(ascii_at(input, t), pattern) {
            t += 1;
        }

        let initial_capture_stack = self.capture_start_stack.clone();
        while t > 0 {
            self.capture_start_stack = initial_capture_stack.clone();
            if let Some(rest) = self.match_here(&input[t..], regexp) {
                debug!(
                    "success matching one or more: matched = {:?} rest = {rest:?}",
                    &input[t..]
                );
                return Some(rest);
            }

            debug!("fail matching one or more");
            t -= 1;
        }

        None
    }

    fn match_capture_group(
        &mut self,
        post_seperator: Option<usize>,
        input: &'i str,
        regexp: &[Pattern],
    ) -> Option<&'i str> {
        debug!("matching capture group at {input:?} with regexp {regexp:?}");

        let Some(post_seperator) = post_seperator else {
            debug!("capture group is single");
            let res = self.match_here(input, regexp);

            return res;
        };

        debug!("capture group is a union");

        self.match_here(input, regexp)
            .or(self.match_here(input, &regexp[post_seperator..]))
    }

    fn match_char(&self, input: char, pattern: &Pattern) -> bool {
        debug!("matching character {input:?} against {pattern:?}");

        match pattern {
            Pattern::Exact(ch) => input == *ch,
            Pattern::Wildcard => input != '\n',
            Pattern::Digit => input.is_ascii_digit(),
            Pattern::AlphaNumeric => input == '_' || input.is_ascii_alphanumeric(),
            Pattern::NegativeGroup(group_chars) => !group_chars.contains(&input),
            Pattern::PositiveGroup(group_chars) => group_chars.contains(&input),
            pattern => unreachable!("pattern {pattern:?} is not matchable against a character"),
        }
    }

    fn match_backref(
        &mut self,
        group_num: usize,
        input: &'i str,
        regexp: &[Pattern],
    ) -> Option<&'i str> {
        debug!("matching backref {group_num} at {input:?} with regexp {regexp:?}");
        let captured = self.groups[group_num];

        debug!(
            "captured group = {:?}, to match = {:?}",
            captured,
            &input[..captured.len()],
        );

        if &input[..captured.len()] != captured {
            debug!("fail matching backref");
            return None;
        }

        debug!("success matching backref");
        let start = captured.len();
        self.match_here(&input[start..], regexp)
    }
}

fn ascii_at(text: &str, index: usize) -> char {
    text.as_bytes()[index] as char
}

#[cfg(test)]
mod tests {
    use super::*;

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn multiple() {
        init();

        assert!(match_pattern("123 apples", r"\d\d\d \w[opq][^abc]les"));
        assert!(!match_pattern("sally has 12 apples", r"\d\\d\\d apples"));
    }

    #[test]
    fn anchors() {
        init();

        assert!(match_pattern("log", r"^log"));
        assert!(!match_pattern("slog", r"^log"));
        assert!(match_pattern("dog", r"dog$"));
        assert!(!match_pattern("dogs", r"dog$"));
    }

    #[test]
    fn one_or_more() {
        init();

        assert!(match_pattern("cats", r"ca+ts"));
        assert!(match_pattern(" wiefbe caccaatshue ehfefh", r"ca+ts"));
        assert!(match_pattern("caaaaaaaaaaaaaaaats", r"ca+ts"));
        assert!(match_pattern("caacbbacts", r"c[abc]+ts"));
        assert!(!match_pattern("cts", r"ca+ts"));
    }

    #[test]
    fn zero_or_one() {
        init();

        assert!(match_pattern("dogs", r"dogs?"));
        assert!(match_pattern("dog", r"dogs?"));
        assert!(!match_pattern("dags", r"do?gs"));
        assert!(match_pattern("dogo", r"dogs?"));
        assert!(!match_pattern("dogo", r"dogs?$"));
        assert!(match_pattern(" wiefbe dgshue ehfefh", r"do?gs"));
    }

    #[test]
    fn wildcard() {
        init();

        assert!(match_pattern("dog", r"d.g"));
        assert!(!match_pattern("cog", r"d.g"));
        assert!(match_pattern("dog", r"do."));
        assert!(match_pattern("do", r"do.?"));
        assert!(match_pattern("dog", r"do.?"));
        assert!(match_pattern("dooooooggggggg", r"do.?"));
        assert!(!match_pattern("do\ng", r"do.g"));
    }

    #[test]
    fn alternation() {
        init();

        assert!(match_pattern("cat", r"(cat|dog)"));
        assert!(match_pattern("dog", r"(cat|dog)"));
        assert!(match_pattern(
            " wf wfnew catweiufe wefwfiwebiuf",
            r"(cat|dog)"
        ));
        assert!(match_pattern(
            " wf wfnew dogweiufe wefwfiwebiuf",
            r"(cat|dog)"
        ));
        assert!(match_pattern(
            " wf wfnew weiufe wefwfiwebiufdog",
            r"(cat|dog)$"
        ));

        let test_str = r"I have (\d\d cats|\d dogs?)";
        assert!(match_pattern("Hey, I have 1 dog!", test_str));
        assert!(match_pattern("Hey, I have 3 dogs!", test_str));
        assert!(!match_pattern("Hey, I have 10 dogs!", test_str));
        assert!(match_pattern("Hey, I have 69 cats!", test_str));
        assert!(!match_pattern("Hey, I have 6 cat!", test_str));
    }

    #[test]
    fn backrefs() {
        init();

        assert!(match_pattern("cat and cat", r"(cat) and \1"));
        assert!(!match_pattern("cat and dog", r"(cat) and \1"));
        assert!(match_pattern(
            "grep 101 is doing grep 101 times",
            r"(\w\w\w\w \d\d\d) is doing \1 times"
        ));
        assert!(!match_pattern(
            "$?! 101 is doing $?! 101 times",
            r"(\w\w\w \d\d\d) is doing \1 times"
        ));
        assert!(!match_pattern(
            "grep yes is doing grep yes times",
            r"(\w\w\w\w \d\d\d) is doing \1 times"
        ));
        assert!(match_pattern(
            "abcd is abcd, not efg",
            r"([abcd]+) is \1, not [^xyz]+"
        ));
        assert!(match_pattern(
            "3 red squares and 3 red circles",
            r"(\d+) (\w+) squares and \1 \2 circles",
        ));
        assert!(!match_pattern(
            "3 red squares and 4 red circles",
            r"(\d+) (\w+) squares and \1 \2 circles",
        ));

        assert!(match_pattern(
            "abcd is abcd, and efg is efg",
            r"([abcd]+) is \1, and ([^xyz]+) is \2"
        ));

        assert!(match_pattern(
            "'cat and cat' is the same as 'cat and cat'",
            r"('(cat) and \2') is the same as \1"
        ));
        assert!(!match_pattern(
            "'cat and cat' is the same as 'cat and dog'",
            r"('(cat) and \2') is the same as \1"
        ));
    }

    #[test]
    fn final_test() {
        assert!(match_pattern(
            "abc-def is abc-def, not efg, abc, or def",
            r"(([abc]+)-([def]+)) is \1, not ([^xyz]+), \2, or \3"
        ))
    }
}
