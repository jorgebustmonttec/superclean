use crate::token::Token;

/// Calculates the line and column where the `token_index`-th token ends.
pub fn compute_position(tokens: &[Token], token_index: usize) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;

    for token in tokens.iter().take(token_index) {
        match token {
            Token::Newline => {
                line += 1;
                column = 1;
            }
            _ => {
                column += token.visual_length();
            }
        }
    }

    (line, column)
}
