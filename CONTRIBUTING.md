# Style guide for LaTeX code

## Line breaks and spaces

1. In text: put each sentence on a separate line, keep one line per sentence.
2. Do not leave trailing spaces at the end of lines.
3. Similarly, leave exactly a single empty line at the end of the file.
4. Do not use more than a single empty line between paragraphs or between sections.
5. Put exactly a single space between words, a single space after (but not before) commas, colons, etc.
6. In equations: put spaces around binary operators (i.e., `V = 2 + 3`), but not after unary operators, also use single spaces to separate factors in a product.

## Punctuation after equations

1. Consider equations as part of text, i.e., use commas or periods after equations as required by English grammar.
2. Leave empty space between equation and punctuation after it, i.e., `V = 2 + 3\,.`.

## References and citations

Connect `Eq.`, `Eqs.`, `Fig.`, etc. with corresponding numbers with `~`, i.e., `Eq.~(\ref{eq:V})` Also, use `~` before citations.

## Label names

Use description label names, i.e., `eq:DBI:potential` instead of `eq23`. Keep new labels as consistent as possible with existing conventions.

## Parentheses

1. Always use `\left` and `\right` with parenthesis, square or triangular brackets.
2. If equation is split into multiple lines, use, i.e., `\left(l_1\right.` and `\left.l_2\right)`.

## Warnings

1. There must be no warnings produced during build.

## Equation environments

1. Use `\begin{equation}` environment for single-line equations, `\begin{equation}\begin{aligned}` for multi-line equations, and use `\begin{align}` if every equations should be numbered.
2. Do not use `\nonumber` for the purpose of having a single number for a multi-line equation.
3. Keep code inside equation environments within 100 columns in width.
