# Partial CFG
Convert context free language (with limited nest level)
to regular language.

## Example


depth=8 regex expansion of simple arithmetic system
```
(0|1|2|[012](0|1|2|[012](0|1|2|[012](0|1|2|[012](0|1|2|[012][012]))))|\((0|1|2|[012](0|1|2|[012](0|1|2|[012](0|1|2|[012][012])))|\((0|1|2|[012](0|1|2|[012](0|1|2|[012][012]))|\((0|1|2|[012](0|1|2|[012][012])|\((0|1|2|[012][012]|\([012]\))\)|[012]\+[012])\)|(0|1|2|[012][012]|\([012]\))\+(0|1|2|[012][012]|\([012]\)))\)|(0|1|2|[012](0|1|2|[012][012])|\((0|1|2|[012][012]|\([012]\))\)|[012]\+[012])\+(0|1|2|[012](0|1|2|[012][012])|\((0|1|2|[012][012]|\([012]\))\)|[012]\+[012]))\)|(0|1|2|[012](0|1|2|[012](0|1|2|[012][012]))|\((0|1|2|[012](0|1|2|[012][012])|\((0|1|2|[012][012]|\([012]\))\)|[012]\+[012])\)|(0|1|2|[012][012]|\([012]\))\+(0|1|2|[012][012]|\([012]\)))\+(0|1|2|[012](0|1|2|[012](0|1|2|[012][012]))|\((0|1|2|[012](0|1|2|[012][012])|\((0|1|2|[012][012]|\([012]\))\)|[012]\+[012])\)|(0|1|2|[012][012]|\([012]\))\+(0|1|2|[012][012]|\([012]\))))
```

this regex *does* match "(1+2)+(2+0)" (visual result: http://regex101.com/r/vE6kH3)


Although current implementatin is just proof-of-concept, the idea might be usable
for automatically generating syntax highlighter config such as .tmBundle
for TextMate and Sublime Text (or maybe other editors).

## Notes
Lots of CFG-related problems are not just hard, but undecidable!
(See http://www.cis.upenn.edu/~jean/gbooks/PCPh04.pdf)

But the pdf says checking given CFG is weaker or equal to regular is decidable.
(but not CFG >= regular)


## Pending Design Decisions
1. Accept EBNF and don't optimize so much
2. convert to BNF and find out reperition

2 is cleaner (and more general) if optimizer is relatively simple (say, < 200 lines).

