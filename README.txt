English parser lexicon pack for Rynn's OCaml syntax parser

IMPORTANT: This is a broad parser-ready starter lexicon, not a mathematically complete list of every English word. English has open-class categories, new words, proper names, domain terms, inflected forms, spelling variants, and multiword expressions.

Main parser categories included:
- adjectives.txt
- adverbs.txt
- auxiliaries.txt
- complementizers.txt
- coordinators.txt
- currencies.txt
- determiners.txt
- gerunds.txt
- modals.txt
- nouns.txt
- numbers.txt
- participles.txt
- prepositions.txt
- pronouns.txt
- question_auxiliaries.txt
- tense_words.txt
- trace_words.txt
- units.txt
- verbs.txt
- wh_determiners.txt
- wh_prepositions.txt
- wh_question_auxiliaries.txt

Extra helper categories:
- gradable_adjectives.txt: adjectives tagged "1" in the source list, meaning they accept -er/-est.
- noun_modifiers.txt: source-list tag "3", nouns often used attributively, e.g. computer science.
- adjectives_multiword.txt: adjectives with spaces, kept separate because your browser terminal/parser tokenizes by spaces.
- generated_adverbs_from_adjectives.txt: rough -ly generated adverbs; useful but noisy.
- proper_nouns.txt: small starter list.

Recommended use:
1. Put the lexicon/ folder at your website root.
2. Let terminal.js expose these files via cat.
3. Use generate_lexicon_ml.py to make ocaml/lexicon_generated.ml.
4. In parser_core.ml, merge these generated lists with your existing base lists.
5. Rebuild parser_web.bc.js with dune/js_of_ocaml.

Stats:
{
  "adjectives.txt": 4988,
  "adjectives_multiword.txt": 8,
  "adverbs.txt": 4852,
  "auxiliaries.txt": 16,
  "complementizers.txt": 17,
  "coordinators.txt": 8,
  "currencies.txt": 33,
  "determiners.txt": 69,
  "generated_adverbs_from_adjectives.txt": 4787,
  "gerunds.txt": 419,
  "gradable_adjectives.txt": 494,
  "modals.txt": 12,
  "noun_modifiers.txt": 54,
  "nouns.txt": 1554,
  "numbers.txt": 48,
  "participles.txt": 89,
  "prepositions.txt": 58,
  "pronouns.txt": 53,
  "proper_nouns.txt": 11,
  "question_auxiliaries.txt": 20,
  "tense_words.txt": 1,
  "trace_words.txt": 5,
  "units.txt": 64,
  "verbs.txt": 419,
  "wh_determiners.txt": 5,
  "wh_prepositions.txt": 6,
  "wh_question_auxiliaries.txt": 20
}
