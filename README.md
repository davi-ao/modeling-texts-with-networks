[![DOI](https://zenodo.org/badge/779033806.svg)](https://zenodo.org/doi/10.5281/zenodo.10892528)

# Modeling Texts with Networks: Comparing Five Approaches to Sentence Representation

**Authors:** Davi Alves Oliveira and Hernane Borges de Barros Pereira

This script was developed and used for the study reported in the article
“Modeling texts with networks” submitted to The European Physical Journal B
(https://epjb.epj.org/) in 2024. The abstract and keywords of the article can
be found below. The script uses data from the Open American National Corpus
(OANC - https://anc.org/). The data consists of a sample of 8 texts that were
modified before use. The modifications were the removal of paratextual
information (e.g., titles, subheading, acknowledgements, declarations of
conflicting interests), the elimination of the symbols ‘.’, ‘!’, ‘?’ and ‘…’
in cases in which they were not used as sentence delimiters. When necessary,
such symbols were replaced by a a word describing it. For example, numbers
with a decimal separator like ‘1.0’ were converted to ‘1point0’. The same
procedure was applied to other symbols (e.g., ‘+’, ‘=’ and ‘º’ were replaced by
‘plus’, ‘equals to’ and ‘degrees’, respectively). Brackets containing
reference numbers, and parenthesis characters were removed. Then, the text was
split, so that each sentence was in a separate line, save in text files and
parsed with the Stanford CoreNLP dependency parser 
(https://stanfordnlp.github.io/CoreNLP/) via de java command line. The parsed 
files were saved in the JSON format.


## Abstract
Complex networks offer a powerful framework for modeling linguistic phenomena. 
This study compares five distinct methods for representing sentences as 
networks, each with unique edge definitions: (1) a lines approach, where edges 
represent token (e.g., word) adjacency; (2) a close-range co-occurrence 
approach, where edge are based on the probability of tokens co-occurring at 
distance one or two; (3) a cliques approach, where edges connect tokens 
co-occurring within the same sentence; (4) a dependency-based approach, where 
edges are defined by syntactic dependencies extracted by a parser; (5) an 
IF-trimmed-subgraphs approach, where edges are determined by the 
Incidence-Fidelity (IF) Index. While the first four approaches are 
well-established in the literature, the last one is a novel proposal. We also 
examined the effects of limiting the vertices to lemmas (i.e., words with 
inflections removed) and to lexical lemmas (i.e., nouns, adjectives, verbs and 
adverbs) as opposed to the unaltered words. Our results reveal that these 
approaches yield networks with varying average minimal path lengths and 
degrees, influencing the interpretation of results. While small-world behavior 
remain consistent across networks, scale-free behavior analysis is affected. 
Notably, excluding functional words significantly alters degree distributions. 
We suggest, in order of relevance and according to the resources available, 
the dependency-based, the close-range co-occurrence and the lines approaches 
for cases in which syntactic relations are central, and the 
IF-trimmed-subgraphs and the cliques approaches for cases in which semantic 
relations are central.

**Keywords:** Textual Network, Semantic Network, Network Science, Syntactic 
Dependency
