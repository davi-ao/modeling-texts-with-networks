# Info -------------------------------------------------------------------------
# Analyze five approaches for graph sentence representation using three
# different vertex definitions
# 
# Authors: Davi Alves Oliveira and Hernane Borges de Barros Pereira
#
# Last update: March 28, 2024

# Initial settings -------------------------------------------------------------
# Install the necessary packages
# install.packages('tidyverse')
# install.packages('tidyjson')
# install.packages('igraph')

# Load required packages
library(tidyverse)
library(tidyjson)
library(igraph)

# Load the parsed sentences from the json files
json_files = list.files('data/', pattern = 'parsed_text_*')
parsed_sentences = lapply(json_files, function(f) {
  read_json(paste0('data/', f)) %>%
    gather_object() %>%
    filter(name == 'sentences') %>%
    select(-name) %>%
    gather_array() %>%
    gather_object() %>%
    mutate(text.index = f %>% str_extract('\\d'))
}) %>%
  bind_rows()

# Generate list of tokens
tokens = parsed_sentences %>%
  filter(name == 'tokens') %>%
  rename(sentence.index = array.index) %>%
  gather_array() %>%
  spread_all() %>%
  filter(pos != '.') %>%
  as_tibble()

# Generate list of words
words = tokens %>%
  select(text.index, sentence.index, index, word) %>%
  mutate(word = word %>% str_to_lower())

# Generate list of lemmas
lemmas = tokens %>%
  select(text.index, sentence.index, index, lemma)

# Generate list of lexical lemmas
lemmas_lexical = tokens %>%
  filter(pos %in% c('JJ', 'JJR', 'JJS', # Adjectives
                    'NN', 'NNS', 'NNP', 'NNPS', # Nouns
                    'RB', 'RBR', 'RBS', # Adverbs
                    'VB', 'VBD', 'VBG', 'VBN', 'VBP', 'VBZ')) %>% # Verbs
  select(text.index, sentence.index, index, lemma)

# Generate list of dependency relations
dependencies = parsed_sentences %>%
  filter(name == 'enhancedPlusPlusDependencies') %>%
  rename(sentence.index = array.index) %>%
  gather_array() %>%
  spread_all() %>%
  filter(!dep %in% c('ROOT', 'punct')) %>%
  as_tibble() %>%
  select(text.index,
         sentence.index, 
         dep, 
         governor, 
         governorGloss, 
         dependent, 
         dependentGloss) %>%
  pivot_longer(c(governorGloss, dependentGloss)) %>%
  mutate(index = ifelse(name %>% startsWith('governor'), 
                        governor, 
                        dependent))

# Create list of vertices for the networks of words (definition 1)
vertices_words = dependencies %>%
  left_join(words, by = c('text.index', 'sentence.index', 'index')) %>%
  select(text.index, sentence.index, word) %>%
  unique() %>%
  filter(!is.na(word)) %>%
  group_by(text.index, sentence.index) %>%
  mutate(index = row_number())

# Create list of vertices for the networks of lemmas (definition 2)
vertices_lemmas = dependencies %>%
  left_join(lemmas, by = c('text.index', 'sentence.index', 'index')) %>%
  select(text.index, sentence.index, lemma) %>%
  unique() %>%
  group_by(text.index, sentence.index) %>%
  mutate(index = row_number())

# Create list of vertices for the networks of lexical lemmas (definition 3)
vertices_lexical = dependencies %>%
  left_join(lemmas_lexical, by = c('text.index', 'sentence.index', 'index')) %>%
  select(text.index, sentence.index, lemma) %>%
  unique() %>%
  filter(!is.na(lemma)) %>%
  group_by(text.index, sentence.index) %>%
  mutate(index = row_number())

# Lines approach (words) -------------------------------------------------------

# Generate list of edges
lines_words_edges = words %>%
  select(-index) %>%
  group_by(text.index, sentence.index) %>%
  left_join(vertices_words, 
            by = c('text.index', 'sentence.index', 'word')) %>%
  filter(!is.na(index)) %>%
  rename(Source = index) %>%
  mutate(Target = Source %>% lead()) %>%
  filter(!is.na(Target)) %>%
  select(-word)

# Generate a representation with vertex definition 1 (words) for each sentence, 
# following the lines approach, in Pajek's .net format
lines_words_subgraphs = lapply(
  lines_words_edges$text.index %>% unique(), function(ti) {
    text = lines_words_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_words %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', word, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = lines_words_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(Source, Target)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
  })
})

# Save the representations in Pajek's .net format
for (ti in seq_along(lines_words_subgraphs)) {
  for (si in seq_along(lines_words_subgraphs[[ti]])) {
    write_file(lines_words_subgraphs[[ti]][[si]], 
               paste0('results/lines_words_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# Lines approach (lemmas) ------------------------------------------------------

# Generate list of edges
lines_lemmas_edges = lemmas %>%
  select(-index) %>%
  group_by(text.index, sentence.index) %>%
  left_join(vertices_lemmas, 
            by = c('text.index', 'sentence.index', 'lemma')) %>%
  filter(!is.na(index)) %>%
  rename(Source = index) %>%
  mutate(Target = Source %>% lead()) %>%
  filter(!is.na(Target)) %>%
  select(-lemma)

# Generate a representation with vertex definition 2 (lemmas) for each sentence, 
# following the lines approach, in Pajek's .net format
lines_lemmas_subgraphs = lapply(
  lines_lemmas_edges$text.index %>% unique(), function(ti) {
    text = lines_lemmas_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_lemmas %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', lemma, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = lines_lemmas_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(Source, Target)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
    })
  })

# Save the representations in Pajek's .net format
for (ti in seq_along(lines_lemmas_subgraphs)) {
  for (si in seq_along(lines_lemmas_subgraphs[[ti]])) {
    write_file(lines_lemmas_subgraphs[[ti]][[si]], 
               paste0('results/lines_lemmas_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# Lines approach (lexical lemmas)-----------------------------------------------

# Generate list of edges
lines_lexical_edges = lemmas_lexical %>%
  select(-index) %>%
  group_by(text.index, sentence.index) %>%
  left_join(vertices_lexical, 
            by = c('text.index', 'sentence.index', 'lemma')) %>%
  filter(!is.na(index)) %>%
  rename(Source = index) %>%
  mutate(Target = Source %>% lead()) %>%
  filter(!is.na(Target)) %>%
  select(-lemma)

# Generate a representation with vertex definition 3 (lexical lemmas) for each 
# sentence, following the lines approach, in Pajek's .net format
lines_lexical_subgraphs = lapply(
  lines_lexical_edges$text.index %>% unique(), function(ti) {
    text = lines_lexical_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_lexical %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', lemma, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = lines_lexical_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(Source, Target)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
    })
  })

# Save the representations in Pajek's .net format
for (ti in seq_along(lines_lexical_subgraphs)) {
  for (si in seq_along(lines_lexical_subgraphs[[ti]])) {
    write_file(lines_lexical_subgraphs[[ti]][[si]], 
               paste0('results/lines_lexical_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# Close-range co-occurrence approach (words) -----------------------------------

# Generate list of trigrams
trigrams_words = words %>%
  group_by(text.index, sentence.index) %>%
  select(-index) %>%
  mutate(word_n = word %>% lead(),
         word_nn = word_n %>% lead()) %>%
  filter(!is.na(word_n) & 
           !is.na(word_nn) & 
           !str_detect(word, '[\\W_]') &
           !str_detect(word_n, '[\\W_]') &
           !str_detect(word_nn, '[\\W_]')) %>%
  ungroup() %>%
  mutate(trigram.index = row_number())

# Generate initial list of edges
edges_closerange_words = trigrams_words %>%
  group_by(text.index, sentence.index) %>%
  mutate(trigram.index = row_number()) %>%
  pivot_longer(-c(text.index, sentence.index, trigram.index), 
               names_to = 'position', 
               values_to = 'word') %>%
  select(-position) %>%
  distinct() %>%
  group_by(text.index, sentence.index, trigram.index) %>%
  rename(Source = word) %>%
  mutate(Target = Source) %>%
  expand(Source, Target) %>%
  filter(Source != Target) %>%
  group_by(text.index, sentence.index) %>%
  mutate(edge.id = mapply(function(s, t) {
    sort(c(s, t)) %>% paste(collapse = '_')
  }, Source, Target)) %>%
  distinct(edge.id, .keep_all = T) %>%
  select(-edge.id) %>%
  ungroup()

# Calculate de probability for each word
p_words = words %>% 
  select(word) %>%
  distinct() %>%
  mutate(p = sapply(word, function(w) {
    edges_closerange_words %>%
      filter(Source == w | Target == w) %>%
      nrow() / nrow(edges_closerange_words)
  }))

# Calculate de probability of co-occurrence for each pair of words
p_cooccurrence_words = edges_closerange_words %>%
  select(-c(text.index, sentence.index)) %>%
  group_by(trigram.index) %>%
  expand(Source, Target) %>%
  filter(Source != Target) %>%
  ungroup() %>%
  select(-trigram.index) %>%
  mutate(edge.id = mapply(function(s, t) {
    sort(c(s, t)) %>% paste(collapse = '_')
  }, Source, Target)) %>%
  distinct(edge.id, .keep_all = T) %>%
  select(-edge.id) %>%
  mutate(p_ST = mapply(function(s, t) {
    edges_closerange_words %>% 
      filter((Source == s | Target == s) &
               (Source == t | Target == t)) %>%
      nrow() / nrow(edges_closerange_words)
  }, Source, Target))

# Generate list of weighted edges
closerange_words_edges = edges_closerange_words %>%
  group_by(text.index, sentence.index) %>%
  left_join(p_words %>% rename(Source = word), by = 'Source') %>%
  rename(p_S = p) %>%
  left_join(p_words %>% rename(Target = word), by = 'Target') %>%
  rename(p_T = p) %>%
  left_join(p_cooccurrence_words, by = c('Source', 'Target')) %>%
  mutate(
    weight = ifelse(p_ST > (p_S * p_T), 1, 0)) %>%
  filter(!is.na(weight)) %>%
  rename(word = Source) %>%
  left_join(vertices_words, by = c('text.index', 'sentence.index', 'word')) %>%
  select(-word) %>%
  rename(Source = index,
         word = Target) %>%
  left_join(vertices_words, by = c('text.index', 'sentence.index', 'word')) %>%
  select(-word) %>%
  rename(Target = index) %>%
  filter(!is.na(Source) & !is.na(Target))

# Generate a representation with vertex definition 1 (words) for each sentence, 
# following the close-range co-occurrence approach, in Pajek's .net format
closerange_words_subgraphs_weighted = lapply(
  closerange_words_edges$text.index %>% unique(), function(ti) {
    text = closerange_words_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_words %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', word, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = closerange_words_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(Source, Target, weight)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
    })
  })

# Save the representations in Pajek's .net format
for (ti in seq_along(closerange_words_subgraphs_weighted)) {
  for (si in seq_along(closerange_words_subgraphs_weighted[[ti]])) {
    write_file(closerange_words_subgraphs_weighted[[ti]][[si]], 
               paste0('results/closerange_words_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# Get list of .net files
closerange_words_dir = 'results/closerange_words_subgraphs/'
closerange_words_files = list.files(closerange_words_dir, 
                                    '^text_\\d_sentence_\\d{3}.net')

# Read the files as igraph objects and trim the edges with weight 0
closerange_words_subgraphs = sapply(
  closerange_words_files, function(cw) {
    g = read_graph(paste0(closerange_words_dir, cw), 'Pajek')
    trimmed = g %>% delete_edges(E(g)[E(g)$weight == 0])
    trimmed %>% delete_edge_attr('weight')
})

# Save the trimmed non-weighted graphs in Pajek's .net format
for (cw_index in seq_along(closerange_words_subgraphs)) {
  write_graph(closerange_words_subgraphs[[cw_index]], 
              paste0('results/closerange_words_subgraphs/trimmed_', 
                     closerange_words_files[cw_index]),
              'Pajek')
}

# Close-range co-occurrence approach (lemmas) ----------------------------------

# Generate list of trigrams
trigrams_lemmas = lemmas %>%
  group_by(text.index, sentence.index) %>%
  select(-index) %>%
  mutate(lemma_n = lemma %>% lead(),
         lemma_nn = lemma_n %>% lead()) %>%
  filter(!is.na(lemma_n) & 
           !is.na(lemma_nn) & 
           !str_detect(lemma, '[\\W_]') &
           !str_detect(lemma_n, '[\\W_]') &
           !str_detect(lemma_nn, '[\\W_]')) %>%
  ungroup() %>%
  mutate(trigram.index = row_number())

# Generate initial list of edges
edges_closerange_lemmas = trigrams_lemmas %>%
  group_by(text.index, sentence.index) %>%
  mutate(trigram.index = row_number()) %>%
  pivot_longer(-c(text.index, sentence.index, trigram.index), 
               names_to = 'position', 
               values_to = 'lemma') %>%
  select(-position) %>%
  distinct() %>%
  group_by(text.index, sentence.index, trigram.index) %>%
  rename(Source = lemma) %>%
  mutate(Target = Source) %>%
  expand(Source, Target) %>%
  filter(Source != Target) %>%
  group_by(text.index, sentence.index) %>%
  mutate(edge.id = mapply(function(s, t) {
    sort(c(s, t)) %>% paste(collapse = '_')
  }, Source, Target)) %>%
  distinct(edge.id, .keep_all = T) %>%
  select(-edge.id) %>%
  ungroup()

# Calculate de probability for each lemma
p_lemmas = lemmas %>% 
  select(lemma) %>%
  distinct() %>%
  mutate(p = sapply(lemma, function(w) {
    edges_closerange_lemmas %>%
      filter(Source == w | Target == w) %>%
      nrow() / nrow(edges_closerange_lemmas)
  }))

# Calculate de probability of co-occurrence for each pair of lemmas
p_cooccurrence_lemmas = edges_closerange_lemmas %>%
  select(-c(text.index, sentence.index)) %>%
  group_by(trigram.index) %>%
  expand(Source, Target) %>%
  filter(Source != Target) %>%
  ungroup() %>%
  select(-trigram.index) %>%
  mutate(edge.id = mapply(function(s, t) {
    sort(c(s, t)) %>% paste(collapse = '_')
  }, Source, Target)) %>%
  distinct(edge.id, .keep_all = T) %>%
  select(-edge.id) %>%
  mutate(p_ST = mapply(function(s, t) {
    edges_closerange_lemmas %>% 
      filter((Source == s | Target == s) &
               (Source == t | Target == t)) %>%
      nrow() / nrow(edges_closerange_lemmas)
  }, Source, Target))

# Generate list of weighted edges
closerange_lemmas_edges = edges_closerange_lemmas %>%
  group_by(text.index, sentence.index) %>%
  left_join(p_lemmas %>% rename(Source = lemma), by = 'Source') %>%
  rename(p_S = p) %>%
  left_join(p_lemmas %>% rename(Target = lemma), by = 'Target') %>%
  rename(p_T = p) %>%
  left_join(p_cooccurrence_lemmas, by = c('Source', 'Target')) %>%
  mutate(
    weight = ifelse(p_ST > (p_S * p_T), 1, 0)) %>%
  filter(!is.na(weight)) %>%
  rename(lemma = Source) %>%
  left_join(vertices_lemmas, by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(Source = index,
         lemma = Target) %>%
  left_join(vertices_lemmas, by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(Target = index) %>%
  filter(!is.na(Source) & !is.na(Target))

# Generate a representation with vertex definition 2 (lemmas) for each sentence, 
# following the close-range co-occurrence approach, in Pajek's .net format
closerange_lemmas_subgraphs_weighted = lapply(
  closerange_lemmas_edges$text.index %>% unique(), function(ti) {
    text = closerange_lemmas_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_lemmas %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', lemma, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = closerange_lemmas_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(Source, Target, weight)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
    })
  })

# Save the representations in Pajek's .net format
for (ti in seq_along(closerange_lemmas_subgraphs_weighted)) {
  for (si in seq_along(closerange_lemmas_subgraphs_weighted[[ti]])) {
    write_file(closerange_lemmas_subgraphs_weighted[[ti]][[si]], 
               paste0('results/closerange_lemmas_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# Get list of .net files
closerange_lemmas_dir = 'results/closerange_lemmas_subgraphs/'
closerange_lemmas_files = list.files(closerange_lemmas_dir, 
                                    '^text_\\d_sentence_\\d{3}.net')

# Read the files as igraph objects and trim the edges with weight 0
closerange_lemmas_subgraphs = sapply(
  closerange_lemmas_files, function(clem) {
    g = read_graph(paste0(closerange_lemmas_dir, clem), 'Pajek')
    trimmed = g %>% delete_edges(E(g)[E(g)$weight == 0])
    trimmed %>% delete_edge_attr('weight')
})

# Save the trimmed non-weighted graphs in Pajek's .net format
for (clem_index in seq_along(closerange_lemmas_subgraphs)) {
  write_graph(closerange_lemmas_subgraphs[[clem_index]], 
              paste0('results/closerange_lemmas_subgraphs/trimmed_', 
                     closerange_lemmas_files[clem_index]),
              'Pajek')
}

# Close-range co-occurrence approach (lexical lemmas) --------------------------

# Generate list of trigrams
trigrams_lexical = lemmas_lexical %>%
  group_by(text.index, sentence.index) %>%
  select(-index) %>%
  mutate(lemma_n = lemma %>% lead(),
         lemma_nn = lemma_n %>% lead()) %>%
  filter(!is.na(lemma_n) & 
           !is.na(lemma_nn) & 
           !str_detect(lemma, '[\\W_]') &
           !str_detect(lemma_n, '[\\W_]') &
           !str_detect(lemma_nn, '[\\W_]')) %>%
  ungroup() %>%
  mutate(trigram.index = row_number())

# Generate initial list of edges
edges_closerange_lexical = trigrams_lexical %>%
  group_by(text.index, sentence.index) %>%
  mutate(trigram.index = row_number()) %>%
  pivot_longer(-c(text.index, sentence.index, trigram.index), 
               names_to = 'position', 
               values_to = 'lemma') %>%
  select(-position) %>%
  distinct() %>%
  group_by(text.index, sentence.index, trigram.index) %>%
  rename(Source = lemma) %>%
  mutate(Target = Source) %>%
  expand(Source, Target) %>%
  filter(Source != Target) %>%
  group_by(text.index, sentence.index) %>%
  mutate(edge.id = mapply(function(s, t) {
    sort(c(s, t)) %>% paste(collapse = '_')
  }, Source, Target)) %>%
  distinct(edge.id, .keep_all = T) %>%
  select(-edge.id) %>%
  ungroup()

# Calculate de probability for each lemma
p_lexical = lemmas_lexical %>% 
  select(lemma) %>%
  distinct() %>%
  mutate(p = sapply(lemma, function(w) {
    edges_closerange_lexical %>%
      filter(Source == w | Target == w) %>%
      nrow() / nrow(edges_closerange_lexical)
  }))

# Calculate de probability of co-occurrence for each pair of lexical lemmas
p_cooccurrence_lexical = edges_closerange_lexical %>%
  select(-c(text.index, sentence.index)) %>%
  group_by(trigram.index) %>%
  expand(Source, Target) %>%
  filter(Source != Target) %>%
  ungroup() %>%
  select(-trigram.index) %>%
  mutate(edge.id = mapply(function(s, t) {
    sort(c(s, t)) %>% paste(collapse = '_')
  }, Source, Target)) %>%
  distinct(edge.id, .keep_all = T) %>%
  select(-edge.id) %>%
  mutate(p_ST = mapply(function(s, t) {
    edges_closerange_lexical %>% 
      filter((Source == s | Target == s) &
               (Source == t | Target == t)) %>%
      nrow() / nrow(edges_closerange_lexical)
  }, Source, Target))

# Generate list of weighted edges
closerange_lexical_edges = edges_closerange_lexical %>%
  group_by(text.index, sentence.index) %>%
  left_join(p_lexical %>% rename(Source = lemma), by = 'Source') %>%
  rename(p_S = p) %>%
  left_join(p_lexical %>% rename(Target = lemma), by = 'Target') %>%
  rename(p_T = p) %>%
  left_join(p_cooccurrence_lexical, by = c('Source', 'Target')) %>%
  mutate(
    weight = ifelse(p_ST > (p_S * p_T), 1, 0)) %>%
  filter(!is.na(weight)) %>%
  rename(lemma = Source) %>%
  left_join(vertices_lexical, by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(Source = index,
         lemma = Target) %>%
  left_join(vertices_lexical, by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(Target = index) %>%
  filter(!is.na(Source) & !is.na(Target))

# Generate a representation with vertex definition 2 (lexical lemmas) for each 
# sentence, following the close-range co-occurrence approach, in Pajek's .net 
# format
closerange_lexical_subgraphs_weighted = lapply(
  closerange_lexical_edges$text.index %>% unique(), function(ti) {
    text = closerange_lexical_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_lexical %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', lemma, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = closerange_lexical_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(Source, Target, weight)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
    })
  })

# Save the representations in Pajek's .net format
for (ti in seq_along(closerange_lexical_subgraphs_weighted)) {
  for (si in seq_along(closerange_lexical_subgraphs_weighted[[ti]])) {
    write_file(closerange_lexical_subgraphs_weighted[[ti]][[si]], 
               paste0('results/closerange_lexical_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# Get list of .net files
closerange_lexical_dir = 'results/closerange_lexical_subgraphs/'
closerange_lexical_files = list.files(closerange_lexical_dir, 
                                     '^text_\\d_sentence_\\d{3}.net')

# Read the files as igraph objects and trim the edges with weight 0
closerange_lexical_subgraphs = sapply(
  closerange_lexical_files, function(clex) {
    g = read_graph(paste0(closerange_lexical_dir, clex), 'Pajek')
    trimmed = g %>% delete_edges(E(g)[E(g)$weight == 0])
    trimmed %>% delete_edge_attr('weight')
  })

# Save the trimmed non-weighted graphs in Pajek's .net format
for (clex_index in seq_along(closerange_lexical_subgraphs)) {
  write_graph(closerange_lexical_subgraphs[[clex_index]], 
              paste0('results/closerange_lexical_subgraphs/trimmed_', 
                     closerange_lexical_files[clex_index]),
              'Pajek')
}

# Cliques approach (words) -----------------------------------------------------

# Generate list of edges
cliques_words_edges = vertices_words %>%
  rename(Source = word) %>%
  mutate(Target = Source) %>%
  expand(Source, Target) %>%
  filter(Source != Target) %>%
  mutate(edge.id = mapply(function(s, t) {
    sort(c(s, t)) %>% paste(collapse = '_')
  }, Source, Target)) %>%
  distinct(edge.id, .keep_all = T) %>%
  select(-edge.id) %>%
  rename(word = Source) %>%
  left_join(vertices_words, 
            by = c('text.index', 'sentence.index', 'word')) %>%
  select(-word) %>%
  rename(Source = index,
         word = Target) %>%
  left_join(vertices_words, 
            by = c('text.index', 'sentence.index', 'word')) %>%
  select(-word) %>%
  rename(Target = index)

# Generate a representation with vertex definition 1 (words) for each sentence, 
# following the cliques approach, in Pajek's .net format
cliques_words_subgraphs = lapply(
  cliques_words_edges$text.index %>% unique(), function(ti) {
    text = cliques_words_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_words %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', word, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = cliques_words_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(Source, Target)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
    })
  })

# Save the representations in Pajek's .net format
for (ti in seq_along(cliques_words_subgraphs)) {
  for (si in seq_along(cliques_words_subgraphs[[ti]])) {
    write_file(cliques_words_subgraphs[[ti]][[si]], 
               paste0('results/cliques_words_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# Cliques approach (lemmas) ----------------------------------------------------

# Generate list of edges
cliques_lemmas_edges = vertices_lemmas %>%
  rename(Source = lemma) %>%
  mutate(Target = Source) %>%
  expand(Source, Target) %>%
  filter(Source != Target) %>%
  mutate(edge.id = mapply(function(s, t) {
    sort(c(s, t)) %>% paste(collapse = '_')
  }, Source, Target)) %>%
  distinct(edge.id, .keep_all = T) %>%
  select(-edge.id) %>%
  rename(lemma = Source) %>%
  left_join(vertices_lemmas, 
            by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(Source = index,
         lemma = Target) %>%
  left_join(vertices_lemmas, 
            by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(Target = index)

# Generate a representation with vertex definition 2 (lemmas) for each sentence, 
# following the cliques approach, in Pajek's .net format
cliques_lemmas_subgraphs = lapply(
  cliques_lemmas_edges$text.index %>% unique(), function(ti) {
    text = cliques_lemmas_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_lemmas %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', lemma, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = cliques_lemmas_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(Source, Target)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
    })
  })

# Save the representations in Pajek's .net format
for (ti in seq_along(cliques_lemmas_subgraphs)) {
  for (si in seq_along(cliques_lemmas_subgraphs[[ti]])) {
    write_file(cliques_lemmas_subgraphs[[ti]][[si]], 
               paste0('results/cliques_lemmas_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# Cliques approach (lexical lemmas) --------------------------------------------

# Generate list of edges
cliques_lexical_edges = vertices_lexical %>%
  rename(Source = lemma) %>%
  mutate(Target = Source) %>%
  expand(Source, Target) %>%
  filter(Source != Target) %>%
  mutate(edge.id = mapply(function(s, t) {
    sort(c(s, t)) %>% paste(collapse = '_')
  }, Source, Target)) %>%
  distinct(edge.id, .keep_all = T) %>%
  select(-edge.id) %>%
  rename(lemma = Source) %>%
  left_join(vertices_lexical, 
            by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(Source = index,
         lemma = Target) %>%
  left_join(vertices_lexical, 
            by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(Target = index)

# Generate a representation with vertex definition 3 (lexical lemmas) for each 
# sentence, following the cliques approach, in Pajek's .net format
cliques_lexical_subgraphs = lapply(
  cliques_lexical_edges$text.index %>% unique(), function(ti) {
    text = cliques_lexical_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_lexical %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', lemma, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = cliques_lexical_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(Source, Target)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
    })
  })

# Save the representations in Pajek's .net format
for (ti in seq_along(cliques_lexical_subgraphs)) {
  for (si in seq_along(cliques_lexical_subgraphs[[ti]])) {
    write_file(cliques_lexical_subgraphs[[ti]][[si]], 
               paste0('results/cliques_lexical_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# Dependency-based approach (words) --------------------------------------------

# Generate list of edges
dependency_words_edges = dependencies %>%
  select(-index) %>%
  rename(index = governor) %>%
  left_join(words, by = c('text.index', 'sentence.index', 'index')) %>%
  rename(governorWord = word) %>%
  select(-index) %>%
  rename(index = dependent) %>%
  left_join(words, by = c('text.index', 'sentence.index', 'index')) %>%
  rename(dependentWord = word) %>%
  select(text.index, sentence.index, governorWord, dependentWord) %>%
  unique() %>%
  rename(word = governorWord) %>%
  left_join(vertices_words, 
            by = c('text.index', 'sentence.index', 'word')) %>%
  select(-word) %>%
  rename(governor = index,
         word = dependentWord) %>%
  left_join(vertices_words, 
            by = c('text.index', 'sentence.index', 'word')) %>%
  select(-word) %>%
  rename(dependent = index) %>%
  filter(!is.na(governor) & !is.na(dependent)) %>%
  group_by(text.index, sentence.index)

# Generate a representation with vertex definition 1 (words) for each sentence, 
# following the dependency-based approach, in Pajek's .net format
dependency_words_subgraphs = lapply(
  dependency_words_edges$text.index %>% unique(), function(ti) {
    text = dependency_words_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_words %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', word, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = dependency_words_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(governor, dependent)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
    })
  })

# Save the representations in Pajek's .net format
for (ti in seq_along(dependency_words_subgraphs)) {
  for (si in seq_along(dependency_words_subgraphs[[ti]])) {
    write_file(dependency_words_subgraphs[[ti]][[si]], 
               paste0('results/dependency_words_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# Dependency-based approach (lemmas) -------------------------------------------

# Generate list of edges
dependency_lemmas_edges = dependencies %>%
  select(-index) %>%
  rename(index = governor) %>%
  left_join(lemmas, by = c('text.index', 'sentence.index', 'index')) %>%
  rename(governorLemma = lemma) %>%
  select(-index) %>%
  rename(index = dependent) %>%
  left_join(lemmas, by = c('text.index', 'sentence.index', 'index')) %>%
  rename(dependentLemma = lemma) %>%
  select(text.index, sentence.index, governorLemma, dependentLemma) %>%
  unique() %>%
  rename(lemma = governorLemma) %>%
  left_join(vertices_lemmas, 
            by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(governor = index,
         lemma = dependentLemma) %>%
  left_join(vertices_lemmas, 
            by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(dependent = index) %>%
  filter(!is.na(governor) & !is.na(dependent)) %>%
  group_by(text.index, sentence.index)

# Generate a representation with vertex definition 2 (lemmas) for each sentence, 
# following the dependency-based approach, in Pajek's .net format
dependency_lemmas_subgraphs = lapply(
  dependency_lemmas_edges$text.index %>% unique(), function(ti) {
    text = dependency_lemmas_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_lemmas %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', lemma, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = dependency_lemmas_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(governor, dependent)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
    })
  })

# Save the representations in Pajek's .net format
for (ti in seq_along(dependency_lemmas_subgraphs)) {
  for (si in seq_along(dependency_lemmas_subgraphs[[ti]])) {
    write_file(dependency_lemmas_subgraphs[[ti]][[si]], 
               paste0('results/dependency_lemmas_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# Dependency-based approach (lexical lemmas) -----------------------------------

# Generate list of edges
dependency_lexical_edges = dependencies %>%
  select(-index) %>%
  rename(index = governor) %>%
  left_join(lemmas_lexical, by = c('text.index', 'sentence.index', 'index')) %>%
  rename(governorLemma = lemma) %>%
  select(-index) %>%
  rename(index = dependent) %>%
  left_join(lemmas_lexical, by = c('text.index', 'sentence.index', 'index')) %>%
  rename(dependentLemma = lemma) %>%
  select(text.index, sentence.index, governorLemma, dependentLemma) %>%
  unique() %>%
  rename(lemma = governorLemma) %>%
  left_join(vertices_lexical, 
            by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(governor = index,
         lemma = dependentLemma) %>%
  left_join(vertices_lexical, 
            by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(dependent = index) %>%
  filter(!is.na(governor) & !is.na(dependent)) %>%
  group_by(text.index, sentence.index)

# Generate a representation with vertex definition 3 (lexical lemmas) for each 
# sentence, following the dependency-based approach, in Pajek's .net format
dependency_lexical_subgraphs = lapply(
  dependency_lexical_edges$text.index %>% unique(), function(ti) {
    text = dependency_lexical_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_lexical %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', lemma, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = dependency_lexical_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(governor, dependent)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
    })
  })

# Save the representations in Pajek's .net format
for (ti in seq_along(dependency_lexical_subgraphs)) {
  for (si in seq_along(dependency_lexical_subgraphs[[ti]])) {
    write_file(dependency_lexical_subgraphs[[ti]][[si]], 
               paste0('results/dependency_lexical_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# IF-trimmed-subgraphs approach (words) ----------------------------------------

# Generate list of sentences
sentences_words = words %>%
  group_by(text.index, sentence.index) %>%
  summarise(sentence = paste(word, collapse = ' ')) %>%
  .$sentence

# Generate list of weighted edges
IFtrimmed_words_edges = vertices_words %>%
  rename(Source = word) %>%
  mutate(Target = Source) %>%
  expand(Source, Target) %>%
  filter(Source != Target) %>%
  mutate(edge.id = mapply(function(s, t) {
    sort(c(s, t)) %>% paste(collapse = '_')
  }, Source, Target)) %>%
  distinct(edge.id, .keep_all = T) %>%
  select(-edge.id) %>%
  mutate(
    # Calculate the number of sentences in which two words co-occur
    FreqPar =  mapply(function(s, t) {
      ((sentences_words %>% str_detect(paste0('\\b', s, '\\b'))) & 
         (sentences_words %>% str_detect(paste0('\\b', t, '\\b')))) %>%
        sum()
    }, Source, Target),
    # Calculate the number of sentences in which at least one of the words occur
    FreqCada = mapply(function(s, t) {
      ((sentences_words %>% str_detect(paste0('\\b', s, '\\b'))) | 
         (sentences_words %>% str_detect(paste0('\\b', t, '\\b')))) %>%
        sum()
    }, Source, Target),
    # Calculate incidence for each pair of vertices
    I = FreqPar/length(sentences_words),
    # Calculate fidelity for each pair of vertices
    F = FreqPar/FreqCada,
    # Calculate Indicence-Fidelity for each pair of vertices
    IF = I * F) %>%
  rename(word = Source) %>%
  left_join(vertices_words, 
            by = c('text.index', 'sentence.index', 'word')) %>%
  select(-word) %>%
  rename(Source = index,
         word = Target) %>%
  left_join(vertices_words, 
            by = c('text.index', 'sentence.index', 'word')) %>%
  select(-word) %>%
  rename(Target = index)

# Generate a representation with vertex definition 1 (words) for each sentence, 
# following the IF-trimmed-subgraphs approach, in Pajek's .net format
IFtrimmed_words_subgraphs_weighted = lapply(
  IFtrimmed_words_edges$text.index %>% unique(), function(ti) {
    text = IFtrimmed_words_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_words %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', word, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = IFtrimmed_words_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(Source, Target, IF)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
    })
  })

# Save the representations in Pajek's .net format
for (ti in seq_along(IFtrimmed_words_subgraphs_weighted)) {
  for (si in seq_along(IFtrimmed_words_subgraphs_weighted[[ti]])) {
    write_file(IFtrimmed_words_subgraphs_weighted[[ti]][[si]], 
               paste0('results/IFtrimmed_words_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# Get list of .net files
IFtrimmed_words_dir = 'results/IFtrimmed_words_subgraphs/'
IFtrimmed_words_files = list.files(IFtrimmed_words_dir, 
                                    '^text_\\d_sentence_\\d{3}.net')

# Read the files as igraph objects and progressively trim the weaker edges
IFtrimmed_words_subgraphs = sapply(
  IFtrimmed_words_files, function(IFw) {
    g = read_graph(paste0(IFtrimmed_words_dir, IFw), 'Pajek')
    
    repeat {
      weaker = E(g)$weight %>% min()
      trimmed = g %>% delete_edges(E(g)[E(g)$weight <= weaker])
      
      if (trimmed %>% is_connected()) {
        g = trimmed
      } else {
        break
      }
    }
    
    g %>% delete_edge_attr('weight')
  })

# Save the trimmed non-weighted graphs in Pajek's .net format
for (IFw_index in seq_along(IFtrimmed_words_subgraphs)) {
  write_graph(IFtrimmed_words_subgraphs[[IFw_index]], 
              paste0('results/IFtrimmed_words_subgraphs/trimmed_', 
                     IFtrimmed_words_files[IFw_index]),
              'Pajek')
}

# IF-trimmed-subgraphs approach (lemmas) ---------------------------------------

# Generate list of sentences
sentences_lemmas = lemmas %>%
  group_by(text.index, sentence.index) %>%
  summarise(sentence = paste(lemma, collapse = ' ')) %>%
  .$sentence

# Generate list of weighted edges
IFtrimmed_lemmas_edges = vertices_lemmas %>%
  rename(Source = lemma) %>%
  mutate(Target = Source) %>%
  expand(Source, Target) %>%
  filter(Source != Target) %>%
  mutate(edge.id = mapply(function(s, t) {
    sort(c(s, t)) %>% paste(collapse = '_')
  }, Source, Target)) %>%
  distinct(edge.id, .keep_all = T) %>%
  select(-edge.id) %>%
  mutate(
    # Calculate the number of sentences in which two lemmas co-occur
    FreqPar =  mapply(function(s, t) {
      ((sentences_lemmas %>% str_detect(paste0('\\b', s, '\\b'))) & 
         (sentences_lemmas %>% str_detect(paste0('\\b', t, '\\b')))) %>%
        sum()
    }, Source, Target),
    # Calculate the number of sentences in which at least one of the lemmas 
    # occur
    FreqCada = mapply(function(s, t) {
      ((sentences_lemmas %>% str_detect(paste0('\\b', s, '\\b'))) | 
         (sentences_lemmas %>% str_detect(paste0('\\b', t, '\\b')))) %>%
        sum()
    }, Source, Target),
    # Calculate incidence for each pair of vertices
    I = FreqPar/length(sentences_lemmas),
    # Calculate fidelity for each pair of vertices
    F = FreqPar/FreqCada,
    # Calculate Incidence-Fidelity for each pair of vertices
    IF = I * F) %>%
  rename(lemma = Source) %>%
  left_join(vertices_lemmas, 
            by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(Source = index,
         lemma = Target) %>%
  left_join(vertices_lemmas, 
            by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(Target = index)

# Generate a representation with vertex definition 2 (lemmas) for each sentence, 
# following the IF-trimmed-subgraphs approach, in Pajek's .net format
IFtrimmed_lemmas_subgraphs_weighted = lapply(
  IFtrimmed_lemmas_edges$text.index %>% unique(), function(ti) {
    text = IFtrimmed_lemmas_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_lemmas %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', lemma, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = IFtrimmed_lemmas_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(Source, Target, IF)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
    })
  })

# Save the representations in Pajek's .net format
for (ti in seq_along(IFtrimmed_lemmas_subgraphs_weighted)) {
  for (si in seq_along(IFtrimmed_lemmas_subgraphs_weighted[[ti]])) {
    write_file(IFtrimmed_lemmas_subgraphs_weighted[[ti]][[si]], 
               paste0('results/IFtrimmed_lemmas_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# Get list of .net files
IFtrimmed_lemmas_dir = 'results/IFtrimmed_lemmas_subgraphs/'
IFtrimmed_lemmas_files = list.files(IFtrimmed_lemmas_dir, 
                                   '^text_\\d_sentence_\\d{3}.net')

# Read the files as igraph objects and progressively trim the weaker edges
IFtrimmed_lemmas_subgraphs = sapply(
  IFtrimmed_lemmas_files, function(IFlem) {
    g = read_graph(paste0(IFtrimmed_lemmas_dir, IFlem), 'Pajek')
    
    repeat {
      weaker = E(g)$weight %>% min()
      trimmed = g %>% delete_edges(E(g)[E(g)$weight <= weaker])
      
      if (trimmed %>% is_connected()) {
        g = trimmed
      } else {
        break
      }
    }
    
    g %>% delete_edge_attr('weight')
  })

# Save the trimmed non-weighted graphs in Pajek's .net format
for (IFlem_index in seq_along(IFtrimmed_lemmas_subgraphs)) {
  write_graph(IFtrimmed_lemmas_subgraphs[[IFlem_index]], 
              paste0('results/IFtrimmed_lemmas_subgraphs/trimmed_', 
                     IFtrimmed_lemmas_files[IFlem_index]),
              'Pajek')
}

# IF-trimmed-subgraphs approach (lexical lemmas) -------------------------------

# Generate list of sentences
sentences_lexical = lemmas_lexical %>%
  group_by(text.index, sentence.index) %>%
  summarise(sentence = paste(lemma, collapse = ' ')) %>%
  .$sentence

# Generate list of weighted edges
IFtrimmed_lexical_edges = vertices_lexical %>%
  rename(Source = lemma) %>%
  mutate(Target = Source) %>%
  expand(Source, Target) %>%
  filter(Source != Target) %>%
  mutate(edge.id = mapply(function(s, t) {
    sort(c(s, t)) %>% paste(collapse = '_')
  }, Source, Target)) %>%
  distinct(edge.id, .keep_all = T) %>%
  select(-edge.id) %>%
  mutate(
    # Calculate the number of sentences in which two lexical lemmas co-occur
    FreqPar =  mapply(function(s, t) {
      ((sentences_lexical %>% str_detect(paste0('\\b', s, '\\b'))) & 
         (sentences_lexical %>% str_detect(paste0('\\b', t, '\\b')))) %>%
        sum()
    }, Source, Target),
    # Calculate the number of sentences in which at least one of the lexical 
    # lemmas occur
    FreqCada = mapply(function(s, t) {
      ((sentences_lexical %>% str_detect(paste0('\\b', s, '\\b'))) | 
         (sentences_lexical %>% str_detect(paste0('\\b', t, '\\b')))) %>%
        sum()
    }, Source, Target),
    # Calculate incidence for each pair of vertices
    I = FreqPar/length(sentences_lexical),
    # Calculate fidelity for each pair of vertices
    F = FreqPar/FreqCada,
    # Calculate Incidence-Fidelity for each pair of vertices
    IF = I * F) %>%
  rename(lemma = Source) %>%
  left_join(vertices_lexical, 
            by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(Source = index,
         lemma = Target) %>%
  left_join(vertices_lexical, 
            by = c('text.index', 'sentence.index', 'lemma')) %>%
  select(-lemma) %>%
  rename(Target = index)

# Generate a representation with vertex definition 3 (lexical lemmas) for each 
# sentence, following the IF-trimmed-subgraphs approach, in Pajek's .net format
IFtrimmed_lexical_subgraphs_weighted = lapply(
  IFtrimmed_lexical_edges$text.index %>% unique(), function(ti) {
    text = IFtrimmed_lexical_edges %>% filter(text.index == ti)
    
    lapply(text$sentence.index %>% unique(), function(si) {
      sentence = vertices_lexical %>% 
        filter(text.index == ti & sentence.index == si)
      
      n = sentence %>% nrow()
      
      V = sentence %>%
        mutate(vertices = paste0(index, ' "', lemma, '"')) %>%
        .$vertices %>%
        paste(collapse = '\n') %>%
        paste0(paste('*Vertices', n), '\n', .)
      
      E = IFtrimmed_lexical_edges %>%
        filter(text.index == ti & sentence.index == si) %>%
        mutate(edges = paste(Source, Target, IF)) %>%
        .$edges %>%
        paste(collapse = '\n') %>%
        paste0('*Edges', '\n', .)
      
      paste0(V, '\n', E)
    })
  })

# Save the representations in Pajek's .net format
for (ti in seq_along(IFtrimmed_lexical_subgraphs_weighted)) {
  for (si in seq_along(IFtrimmed_lexical_subgraphs_weighted[[ti]])) {
    write_file(IFtrimmed_lexical_subgraphs_weighted[[ti]][[si]], 
               paste0('results/IFtrimmed_lexical_subgraphs/text_', 
                      ti, 
                      '_sentence_', 
                      str_pad(si, 3, pad = '0'), 
                      '.net'))
  }
}

# Get list of .net files
IFtrimmed_lexical_dir = 'results/IFtrimmed_lexical_subgraphs/'
IFtrimmed_lexical_files = list.files(IFtrimmed_lexical_dir, 
                                    '^text_\\d_sentence_\\d{3}.net')

# Read the files as igraph objects and progressively trim the weaker edges
IFtrimmed_lexical_subgraphs = sapply(
  IFtrimmed_lexical_files, function(IFlex) {
    g = read_graph(paste0(IFtrimmed_lexical_dir, IFlex), 'Pajek')
    
    repeat {
      weaker = E(g)$weight %>% min()
      trimmed = g %>% delete_edges(E(g)[E(g)$weight <= weaker])
      
      if (trimmed %>% is_connected()) {
        g = trimmed
      } else {
        break
      }
    }
    
    g %>% delete_edge_attr('weight')
  })

# Save the trimmed non-weighted graphs in Pajek's .net format
for (IFlex_index in seq_along(IFtrimmed_lexical_subgraphs)) {
  write_graph(IFtrimmed_lexical_subgraphs[[IFlex_index]], 
              paste0('results/IFtrimmed_lexical_subgraphs/trimmed_', 
                     IFtrimmed_lexical_files[IFlex_index]),
              'Pajek')
}

# Networks of segments ---------------------------------------------------------

# Load subgraphs as igraph objects
lines_words_dir = 'results/lines_words_subgraphs/'
lines_words_files = list.files(lines_words_dir)
lines_words_subgraphs_igraph = sapply(lines_words_files, function(si) {
  read_graph(paste0(lines_words_dir, si), 'Pajek')
})

lines_lemmas_dir = 'results/lines_lemmas_subgraphs/'
lines_lemmas_files = list.files(lines_lemmas_dir)
lines_lemmas_subgraphs_igraph = sapply(lines_lemmas_files, function(si) {
  read_graph(paste0(lines_lemmas_dir, si), 'Pajek')
})

lines_lexical_dir = 'results/lines_lexical_subgraphs/'
lines_lexical_files = list.files(lines_lexical_dir)
lines_lexical_subgraphs_igraph = sapply(lines_lexical_files, function(si) {
  read_graph(paste0(lines_lexical_dir, si), 'Pajek')
})

closerange_words_dir = 'results/closerange_words_subgraphs/'
closerange_words_files = list.files(closerange_words_dir, 
                                    pattern = '^trimmed')
closerange_words_subgraphs_igraph = sapply(
  closerange_words_files, function(si) {
    read_graph(paste0(closerange_words_dir, si), 'Pajek')
})

closerange_lemmas_dir = 'results/closerange_lemmas_subgraphs/'
closerange_lemmas_files = list.files(closerange_lemmas_dir, 
                                     pattern = '^trimmed')
closerange_lemmas_subgraphs_igraph = sapply(
  closerange_lemmas_files, function(si) {
    read_graph(paste0(closerange_lemmas_dir, si), 'Pajek')
})

closerange_lexical_dir = 'results/closerange_lexical_subgraphs/'
closerange_lexical_files = list.files(closerange_lexical_dir, 
                                      pattern = '^trimmed')
closerange_lexical_subgraphs_igraph = sapply(
  closerange_lexical_files, function(si) {
    read_graph(paste0(closerange_lexical_dir, si), 'Pajek')
})

cliques_words_dir = 'results/cliques_words_subgraphs/'
cliques_words_files = list.files(cliques_words_dir)
cliques_words_subgraphs_igraph = sapply(cliques_words_files, function(si) {
  read_graph(paste0(cliques_words_dir, si), 'Pajek')
})

cliques_lemmas_dir = 'results/cliques_lemmas_subgraphs/'
cliques_lemmas_files = list.files(cliques_lemmas_dir)
cliques_lemmas_subgraphs_igraph = sapply(cliques_lemmas_files, function(si) {
  read_graph(paste0(cliques_lemmas_dir, si), 'Pajek')
})

cliques_lexical_dir = 'results/cliques_lexical_subgraphs/'
cliques_lexical_files = list.files(cliques_lexical_dir)
cliques_lexical_subgraphs_igraph = sapply(cliques_lexical_files, function(si) {
  read_graph(paste0(cliques_lexical_dir, si), 'Pajek')
})

dependency_words_dir = 'results/dependency_words_subgraphs/'
dependency_words_files = list.files(dependency_words_dir)
dependency_words_subgraphs_igraph = sapply(
  dependency_words_files, function(si) {
    read_graph(paste0(dependency_words_dir, si), 'Pajek')
})

dependency_lemmas_dir = 'results/dependency_lemmas_subgraphs/'
dependency_lemmas_files = list.files(dependency_lemmas_dir)
dependency_lemmas_subgraphs_igraph = sapply(
  dependency_lemmas_files, function(si) {
    read_graph(paste0(dependency_lemmas_dir, si), 'Pajek')
})

dependency_lexical_dir = 'results/dependency_lexical_subgraphs/'
dependency_lexical_files = list.files(dependency_lexical_dir)
dependency_lexical_subgraphs_igraph = sapply(
  dependency_lexical_files, function(si) {
    read_graph(paste0(dependency_lexical_dir, si), 'Pajek')
})

IFtrimmed_words_dir = 'results/IFtrimmed_words_subgraphs/'
IFtrimmed_words_files = list.files(IFtrimmed_words_dir, 
                                   pattern = '^trimmed')
IFtrimmed_words_subgraphs_igraph = sapply(
  IFtrimmed_words_files, function(si) {
    read_graph(paste0(IFtrimmed_words_dir, si), 'Pajek')
  })

IFtrimmed_lemmas_dir = 'results/IFtrimmed_lemmas_subgraphs/'
IFtrimmed_lemmas_files = list.files(IFtrimmed_lemmas_dir, 
                                    pattern = '^trimmed')
IFtrimmed_lemmas_subgraphs_igraph = sapply(
  IFtrimmed_lemmas_files, function(si) {
    read_graph(paste0(IFtrimmed_lemmas_dir, si), 'Pajek')
  })

IFtrimmed_lexical_dir = 'results/IFtrimmed_lexical_subgraphs/'
IFtrimmed_lexical_files = list.files(IFtrimmed_lexical_dir, 
                                     pattern = '^trimmed')
IFtrimmed_lexical_subgraphs_igraph = sapply(
  IFtrimmed_lexical_files, function(si) {
    read_graph(paste0(IFtrimmed_lexical_dir, si), 'Pajek')
  })


# Create networks of segments from subgraphs
G_lines_words = lines_words_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

G_lines_lemmas = lines_lemmas_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

G_lines_lexical = lines_lexical_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

G_closerange_words = closerange_words_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

G_closerange_lemmas = closerange_lemmas_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

G_closerange_lexical = closerange_lexical_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

G_cliques_words = cliques_words_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

G_cliques_lemmas = cliques_lemmas_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

G_cliques_lexical = cliques_lexical_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

G_dependency_words = dependency_words_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

G_dependency_lemmas = dependency_lemmas_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

G_dependency_lexical = dependency_lexical_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

G_IFtrimmed_words = IFtrimmed_words_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

G_IFtrimmed_lemmas = IFtrimmed_lemmas_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

G_IFtrimmed_lexical = IFtrimmed_lexical_subgraphs_igraph %>% 
  reduce(function(a, b) { union(a, b) }) %>% 
  simplify()

# Set the id for saving as Pajek's .net format
V(G_lines_words)$id = V(G_lines_words)$name
V(G_lines_lemmas)$id = V(G_lines_lemmas)$name
V(G_lines_lexical)$id = V(G_lines_lexical)$name
V(G_closerange_words)$id = V(G_closerange_words)$name
V(G_closerange_lemmas)$id = V(G_closerange_lemmas)$name
V(G_closerange_lexical)$id = V(G_closerange_lexical)$name
V(G_cliques_words)$id = V(G_cliques_words)$name
V(G_cliques_lemmas)$id = V(G_cliques_lemmas)$name
V(G_cliques_lexical)$id = V(G_cliques_lexical)$name
V(G_dependency_words)$id = V(G_dependency_words)$name
V(G_dependency_lemmas)$id = V(G_dependency_lemmas)$name
V(G_dependency_lexical)$id = V(G_dependency_lexical)$name
V(G_IFtrimmed_words)$id = V(G_IFtrimmed_words)$name
V(G_IFtrimmed_lemmas)$id = V(G_IFtrimmed_lemmas)$name
V(G_IFtrimmed_lexical)$id = V(G_IFtrimmed_lexical)$name

# Save networks as Pajek's .net format
write_graph(G_lines_words, 
            'results/network_lines_words.net', 'Pajek')
write_graph(G_lines_lemmas, 
            'results/network_lines_lemmas.net', 'Pajek')
write_graph(G_lines_lexical, 
            'results/network_lines_lexical.net', 'Pajek')
write_graph(G_closerange_words, 
            'results/network_closerange_words.net', 'Pajek')
write_graph(G_closerange_lemmas, 
            'results/network_closerange_lemmas.net', 'Pajek')
write_graph(G_closerange_lexical, 
            'results/network_closerange_lexical.net', 'Pajek')
write_graph(G_cliques_words, 
            'results/network_cliques_words.net', 'Pajek')
write_graph(G_cliques_lemmas, 
            'results/network_cliques_lemmas.net', 'Pajek')
write_graph(G_cliques_lexical, 
            'results/network_cliques_lexical.net', 'Pajek')
write_graph(G_dependency_words, 
            'results/network_dependency_words.net', 'Pajek')
write_graph(G_dependency_lemmas, 
            'results/network_dependency_lemmas.net', 'Pajek')
write_graph(G_dependency_lexical, 
            'results/network_dependency_lexical.net', 'Pajek')
write_graph(G_IFtrimmed_words, 
            'results/network_IFtrimmed_words.net', 'Pajek')
write_graph(G_IFtrimmed_lemmas, 
            'results/network_IFtrimmed_lemmas.net', 'Pajek')
write_graph(G_IFtrimmed_lexical, 
            'results/network_IFtrimmed_lexical.net', 'Pajek')

# Subgraphs' metrics -----------------------------------------------------------

# Calculate metrics for the isolated subgraphs
metrics_subgraphs = tibble(
  # Sentence length meaused by number of unique tokens
  sentence_lengths_words = sapply(lines_words_subgraphs_igraph, 
                                  function(s_i) {gorder(s_i)}),
  sentence_lengths_lemmas = sapply(lines_lemmas_subgraphs_igraph, 
                                   function(s_i) {gorder(s_i)}),
  sentence_lengths_lexical = sapply(lines_lexical_subgraphs_igraph, 
                                   function(s_i) {gorder(s_i)}),
  # Average minimal path length
  L_lines_words = sapply(lines_words_subgraphs_igraph, 
                         mean_distance),
  L_lines_lemmas = sapply(lines_lemmas_subgraphs_igraph, 
                          mean_distance),
  L_lines_lexical = sapply(lines_lexical_subgraphs_igraph, 
                           mean_distance),
  L_closerange_words = sapply(closerange_words_subgraphs_igraph, 
                              mean_distance),
  L_closerange_lemmas = sapply(closerange_lemmas_subgraphs_igraph, 
                               mean_distance),
  L_closerange_lexical = sapply(closerange_lexical_subgraphs_igraph,
                                mean_distance),
  L_cliques_words = sapply(cliques_words_subgraphs_igraph, 
                           mean_distance),
  L_cliques_lemmas = sapply(cliques_lemmas_subgraphs_igraph, 
                            mean_distance),
  L_cliques_lexical = sapply(cliques_lexical_subgraphs_igraph, 
                             mean_distance),
  L_dependency_words = sapply(dependency_words_subgraphs_igraph, 
                              mean_distance),
  L_dependency_lemmas = sapply(dependency_lemmas_subgraphs_igraph, 
                               mean_distance),
  L_dependency_lexical = sapply(dependency_lexical_subgraphs_igraph, 
                                mean_distance),
  L_IFtrimmed_words = sapply(IFtrimmed_words_subgraphs_igraph, 
                             mean_distance),
  L_IFtrimmed_lemmas = sapply(IFtrimmed_lemmas_subgraphs_igraph, 
                              mean_distance),
  L_IFtrimmed_lexical = sapply(IFtrimmed_lexical_subgraphs_igraph, 
                               mean_distance),
  # Average degree
  average_k_lines_words = sapply(lines_words_subgraphs_igraph, 
                                 function(s_i) {degree(s_i)} %>% mean()),
  average_k_lines_lemmas = sapply(lines_lemmas_subgraphs_igraph, 
                                 function(s_i) {degree(s_i)} %>% mean()),
  average_k_lines_lexical = sapply(lines_lexical_subgraphs_igraph, 
                                 function(s_i) {degree(s_i)} %>% mean()),
  average_k_closerange_words = sapply(closerange_words_subgraphs_igraph, 
                                 function(s_i) {degree(s_i)} %>% mean()),
  average_k_closerange_lemmas = sapply(closerange_lemmas_subgraphs_igraph, 
                                  function(s_i) {degree(s_i)} %>% mean()),
  average_k_closerange_lexical = sapply(closerange_lexical_subgraphs_igraph, 
                                   function(s_i) {degree(s_i)} %>% mean()),
  average_k_cliques_words = sapply(cliques_words_subgraphs_igraph, 
                                 function(s_i) {degree(s_i)} %>% mean()),
  average_k_cliques_lemmas = sapply(cliques_lemmas_subgraphs_igraph, 
                                  function(s_i) {degree(s_i)} %>% mean()),
  average_k_cliques_lexical = sapply(cliques_lexical_subgraphs_igraph, 
                                   function(s_i) {degree(s_i)} %>% mean()),
  average_k_dependency_words = sapply(dependency_words_subgraphs_igraph, 
                                 function(s_i) {degree(s_i)} %>% mean()),
  average_k_dependency_lemmas = sapply(dependency_lemmas_subgraphs_igraph, 
                                  function(s_i) {degree(s_i)} %>% mean()),
  average_k_dependency_lexical = sapply(dependency_lexical_subgraphs_igraph, 
                                   function(s_i) {degree(s_i)} %>% mean()),
  average_k_IFtrimmed_words = sapply(IFtrimmed_words_subgraphs_igraph, 
                                 function(s_i) {degree(s_i)} %>% mean()),
  average_k_IFtrimmed_lemmas = sapply(IFtrimmed_lemmas_subgraphs_igraph, 
                                  function(s_i) {degree(s_i)} %>% mean()),
  average_k_IFtrimmed_lexical = sapply(IFtrimmed_lexical_subgraphs_igraph, 
                                   function(s_i) {degree(s_i)} %>% mean()),
)

# Save results as a CSV file
write_csv(metrics_subgraphs, 'results/metrics_subgraphs.csv')

# Networks' metrics ------------------------------------------------------------

# Calculate metrics for the networks
metrics_networks = tibble(
  approach = c(rep('lines', 3), 
               rep('closerange', 3), 
               rep('cliques', 3), 
               rep('dependency', 3), 
               rep('IFtrimmed', 3)),
  definition = rep(c('words', 'lemmas', 'lexical'), 5),
  # Order (number of vertices)
  n = c(gorder(G_lines_words),
        gorder(G_lines_lemmas),
        gorder(G_lines_lexical),
        gorder(G_closerange_words),
        gorder(G_closerange_lemmas),
        gorder(G_closerange_lexical),
        gorder(G_cliques_words),
        gorder(G_cliques_lemmas),
        gorder(G_cliques_lexical),
        gorder(G_dependency_words),
        gorder(G_dependency_lemmas),
        gorder(G_dependency_lexical),
        gorder(G_IFtrimmed_words),
        gorder(G_IFtrimmed_lemmas),
        gorder(G_IFtrimmed_lexical)),
  # Size (number of edges)
  m = c(gsize(G_lines_words),
        gsize(G_lines_lemmas),
        gsize(G_lines_lexical),
        gsize(G_closerange_words),
        gsize(G_closerange_lemmas),
        gsize(G_closerange_lexical),
        gsize(G_cliques_words),
        gsize(G_cliques_lemmas),
        gsize(G_cliques_lexical),
        gsize(G_dependency_words),
        gsize(G_dependency_lemmas),
        gsize(G_dependency_lexical),
        gsize(G_IFtrimmed_words),
        gsize(G_IFtrimmed_lemmas),
        gsize(G_IFtrimmed_lexical)),
  # Density
  Delta = c(edge_density(G_lines_words),
            edge_density(G_lines_lemmas),
            edge_density(G_lines_lexical),
            edge_density(G_closerange_words),
            edge_density(G_closerange_lemmas),
            edge_density(G_closerange_lexical),
            edge_density(G_cliques_words),
            edge_density(G_cliques_lemmas),
            edge_density(G_cliques_lexical),
            edge_density(G_dependency_words),
            edge_density(G_dependency_lemmas),
            edge_density(G_dependency_lexical),
            edge_density(G_IFtrimmed_words),
            edge_density(G_IFtrimmed_lemmas),
            edge_density(G_IFtrimmed_lexical)),
  # Average minimal path length
  L = c(mean_distance(G_lines_words),
        mean_distance(G_lines_lemmas),
        mean_distance(G_lines_lexical),
        mean_distance(G_closerange_words),
        mean_distance(G_closerange_lemmas),
        mean_distance(G_closerange_lexical),
        mean_distance(G_cliques_words),
        mean_distance(G_cliques_lemmas),
        mean_distance(G_cliques_lexical),
        mean_distance(G_dependency_words),
        mean_distance(G_dependency_lemmas),
        mean_distance(G_dependency_lexical),
        mean_distance(G_IFtrimmed_words),
        mean_distance(G_IFtrimmed_lemmas),
        mean_distance(G_IFtrimmed_lexical)),
  # Average degree
  average_k = c(degree(G_lines_words) %>% mean(),
                degree(G_lines_lemmas) %>% mean(),
                degree(G_lines_lexical) %>% mean(),
                degree(G_closerange_words) %>% mean(),
                degree(G_closerange_lemmas) %>% mean(),
                degree(G_closerange_lexical) %>% mean(),
                degree(G_cliques_words) %>% mean(),
                degree(G_cliques_lemmas) %>% mean(),
                degree(G_cliques_lexical) %>% mean(),
                degree(G_dependency_words) %>% mean(),
                degree(G_dependency_lemmas) %>% mean(),
                degree(G_dependency_lexical) %>% mean(),
                degree(G_IFtrimmed_words) %>% mean(),
                degree(G_IFtrimmed_lemmas) %>% mean(),
                degree(G_IFtrimmed_lexical) %>% mean()),
  # Modularity
  Q = c(modularity(cluster_louvain(G_lines_words)),
        modularity(cluster_louvain(G_lines_lemmas)),
        modularity(cluster_louvain(G_lines_lexical)),
        modularity(cluster_louvain(G_closerange_words)),
        modularity(cluster_louvain(G_closerange_lemmas)),
        modularity(cluster_louvain(G_closerange_lexical)),
        modularity(cluster_louvain(G_cliques_words)),
        modularity(cluster_louvain(G_cliques_lemmas)),
        modularity(cluster_louvain(G_cliques_lexical)),
        modularity(cluster_louvain(G_dependency_words)),
        modularity(cluster_louvain(G_dependency_lemmas)),
        modularity(cluster_louvain(G_dependency_lexical)),
        modularity(cluster_louvain(G_IFtrimmed_words)),
        modularity(cluster_louvain(G_IFtrimmed_lemmas)),
        modularity(cluster_louvain(G_IFtrimmed_lexical))),
  # Local efficiency
  E_loc = c(average_local_efficiency(G_lines_words),
            average_local_efficiency(G_lines_lemmas),
            average_local_efficiency(G_lines_lexical),
            average_local_efficiency(G_closerange_words),
            average_local_efficiency(G_closerange_lemmas),
            average_local_efficiency(G_closerange_lexical),
            average_local_efficiency(G_cliques_words),
            average_local_efficiency(G_cliques_lemmas),
            average_local_efficiency(G_cliques_lexical),
            average_local_efficiency(G_dependency_words),
            average_local_efficiency(G_dependency_lemmas),
            average_local_efficiency(G_dependency_lexical),
            average_local_efficiency(G_IFtrimmed_words),
            average_local_efficiency(G_IFtrimmed_lemmas),
            average_local_efficiency(G_IFtrimmed_lexical)),
  # Global efficiency
  E_glob = c(global_efficiency(G_lines_words),
             global_efficiency(G_lines_lemmas),
             global_efficiency(G_lines_lexical),
             global_efficiency(G_closerange_words),
             global_efficiency(G_closerange_lemmas),
             global_efficiency(G_closerange_lexical),
             global_efficiency(G_cliques_words),
             global_efficiency(G_cliques_lemmas),
             global_efficiency(G_cliques_lexical),
             global_efficiency(G_dependency_words),
             global_efficiency(G_dependency_lemmas),
             global_efficiency(G_dependency_lexical),
             global_efficiency(G_IFtrimmed_words),
             global_efficiency(G_IFtrimmed_lemmas),
             global_efficiency(G_IFtrimmed_lexical)),
  # Parameter of the power-law fit for the cumulative probability distributions 
  # of the degrees. Originally, xmin and p-values were calculated following the 
  # procedures described in https://github.com/csgillespie/poweRlaw using the 
  # poweRlaw package. Here we use the fit_power_law function from igraph for
  # computational efficiency as both functions result in the same values for
  # gamma
  gamma = c(fit_power_law(degree(G_lines_words), xmin = 10)$alpha,
            fit_power_law(degree(G_lines_lemmas), xmin = 10)$alpha,
            fit_power_law(degree(G_lines_lexical), xmin = 2)$alpha,
            fit_power_law(degree(G_closerange_words), xmin = 19)$alpha,
            fit_power_law(degree(G_closerange_lemmas), xmin = 15)$alpha,
            fit_power_law(degree(G_closerange_lexical), xmin = 54)$alpha,
            fit_power_law(degree(G_cliques_words), xmin = 140)$alpha,
            fit_power_law(degree(G_cliques_lemmas), xmin = 222)$alpha,
            fit_power_law(degree(G_cliques_lexical), xmin = 64)$alpha,
            fit_power_law(degree(G_dependency_words), xmin = 20)$alpha,
            fit_power_law(degree(G_dependency_lemmas), xmin = 17)$alpha,
            fit_power_law(degree(G_dependency_lexical), xmin = 8)$alpha,
            fit_power_law(degree(G_IFtrimmed_words), xmin = 21)$alpha,
            fit_power_law(degree(G_IFtrimmed_lemmas), xmin = 19)$alpha,
            fit_power_law(degree(G_IFtrimmed_lexical), xmin = 21)$alpha)
)

# Save results as a CSV file
write_csv(metrics_networks, 'results/metrics_networks.csv')
