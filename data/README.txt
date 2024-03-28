"The files 'parsed_text_1.json', 'parsed_text_2.json', 'parsed_text_3.json',
'parsed_text_4.json', and 'parsed_text_5.json' comprise a Transparently
Modified Version of a sample from the biomed section of the O-ANC corpus. The
files correspond respectively to the texts '1468-6708-3-1.txt',
'1471-213X-1-2.txt', '1472-6750-3-4.txt', '1472-6793-2-17.txt', and 'ar612.txt'
that were randomly selected and are available at https://anc.org/data/oanc/download/.
The modifications were done by Davi Alves Oliveira and Hernane Borges de Barros
Pereira in March 2024 for the study reported in the article “Modeling texts with
networks” submitted to The European Physical Journal B (https://epjb.epj.org/) in 2024.
The biomed section of the O-ANC is from Medical research articles (2000-2003)
licensed under the Biomed Central Open Access License.

Each text file was parsed with the Stanford CoreNLP dependency parser 
(https://stanfordnlp.github.io/CoreNLP/) using the command 'java -cp "*"
edu.stanford.nlp.pipeline.StanfordCoreNLP -annotators tokenize,pos,lemma,parse,
depparse -file [file_path] -outputFormat json', where [file_path] was the path
of the file being parsed. Details of modification are given in the README.txt
file of the code directory.