# tacos2text

**Personnal proof of concept for implementation in a serious game**

Load a text2vect model written in explicit text format (here trained with R) and use it to rename a tree that represents questions from a skill assessment test (each leaf has a text, the nodes are renamed with an adjective/job-name meant to describe the associated leafs).

Also equipped to allow interaction with the said tree. As the user answers question, an algorithm select the next question to ask in order to manage exploration an exploitation (UCT algorithm) : the user can answer only part of the test and the algorithm will still have efficiently gathered data on the skills he has/likes.

The model is cached (using protobuf) to improve loading time, the final tree is exported in HTML to allow for interactive visualization using D3.

technologies involved :
- using a **text2vect model** to summarize a group of sentences in a single word
- **UCT algorithm** to select the next question to ask
- caching the model with **protobuf**
- outputting the tree to get a nice display using **D3**
