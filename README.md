# tacos2text

Personnal code to load a text2vect model writen in explicit text format (in my tests, trained with R) and use it to rename a tree that represents question from a skill assesment test (each leaf has a text, the nodes are renammed with an adjective/job-name meant to describe the associated leafs).

Also equiped to allow interaction with the said tree. As the user answers question, an algorithm select the next question to ask in order to manage exploration an exploitation (UCT algorithm) : the user can answer only part of the test and the algorithm will still have efficiently gathered data on the skills he has/likes.

The final tree is exported in html to allow for interactive visualisation.

(proof of concept for implementation in a serious game)
