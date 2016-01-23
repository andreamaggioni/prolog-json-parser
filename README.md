# prolog-json-parser
####Progetto universitario
Lo scopo del progetto è quello di realizzare un parse json, scritto in **prolog**.
#####Esempi
```prolog
?- json_parse('{"nome" : "Mario", "cognome" : "Rossi"}', O), json_dot(O, [nome], R).
O = json_object([(nome, 'Mario'), (cognome, 'Rossi')])
R = 'Mario'
```
