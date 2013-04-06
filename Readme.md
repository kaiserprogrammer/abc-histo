# ABC-Histo

## Usage

```lisp
(let ((*db* (make-instance 'memory-db)))
  (add-dir "~/.abc/")
  (print (top 15))
  (print (top 10 :topic "programming"))
  (print (search-histo "G" :topic "programming")))
```
