# ABC-Histo

## Usage

```lisp
(let ((*db* (make-instance 'memory-db)))
  (add-dir "~/.abc/")
  (sort (histo *db*) #'> :key #'cdr))
```
