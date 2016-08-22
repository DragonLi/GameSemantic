module GS.ICA.AST

type ast = 
 | True
 | False
 | Num of int
 | Skip
 | Var of string
 | Lambda of ast*ast
 | App of ast*ast
 | Expr of ast*ast*string
 | If of ast*ast*ast
 | New of ast*ast
 | Seq of ast*ast
 | Deref of ast
 | Assg of ast*ast
 | Par of ast*ast
 | Sem of ast*ast
 | Grab of ast
 | Release of ast