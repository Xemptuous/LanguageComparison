(ns lexer.token)

(defrecord Token [type literal])

(def singleTokenMap {\! :exclamation
                     \@ :at
                     \# :hashtag
                     \$ :dollar
                     \% :percent
                     \^ :caret
                     \& :ampersand
                     \* :asterisk
                     \( :lparen
                     \) :rparen
                     \- :minus
                     \_ :underscore
                     \+ :plus
                     \= :assign
                     \[ :lbracket
                     \] :rbracket
                     \{ :lbrace
                     \} :rbrace
                     \; :semicolon
                     \: :colon
                     \' :char
                     \" :string
                     \, :comma
                     \. :period
                     \< :lessthan
                     \> :greaterthan
                     \/ :slash
                     \? :question
                     \\ :backslash
                     \| :pipe})

(def doubleTokenMap {"==" :equal
                     "!=" :not_equal
                     "+=" :plus_eq
                     "-=" :minus_eq
                     "*=" :mult_eq
                     "/=" :div_eq
                     "<=" :lt_equal
                     ">=" :gt_equal
                     "++" :increment
                     "--" :decrement
                     "//" :comment})

(def keywordMap {"let" :let
                 "const" :const
                 "struct" :struct
                 "fn" :function
                 "if" :if
                 "else" :else
                 "switch" :switch
                 "case" :case
                 "break" :break
                 "return" :return
                 "while" :while
                 "for" :for
                 "and" :and
                 "or" :or
                 "in" :in
                 "true" :boolean
                 "false" :boolean
                 "bool" :bool
                 "int" :int
                 "f32" :f32
                 "str" :str
                 "nil" :nil
                 "void" :void})
