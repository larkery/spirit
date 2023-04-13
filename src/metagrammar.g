G = item+ sp;
<item> = command | grammar;
command = label sp parameters? sp <':' #"\s*"> sentence <';'> sp
parameters = #"\{.+?\}";
grammar = label sp <'=' #"\s*"> sentence <';'> sp
sentence = word-sequence | alternates;
word-sequence = (optional-words | word | grammar-rule | capture)+;
grammar-rule = <'<'> label <'>'>;
capture = <'{'> sp label sp <':'> sp sentence <'}'>;
optional-words = (<'['> word+ <']'>);
alternates = sentence sp <'|'> sp sentence;
<label> = #"[a-z./_-]+";
<sp> = <#"\s*">
<word> = <#"\s*"> #"\w+" <#"\s*">;

