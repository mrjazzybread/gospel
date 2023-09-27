"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[480],{3905:function(e,n,t){t.d(n,{Zo:function(){return p},kt:function(){return h}});var a=t(7294);function i(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function o(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,a)}return t}function r(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?o(Object(t),!0).forEach((function(n){i(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):o(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function s(e,n){if(null==e)return{};var t,a,i=function(e,n){if(null==e)return{};var t,a,i={},o=Object.keys(e);for(a=0;a<o.length;a++)t=o[a],n.indexOf(t)>=0||(i[t]=e[t]);return i}(e,n);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)t=o[a],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(i[t]=e[t])}return i}var l=a.createContext({}),c=function(e){var n=a.useContext(l),t=n;return e&&(t="function"==typeof e?e(n):r(r({},n),e)),t},p=function(e){var n=c(e.components);return a.createElement(l.Provider,{value:n},e.children)},d="mdxType",u={inlineCode:"code",wrapper:function(e){var n=e.children;return a.createElement(a.Fragment,{},n)}},m=a.forwardRef((function(e,n){var t=e.components,i=e.mdxType,o=e.originalType,l=e.parentName,p=s(e,["components","mdxType","originalType","parentName"]),d=c(t),m=i,h=d["".concat(l,".").concat(m)]||d[m]||u[m]||o;return t?a.createElement(h,r(r({ref:n},p),{},{components:t})):a.createElement(h,r({ref:n},p))}));function h(e,n){var t=arguments,i=n&&n.mdxType;if("string"==typeof e||i){var o=t.length,r=new Array(o);r[0]=m;var s={};for(var l in n)hasOwnProperty.call(n,l)&&(s[l]=n[l]);s.originalType=e,s[d]="string"==typeof e?e:i,r[1]=s;for(var c=2;c<o;c++)r[c]=t[c];return a.createElement.apply(null,r)}return a.createElement.apply(null,t)}m.displayName="MDXCreateElement"},5422:function(e,n,t){t.r(n),t.d(n,{assets:function(){return l},contentTitle:function(){return r},default:function(){return d},frontMatter:function(){return o},metadata:function(){return s},toc:function(){return c}});var a=t(7462),i=(t(7294),t(3905));const o={sidebar_position:6},r="Function Contracts",s={unversionedId:"language/function-contracts",id:"language/function-contracts",title:"Function Contracts",description:"A function contract is a formal Gospel specification attached to the declaration",source:"@site/docs/language/function-contracts.md",sourceDirName:"language",slug:"/language/function-contracts",permalink:"/gospel/language/function-contracts",draft:!1,tags:[],version:"current",sidebarPosition:6,frontMatter:{sidebar_position:6},sidebar:"tutorialSidebar",previous:{title:"Constant Specifications",permalink:"/gospel/language/constant-specifications"},next:{title:"Logical Declarations",permalink:"/gospel/language/logical"}},l={},c=[{value:"Default Behaviour",id:"default-behaviour",level:2},{value:"Preconditions",id:"preconditions",level:2},{value:"<code>requires</code>",id:"requires",level:3},{value:"<code>checks</code>",id:"checks",level:3},{value:"Postconditions",id:"postconditions",level:2},{value:"Exceptional Postconditions",id:"exceptional-postconditions",level:2},{value:"Code Equivalence",id:"code-equivalence",level:2},{value:"Non-Termination",id:"non-termination",level:2},{value:"Data Mutability",id:"data-mutability",level:2},{value:"Pure Functions",id:"pure-functions",level:2},{value:"Data Consumption",id:"data-consumption",level:2},{value:"Ghost Parameters",id:"ghost-parameters",level:2}],p={toc:c};function d(e){let{components:n,...t}=e;return(0,i.kt)("wrapper",(0,a.Z)({},p,t,{components:n,mdxType:"MDXLayout"}),(0,i.kt)("h1",{id:"function-contracts"},"Function Contracts"),(0,i.kt)("p",null,"A function contract is a formal Gospel specification attached to the declaration\nof an OCaml function in an interface. Here is an example:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ocaml"},"val euclidean_division: int -> int -> int * int\n(*@ q, r = euclidean_division x y\n    requires y > 0\n    ensures  x = q * y + r\n    ensures  0 <= r < y *)\n")),(0,i.kt)("p",null,"A function contract is composed of two parts:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"The first line is the ",(0,i.kt)("strong",{parentName:"li"},"header")," of the contract; it names the function\narguments and results and must appear at the beginning of the contract."),(0,i.kt)("li",{parentName:"ul"},"The next lines contain as many specification clauses as needed. The\nprevious example features three clauses: one precondition introduced by\n",(0,i.kt)("inlineCode",{parentName:"li"},"requires")," and two postconditions introduced by ",(0,i.kt)("inlineCode",{parentName:"li"},"ensures"),".")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ebnf",metastring:'title="Function contract syntax"',title:'"Function',contract:!0,'syntax"':!0},'contract = header? clause*\nheader = (identifier_tuple "=")? identifier parameter+\nclause = "requires" formula\n       | "checks" formula\n       | "ensures" formula\n       | "raises" exception_case ("|" exception-case)*\n       | "modifies" expr ("," expr)*\n       | "equivalent" string_literal\n       | "diverges"\n       | "pure"\n       | "consumes" expr ("," expr)*\nexception_case = qualid "->" formula\n               | qualid pattern "->" formula\n               | qualid\nidentifier_tuple = identifier ("," identifier)*\nparameter = "()" | identifier | "~" identifier | "?" identifier\n')),(0,i.kt)("admonition",{type:"tip"},(0,i.kt)("p",{parentName:"admonition"},"Even if the order of clauses is not imposed by the grammar, we suggest to use\nthe following systematic order for uniformity:"),(0,i.kt)("ul",{parentName:"admonition"},(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"requires")," preconditions,"),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"checks")," preconditions,"),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"modifies")," and ",(0,i.kt)("inlineCode",{parentName:"li"},"consumes")," effects,"),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"ensures")," postconditions,"),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"raises")," exceptional postconditions."))),(0,i.kt)("h2",{id:"default-behaviour"},"Default Behaviour"),(0,i.kt)("p",null,"To avoid boilerplate for usual properties, Gospel applies a default contract\nwhenever a function has a specification attached. Of course, any\nexplicitly declared clause overrides this implicit contract."),(0,i.kt)("p",null,"When a function has a contract attached, the default contract contains the\nfollowing properties:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"The function ",(0,i.kt)("strong",{parentName:"li"},"terminates"),"."),(0,i.kt)("li",{parentName:"ul"},"The function ",(0,i.kt)("strong",{parentName:"li"},"does not raise any exception")," other than ",(0,i.kt)("inlineCode",{parentName:"li"},"Stack_overflow")," or\n",(0,i.kt)("inlineCode",{parentName:"li"},"Out_of_memory"),"."),(0,i.kt)("li",{parentName:"ul"},"The function ",(0,i.kt)("strong",{parentName:"li"},"does not have any visible side-effect on the memory"),". In other\nwords, if it mutates some data, this has no observable influence on the values\nin the rest of the program.")),(0,i.kt)("admonition",{type:"caution"},(0,i.kt)("p",{parentName:"admonition"},"   In the absence of a contract attached to a function declaration, you cannot\nmake any assumptions about its behaviour: the function may diverge, raise\nunlisted exceptions, or modify mutable states. However, ",(0,i.kt)("strong",{parentName:"p"},"it still cannot\nbreak any type invariant.")),(0,i.kt)("p",{parentName:"admonition"},"   You can still enable the default implicit properties about exceptions, mutability,\nnon-termination, etc., by creating an empty contract:"),(0,i.kt)("pre",{parentName:"admonition"},(0,i.kt)("code",{parentName:"pre",className:"language-ocaml"},"val euclidean_division: int -> int -> int * int\n(*@ q, r = euclidean_division x y *)\n"))),(0,i.kt)("h2",{id:"preconditions"},"Preconditions"),(0,i.kt)("p",null,"Preconditions are ",(0,i.kt)("strong",{parentName:"p"},"properties that must hold at the function entry"),". You can use\nthem to describe requirements on the function's inputs, but you can also possibly use them on a\nglobal state that exists outside of the function arguments."),(0,i.kt)("p",null,"You can express preconditions using the keyword ",(0,i.kt)("inlineCode",{parentName:"p"},"requires")," or ",(0,i.kt)("inlineCode",{parentName:"p"},"checks")," followed by a\nformula."),(0,i.kt)("h3",{id:"requires"},(0,i.kt)("inlineCode",{parentName:"h3"},"requires")),(0,i.kt)("p",null,"A ",(0,i.kt)("inlineCode",{parentName:"p"},"requires")," clause states under which conditions a specified function has a\nwell-specified behaviour."),(0,i.kt)("p",null,"Whenever a ",(0,i.kt)("inlineCode",{parentName:"p"},"requires")," precondition is violated, its behaviour becomes\nunspecified, and the call should be considered faulty. Even if the function\nexecution terminates, any other information provided by the contract\n(postconditions, exceptions, effects, ...) cannot be assumed."),(0,i.kt)("p",null,"For example, the following function requires ",(0,i.kt)("inlineCode",{parentName:"p"},"y")," to be positive to behave correctly."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"{3}","{3}":!0},"val eucl_division: int -> int -> int * int\n(*@ q, r = eucl_division x y\n    requires y > 0\n    ensures  x = q * y + r\n    ensures  0 <= r < y *)\n")),(0,i.kt)("h3",{id:"checks"},(0,i.kt)("inlineCode",{parentName:"h3"},"checks")),(0,i.kt)("p",null,"Preconditions introduced with ",(0,i.kt)("inlineCode",{parentName:"p"},"checks")," hold at function entry. However, unlike\n",(0,i.kt)("inlineCode",{parentName:"p"},"requires")," clauses, the behaviour of the function is well-specified in case the\nprestate doesn't meet such a precondition. The function fails by raising an\nOCaml ",(0,i.kt)("inlineCode",{parentName:"p"},"Invalid_argument")," exception and doesn't modify any existing state. The\ncall is not faulty, but the caller is now in charge of handling the exception."),(0,i.kt)("p",null,"For example, if we change the function contract of ",(0,i.kt)("inlineCode",{parentName:"p"},"eucl_division"),"\nabove by replacing ",(0,i.kt)("inlineCode",{parentName:"p"},"requires")," with ",(0,i.kt)("inlineCode",{parentName:"p"},"checks"),", it now states that the\nfunction raises ",(0,i.kt)("inlineCode",{parentName:"p"},"Invalid_argument")," whenever ",(0,i.kt)("inlineCode",{parentName:"p"},"y <= 0"),"."),(0,i.kt)("admonition",{title:"Combining multiple pre-conditions",type:"note"},(0,i.kt)("p",{parentName:"admonition"},"Whenever multiple preconditions of the same kind coexist, they hold as a\nconjunction, which means"),(0,i.kt)("pre",{parentName:"admonition"},(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"invalidSyntax",invalidSyntax:!0},"(*@ ...\n    requires P\n    requires Q *)\n")),(0,i.kt)("p",{parentName:"admonition"},"is equivalent to:"),(0,i.kt)("pre",{parentName:"admonition"},(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"invalidSyntax",invalidSyntax:!0},"(*@ ...\n    requires P /\\ Q *)\n")),(0,i.kt)("p",{parentName:"admonition"},"When combining ",(0,i.kt)("inlineCode",{parentName:"p"},"checks")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"requires")," preconditions, the declaration order\ndoesn't matter. The ",(0,i.kt)("inlineCode",{parentName:"p"},"requires")," clauses take precedence and must always be\nrespected; otherwise, the ",(0,i.kt)("inlineCode",{parentName:"p"},"checks")," behaviour cannot be assumed. This means that\nultimately,"),(0,i.kt)("pre",{parentName:"admonition"},(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"invalidSyntax",invalidSyntax:!0},"(*@ ...\n    requires P\n    checks Q *)\n")),(0,i.kt)("p",{parentName:"admonition"},"is equivalent to:"),(0,i.kt)("pre",{parentName:"admonition"},(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"invalidSyntax",invalidSyntax:!0},"(*@ ...\n    requires P\n    checks P -> Q *)\n"))),(0,i.kt)("admonition",{type:"tip"},(0,i.kt)("p",{parentName:"admonition"},"Specification formulas can often be written using few clauses, but splitting the\nspecification into several smaller clauses leads to better readability and\nmaintainability and is therefore encouraged.")),(0,i.kt)("h2",{id:"postconditions"},"Postconditions"),(0,i.kt)("p",null,"Postconditions are ",(0,i.kt)("strong",{parentName:"p"},"properties that hold at the function exit"),". They are used\nto specify how the function's outputs relate to its inputs and how the call\nmutated the memory."),(0,i.kt)("admonition",{type:"caution"},(0,i.kt)("p",{parentName:"admonition"},"  ",(0,i.kt)("strong",{parentName:"p"},"When a function raises an exception, its postconditions are not expected to\nhold.")," You must use exceptional postconditions instead.")),(0,i.kt)("p",null,"Gospel introduces postconditions using the keyword ",(0,i.kt)("inlineCode",{parentName:"p"},"ensures")," followed by a\nformula."),(0,i.kt)("p",null,"As discussed in the previous section, the property expressed by the formula is\nverified after the function call only if the preconditions were satisfied."),(0,i.kt)("admonition",{title:"Combining multiple postconditions",type:"note"},(0,i.kt)("p",{parentName:"admonition"},"The handling of multiple postconditions is identical to preconditions of the\nsame kind. Multiple postconditions are merged as a conjunction:"),(0,i.kt)("pre",{parentName:"admonition"},(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"invalidSyntax",invalidSyntax:!0},"(*@ ...\n    ensures P\n    ensures Q *)\n")),(0,i.kt)("p",{parentName:"admonition"},"is equivalent to:"),(0,i.kt)("pre",{parentName:"admonition"},(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"invalidSyntax",invalidSyntax:!0},"(*@ ...\n    ensures P /\\ Q *)\n"))),(0,i.kt)("h2",{id:"exceptional-postconditions"},"Exceptional Postconditions"),(0,i.kt)("p",null,"Exceptional postconditions are used to specify the exceptions that can be\nraised by the function and what postconditions hold in those cases."),(0,i.kt)("p",null,"By default, functions should not raise any exceptions, and doing so is a\nviolation of the specification. Whenever a function can raise an exception as\npart of its expected behaviour, this exception must be listed along with the\nassociated postconditions."),(0,i.kt)("admonition",{type:"info"},(0,i.kt)("p",{parentName:"admonition"},"Some exceptions are implicitly allowed and do not have to be listed because\nthey could be unexpectedly triggered, depending on the specifics of the machine\nthe code is executed on."),(0,i.kt)("p",{parentName:"admonition"},(0,i.kt)("strong",{parentName:"p"},"The implicitly allowed exceptions are ",(0,i.kt)("inlineCode",{parentName:"strong"},"Stack_overflow")," and ",(0,i.kt)("inlineCode",{parentName:"strong"},"Out_of_memory"),".")),(0,i.kt)("p",{parentName:"admonition"},"This is equivalent to adding a ",(0,i.kt)("inlineCode",{parentName:"p"},"raises Out_of_memory | Stack_overflow -> true"),"\nclause to every function contract. Of course, you can still override that\nbehaviour by stating a property whenever these exceptions are raised, like any\nother exception:"),(0,i.kt)("pre",{parentName:"admonition"},(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"invalidSyntax",invalidSyntax:!0},"(*@ ...\n    raises Stack_overflow -> false *)\n"))),(0,i.kt)("p",null,"Exceptional clauses are expressed using a ",(0,i.kt)("inlineCode",{parentName:"p"},"raises")," keyword, followed by a list\nof cases associating each exception with its formula. Those clauses use a syntax similar to\npattern-matching."),(0,i.kt)("p",null,"Gospel expects each ",(0,i.kt)("inlineCode",{parentName:"p"},"raises")," clause to perform an exhaustive pattern-matching\nfor each exception constructor listed in this clause. Similar to OCaml's\npattern-matching, when an exception is raised, the postcondition that's\nsatisfied is the first match in the list of cases."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"invalidSyntax",invalidSyntax:!0},"(*@ ...\n    raises Unix_error (ENAMETOOLONG, _, _) -> P\n         | Unix_error _                    -> Q *)\n")),(0,i.kt)("p",null,"The previous contract (notice that it's an exhaustive pattern-matching on\nthe ",(0,i.kt)("inlineCode",{parentName:"p"},"Unix_error")," exception) only states that ",(0,i.kt)("inlineCode",{parentName:"p"},"P")," holds whenever ",(0,i.kt)("inlineCode",{parentName:"p"},"Unix_error")," is\nraised with argument ",(0,i.kt)("inlineCode",{parentName:"p"},"ENAMETOOLONG"),", and ",(0,i.kt)("inlineCode",{parentName:"p"},"Q")," holds whenever the function\nraises ",(0,i.kt)("inlineCode",{parentName:"p"},"Unix_error")," with a different argument. (",(0,i.kt)("inlineCode",{parentName:"p"},"P")," doesn't necessarily hold in\nthis case.)"),(0,i.kt)("admonition",{title:"Combining multiple exceptional post-conditions",type:"note"},(0,i.kt)("p",{parentName:"admonition"},"When multiple exceptional postconditions exist, they hold independently of\neach other, meaning that the raised exception is matched against each ",(0,i.kt)("inlineCode",{parentName:"p"},"raises"),"'s\ncase list, and each matching post-condition must hold in conjunction. For\ninstance, the contract:"),(0,i.kt)("pre",{parentName:"admonition"},(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"invalidSyntax",invalidSyntax:!0},'(*@ ...\n    raises Error "foo" -> P | Error _ -> Q\n    raises Error x -> R *)\n')),(0,i.kt)("p",{parentName:"admonition"},"implies that"),(0,i.kt)("ul",{parentName:"admonition"},(0,i.kt)("li",{parentName:"ul"},"when ",(0,i.kt)("inlineCode",{parentName:"li"},'Error "foo"')," is raised, both ",(0,i.kt)("inlineCode",{parentName:"li"},"P")," and ",(0,i.kt)("inlineCode",{parentName:"li"},"R")," hold, but not necessarily ",(0,i.kt)("inlineCode",{parentName:"li"},"Q"),";"),(0,i.kt)("li",{parentName:"ul"},"when ",(0,i.kt)("inlineCode",{parentName:"li"},"Error")," is raised with with an argument different from ",(0,i.kt)("inlineCode",{parentName:"li"},'"foo"'),", both ",(0,i.kt)("inlineCode",{parentName:"li"},"Q"),"\nand ",(0,i.kt)("inlineCode",{parentName:"li"},"R")," hold, but not necessarily ",(0,i.kt)("inlineCode",{parentName:"li"},"P"),"."))),(0,i.kt)("h2",{id:"code-equivalence"},"Code Equivalence"),(0,i.kt)("p",null,"Complementary to other specification clauses, Gospel allows you to talk about\n",(0,i.kt)("em",{parentName:"p"},"code equivalence")," in the function contract. Put it in a string containing\nthe OCaml code that behaves like the function, preceded by the ",(0,i.kt)("inlineCode",{parentName:"p"},"equivalent")," keyword."),(0,i.kt)("p",null,"This is useful when specifying functions whose behaviour can hardly be expressed\nin pure logic:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"invalidSyntax",invalidSyntax:!0},"type 'a t\nval iter : ('a -> unit) -> 'a t -> unit\n(*@ iter f t\n    ...\n    equivalent \"List.iter f (to_list t)\" *)\n")),(0,i.kt)("p",null,"With such a specification, no logical assertion is provided, but applying ",(0,i.kt)("inlineCode",{parentName:"p"},"iter"),"\nto ",(0,i.kt)("inlineCode",{parentName:"p"},"f")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"t")," is equivalent to applying ",(0,i.kt)("inlineCode",{parentName:"p"},"List.iter")," to ",(0,i.kt)("inlineCode",{parentName:"p"},"f")," and the conversion\nof ",(0,i.kt)("inlineCode",{parentName:"p"},"t")," to a list. This doesn't leak implementation details, as ",(0,i.kt)("inlineCode",{parentName:"p"},"iter")," might in\nfact be implemented in a different, more efficient way. It does however make the\nspecification concise and elegant."),(0,i.kt)("admonition",{type:"danger"},(0,i.kt)("p",{parentName:"admonition"},"At the moment, the Gospel type-checker does ",(0,i.kt)("strong",{parentName:"p"},"not")," type-check the code provided\ninside the ",(0,i.kt)("inlineCode",{parentName:"p"},"equivalent")," clauses and will take it as-is.")),(0,i.kt)("h2",{id:"non-termination"},"Non-Termination"),(0,i.kt)("p",null,"By default, OCaml functions with an attached contract implicitly terminate."),(0,i.kt)("p",null,"If a function is allowed to not terminate (e.g., a server main loop, a function\nwaiting for a signal or event, etc.), one can add this information to the\ncontract using the ",(0,i.kt)("inlineCode",{parentName:"p"},"diverges")," keyword."),(0,i.kt)("p",null,"The following example states that the execution of the function ",(0,i.kt)("inlineCode",{parentName:"p"},"run")," may not\nterminate. It doesn't specify whether this function is always non-terminating\nor not."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"invalidSyntax",invalidSyntax:!0},"val run : unit -> unit\n(*@ run ()\n    diverges *)\n")),(0,i.kt)("h2",{id:"data-mutability"},"Data Mutability"),(0,i.kt)("p",null,"In the default specification, functions don't mutate any observable data. If\nyour function mutates an argument or some global state, you may specify it using\nthe keyword ",(0,i.kt)("inlineCode",{parentName:"p"},"modifies"),", followed by an identifier. In the following, the\n",(0,i.kt)("inlineCode",{parentName:"p"},"contents")," model of ",(0,i.kt)("inlineCode",{parentName:"p"},"a")," can be modified by ",(0,i.kt)("inlineCode",{parentName:"p"},"inplace_map"),"."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"{3}","{3}":!0},"type 'a t\n(*@ mutable model contents: int *)\n\nval inplace_map : ('a -> 'a) -> 'a t -> unit\n(*@ inplace_map f a\n    modifies a.contents *)\n")),(0,i.kt)("p",null,"If the function only modifies a few models of a value, these may be explicitly\nadded to the clause."),(0,i.kt)("p",null,"If a specific model is not mentioned, the whole data structure and its mutable\nmodels are potentially mutated."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"{3}","{3}":!0},"val inplace_map : ('a -> 'a) -> 'a t -> unit\n(*@ inplace_map f a\n    modifies a *)\n")),(0,i.kt)("p",null,"In this example, all the mutable models of ",(0,i.kt)("inlineCode",{parentName:"p"},"a")," can be mutated by ",(0,i.kt)("inlineCode",{parentName:"p"},"inplace_map"),"."),(0,i.kt)("admonition",{type:"note"},(0,i.kt)("p",{parentName:"admonition"},"When a ",(0,i.kt)("inlineCode",{parentName:"p"},"modifies")," clause is present, it affects all the declared postconditions\nand exceptional postconditions, meaning that the function may mutate data even\nin the case of exceptional postconditions."),(0,i.kt)("p",{parentName:"admonition"},"If your data was not mutated in an exceptional poststate, for instance if the\nfunction raised an exception ",(0,i.kt)("strong",{parentName:"p"},"instead")," of mutating the data, you have to\nmanually specify it:"),(0,i.kt)("pre",{parentName:"admonition"},(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"{4}","{4}":!0},"exception E\nval inplace_map : ('a -> 'a) -> 'a t -> unit\n(*@ inplace_map f a\n    modifies a.contents\n    raises E -> a.contents = old (a.contents) *)\n"))),(0,i.kt)("h2",{id:"pure-functions"},"Pure Functions"),(0,i.kt)("p",null,"An OCaml function can be declared as ",(0,i.kt)("inlineCode",{parentName:"p"},"pure"),", which means it"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"has no side effect;"),(0,i.kt)("li",{parentName:"ul"},"raises no exception;"),(0,i.kt)("li",{parentName:"ul"},"terminates.")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"{2}","{2}":!0},"val length : 'a t -> int\n(*@ pure *)\n")),(0,i.kt)("p",null,"Pure functions can be used in further Gospel specifications.\nOn the contrary, OCaml functions not declared as ",(0,i.kt)("inlineCode",{parentName:"p"},"pure")," cannot be used\nin specifications."),(0,i.kt)("h2",{id:"data-consumption"},"Data Consumption"),(0,i.kt)("p",null,"Gospel provides a specific syntax to specify that some data has been consumed by\nthe function, and should be considered dirty (that is, not used anymore) in\nthe rest of the program. This is expressed with the ",(0,i.kt)("inlineCode",{parentName:"p"},"consumes"),"\nkeyword:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"invalidSyntax",invalidSyntax:!0},"val destructive_transfer: 'a t -> 'a t -> unit\n(*@ destructive_transfer src dst\n    consumes src\n    ... *)\n")),(0,i.kt)("h2",{id:"ghost-parameters"},"Ghost Parameters"),(0,i.kt)("p",null,"Functions can take or return ghost values to ease the writing of function\ncontracts. Such values appear within brackets in the contract header."),(0,i.kt)("p",null,"Consider the following ",(0,i.kt)("inlineCode",{parentName:"p"},"log2")," function:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ocaml"},"val log2: int -> int\n(*@ r = log2 [i: integer] x\n    requires i >= 0\n    requires x = pow 2 i\n    ensures r = i *)\n")),(0,i.kt)("p",null,"In this contract, the ghost parameter ",(0,i.kt)("inlineCode",{parentName:"p"},"i")," is used in both the preconditions and\npostconditions. By introducing it as a ghost value, we avoid using quantifiers\nto state the existence of ",(0,i.kt)("inlineCode",{parentName:"p"},"i"),"."),(0,i.kt)("admonition",{type:"note"},(0,i.kt)("p",{parentName:"admonition"},"Since the type of ghost parameters does not appear in the OCaml signature, it\nmust be given explicitly.")))}d.isMDXComponent=!0}}]);