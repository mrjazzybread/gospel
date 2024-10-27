"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[888],{5680:(e,n,r)=>{r.d(n,{xA:()=>c,yg:()=>y});var t=r(6540);function a(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}function o(e,n){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(e);n&&(t=t.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),r.push.apply(r,t)}return r}function i(e){for(var n=1;n<arguments.length;n++){var r=null!=arguments[n]?arguments[n]:{};n%2?o(Object(r),!0).forEach((function(n){a(e,n,r[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):o(Object(r)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(r,n))}))}return e}function l(e,n){if(null==e)return{};var r,t,a=function(e,n){if(null==e)return{};var r,t,a={},o=Object.keys(e);for(t=0;t<o.length;t++)r=o[t],n.indexOf(r)>=0||(a[r]=e[r]);return a}(e,n);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(t=0;t<o.length;t++)r=o[t],n.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(a[r]=e[r])}return a}var s=t.createContext({}),p=function(e){var n=t.useContext(s),r=n;return e&&(r="function"==typeof e?e(n):i(i({},n),e)),r},c=function(e){var n=p(e.components);return t.createElement(s.Provider,{value:n},e.children)},u="mdxType",m={inlineCode:"code",wrapper:function(e){var n=e.children;return t.createElement(t.Fragment,{},n)}},g=t.forwardRef((function(e,n){var r=e.components,a=e.mdxType,o=e.originalType,s=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),u=p(r),g=a,y=u["".concat(s,".").concat(g)]||u[g]||m[g]||o;return r?t.createElement(y,i(i({ref:n},c),{},{components:r})):t.createElement(y,i({ref:n},c))}));function y(e,n){var r=arguments,a=n&&n.mdxType;if("string"==typeof e||a){var o=r.length,i=new Array(o);i[0]=g;var l={};for(var s in n)hasOwnProperty.call(n,s)&&(l[s]=n[s]);l.originalType=e,l[u]="string"==typeof e?e:a,i[1]=l;for(var p=2;p<o;p++)i[p]=r[p];return t.createElement.apply(null,i)}return t.createElement.apply(null,r)}g.displayName="MDXCreateElement"},244:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>s,contentTitle:()=>i,default:()=>m,frontMatter:()=>o,metadata:()=>l,toc:()=>p});var t=r(8168),a=(r(6540),r(5680));const o={sidebar_position:2},i="Lexical Conventions",l={unversionedId:"language/lexical-conventions",id:"language/lexical-conventions",title:"Lexical Conventions",description:"Gospel borrows most of [OCaml's lexical",source:"@site/docs/language/lexical-conventions.md",sourceDirName:"language",slug:"/language/lexical-conventions",permalink:"/gospel/language/lexical-conventions",draft:!1,tags:[],version:"current",sidebarPosition:2,frontMatter:{sidebar_position:2},sidebar:"tutorialSidebar",previous:{title:"Gospel Special Comment Syntax",permalink:"/gospel/language/syntax"},next:{title:"Expressions",permalink:"/gospel/language/expressions"}},s={},p=[],c={toc:p},u="wrapper";function m(e){let{components:n,...r}=e;return(0,a.yg)(u,(0,t.A)({},c,r,{components:n,mdxType:"MDXLayout"}),(0,a.yg)("h1",{id:"lexical-conventions"},"Lexical Conventions"),(0,a.yg)("p",null,"Gospel borrows most of ",(0,a.yg)("a",{parentName:"p",href:"https://caml.inria.fr/pub/docs/manual-ocaml/lex.html"},"OCaml's lexical\nconventions"),".\nThis means that the whitespace rules are the same. The same kind\nof variable names are allowed, integers are written the same way,\netc. There are however a number of exceptions:"),(0,a.yg)("ul",null,(0,a.yg)("li",{parentName:"ul"},(0,a.yg)("p",{parentName:"li"},"There are extra reserved keywords:"),(0,a.yg)("pre",{parentName:"li"},(0,a.yg)("code",{parentName:"pre"},"axiom        checks   coercion   consumes    diverges   ensures    ephemeral\nequivalent   exists   forall     invariant   model      modifies   old\npredicate    pure     raises     requires    variant\n"))),(0,a.yg)("li",{parentName:"ul"},(0,a.yg)("p",{parentName:"li"},"There are reserved symbols:"),(0,a.yg)("pre",{parentName:"li"},(0,a.yg)("code",{parentName:"pre"},"<->    /\\    \\/\n"))),(0,a.yg)("li",{parentName:"ul"},(0,a.yg)("p",{parentName:"li"},"There is an extra literal modifier for literals of type ",(0,a.yg)("inlineCode",{parentName:"p"},"int"),". Unmodified\nliterals (e.g. ",(0,a.yg)("inlineCode",{parentName:"p"},"42"),") are of type ",(0,a.yg)("inlineCode",{parentName:"p"},"integer"),", but Gospel adds a ",(0,a.yg)("inlineCode",{parentName:"p"},"i")," modifier to\nwrite literals of type ",(0,a.yg)("inlineCode",{parentName:"p"},"int")," (e.g. ",(0,a.yg)("inlineCode",{parentName:"p"},"42i"),")."))))}m.isMDXComponent=!0}}]);