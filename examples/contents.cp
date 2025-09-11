--| Pass

open doc;

type TOC = { toc : String };

type ContentsSig<Element> = DocSig'<Element> & {
  WithTOC : Element -> Element;
};

contents = trait implements ContentsSig<TOC => HTML> inherits html' => {
  [self]@(WithTOC e).html = self.toc ++ e.html;
};

listItem (l:Int) (s:String) : String =
  if l == 0 then "<li>" ++ s ++ "</li>"
  else "<li style=\"list-style-type:none\"><ul>" ++ listItem (l-1) s ++ "</ul></li>";

toc = trait implements ContentsSig<HTML => TOC> => {
  (Comp l r).toc = l.toc ++ r.toc;
  (Section e).toc = listItem 0 e.html;
  (SubSection e).toc = listItem 1 e.html;
  (SubSubSection e).toc = listItem 2 e.html;
  (WithTOC e).toc = "<ul id=\"toc\">" ++ e.toc ++ "</ul>\n";
  _.toc = "";
};

doc T = trait [self : ContentsSig<T>] => {
  body = open self in `\WithTOC[
    \Section[Welcome to \Emph[PLGround]!]
      \Href("https://plground.org")[PLGround] provides a wiki-like document repository. \\
      Documents are written in ExT and rendered with an in-browser interpreter.
    \Section[What is \Emph[ExT]?]
      \SubSection[Overview]
        ExT is a DSL embedded in CP (a new compositional programming language).
      \SubSection[Key Concepts of Compositional Programming]
        \Enumerate[
          \Item[\Bold[Compositional interfaces] extend usual OOP interfaces to allow the specification of the signatures of constructors, and can be parametrized by sorts (which abstract over concrete datatypes).]
          \Item[\Bold[Compositional traits] extend first-class traits to allow not only the definition of virtual methods but also the definition of virtual constructors.]
          \Item[\Bold[Method patterns] provide a lightweight syntax to define implementations for nested traits, which arise from virtual constructors.]
          \Item[Finally, a powerful form of \Bold[nested trait composition] is used to compose compositional traits. Nested trait composition plays a similar role to traditional class inheritance, but it generalizes to the composition of nested traits. Thus it enables a form of inheritance of whole hierarchies, similar to the forms of composition found in family polymorphism.]
        ]
    \Section[What is more...]
      Please refer to our paper...
  ]`
};

(new doc @(HTML & TOC) , contents , toc).body.html
