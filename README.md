# CP - The Next Generation

CP is a *compositional programming* language, founded on a core calculus named *Fi+*.

A crash course in CP can be found in Chapter 3 of Yaozhu Sun's PhD thesis: [*Compositional Programming in Action*](https://github.com/yzyzsun/PhD-thesis/blob/main/Thesis.pdf).

## Language Features

- A typed lambda calculus with five base types (`Int` `Double` `String` `Bool` `()`), built-in arrays (`[1; 2; 3] : [Int]`), and references[^Ref];
- The merge operator[^Merge], disjoint intersection types[^λi] and disjoint polymorphism[^Fi];
- Distributive intersection subtyping and nested composition[^NeColus];
- Compositional programming[^CP] with first-class traits[^SEDEL];
- Iso-recursive types with nominal unfolding[^IsoRec];
- Generalized record operations with type difference[^TypeDiff];
- Named and optional arguments via a blend of intersection and union types[^IU];
- Type-directed operational semantics for *Fi+*[^Fi+] with {HOAS,substitution,closure}-based {big,small}-step variants;
- A compiler targeting JavaScript, which features type-safe compilation of dynamic inheritance[^Comp];
- A compositionally embedded DSL for document authoring called ExT[^ExT].

[^Merge]: Jana Dunfield. [Elaborating Intersection and Union Types](https://research.cs.queensu.ca/home/jana/papers/intcomp-jfp/Dunfield14_elaboration.pdf). *JFP 2014*.  
[^λi]: Bruno C. d. S. Oliveira, Zhiyuan Shi, and João Alpuim. [Disjoint Intersection Types](https://i.cs.hku.hk/~bruno/papers/icfp2016.pdf). *ICFP 2016*.  
[^Fi]: João Alpuim, Bruno C. d. S. Oliveira, and Zhiyuan Shi. [Disjoint Polymorphism](https://i.cs.hku.hk/~bruno/papers/ESOP2017.pdf). *ESOP 2017*.  
[^SEDEL]: Xuan Bi and Bruno C. d. S. Oliveira. [Typed First-Class Traits](https://i.cs.hku.hk/~bruno/papers/traits.pdf). *ECOOP 2018*.  
[^NeColus]: Xuan Bi, Bruno C. d. S. Oliveira, and Tom Schrijvers. [The Essence of Nested Composition](https://i.cs.hku.hk/~bruno/papers/nested.pdf). *ECOOP 2018*.  
[^CP]: Weixin Zhang, Yaozhu Sun, and Bruno C. d. S. Oliveira. [Compositional Programming](https://i.cs.hku.hk/~bruno/papers/toplas2021.pdf). *TOPLAS 2021*.  
[^Fi+]: Andong Fan, Xuejing Huang, Han Xu, Yaozhu Sun, and Bruno C. d. S. Oliveira. [Direct Foundations for Compositional Programming](https://i.cs.hku.hk/~bruno/papers/ecoop22direct_extended.pdf). *ECOOP 2022*.  
[^IsoRec]: Yaoda Zhou, Bruno C. d. S. Oliveira, and Andong Fan. [A Calculus with Recursive Types, Record Concatenation and Subtyping](https://i.cs.hku.hk/~bruno/papers/aplas22recursive.pdf). *APLAS 2022*.  
[^ExT]: Yaozhu Sun, Utkarsh Dhandhania, and Bruno C. d. S. Oliveira. [Compositional Embeddings of Domain-Specific Languages](https://i.cs.hku.hk/~bruno/papers/oopsla22extended.pdf). *OOPSLA 2022*.  
[^TypeDiff]: Han Xu, Xuejing Huang, and Bruno C. d. S. Oliveira. [Making a Type Difference](https://i.cs.hku.hk/~bruno/papers/popl23making.pdf). *POPL 2023*.  
[^Ref]: Wenjia Ye, Yaozhu Sun, and Bruno C. d. S. Oliveira. [Imperative Compositional Programming](https://i.cs.hku.hk/~bruno/papers/oopsla24_imperative.pdf). *OOPSLA 2024*.  
[^IU]: Yaozhu Sun and Bruno C. d. S. Oliveira. [Named Arguments as Intersections, Optional Arguments as Unions](https://i.cs.hku.hk/~bruno/papers/esop25named.pdf). *ESOP 2025*.  
[^Comp]: Yaozhu Sun, Xuejing Huang, and Bruno C. d. S. Oliveira. [Type-Safe Compilation of Dynamic Inheritance via Merging](https://i.cs.hku.hk/~bruno/papers/TOPLAS25.pdf). *TOPLAS 2025*.
