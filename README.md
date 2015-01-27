# stream-paper

- Intro
- Problem w/ functional representation
- Solution
  - `M (M a)`
  - Functional Library
- Optimization
  - `M (Int -> M a)`
- Evaluation
  - Turbo?
  - Filter?
- Feldspar / DSL
- Related Work
  - FRP
  - conduits
  - pipes
  - machines
- Future Work
  - switch is hard
  - Pull / Push

```haskell
loop  :: Pull1 o -> (i -> Pull1 o -> o) -> Seq i -> Seq o

delay :: Pull1 a -> Seq a -> Seq [a]
```
