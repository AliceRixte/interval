# Intervals

This is a work in progress.

Interval with open or closed bounds.

```haskell
data IntervalBound a = OpenBound !a | ClosedBound !a

data Interval a =
    EmptyInterval
  | Interval (IntervalBound a) (IntervalBound a)
```

Combined with the [Infinity](https://github.com/AliceRixte/infinity-class) class , this allows to contain any kind of interval.


I abandonned this to use Kmett's [intervals](https://hackage.haskell.org/package/intervals) package instead, which only deals with closed intervals.

The package [data-interval](https://hackage.haskell.org/package/data-interval) already deals with open bounds and lower bounds with a more complicated type :

```haskell
data Interval r
  = Whole
  | Empty
  | Point !r
  | LessThan !r
  | LessOrEqual !r
  | GreaterThan !r
  | GreaterOrEqual !r
  -- For constructors below
  -- the first argument is strictly less than the second one
  | BothClosed !r !r
  | LeftOpen !r !r
  | RightOpen !r !r
  | BothOpen !r !r
```
