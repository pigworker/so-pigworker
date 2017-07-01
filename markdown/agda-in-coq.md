Let me teach you one weird trick. It may not be the answer to all your concerns, but it might help, at least conceptually.

Let's implement addition for natural numbers, the latter being given by

    Inductive nat : Set :=
      | zero : nat
      | suc : nat -> nat.

You can try to write addition by tactics, but this happens.

    Theorem plus' : nat -> nat -> nat.
    Proof.
      induction 1.

    plus' < 2 subgoals
  
      ============================
       nat -> nat

    subgoal 2 is:
     nat -> nat

you can't see what you're doing.

The trick is to look more closely at what you're doing. We can introduce a gratuitously dependent type, cloning `nat`.

    Inductive PLUS (x y : nat) : Set :=
      | defPLUS : nat -> PLUS x y.

The idea is that `PLUS x y` is the type of "the way to compute `plus x y`". We'll need a projection, allowing us to extract the result of such a computation.

    Theorem usePLUS : forall x y, PLUS x y -> nat.
    Proof.
      induction 1.
        exact n.
    Defined.

Now we're ready to program by proving.

    Theorem mkPLUS : forall x y, PLUS x y.
    Proof.

    mkPLUS < 1 subgoal
  
      ============================
       forall x y : nat, PLUS x y

The conclusion of the goal shows us our current left-hand side and context. The analogue of `C-c C-c` in Agda is...

      induction x.

    mkPLUS < 2 subgoals
  
      ============================
       forall y : nat, PLUS zero y

    subgoal 2 is:
     forall y : nat, PLUS (suc x) y

And you can see it has done a case split! Let's knock off the base case.

        intros y.
          exact (defPLUS zero y    y).

Invoking the constructor of PLUS is like writing an equation. Imagine an `=` sign before its third argument. For the step case, we need to make a recursive call.

        intros y.
          eapply (fun h => (defPLUS (suc x) y    (suc (usePLUS x y h)))).

To make the recursive call, we invoke `usePLUS` with the arguments we want, here `x` and `y`, but we abstract over the third argument, which is the explanation of how actually to compute it. We are left with just that subgoal, effectively the termination check.

    mkPLUS < 1 subgoal
  
      x : nat
      IHx : forall y : nat, PLUS x y
      y : nat
      ============================
       PLUS x y

And now, rather than using Coq's guardedness check, you use...

            auto.

...which checks that the inductive hypotheses cover the recursive call. We're

    Defined.

We have a worker, but we need a wrapper.

    Theorem plus : nat -> nat -> nat.
    Proof.
      intros x y.
        exact (usePLUS x y (mkPLUS x y)).
    Defined.

And we're ready to go.

    Eval compute in (plus (suc (suc zero)) (suc (suc zero))).

    Coq <      = suc (suc (suc (suc zero)))
         : nat

You *have* an interactive construction tool. You *can* game it to show you the pertinent details of the problem you're solving by making types more informative. The resulting proof script...

    Theorem mkPLUS : forall x y, PLUS x y.
    Proof.
      induction x.
        intros y.
          exact             (defPLUS zero    y    y).
        intros y.
          eapply (fun h =>  (defPLUS (suc x) y    (suc (usePLUS x y h)))).
            auto.
    Defined.

...is explicit about the program it constructs. You can see that's defining addition.

If you automate this setup for program construction, then layer on an interface showing you the program you're building and the key problem-simplifying tactics, you get a funny little programming language called Epigram 1.
