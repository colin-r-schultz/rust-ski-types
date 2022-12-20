use std::any::type_name;
#[macro_use]
extern crate type_operators;
/* 
type_operators! {
    [A, B, C, D, E]

    data Combinator {
        S,
        K,
        I,
        App(Combinator, Combinator),
    }

    data StepResult {
        Change(Combinator),
        NoChange(Combinator),
    }

    (Unwrapped) Unwrap(StepResult): Combinator {
        forall (X: Combinator) {
            [(Change X)] => X
            [(NoChange X)] => X
        }
    }

    (Step) Stepping(Combinator): StepResult {
        [S] => (NoChange S)
        [K] => (NoChange K)
        [I] => (NoChange I)

        forall (X: Combinator) {
            [(App S X)] => (NoChange (App S X))
            [(App K X)] => (NoChange (App K X))
            [(App I X)] => (Change X)
        }

        forall (X: Combinator, Y: Combinator) {
            [(App (App S X) Y)] => (NoChange (App (App S X) Y))
            [(App (App K X) Y)] => (Change X)
            [(App (App I X) Y)] => (Change (App X Y))
        }

        forall (X: Combinator, Y: Combinator, Z: Combinator) {
            [(App (App (App S X) Y) Z)] => (Change (App (App X Z) (App Y Z)))
            [(App (App (App I X) Y) Z)] => (Change (App (App X Y) Z))
            [(App (App (App K X) Y) Z)] => (Change (App X Z))
        }

        forall (V: Combinator, W: Combinator, X: Combinator, Y: Combinator, Z: Combinator) {
            [(App (App (App (App V W) X) Y) Z)] => (Change (App (@Unwrap (# (App (App (App V W) X) Y))) Z))
        }
    }

    // (BigStep) BigStepping(StepResult): Combinator {
    //     forall (X: Combinator) {
    //         [(NoChange X)] => X
    //         [(Change X)] => (# (@Stepping X))
    //     }
    // }

    // (Run) Running(Combinator): Combinator {
    //     forall (X: Combinator) {
    //         [X] => (@BigStepping (Change X))
    //     }
    // }

}
// */

type_operators! {
    [A, B, C, D, E]

    data Combinator {
        S,
        K,
        I,
        App(Combinator, Combinator),
    }

    data Change {
        Yes,
        No,
    }

    (Step) Stepping(Combinator): Combinator {
        [S] => S
        [K] => K
        [I] => I

        forall (X: Combinator) {
            [(App S X)] => (App S X)
            [(App K X)] => (App K X)
            [(App I X)] => X
        }

        forall (X: Combinator, Y: Combinator) {
            [(App (App S X) Y)] => (App (App S X) Y)
            [(App (App K X) Y)] => X
            [(App (App I X) Y)] => (App X Y)
        }

        forall (X: Combinator, Y: Combinator, Z: Combinator) {
            [(App (App (App S X) Y) Z)] => (App (App X Z) (App Y Z))
            [(App (App (App I X) Y) Z)] => (App (App X Y) Z)
            [(App (App (App K X) Y) Z)] => (App X Z)
        }

        forall (V: Combinator, W: Combinator, X: Combinator, Y: Combinator, Z: Combinator) {
            [(App (App (App (App V W) X) Y) Z)] => (App (# (App (App (App V W) X) Y)) Z)
        }
    }

    (Reducible) WillChange(Combinator): Change {
        [S] => No
        [K] => No
        [I] => No

        forall (X: Combinator) {
            [(App S X)] => No
            [(App K X)] => No
            [(App I X)] => Yes
        }

        forall (X: Combinator, Y: Combinator) {
            [(App (App S X) Y)] => No
            [(App (App K X) Y)] => Yes
            [(App (App I X) Y)] => Yes
        }

        forall (W: Combinator, X: Combinator, Y: Combinator, Z: Combinator) {
            [(App (App (App W X) Y) Z)] => Yes
        }
    }

    (BigStep) BigStepping(Combinator, Change): Combinator {
        forall (X: Combinator) {
            [X, No] => X
            [X, Yes] => (# (@Stepping X) (@WillChange X))
        }
    }

    (Run) Running(Combinator): Combinator {
        forall (X: Combinator) {
            [X] => (@BigStepping X Yes)
        }
    }

}

type Zero = App<S, K>;
type Succ = App<S, App<App<S, App<K, S>>, K>>;
type Repeat = App<App<S, I>, I>;

fn main() {
    println!(
        "{}",
        //type_name::<Reduce<Node<Node<Node<Leaf, Leaf>, Node<Leaf, Leaf>>, Leaf>>>()
        type_name::<Run<App<Repeat, Repeat>>>()
    )
}
