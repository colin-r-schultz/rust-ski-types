#[macro_use]
extern crate type_operators;

// defines SKI combinators and Run trait which runs the program
type_operators! {
    [A, B]

    data Bool {
        T,
        F,
    }

    concrete Combinator => String {
        S => String::from("S"),
        K => String::from("K"),
        I => String::from("I"),
        App(L: Combinator, R: Combinator) => format!("({} {})", L, R),
    }

    (NextState) Step(Combinator): Combinator {
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

    (CanReduce) Reducible(Combinator): Bool {
        [S] => F
        [K] => F
        [I] => F

        forall (X: Combinator) {
            [(App S X)] => F
            [(App K X)] => F
            [(App I X)] => T
        }

        forall (X: Combinator, Y: Combinator) {
            [(App (App S X) Y)] => F
            [(App (App K X) Y)] => T
            [(App (App I X) Y)] => T
        }

        forall (W: Combinator, X: Combinator, Y: Combinator, Z: Combinator) {
            [(App (App (App W X) Y) Z)] => T
        }
    }

    (FinalState) BigStep(Combinator, Bool): Combinator {
        forall (X: Combinator) {
            [X, F] => X
            [X, T] => (# (@Step X) (@Reducible X))
        }
    }

    (Run) Running(Combinator): Combinator {
        forall (X: Combinator) {
            [X] => (@BigStep X (@Reducible X))
        }
    }

}

// macro to produce SKI program type from string form
macro_rules! ski {
    (($l:tt $r:tt $($rest:tt) +)) => {
        ski!{(($l $r) $($rest )*)}
    };
    (($l:tt $r:tt)) => {
        App<ski!{$l},ski!{$r}>
    };
    (($c:tt)) => {
        ski!{$c}
    };
    ($c:ident) => {
        $c
    };
    ($($str:tt) +) => {
        ski!{($($str )*)}
    }
}

// function to remove redundant parentheses from combinator string
fn pretty_print<C: Combinator>() -> String {
    let repr = C::reify();
    let mut pretty = String::new();
    let mut depth = 0;
    let mut traversing_left = true;
    for c in repr.chars() {
        if traversing_left && c == '(' {
            depth += 1;
        } else if depth > 0 && c == ')' {
            depth -= 1;
        } else {
            pretty.push(c);
        }
        traversing_left = c == '(';
    }
    pretty
}

fn main() {
    // Examples from https://en.wikipedia.org/wiki/SKI_combinator_calculus#Examples_of_reduction
    println!("{}", pretty_print::<Run<ski! {S K I (K I S)}>>());
    println!("{}", pretty_print::<Run<ski! {K S (I (S K S I))}>>());
    println!("{}", pretty_print::<Run<ski! {S K I K}>>());
    //println!("{}", pretty_print::<Run<ski!{(S I I) (S I I)}>>()); // Does not compile! Infinite loop!
}
