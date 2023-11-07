# Isabelle-CAS-Integration

Developed from the dissertations of Thomas Hickman and Christian Pardillo Laursen.

## Steps for installation
 - Install [Isabelle 2022-1](https://isabelle.in.tum.de).

 - Add the [Ordinary_Differential_Equations](https://www.isa-afp.org/entries/Ordinary_Differential_Equations.html) entry to your Isabelle ROOTS - follow [these](https://www.isa-afp.org/using.html) instructions.

 - Download and activate the [Wolfram Engine](https://www.wolfram.com/engine/). WolframScript is installed with it, and is called from bash.

 OR

 - Install [SageMath](https://www.sagemath.org/download.html) and optionally [FriCAS](http://fricas.sourceforge.net/).

 - Add the file `config.sml` configures the path of the file `sage-integration/ConvertToIsabelle.py`. An example of this file is found in `config-example.sml`.

 - Finally, launch Isabelle/jEdit with the ODE heap image:
``isabelle jedit -d $PATH_TO_AFP/thys -l Ordinary_Differential_Equations``,

## Usage

Examples can be found in the two test sets: `Keymaera_tests.thy` and `TestSet.thy`.

## Contents:

### Arithmetic Expressions for integration with CAS
 - Arith_Expr.ML: We introduce the AExp type, intended to represent arithmetic expressions generically in order to provide an intermediate representation that can be easily converted from and to Isabellle and CAS.

 - Subst_ODE.ML: Mapping between ODEs as substitutions and AExps.
 - Tupled_ODE.ML: Mapping between ODEs as tuples and AExps.

### CAS integrations
 - Sage integration: ConvertToSage.ML implements desolve, taking an AExp SODE and returning a tuple (solution, domain).
 - Wolfram integration: see README
