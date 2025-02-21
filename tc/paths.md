# Version 1: Maybe Types

There are essentially two ways we can go about building our typechecker, version 1 (this version) makes use of Maybe Types. Suspensions return Nothing which has to be handled on a case by case basis.

For example:
- T-If Nothings get filtered out and the only branches we care about are the ones which actually return a type
- T-Let: would have the Nothings propagate and the whole expression would evaluate to Nothing
- T-App: let's ay we have the function \x.x+5 and we want to apply this function to some computation M which suspends:
  (\x.x+5) (suspend V)
  then the type of the function would be A -> Nothing. which is weird. Clearly argument x should be of type Int, but we instead pass in Nothing. I guess we can allow the Nothing to pass in, since it will ultimately just halt execution?
- T-Return: is typeable jsut fine
- T-Spawn is weird. Let's say that we have spawn M where M = suspend V. So really we have spawn (suspend V). So long as the session pre- and postconditions are `end` then this will have type unit. We don't yet care about the type of M because it won't be evaluated until much later. Now, how do we typecheck this? well.. unclear
- T-Suspend is the whole reason this is a pain in my ass, but essentially, if the precondition and handler session type match, then this will typecheck and return Nothing
- T-Register and T-NewAP remain unaffected


# Version 2: Constraints

The second version is to use constraints. Essentially as we run through our rules and infer types we also keep "a bag" of constraints as they appear. At the end of our inference, we solve / unify our constraints which will either tell us the final type of our expression / value, or will tell us that we have a type error in our program.

This means we don't need to worry about Nothing types as we would just have additional constraint rules for what expression can go where.
