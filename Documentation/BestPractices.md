# Best Practices in Cuis Smalltalk

This document is supposed to shine some light on certain
practices within Cuis which are supposed to be followed
for various reasons. It tries to help to understand some
not so obvious design decisions without linking to the
cuis-dev mailinglist.

# Message Conventions

This section deals with message sending, the format
of certain messages and what message is supposed to be
used when.

## Replace `#respondsTo:` with `#is:`

The message `#respondsTo:` which is used in code like

```smalltalk
  | foo |
  foo := Object new.
  (foo respondsTo: #saveAsModel)
    ifTrue: [ foo saveAsmodel ].
```

should be replaced by `#is:` which is derived from messages
like `#isBag`, `#isDictionary`, `#isForm`, etc. A changed
example would then look like this:

```smalltalk
  | foo |
  foo := Object new.
  (foo is: #saveableAsModel)
    ifTrue: [ foo saveAsmodel ].
```

The reasoning for this is because:

1. `#is:` looks like the forms `#isBag`, `#isDictionary`, etc.
   which test for the type of the receiving object. 
   
2. `#respondsTo:` and the `#isBag` are of the same form,
   they both receive a symbol and return true or false if
   the object conforms in someway to the asked question
   in the argument. So they can be unified into `#is:`
   from that perspective.
   
3. `#isBag`, etc. is normally used in the form that if
   an object responds to `#isBag` then it it's probably
   a bag and then it can be treated as such one.
   So before `isBag` can be called, `respondsTo` needs
   to be called. Objects used in this way are not uniform
   anymore, so instead of `((obj respondsTo: #isFoo) and: [ obj isFoo]) ifTrue: [obj foo]`
   you should just write `obj is: #fooish ifTrue: [obj foo]`
   
4. `#respondsTo:` goes up the class hierarchy when doing lookups,
   it looks into the selectors of the object, then goes up the class
   hierarchy until a result is found. By using `#is:` the check
   can be faster as a series of if expressions can be used earlier.
   
5. `#is:` is more flexible. You can use it to ask arbitrary questions
   to the object. `obj is: Class. obj is: #Bag. obj is: 1.` and each
   object can decide of how it handles this based on expectation.
   `#respondsTo:` is reserved more for testing if an object can handle
   a specific message.
