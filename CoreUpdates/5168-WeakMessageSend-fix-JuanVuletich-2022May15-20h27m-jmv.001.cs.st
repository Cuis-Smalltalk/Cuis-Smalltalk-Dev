'From Cuis 6.0 [latest update: #5166] on 15 May 2022 at 8:34:54 pm'!

!WeakMessageSend methodsFor: 'private' stamp: 'jmv 5/15/2022 20:32:23'!
collectArguments: anArgArray
	"Private"
    | staticArgs |
    staticArgs := self arguments.
    ^(anArgArray size = staticArgs size)
        ifTrue: [anArgArray]
        ifFalse:
            [(staticArgs isEmpty
                ifTrue: [ staticArgs := Array new: selector numArgs]
                ifFalse: [staticArgs asNewArray] )
                    replaceFrom: 1
                    to: (anArgArray size min: staticArgs size)
                    with: anArgArray
                    startingAt: 1]
! !

