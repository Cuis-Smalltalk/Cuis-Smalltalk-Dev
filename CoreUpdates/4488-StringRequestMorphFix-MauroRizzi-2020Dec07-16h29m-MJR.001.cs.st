'From Cuis 5.0 [latest update: #4384] on 7 December 2020 at 4:30:41 pm'!

!StringRequestMorph methodsFor: 'private' stamp: 'MJR 12/7/2020 16:29:34'!
textBox
	
	^submorphs at: 2! !


!StringRequestMorph methodsFor: 'private' stamp: 'MJR 12/7/2020 16:30:05'!
getUserResponseOrCancel: aBlock
	"Wait for the user to accept or cancel, and answer the result string. Answers the empty string if the user cancels."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop."
	| w delay done canceled |
	w _ self world.
	w isNil ifTrue: [^ response asString].
	done _ false.
	canceled _ false.
	Preferences focusFollowsMouse ifFalse: [self textBox focusText].
	acceptBlock _ [:aString| done _ true].
	cancelBlock _ [done _ true. canceled _ true].
	delay _ Delay forMilliseconds: 10.
	[done not and: [self isInWorld]] whileTrue: [ w doOneMinimalCycleNow. delay wait ].
	self delete.
	w doOneMinimalCycleNow.
	canceled ifTrue: [^ aBlock value].
	^ response asString! !

