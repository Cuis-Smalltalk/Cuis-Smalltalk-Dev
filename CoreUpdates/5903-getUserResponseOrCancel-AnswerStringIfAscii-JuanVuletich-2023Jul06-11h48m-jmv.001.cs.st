'From Cuis 6.0 [latest update: #5898] on 6 July 2023 at 11:51:29 am'!

!StringRequestMorph methodsFor: 'private' stamp: 'jmv 7/6/2023 11:51:15'!
getUserResponseOrCancel: aBlock
	"Wait for the user to accept or cancel, and answer the result string. Answers the empty string if the user cancels."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop."
	| w delay done canceled |
	w := self world.
	w isNil ifTrue: [^ response asString asByteStringIfAscii ].
	done := false.
	canceled := false.
	(Preferences at: #focusFollowsMouse) ifFalse: [self textBox focusText].
	acceptBlock := [:aString| done := true].
	cancelBlock := [done := true. canceled := true].
	delay := Delay forMilliseconds: 10.
	[done not and: [self isInWorld]] whileTrue: [ w doOneMinimalCycleNow. delay wait ].
	self delete.
	w doOneMinimalCycleNow.
	canceled ifTrue: [^ aBlock value].
	^ response asString asByteStringIfAscii.! !

