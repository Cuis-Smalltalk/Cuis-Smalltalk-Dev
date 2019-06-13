'From Cuis 5.0 of 7 November 2016 [latest update: #3798] on 12 June 2019 at 9:19:25 am'!

!ParseNode methodsFor: 'testing' stamp: 'jmv 6/12/2019 09:17:45'!
isSuperPseudoVariable	
	"Overridden in VariableNode."
	^false! !


!VariableNode methodsFor: 'testing' stamp: 'jmv 6/12/2019 09:17:53'!
isSuperPseudoVariable
	"Answer if this ParseNode represents the 'self' pseudo-variable."

	^ key = 'super' or: [name = '{{super}}']! !


!MessageNode methodsFor: 'private' stamp: 'jmv 6/12/2019 09:18:46'!
pvtCheckForPvtSelector: encoder
	"If the code being compiled is trying to send a private message (e.g. 'pvtCheckForPvtSelector:') to anyone other than self, then complain to encoder."

	selector isPvtSelector ifTrue: [
		(receiver isSelfPseudoVariable or: [ receiver isSuperPseudoVariable ]) ifFalse: [
			encoder notify: 'Private messages may only be sent to self or super']].! !

