'From Cuis 5.0 [latest update: #3864] on 5 September 2019 at 7:58:35 pm'!

!MessageNode methodsFor: 'private' stamp: 'jmv 6/13/2019 08:52:31'!
pvtCheckForPvtSelector: encoder
	"If the code being compiled is trying to send a private message (e.g. 'pvtCheckForPvtSelector:') to anyone other than self, then complain to encoder."

	selector isInitializePvtSelector ifTrue: [
		(receiver isSelfNewMessageSend or: [receiver isSelfBasicNewMessageSend]) ifFalse: [
			encoder notify: 'Private instance initialization messages may only be sent to ''self new'' or "self basicNew" (by class instance creation methods)']].
	selector isPvtSelector ifTrue: [
		(receiver isSelfPseudoVariable or: [ receiver isSuperPseudoVariable ]) ifFalse: [
			encoder notify: 'Private messages may only be sent to self or super']].! !

