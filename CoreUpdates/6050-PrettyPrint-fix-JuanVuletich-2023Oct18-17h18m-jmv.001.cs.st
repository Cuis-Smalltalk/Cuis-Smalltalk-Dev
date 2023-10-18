'From Cuis 6.0 [latest update: #6049] on 18 October 2023 at 5:20:54 pm'!

!ParseNode methodsFor: 'accessing' stamp: 'jmv 10/18/2023 17:15:04'!
addComment: newComment 
	"self halt."

	comment := comment isNil
		ifTrue: [newComment]
		ifFalse: [comment, newComment]! !


!Parser methodsFor: 'private' stamp: 'jmv 10/18/2023 17:20:13'!
addComment
	(parseNode notNil and: [ currentComment notNil ])
		ifTrue: [
			parseNode addComment: currentComment.
			currentComment := nil ]! !

