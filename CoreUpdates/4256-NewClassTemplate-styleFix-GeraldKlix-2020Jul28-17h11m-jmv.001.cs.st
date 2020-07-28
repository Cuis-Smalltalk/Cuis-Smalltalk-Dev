'From Cuis 5.0 [latest update: #4255] on 28 July 2020 at 5:12:12 pm'!

!Browser methodsFor: 'shout styling' stamp: 'KLG 7/27/2020 19:00:02'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	| type |
	
	self isModeStyleable ifFalse: [^false].
	type _ self editSelection.
	(#(newMessage editMessage editClass newClass) includes: type) ifFalse:[^false].
	anSHTextStyler classOrMetaClass: ((#(editClass newClass) includes: type)  ifFalse:[
		self selectedClassOrMetaClass]).
	^true! !

