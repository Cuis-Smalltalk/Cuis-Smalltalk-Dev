'From Cuis7.9 [latest update: #8398] on 25 November 2026 at 3:22:11 pm'!

!AdditionalMethodState methodsFor: 'copying' stamp: 'jmv 25/Nov/2026, 6:21:06 pm (UTC)'!
, arrayOfPropertyOrPragma "<Association|Pragma>"
	"Answer a copy of the receiver which includes aPropertyOrPragma"
	| bs copy |
	"arrayOfPropertyOrPragma do: [ :element |
			(#(Association Pragma)
				includes: element class name) ifFalse: [
		self error: self class name, ' instances should hold only Method Properties (Association) and Pragmas.']]."
	copy := self class new: (bs := self basicSize) + arrayOfPropertyOrPragma size.
	1 to: bs do: [ :i |
		copy basicAt: i put: (self basicAt: i) ].
	1 to: arrayOfPropertyOrPragma size do: [ :i |
		copy basicAt: bs + i put: (arrayOfPropertyOrPragma basicAt: i) ].
	^copy
		selector: selector;
		setMethod: method;
		yourself! !

