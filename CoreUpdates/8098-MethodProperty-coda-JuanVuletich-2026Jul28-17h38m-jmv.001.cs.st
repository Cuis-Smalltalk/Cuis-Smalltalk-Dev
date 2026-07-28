'From Cuis7.9 [latest update: #8097] on 28 July 2026 at 5:40:43 pm'!

!AdditionalMethodState methodsFor: 'copying' stamp: 'jmv 28/Jul/2026, 8:38:47 pm (UTC)'!
copyWith: aPropertyOrPragma "<MethodProperty|Pragma>"
	"Answer a copy of the receiver which includes aPropertyOrPragma"
	| bs copy |
	(#(MethodProperty Pragma)
			includes: aPropertyOrPragma class name) ifFalse: [
		self error: self class name, ' instances should hold only MethodProperties and Pragmas.'].
	copy := self class new: (bs := self basicSize) + 1.
	1 to: bs do: [ :i |
		copy basicAt: i put: (self basicAt: i)].
	copy basicAt: bs + 1 put: aPropertyOrPragma.
	^copy
		selector: selector;
		setMethod: method;
		yourself
! !

!MethodProperty methodsFor: 'literals' stamp: 'jmv 28/Jul/2026, 8:51:15 pm (UTC)'!
allLiteralsDo: aBlock

	key allLiteralsDo: aBlock.
	value allLiteralsDo: aBlock.
! !

!MethodProperty reorganize!
('accessing' key key: key:value: value value:)
('literals' allLiteralsDo:)
('testing' isPragma)
!
