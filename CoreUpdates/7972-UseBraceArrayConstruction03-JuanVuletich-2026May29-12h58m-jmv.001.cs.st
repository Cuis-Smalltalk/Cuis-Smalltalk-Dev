'From Cuis7.7 [latest update: #7970] on 29 May 2026 at 1:03:14 pm'!

!VariableNode class methodsFor: 'class initialization' stamp: 'jmv 6/Feb/2024 09:39:13'!
initClassCachedState
	"
	VariableNode initialize. Decompiler initialize
	"
	| encoder |
	encoder := Encoder new.
	StdVariables := Dictionary newWithRoomForMoreThan: 16.
	encoder
		fillDict: StdVariables
		with: VariableNode
		mapping: #('self' 'thisContext' 'super' 'nil' 'false' 'true' )
		to: {LdSelf. LdThisContext. LdSuper. LdNil. LdFalse. LdTrue}.
	StdSelectors := Dictionary newWithRoomForMoreThan: 64.
	encoder
		fillDict: StdSelectors
		with: SpecialSelectorNode
		mapping: ((1 to: Smalltalk specialSelectorSize) collect: 
							[:i | Smalltalk specialSelectorAt: i])
		to: (1 to: Smalltalk specialSelectorSize) asArray.
	StdLiterals := LiteralDictionary newWithRoomForMoreThan: 16.
	encoder
		fillDict: StdLiterals
		with: LiteralNode
		mapping: #(-1 0 1 2 )
		to: (LdMinus1 to: LdMinus1 + 3).
	encoder initScopeAndLiteralTables.

	NodeNil := encoder encodeVariable: 'nil'.
	NodeTrue := encoder encodeVariable: 'true'.
	NodeFalse := encoder encodeVariable: 'false'.
	NodeSelf := encoder encodeVariable: 'self'.
	NodeThisContext := encoder encodeVariable: 'thisContext'.
	NodeSuper := encoder encodeVariable: 'super'! !


!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 20/Dec/2024 16:32:24'!
splitNameVersionExtensionFor: fileName
	" answer an array with the root name, version # and extension.
	See comment in senders for more details"

	self baseNameAndExtensionFor: fileName do: [ :b :extension |
		| baseName version i j |
		baseName := b.
		i := j := baseName findLast: [:c | c isDigit not].
		i = 0
			ifTrue: [version := 0]
			ifFalse: [
				(baseName at: i) == $.
					ifTrue: [
						version := (baseName copyFrom: i+1 to: baseName size) asNumber.
						j := j - 1]
					ifFalse: [version := 0].
				baseName := baseName copyFrom: 1 to: j ].
		^ {baseName. version. extension} ]! !

