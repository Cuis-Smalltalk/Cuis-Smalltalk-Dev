'From Cuis7.9 [latest update: #8135] on 10 August 2026 at 12:06:26 pm'!

!ClassDescription methodsFor: 'private' stamp: 'jmv 10/Aug/2026, 3:05:56 pm (UTC)'!
logMethodSource: code forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp
	| priorMethodOrNil |

	priorMethodOrNil := self compiledMethodAt: aCompiledMethodWithNode selector ifAbsent: nil.

	aCompiledMethodWithNode method
		putSource: code
		class: self
		category: category
		withStamp: changeStamp
		inFile: 2
		priorMethod: priorMethodOrNil.
! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 10/Aug/2026, 3:06:06 pm (UTC)'!
compile: code classified: category withStamp: changeStamp methodProperties: aCollectionOrNil notifying: requestor logSource: logSource
	
	| methodAndNode selector newMethod isExistingMethod updatedCode |
	methodAndNode := self basicCompile: code notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	selector := methodAndNode selector.
	newMethod := methodAndNode method.
	isExistingMethod := self includesSelector: selector.
	isExistingMethod
		ifTrue: [
			(self theNonMetaClass isOkToChangeMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method modification not allowed']]
		ifFalse: [
			(self theNonMetaClass isOkToAddMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method addition not allowed']].

	"Any Pragmas were just added when compiling code. Add method properties now."
	aCollectionOrNil
		ifNotNil: [ aCollectionOrNil do: [ :assoc | newMethod addPragmaOrProperty: assoc]]
		ifNil: [
			"If method properties are not provided, then keep those of the existing method, if any."
			isExistingMethod ifTrue: [
				(self compiledMethodAt: selector) withPropertiesDo: [ :additionalMethodState |
					additionalMethodState propertiesDo: [ :assoc | newMethod addPragmaOrProperty: assoc ]] ifSelector: [ :s | ]]].

	logSource ifTrue: [
		"Re-fetching source code may be needed if, for example, some variable declaration was corrected during compilation."
		updatedCode := CodeProvider normalizeCodeForSaving: (requestor ifNil: [code] ifNotNil: [requestor text]) in: self.
		self logMethodSource: updatedCode forMethodWithNode: methodAndNode 
			inCategory: category withStamp: changeStamp.
	].
	self addAndClassifySelector: selector withMethod: newMethod
		inCategory: category methodTimeStamp: changeStamp.
	^ selector! !

!methodRemoval: ClassDescription #logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: stamp: 'jmv 10/Aug/2026, 3:06:08 pm (UTC)'!
ClassDescription removeSelector: #logMethodSource:forMethodWithNode:inCategory:withStamp:notifying:!
