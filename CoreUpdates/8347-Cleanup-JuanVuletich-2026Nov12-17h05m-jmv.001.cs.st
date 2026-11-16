'From Cuis7.9 [latest update: #8342] on 12 November 2026 at 5:13:15 pm'!

!ClassDescription methodsFor: 'compiling' stamp: 'jmv 12/Nov/2026, 8:06:13 pm (UTC)'!
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
		methodAndNode method
			writeToChangesFile: updatedCode
			class: self
			category: category
			withStamp: changeStamp ].
	self addAndClassifySelector: selector withMethod: newMethod
		inCategory: category methodTimeStamp: changeStamp.
	^ selector! !

!methodRemoval: ClassDescription #logMethodSource:forMethodWithNode:inCategory:withStamp: stamp: 'jmv 12/Nov/2026, 8:06:16 pm (UTC)'!
ClassDescription removeSelector: #logMethodSource:forMethodWithNode:inCategory:withStamp:!
