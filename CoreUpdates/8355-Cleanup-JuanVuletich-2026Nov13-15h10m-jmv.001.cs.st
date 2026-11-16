'From Cuis7.9 [latest update: #8351] on 13 November 2026 at 3:13:47 pm'!

!ClassDescription methodsFor: 'compiling' stamp: 'jmv 13/Nov/2026, 5:47:03 pm (UTC)'!
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
		updatedCode := requestor ifNil: [code] ifNotNil: [requestor text].
		updatedCode := CodeProvider normalizeCodeForSaving: updatedCode in: self.
		methodAndNode method
			writeToChangesFile: updatedCode
			class: self
			category: category
			withStamp: changeStamp ].
	self addAndClassifySelector: selector withMethod: newMethod inCategory: category methodTimeStamp: changeStamp.
	^ selector! !

