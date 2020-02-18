'From Cuis 5.0 [latest update: #4035] on 29 January 2020 at 10:25:30 am'!

!Object class methodsFor: 'compiling' stamp: 'pb 1/29/2020 10:24:14'!
isOkToAddMethod: selector isMeta: isMeta
	"A hook allowing some classes to disallow and/or react to addition of certain selectors before the change is committed
	This implementor is only for convenience, as the method is meant to be redefined in class side as needed."
	^super isOkToAddMethod: selector isMeta: isMeta! !

!Object class methodsFor: 'compiling' stamp: 'pb 1/29/2020 10:24:22'!
isOkToChangeMethod: selector isMeta: isMeta
	"A hook allowing some classes to disallow and/or react to recompilation of certain selectors before the change is committed
	This implementor is only for convenience, as the method is meant to be redefined in class side as needed."
	^super isOkToChangeMethod: selector isMeta: isMeta! !

!Object class methodsFor: 'compiling' stamp: 'pb 1/29/2020 10:24:26'!
isOkToRemoveMethod: selector isMeta: isMeta
	"A hook allowing some classes to disallow and/or react to removal of certain selectors before the change is committed
	This implementor is only for convenience, as the method is meant to be redefined in class side as needed."
	^super isOkToRemoveMethod: selector isMeta: isMeta! !


!ClassDescription methodsFor: 'compiling' stamp: 'pb 1/29/2020 10:09:40'!
isOkToAddMethod: selector isMeta: isMeta
	"A hook allowing some classes to disallow and/or react to addition of certain selectors before the change is committed"
	^true! !

!ClassDescription methodsFor: 'compiling' stamp: 'pb 1/29/2020 10:24:45'!
isOkToChangeMethod: selector isMeta: isMeta
	"A hook allowing some classes to disallow and/or react to recompilation of certain selectors before the change is committed"
	^true! !

!ClassDescription methodsFor: 'compiling' stamp: 'pb 1/29/2020 10:10:16'!
isOkToRemoveMethod: selector isMeta: isMeta
	"A hook allowing some classes to disallow and/or react to removal of certain selectors before the change is committed"
	^true! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'pb 1/29/2020 10:21:42'!
removeSelector: selector 
	| priorMethod priorProtocol | 
	"Remove the message whose selector is given from the method 
	dictionary of the receiver, if it is there. Answer nil otherwise."

	priorMethod _ self compiledMethodAt: selector ifAbsent: [^ nil].
	(self theNonMetaClass isOkToRemoveMethod: selector isMeta: self isMeta)
		ifFalse: [self error: 'Method removal not allowed'].
	priorProtocol _ self whichCategoryIncludesSelector: selector.
	SystemChangeNotifier uniqueInstance doSilently: [
		self organization removeElement: selector].
	super removeSelector: selector.
	SystemChangeNotifier uniqueInstance 
			methodRemoved: priorMethod selector: selector inProtocol: priorProtocol class: self.! !

!ClassDescription methodsFor: 'compiling' stamp: 'pb 1/29/2020 10:21:23'!
compile: text classified: category withStamp: changeStamp notifying: requestor logSource: logSource
	| methodAndNode selector isExistingMethod |
	methodAndNode _ self basicCompile: text asString notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	selector _ methodAndNode selector.
	isExistingMethod _ self includesSelector: selector.
	isExistingMethod
		ifTrue: [
			(self theNonMetaClass isOkToChangeMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method modification not allowed']]
		ifFalse: [
			(self theNonMetaClass isOkToAddMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method addition not allowed']].
	logSource ifTrue: [
		self logMethodSource: text forMethodWithNode: methodAndNode 
			inCategory: category withStamp: changeStamp notifying: requestor.
	].
	self addAndClassifySelector: selector withMethod: methodAndNode 
		method inProtocol: category notifying: requestor.
	^ methodAndNode selector! !


!Object class reorganize!
('instance creation' initializedInstance newFrom: readFrom: unStream:)
('documentation' howToModifyPrimitives whatIsAPrimitive)
('object serialization' createFrom:size:version:)
('class initialization' initialize)
('windowColor' windowColor)
('package support' package)
('compiling' isOkToAddMethod:isMeta: isOkToChangeMethod:isMeta: isOkToRemoveMethod:isMeta:)
!

