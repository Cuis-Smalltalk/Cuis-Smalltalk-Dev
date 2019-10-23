'From Cuis 5.0 [latest update: #3924] on 23 October 2019 at 8:56:55 am'!

!Object methodsFor: 'error handling' stamp: 'HAW 10/23/2019 08:11:45'!
shouldNotHappen
	"Used to announce that something that should not happen by design, happened. 
	For example: (Array with: 1) at: 1 ifAbsent: [self shouldNotHappen].
	See #shouldNotHappenBecause: also"
	
	self error: self shouldNotHappenErrorMessage! !

!Object methodsFor: 'error handling' stamp: 'HAW 10/23/2019 08:12:51'!
shouldNotHappenBecause: aReason
	"Used to announce that something that should not happen by design, happened. 
	For example: (Array with: 1) at: 1 ifAbsent: [self shouldNotHappenBecause: 'The array has one element'].
	See #shouldNotHappen also"
	
	self error: self shouldNotHappenBecauseErrorMessage, aReason! !

!Object methodsFor: 'error handling' stamp: 'HAW 10/23/2019 08:13:13'!
shouldNotHappenBecauseErrorMessage

	^self shouldNotHappenErrorMessage, ' because: '! !

!Object methodsFor: 'error handling' stamp: 'HAW 10/23/2019 08:11:58'!
shouldNotHappenErrorMessage

	^'Should not happen'! !


!BlockNode methodsFor: 'code generation (closures)' stamp: 'HAW 10/23/2019 08:15:24'!
addRemoteTemp: aTempVariableNode rootNode: rootNode "<MethodNode>"
	"Add aTempVariableNode to my actualScope's sequence of
	 remote temps.  If I am an optimized block then the actual
	 scope is my actualScopeIfOptimized, otherwise it is myself."
	remoteTempNode == nil ifTrue:
		[remoteTempNode := RemoteTempVectorNode new
								name: self remoteTempNodeName
								index: arguments size + temporaries size
								type: LdTempType
								scope: 0.
		 actualScopeIfOptimized
			ifNil:
				[self addTempNode: remoteTempNode.
				 remoteTempNode definingScope: self]
			ifNotNil: [actualScopeIfOptimized addHoistedTemps: { remoteTempNode }]].
	remoteTempNode addRemoteTemp: aTempVariableNode encoder: rootNode encoder.
	"use remove:ifAbsent: because the deferred analysis for optimized
	 loops can result in the temp has already been hoised into the root."
	self removeTempNode: aTempVariableNode ifAbsent: [
		self actualScope removeTempNode: aTempVariableNode ifAbsent: [self shouldNotHappen ]].
	^remoteTempNode! !


!MethodNode methodsFor: 'source mapping' stamp: 'HAW 10/23/2019 08:14:45'!
selectorLastPosition

	"If #DoIt selector, returns 0 - Hernan"

	^self selector isUnary
		ifTrue: [ selectorKeywordsRanges ifNil: [ 0 ] ifNotNil: [ selectorKeywordsRanges last last ]]
		ifFalse: [
			(encoder 
				rangeForNode: arguments last
				ifAbsent: [ self shouldNotHappenBecause: 'arguments are part of the encoder'  ]) first last ]! !

