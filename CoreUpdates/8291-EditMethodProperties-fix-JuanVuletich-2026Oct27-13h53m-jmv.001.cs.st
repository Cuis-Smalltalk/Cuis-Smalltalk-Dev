'From Cuis7.9 [latest update: #8290] on 27 October 2026 at 1:54:11 pm'!

!CompiledMethod methodsFor: 'copying' stamp: 'jmv 27/Oct/2026, 4:26:49 pm (UTC)'!
postCopy
	"Use new instances."
	| props |
	props := self propertiesAsArray.
	props := props collect: [ :assoc | (Association key: assoc key value: assoc value) ].
	self methodProperties: props.! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 27/Oct/2026, 4:30:58 pm (UTC)'!
compileProperties: aString for: method
	"Keep any Pragmas, but use only newly specified Properties"

	(Compiler evaluate: aString) ifNotNil: [ :props | 
		props isArray ifTrue: [ | priorMethod |
			priorMethod := method shallowCopy.
			"method will have a new fresh instance of AdditionalMethodState,
			not shared with priorMethod at all. making priorMethod a #shallowCopy is therefore OK, "
			method methodProperties: props.
			"Save to Changes and User Changes files."
			method
				putSource: method sourceCode
				class: method methodClass
				category: (method methodClass organization categoryOfElement: method selector)
				withStamp: method authorAndStampAsSaved
				inFile: 2
				priorMethod: priorMethod.
			SystemChangeNotifier uniqueInstance
				methodChangedFrom: priorMethod
				to: method
				selector: method selector
				inClass: method methodClass.
			^ true ]].
	^false! !

!methodRemoval: SystemChangeNotifier #methodChanged: stamp: 'jmv 27/Oct/2026, 4:53:45 pm (UTC)'!
SystemChangeNotifier removeSelector: #methodChanged:!
