'From Cuis7.9 [latest update: #8397] on 25 November 2026 at 10:45:29 am'!

!MethodVersionsBrowser methodsFor: 'init & update' stamp: 'jmv 25/Nov/2026, 1:34:52 pm (UTC)'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| historyPointer nextHistoryPointer methodProperties authorAndStampLocalTZ |

	selectorOfMethod := selector.
	currentCompiledMethod := method.
	classOfMethod := meta ifTrue: [class class] ifFalse: [class].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	listIndex := 0.

	historyPointer := method historyPointer.
	[ historyPointer = 0 ] whileFalse: [
		nextHistoryPointer := 0.
		CodeHistory current
			methodSourceAndMetadataFor: historyPointer
			intoMethodDefinition: [ :methodSource :authorAndStampAsSaved :methodPropertiesString |
				"If there is no source and the following are not available, they will be '' and nil."
				authorAndStampLocalTZ := CompiledMethod authorAndStampLocalTZ: authorAndStampAsSaved.
				methodProperties := Compiler evaluate: methodPropertiesString.
				self addItem:
						(MethodDefinitionChange new
							class: class name category: category meta: meta stamp: authorAndStampLocalTZ
							properties: (methodProperties ifNil: [#()])
							selector: selectorOfMethod
							string: methodSource)
					text:
						"Maybe add an indication of library from where this version was loaded."
						authorAndStampLocalTZ , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector
						, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodPropertiesString, '] '])]
			methodRemoval: [
				self addItem: (MethodDeletionChange new methodReference:
										(MethodReference class: classOfMethod selector: selectorOfMethod))
					text: '---------- method removal: ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector ]
			priorReferenceInto: [ :p |
				"If no previous version in CodeHistory, take it from method preamble in .changes if present.
				(these will most likely be completely removed in Cuis 8.0)"
				nextHistoryPointer := p ].
		CodeHistory current
			previousAndNextFrom: historyPointer
			into: [ :p :n | p = 0 ifFalse: [ nextHistoryPointer := p ]].
		historyPointer := nextHistoryPointer ].! !

