'From Cuis7.9 [latest update: #8290] on 27 October 2026 at 1:54:58 pm'!

!MethodVersionsBrowser methodsFor: 'init & update' stamp: 'jmv 27/Oct/2026, 4:45:31 pm (UTC)'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| methodProperties historyPointer |

	selectorOfMethod := selector.
	currentCompiledMethod := method.
	classOfMethod := meta ifTrue: [class class] ifFalse: [class].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	listIndex := 0.

	historyPointer := method historyPointer.
	[ historyPointer = 0 ] whileFalse: [
		CodeHistory current
			methodSourceAndMetadataFor: historyPointer
			into: [ :methodSource :stampString :methodPropertiesString :prevHistoryPointer :nextHistoryPointer |
				methodSource
					ifNil: [
						self addItem: (MethodDeletionChange new methodReference:
									(MethodReference class: classOfMethod selector: selectorOfMethod))
							text: '---------- method removal: ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector ]
					ifNotNil: [
						methodProperties := Compiler evaluate: methodPropertiesString.
						self addItem:
								(MethodDefinitionChange new
									class: class name category: category meta: meta stamp: stampString
									properties: (methodProperties ifNil: [#()])
									selector: selectorOfMethod
									string: methodSource)
							text: stampString , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector
								, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodPropertiesString, '] ']) ].
				historyPointer := prevHistoryPointer ]].
! !

