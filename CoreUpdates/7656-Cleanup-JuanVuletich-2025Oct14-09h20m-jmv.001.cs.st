'From Cuis7.5 [latest update: #7655] on 14 October 2025 at 9:20:58 am'!

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 10/14/2025 08:41:07'!
selectorDocumentationText

	| selectedEntry selectorsClasses |

	selectedEntry := completer selectedEntry.
	(completer isCategoryEntry: selectedEntry) ifTrue: [ ^'' ].
	selectedEntry := selectedEntry asSymbol.
	selectorsClasses := completer selectorsClasses.

	^ selectorsClasses isEmpty
		ifTrue: [ self selectorDocumentationTextForAllI: (Smalltalk allImplementorsOf: selectedEntry) ]
		ifFalse: [ self selectorDocumentationTextOf: selectedEntry forAll: selectorsClasses ].
	! !

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 10/14/2025 09:11:11'!
showSelectorDocumentation

	| docExtent relativeSelected dy ourBounds docPos |
	docExtent := `600@250`.
	relativeSelected := (self selected - self firstVisible) min: self itemsPerPage - 1 max: 0.
	dy := relativeSelected * self itemHeight + 1.
	ourBounds := self displayBounds.
	docPos := ourBounds right + docExtent x < self world morphWidth
		ifTrue: [ ourBounds topRight + (0@dy) ]
		ifFalse: [ ourBounds topLeft + (docExtent x negated @ dy) ].

	self setDefaultColors.

	self selectorDocumentation
		model: (TextModel withText: self selectorDocumentationText);
		position: docPos extent: docExtent;
		wrapFlag: false;
		show;
		fitInOwner.! !

!methodRemoval: SmalltalkCompleterMorph #selectorDocumentationExtent stamp: 'jmv 10/14/2025 09:20:25'!
SmalltalkCompleterMorph removeSelector: #selectorDocumentationExtent!
!methodRemoval: SmalltalkCompleterMorph #selectorDefaultDocumentationLocation stamp: 'jmv 10/14/2025 09:20:25'!
SmalltalkCompleterMorph removeSelector: #selectorDefaultDocumentationLocation!
!methodRemoval: SmalltalkCompleterMorph #selectorDocumentationTextForAllImplementorsOf: stamp: 'jmv 10/14/2025 09:20:25'!
SmalltalkCompleterMorph removeSelector: #selectorDocumentationTextForAllImplementorsOf:!
!methodRemoval: AutoCompleterMorph #isXOutOfScreen:with: stamp: 'jmv 10/14/2025 09:20:25'!
AutoCompleterMorph removeSelector: #isXOutOfScreen:with:!
!methodRemoval: AutoCompleterMorph #isYOutOfScreen:with: stamp: 'jmv 10/14/2025 09:20:25'!
AutoCompleterMorph removeSelector: #isYOutOfScreen:with:!
!methodRemoval: AutoCompleterMorph #adjust:ifOutOfScreenWith:xOffset:yOffset: stamp: 'jmv 10/14/2025 09:20:25'!
AutoCompleterMorph removeSelector: #adjust:ifOutOfScreenWith:xOffset:yOffset:!
