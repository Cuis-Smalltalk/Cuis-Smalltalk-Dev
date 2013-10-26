'From Cuis 4.0 of 21 April 2012 [latest update: #1386] on 21 August 2012 at 4:54:10 pm'!
!classDefinition: #AutoCompleterMorph category: #UCompletion!
BorderedRectMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'UCompletion'!
!classDefinition: #MagnifierMorph category: #'Morphic-Widgets'!
BorderedRectMorph subclass: #MagnifierMorph
	instanceVariableNames: 'magnification trackPointer lastPos srcExtent auxCanvas magnifiedForm '
	classVariableNames: 'RecursionLock '
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 8/21/2012 16:53'!
allInstVarNamesEverywhere
	"Answer the set of inst var names used by the receiver, all superclasses, and all subclasses"

	| aList |
	aList _ OrderedCollection new.
	(self allSuperclasses , self withAllSubclasses asOrderedCollection) do:
		[:cls | aList addAll: cls instVarNames].
	^ aList asSet

	"BorderedRectMorph allInstVarNamesEverywhere"! !


!BorderedRectMorph class methodsFor: 'instance protocol testing' stamp: 'jmv 8/21/2012 16:52'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(BorderedRectMorph)! !

!classDefinition: #MagnifierMorph category: #'Morphic-Widgets'!
BorderedRectMorph subclass: #MagnifierMorph
	instanceVariableNames: 'magnification trackPointer lastPos srcExtent auxCanvas magnifiedForm'
	classVariableNames: 'RecursionLock'
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #AutoCompleterMorph category: #UCompletion!
BorderedRectMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'UCompletion'!
!classRemoval: #BorderedMorph!
Smalltalk removeClassNamed: #BorderedMorph!
