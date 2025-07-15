'From Cuis7.5 [latest update: #7353] on 15 July 2025 at 10:16:29 am'!
!classDefinition: #MethodChangeRecord category: #'Tools-Changes'!
Object subclass: #MethodChangeRecord
	instanceVariableNames: 'changeType currentMethod infoFromRemoval stamp '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!MethodChangeRecord commentStamp: '<historical>' prior: 0!
MethodChangeRecords are used to record method changes.  Here is a simple summary of the relationship between the changeType symbol and the recording of prior state

					|	prior == nil			|	prior not nil	
	---------	|----------------------------	|--------------------
			add		|	add					|	change
	---------	|----------------------------	|--------------------
			remove	|	addedThenRemoved	|	remove

Structure:
changeType		symbol -- as summarized above
currentMethod	method
				This is the current version of the method.
				It can be used to assert this change upon entry to a layer. 
infoFromRemoval -- an array of size 2.
				The first element is the source index of the last version of the method.
				The second element is the category in which it was defined, so it
				can be put back there if re-accepted from a version browser.

Note that the above states each have an associated revoke action:
	add --> remove
	change --> change back
	remove --> add back
	addedThenRemoved --> no change
However all of these are accomplished trivially by restoring the original method dictionary.!


!ClassChangeRecord methodsFor: 'method changes' stamp: 'jmv 7/15/2025 09:02:59'!
noteNewMethod: newMethod selector: selector priorMethod: methodOrNil

	| methodChange |
	methodChange := self findOrMakeMethodChangeAt: selector priorMethod: methodOrNil.
	methodOrNil
		ifNil: [ methodChange noteChangeType: #add ]
		ifNotNil: [ methodChange noteChangeType: #change ].
! !

!methodRemoval: MethodChangeRecord #storeDataOn: stamp: 'jmv 7/15/2025 10:16:14'!
MethodChangeRecord removeSelector: #storeDataOn:!
!methodRemoval: MethodChangeRecord #currentMethod stamp: 'jmv 7/15/2025 10:16:14'!
MethodChangeRecord removeSelector: #currentMethod!
!methodRemoval: MethodChangeRecord #noteNewMethod: stamp: 'jmv 7/15/2025 10:16:14'!
MethodChangeRecord removeSelector: #noteNewMethod:!
!methodRemoval: ClassChangeRecord #compileAll:from: stamp: 'jmv 7/15/2025 10:16:14'!
ClassChangeRecord removeSelector: #compileAll:from:!
!classDefinition: #MethodChangeRecord category: #'Tools-Changes'!
Object subclass: #MethodChangeRecord
	instanceVariableNames: 'changeType infoFromRemoval stamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
