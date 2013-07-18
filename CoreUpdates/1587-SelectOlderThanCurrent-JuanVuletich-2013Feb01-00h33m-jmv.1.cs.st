'From Cuis 4.1 of 12 December 2012 [latest update: #1586] on 1 February 2013 at 1:02:36 am'!

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 2/1/2013 01:01'!
selectMethodsOlderThanCurrent
	"Selects all method definitions for which there is some newer counterpart in the current image.
	This is, select methods that installing would be go back in time"

	| cm doSelect |
	Cursor read showWhile: [
		| change class |
		1 to: changeList size do: [ :i | 
			change _ changeList at: i.
			doSelect _ false.
			(change changeType == #method and: [
				((class _ change changeClass) notNil)]) ifTrue: [
					cm _ class compiledMethodAt: change methodSelector ifAbsent: nil.
					cm ifNotNil: [
						doSelect _ change dateAndTime < cm dateAndTime ].
					].
			listSelections at: i put: doSelect ]].
	self changed: #allSelections.
	self changed: #annotation! !


!ChangeRecord methodsFor: 'access' stamp: 'jmv 2/1/2013 01:00'!
dateAndTime

	^CompiledMethod timeStamp: self stamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ]! !


!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 2/1/2013 00:58'!
dateAndTime
	"Answer a DateAndTime object indicating when a method was last saved.  If there is no date stamp, return nil"
	"
	(CompiledMethod compiledMethodAt: #dateAndTime) dateAndTime
	"

	^self class timeStamp: self timeStamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ]! !


!CompiledMethod class methodsFor: 'services' stamp: 'jmv 2/1/2013 00:56'!
timeStamp: aStamp partsDo: aBlock
	"
	CompiledMethod>>#timeStampPartsDo: timeStampPartsDo: [ :authorInitials :dateAndTime |
		('*',authorInitials,'*') print. dateAndTime print ]
	"
	| stamp dateIndex aux dateAndTimePart |
	stamp _ aStamp.
	"Account for some unfortunately hacked stamps such as this: 'cwp 10/17/2007 23:29/eem 9/5/2009 11:10 - => :=' "
	aux _ stamp lastIndexOf: $/.
	aux _ stamp indexOf: $: startingAt: aux ifAbsent: [ 0 ].
	aux > 0 ifTrue: [
		(aux > 0 and: [ aux + 2 ~= stamp size]) ifTrue: [
			stamp _ stamp copyFrom: 1 to: aux + 2 ]].

	"Find start of date, if not possible, consider the whole stamp the author, and no date"
	aux _ stamp
		lastIndexOf: $/
		startingAt: stamp size
		ifAbsent: [ ^ aBlock value: stamp value: nil ].
	aux _ stamp
		lastIndexOf: $/
		startingAt: aux
		ifAbsent: [ ^ aBlock value: stamp value: nil ].
	dateIndex _ stamp
		lastIndexOf: $ 
		startingAt: aux
		ifAbsent: [ ^ aBlock value: stamp value: nil ].
	"If only date, no time, add midnight time"
	dateAndTimePart _ stamp copyFrom: dateIndex + 1 to: stamp size.
	(dateAndTimePart indexOf: $:) = 0 ifTrue:[
		dateAndTimePart _ dateAndTimePart, ' 00:00' ].
	"Done"
	^aBlock
		value: (stamp copyFrom: 1 to: dateIndex-1)
		value: (DateAndTime fromString: dateAndTimePart)! !


!ChangeListWindow methodsFor: 'menu building' stamp: 'jmv 2/1/2013 00:45'!
listMenu
	"Fill aMenu up so that it comprises the primary changelist-browser menu"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'change list'.
	aMenu addStayUpIcons.
	aMenu addList: #(
	('fileIn selections'							fileInSelections							
			'import the selected items into the image'																		model)
	('fileOut selections...	'					fileOutSelections						
			'create a new file containing the selected items'																model)
	-
	('compare to current'						compareToCurrentVersion			
			'open a separate window which shows the text differences between the on-file version and the in-image version.' model)
	('toggle diffing (D)'							toggleDiffing							
			'start or stop showing diffs in the code pane.'																	model)
	-
	('select unchanged methods'				selectUnchangedMethods				
			'select methods in the file whose in-image versions are the same as their in-file counterparts'					model)
	('select methods equivalent to current'		selectEquivalentMethods				
			'select methods in the file whose in-image versions have the same behavior as their in-file counterparts'		model)
	('select new methods'						selectNewMethods						
			'select methods in the file that do not currently exist in the image'												model)
	('select methods older than current'		selectMethodsOlderThanCurrent						
			'select methods in the file that are older than the one currently in the image'									model)
	('select changes for absent classes'		selectAllForAbsentClasses		
			'select methods in the file for classes that are not defined in the image'										model)
	('select all changes for this class'			selectAllForThisClass			
			'select all methods in the file that belong to the currently-selected class'										model)
	('select removals of sent methods'			selectRemovalsOfSent					
			'select all method removals of methods that have some sender in the image'									model)
	-
	('select all (a)'								selectAll									
			'select all the items in the list'																					model)
	('deselect all'								deselectAll								
			'deselect all the items in the list'																				model)
	('invert selections'							invertSelections							
			'select every item that is not currently selected, and deselect every item that *is* currently selected'		model)
	-
	('browse all versions of single selection'	browseVersions							
			'open a version browser showing the versions of the currently selected method')
	('browse current versions of selections'	browseCurrentVersionsOfSelections 
			'open a message-list browser showing the current (in-image) counterparts of the selected methods')
	('destroy current methods of selections'	destroyCurrentCodeOfSelections	
			'remove (*destroy*) the in-image counterparts of all selected methods'										model)
	-
	('remove doIts'								removeDoIts								
			'remove all items that are doIts rather than definitions'															model)
	('remove older versions'					removeOlderMethodVersions			
			'remove all but the most recent versions of methods in the list'												model)
	('remove up-to-date versions'				removeUpToDate		
			'remove all items whose code is the same as the counterpart in-image code'									model)
	('remove empty class comments'			removeEmptyClassComments			
			'remove all empty class comments'																			model)
	('remove selected items'					removeSelections						
			'remove the selected items from the change-list'																model)
	('remove unselected items'					removeNonSelections					
			'remove all the items not currently selected from the change-list'												model)).
	^ aMenu! !


!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 2/1/2013 00:57'!
author
	"
	(CompiledMethod compiledMethodAt: #timeStamp) author
	(BitBlt>>#destRect) author
	"
	
	^self class timeStamp: self timeStamp partsDo: [ :authorInitials :dateAndTime | authorInitials ]! !

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 2/1/2013 00:58'!
dateMethodLastSubmitted
	"Answer a Date object indicating when a method was last saved.  If there is no date stamp, return nil"
	"
	(CompiledMethod compiledMethodAt: #dateMethodLastSubmitted) dateMethodLastSubmitted
	"

	^self class timeStamp: self timeStamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ifNotNil: [ dateAndTime date ]]! !

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 2/1/2013 00:59'!
dateSortingValue
	"Answer an integer that is suitable for chronologically sorting methods.
	It is the number of whole minutes since 'The dawn of Squeak history'
	"
	"
	(CompiledMethod compiledMethodAt: #dateAndTime) dateSortingValue
	"

	^self class timeStamp: self timeStamp partsDo: [ :authorInitials :dateAndTime |
		dateAndTime
			ifNil: [ 0 ]
			ifNotNil: [ (dateAndTime - (DateAndTime fromString: '01/01/1996 00:00')) totalMinutes max: 0 ]]! !

!methodRemoval: CompiledMethod #dateAndtime!
CompiledMethod removeSelector: #dateAndtime!
!methodRemoval: CompiledMethod #timeStampPartsDo:!
CompiledMethod removeSelector: #timeStampPartsDo:!
