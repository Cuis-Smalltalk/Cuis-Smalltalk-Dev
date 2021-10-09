'From Cuis 5.0 [latest update: #4907] on 8 October 2021 at 8:24:38 pm'!
!classDefinition: #AutoCompleterMorph category: #'Tools-Autocompletion'!
WidgetMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity originalPosition showUp itemsPerPage downButtonForm upButtonForm '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 10/8/2021 20:22:46'!
drawDownArrowOn: aCanvas thickness: scrollbarThickness

	downButtonForm ifNil: [
		downButtonForm _ ScrollBar new instVarNamed: 'downButton' :: imageForm: 32 ].
	aCanvas
		image: downButtonForm
		at: self downButtonPosition.
! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 10/8/2021 20:21:15'!
drawScrollBarMovementBarOn: aCanvas thickness: scrollbarThickness
	
	| height top bottom |

	height _ extent y - (2 * scrollbarThickness).
	top _ (1.0 * self firstVisible-1 / self entryCount * height) ceiling + 1 + scrollbarThickness-1.
	bottom _ (1.0 * self lastVisible / self entryCount * height) floor + 1 + scrollbarThickness -1.
	aCanvas
		fillRectangle: (extent x - scrollbarThickness+2@top corner:  extent x-2 @ bottom)
		color: `Color veryLightGray lighter`! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 10/8/2021 20:23:23'!
drawUpArrowOn: aCanvas thickness: scrollbarThickness

	upButtonForm ifNil: [
		upButtonForm _ ScrollBar new instVarNamed: 'upButton' :: imageForm: 32 ].
	aCanvas
		image: upButtonForm
		at: self upButtonPosition.
! !

!classDefinition: #AutoCompleterMorph category: #'Tools-Autocompletion'!
WidgetMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity originalPosition showUp itemsPerPage downButtonForm upButtonForm'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!
